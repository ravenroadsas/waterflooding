# STEP 4 - JOBS: portfolio, approval (engineers propose, a lead approves), execution, tracking ------

jobs_ui <- function() {
  htmltools::tagList(
    htmltools::div(class = "wf-section-intro",
      htmltools::div(class = "wf-step", "Step 4 of 4"), htmltools::h2("Jobs"),
      htmltools::p("A job is one rig visit on one well, combining the opportunities an engineer picked. Engineers propose, a lead approves;",
                   "the forecast is frozen at approval and the executed job is evaluated against it."),
      htmltools::div(class = "wf-inline-tools", shiny::textInput("job_user", "Acting as", Sys.getenv("USER", "engineer"), width = "200px"),
                     shiny::uiOutput("job_role", inline = TRUE))),
    shiny::uiOutput("job_funnel"),
    bslib::layout_columns(col_widths = c(6, 6),
      bslib::navset_card_underline(id = "job_tabs",
        bslib::nav_panel("Portfolio", value = "pf",
          htmltools::p(class = "wf-muted small", "Proposed and approved jobs. Risked oil = P(success) × Base oil in 12 months; size = cost; orange = triggered by an opportunity."),
          plotly::plotlyOutput("job_pf", height = "330px"), DT::DTOutput("job_rank")),
        bslib::nav_panel("Board", value = "board", shiny::uiOutput("job_board")),
        bslib::nav_panel("Past interventions", value = "hist", DT::DTOutput("job_hist_table"))),
      shiny::uiOutput("job_detail")))
}

jobs_server <- function(input, output, session, ctx) {
  sel_job <- shiny::reactiveVal(NULL)
  bump <- function() ctx$store_tick(ctx$store_tick() + 1)
  jobs <- shiny::reactive({ ctx$store_tick(); store_jobs(ctx$con) })
  output$job_role <- shiny::renderUI(if (job_can_approve(ctx$user())) badge(if (nzchar(Sys.getenv("WF_LEADS"))) "lead: can approve" else "can approve (WF_LEADS not set)", pal$ok)
                                     else badge("engineer: proposes", "#94a3b8"))

  # live portfolio numbers of open jobs (current forecasts), frozen numbers for the rest
  pf <- shiny::reactive({
    j <- jobs(); if (!nrow(j)) return(j)
    op <- ctx$opps(); oc <- store_outcomes(ctx$con)
    rows <- lapply(seq_len(nrow(j)), function(i) {
      it <- store_job_items(ctx$con, j$id[i])
      p <- tryCatch(job_proposal(op, it$opp_key, ctx$res(), ctx$asof(), ctx$settings(), oc, isTRUE(j$als_change[i] == 1)), error = function(e) NULL)
      data.table::data.table(id = j$id[i], n_items = nrow(it), content = paste(sprintf("%s %s", action_job(it$action), data.table::fcoalesce(it$interval_id, "")), collapse = " + "),
                             np12 = p$np12 %||% NA_real_, risked_np12 = p$risked_np12 %||% NA_real_, ps = p$ps %||% NA_real_, unc = p$unc %||% NA_real_,
                             qo_base = p$qo[["Base"]] %||% NA_real_)
    })
    x <- merge(j, data.table::rbindlist(rows), by = "id")
    x[, triggers := data.table::fcoalesce(triggers, "")]
    score_jobs(x, ctx$settings())
  })

  output$job_funnel <- shiny::renderUI({
    j <- jobs()
    n <- table(factor(j$status, job_status_levels))
    htmltools::div(class = "wf-funnel", lapply(job_status_levels[1:5], function(k) htmltools::div(class = "wf-fs", style = sprintf("--fc:%s", job_status_colors[[k]]),
      htmltools::div(class = "l", k), htmltools::tags$b(n[[k]]))))
  })

  open_pf <- shiny::reactive({ x <- pf(); if (!nrow(x)) x else x[status %in% c("proposed", "approved")][order(-score)] })
  output$job_pf <- plotly::renderPlotly({
    x <- open_pf(); if (!nrow(x) || !any(is.finite(x$risked_np12))) return(empty_plot("No proposed or approved jobs yet: propose one from Opportunities › ADPERF workbench"))
    x[, sz := 10 + 30 * sqrt(cost_usd / max(cost_usd, na.rm = TRUE))]
    p <- plotly::plot_ly(source = "jobpf")
    for (tg in c(FALSE, TRUE)) {
      d <- x[(nzchar(triggers)) == tg]
      if (!nrow(d)) next
      p <- plotly::add_markers(p, data = d, x = ~risked_np12, y = ~unc, customdata = ~id, name = if (tg) "triggered by an opportunity" else "planned",
        marker = list(size = ~sz, color = if (tg) "#f97316" else "#3b82f6", opacity = 0.85, line = list(color = pal$bg, width = 1)),
        text = ~sprintf("#%s %s · %s<br>%s<br>risked oil 12 m %s stb · cost %s k USD · P %s%s<br>score %s", id, well, status, content, fmt_num(risked_np12),
                        fmt_num(cost_usd / 1000), fmtn(ps), ifelse(nzchar(triggers), paste0("<br>trigger: ", triggers), ""), score), hoverinfo = "text")
    }
    p <- plotly::layout(pl_theme(p, "Risked oil in 12 months, stb", "(Alto − Bajo) / Base"),
                        xaxis = axis_style("Risked oil in 12 months, stb", rangemode = "tozero"), yaxis = axis_style("(Alto − Bajo) / Base", rangemode = "tozero"))
    plotly::event_register(p, "plotly_click")
  })
  shiny::observeEvent(plotly::event_data("plotly_click", source = "jobpf"), { k <- plotly::event_data("plotly_click", source = "jobpf")$customdata; if (length(k)) sel_job(as.integer(unlist(k)[1])) })
  output$job_rank <- DT::renderDT({
    x <- open_pf(); if (!nrow(x)) return(dt_dark(data.table::data.table(Message = "No proposed or approved jobs"), dom = "t"))
    dt_dark(x[, .(`#` = id, Well = well, Status = status, Content = content, `Risked oil 12 m` = fmt_num(risked_np12), `Cost k USD` = fmt_num(cost_usd / 1000),
                  `P(success)` = fmtn(ps), Trigger = ifelse(nzchar(triggers), triggers, "planned"), Score = score)], dom = "tip", pageLength = 10, ordering = FALSE)
  })
  shiny::observeEvent(input$job_rank_rows_selected, sel_job(open_pf()$id[input$job_rank_rows_selected]))

  output$job_board <- shiny::renderUI({
    x <- pf(); if (!nrow(x)) return(htmltools::div(class = "wf-muted", "No jobs yet."))
    htmltools::div(class = "wf-board", lapply(job_status_levels[1:5], function(k) {
      d <- x[status == k]
      htmltools::div(class = "wf-col", htmltools::div(class = "wf-col-h", style = sprintf("--fc:%s", job_status_colors[[k]]), sprintf("%s (%d)", k, nrow(d))),
        lapply(seq_len(nrow(d)), function(i) htmltools::tags$a(class = "wf-bcard", href = "#",
          onclick = sprintf("Shiny.setInputValue('job_pick', %d, {priority: 'event'}); return false;", d$id[i]),
          htmltools::tags$b(sprintf("#%d %s", d$id[i], d$well[i])), htmltools::span(d$content[i]),
          htmltools::span(class = "wf-muted small", sprintf("score %s%s", d$score[i], if (nzchar(d$triggers[i])) paste(" ·", d$triggers[i]) else "")))))
    }))
  })
  shiny::observeEvent(input$job_pick, sel_job(as.integer(input$job_pick)))
  # a newly proposed job comes into focus (also the first time the page opens)
  last_max <- shiny::reactiveVal(0)
  shiny::observeEvent(jobs(), { j <- jobs(); if (!nrow(j)) return(); m <- max(j$id); if (m > last_max()) { sel_job(m); last_max(m) } })

  cur <- shiny::reactive({ id <- sel_job(); ctx$store_tick(); if (is.null(id)) NULL else store_job(ctx$con, id) })
  cur_pf <- shiny::reactive({ x <- pf(); id <- sel_job(); if (is.null(id) || !nrow(x)) NULL else x[id == sel_job()] })
  evaluation <- shiny::reactive({ j <- cur(); if (is.null(j) || !j$status %in% c("executed", "evaluated")) NULL else job_evaluate(ctx$con, ctx$res(), j$id, ctx$asof()) })

  output$job_detail <- shiny::renderUI({
    j <- cur(); if (is.null(j)) return(bslib::card(bslib::card_body(htmltools::div(class = "wf-muted", "Select a job."))))
    p <- cur_pf(); lead <- job_can_approve(ctx$user())
    v <- if (!is.null(evaluation())) evaluation()$verdict else NULL
    bslib::card(bslib::card_body(
      htmltools::div(class = "wf-rec-head",
        htmltools::h4(sprintf("#%d %s · %s", j$id, j$name, j$well)), badge(j$status, job_status_colors[[j$status]]),
        if (nzchar(j$triggers %||% "")) badge(paste("✕", j$triggers), pal$crit),
        if (isTRUE(j$als_change == 1)) badge("lift change", "#c084fc"),
        shiny::actionLink("job_w360", "Well 360 →", class = "wf-link")),
      htmltools::div(class = "wf-muted small", sprintf("Proposed by %s on %s%s", j$created_by, substr(j$created, 1, 10),
        if (!is.na(j$approved_by)) sprintf(" · approved by %s on %s", j$approved_by, substr(j$approved, 1, 10)) else "")),
      if (!is.null(p) && nrow(p)) htmltools::div(class = "wf-kpi-row compact",
        kpi("Oil 12 m", fmt_num(p$np12), sprintf("risked %s", fmt_num(p$risked_np12))), kpi("Cost", sprintf("%s k", fmt_num(j$cost_usd / 1000)), "USD"),
        kpi("P(success)", fmtn(p$ps)), kpi("Uncertainty", fmtn(p$unc), "(Alto − Bajo) / Base"), kpi("Score", p$score)),
      DT::DTOutput("job_items"),
      plotly::plotlyOutput("job_fc", height = "250px"),
      if (!is.null(v)) htmltools::div(badge(v$verdict, switch(v$verdict, "above Alto" = pal$ok, "within range" = pal$accent, "below Bajo" = pal$crit, pal$muted)),
                                      if (is.finite(v$ratio)) badge(sprintf("actual / Base %s over %s months", fmtn(v$ratio), v$months), pal$warn)),
      shiny::textAreaInput("job_comment", "Comment", rows = 2, width = "100%"),
      htmltools::div(class = "wf-btn-row",
        if (j$status == "proposed") htmltools::tagList(
          if (lead) shiny::actionButton("job_approve", "Approve", class = "btn-sm btn-primary") else htmltools::span(class = "wf-muted small", "Waiting for a lead's approval."),
          if (lead) shiny::actionButton("job_reject", "Reject", class = "btn-sm btn-outline-light")),
        if (j$status == "approved") htmltools::tagList(shiny::dateInput("job_date", NULL, value = ctx$asof(), width = "140px"),
                                                        shiny::actionButton("job_exec", "Mark executed", class = "btn-sm btn-primary")),
        if (j$status == "executed" && !is.null(v) && v$verdict %in% c("above Alto", "within range", "below Bajo"))
          shiny::actionButton("job_eval", "Record evaluation", class = "btn-sm btn-primary"),
        if (j$status %in% c("proposed", "approved")) shiny::actionButton("job_cancel", "Cancel job", class = "btn-sm btn-outline-light"),
        if (j$status == "rejected") shiny::actionButton("job_reopen", "Propose again", class = "btn-sm btn-outline-info")),
      htmltools::h6(class = "wf-h6", "History"), DT::DTOutput("job_history")))
  })
  output$job_items <- DT::renderDT({
    j <- cur(); shiny::req(j); it <- store_job_items(ctx$con, j$id)
    dt_dark(it[, .(Item = action_label(action), Unit = sand, Interval = interval_id, `qo bopd` = round(qo), `qw bwpd` = round(qw), `Oil 12 m` = fmt_num(np12), `P(success)` = round(ps, 2))],
            dom = "t", ordering = FALSE)
  })
  output$job_fc <- plotly::renderPlotly({
    j <- cur(); shiny::req(j)
    e <- evaluation()$eval
    if (!is.null(e) && nrow(e)) {
      p <- plotly::plot_ly(e, x = ~month)
      if (any(is.finite(e$fc_alto))) p <- plotly::add_ribbons(p, ymin = ~fc_bajo, ymax = ~fc_alto, name = "Bajo-Alto (frozen)", fillcolor = "rgba(34,211,238,0.12)", line = list(width = 0))
      if (any(is.finite(e$fc_base))) p <- plotly::add_lines(p, y = ~fc_base, name = "Base (frozen)", line = list(color = pal$oil, dash = "dash"))
      p <- plotly::add_lines(p, y = ~inc_qo, name = "Actual incremental", line = list(color = pal$warn, width = 2.5))
      return(pl_theme(p, "Month after the job", "Oil, bopd"))
    }
    fr <- store_frozen(ctx$con, paste0("JOB:", j$id))
    if (is.null(fr)) return(empty_plot("No forecast for this job"))
    plot_forecast(fr)
  })
  output$job_history <- DT::renderDT({ j <- cur(); shiny::req(j); dt_dark(store_job_history(ctx$con, j$id)[, .(ts, from = from_status, to = to_status, user, comment)], dom = "t", ordering = FALSE) })
  output$job_hist_table <- DT::renderDT({
    a <- ctx$res()$ds$interventions
    if (is.null(a) || !nrow(a)) return(dt_dark(data.table::data.table(Message = "No Interventions table loaded"), dom = "t"))
    dt_dark(a[order(-date)], pageLength = 15)
  })

  act <- function(to, date = NULL) {
    j <- cur(); shiny::req(j)
    r <- tryCatch({ job_set_status(ctx$con, j$id, to, ctx$user(), input$job_comment %||% "", ctx$opps(), ctx$res(), date); TRUE },
                  error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); FALSE })
    if (r) { bump(); shiny::showNotification(sprintf("Job #%d %s", j$id, to)) }
  }
  shiny::observeEvent(input$job_approve, act("approved"))
  shiny::observeEvent(input$job_reject, act("rejected"))
  shiny::observeEvent(input$job_cancel, act("cancelled"))
  shiny::observeEvent(input$job_reopen, act("proposed"))
  shiny::observeEvent(input$job_exec, act("executed", input$job_date))
  shiny::observeEvent(input$job_eval, {
    j <- cur(); v <- evaluation()$verdict; shiny::req(j, v)
    for (k in store_job_items(ctx$con, j$id)$opp_key) {
      store_add_outcome(ctx$con, k, v$verdict, "frozen job forecast", sprintf("actual / Base %s", fmtn(v$ratio)), j$name)
      store_set_state(ctx$con, k, status = "outcome_evaluated", user = ctx$user(), comment = paste(v$verdict, "·", j$name))
    }
    act("evaluated")
  })
  shiny::observeEvent(input$job_w360, { j <- cur(); if (!is.null(j)) ctx$open_w360(j$well) })
  invisible(sel_job)
}
