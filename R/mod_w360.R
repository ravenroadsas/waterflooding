# WELL 360: side-panel drill-down for one well (any drive) -------------------------------------

w360_modal <- function(well, crumb) {
  shiny::modalDialog(title = NULL, size = "xl", easyClose = TRUE, footer = NULL,
    htmltools::div(class = "wf-drawer-mark wf-p360",
      htmltools::div(class = "wf-crumb", lapply(crumb, function(x) htmltools::tagList(htmltools::span(x), "›")), htmltools::tags$b(well),
                     htmltools::span(style = "margin-left:auto"), shiny::modalButton("Close")),
      shiny::uiOutput("w360_head"),
      bslib::navset_card_underline(id = "w360_tabs",
        bslib::nav_panel("Opportunities", DT::DTOutput("w360_opps")),
        bslib::nav_panel("History", plotly::plotlyOutput("w360_hist", height = "360px")),
        bslib::nav_panel("Intervals", bslib::layout_columns(col_widths = c(5, 7),
          plotly::plotlyOutput("w360_strip", height = "360px"), DT::DTOutput("w360_iv"))),
        bslib::nav_panel("Forecasts", shiny::uiOutput("w360_fc_pick"), plotly::plotlyOutput("w360_fc", height = "330px")),
        bslib::nav_panel("Patterns", shiny::uiOutput("w360_pat_note"), DT::DTOutput("w360_pats")),
        bslib::nav_panel("Jobs", DT::DTOutput("w360_jobs")))))
}

w360_server <- function(input, output, session, ctx, wsel) {
  r <- shiny::reactive({ shiny::req(wsel()); ctx$res() })
  hist <- shiny::reactive(well_history(r(), wsel(), ctx$asof()))
  ivw <- shiny::reactive({ iv <- r()$ds$intervals; if (is.null(iv)) NULL else iv[well == wsel()] })
  ops <- shiny::reactive({ s <- ctx$opps()$summary; if (!nrow(s)) s else s[well == wsel()] })
  wp <- shiny::reactive(well_patterns(r(), ctx$asof())[well == wsel()])

  output$w360_head <- shiny::renderUI({
    w <- wsel(); h <- hist(); m <- r()$well_master[well == w]; x <- wp()
    last <- if (nrow(h)) utils::tail(h, 6) else NULL
    wc <- if (!is.null(last)) sum(last$bwpd) / max(sum(last$bopd + last$bwpd), 1e-6) else NA_real_
    drive <- if (nrow(x[flooded == TRUE])) x[flooded == TRUE]$mechanism[1] else "primary"
    np <- if (nrow(h)) sum(h$bopd * days_in_month(h$date)) else NA_real_
    htmltools::div(class = "wf-p360-head",
      htmltools::h3(paste("Well", w)), badge(tolower(m$well_type %||% "producer"), "#94a3b8"),
      badge(drive, if (drive == "primary") "#94a3b8" else "#60a5fa"),
      if (nrow(x)) badge(paste("patterns", paste(sprintf("%s %s", x$pattern, fmt_pct(x$coeff, 0)), collapse = " · ")), "#a78bfa"),
      if (nrow(ops())) badge(sprintf("%d opportunities", nrow(ops())), pal$accent),
      if (nrow(ops()[as.character(status) %in% c("screening_only", "candidate", "validated_candidate")]))
        shiny::actionButton("w360_job", "Build a job on this well", class = "btn-sm btn-primary"),
      htmltools::div(class = "wf-kpi-row compact",
        kpi("Oil (6 m)", fmt_int(if (!is.null(last)) mean(last$bopd) else NA), "bopd"),
        kpi("Water (6 m)", fmt_int(if (!is.null(last)) mean(last$bwpd) else NA), "bwpd"),
        if (!is.null(last) && any(last$bwipd > 0)) kpi("Injection (6 m)", fmt_int(mean(last$bwipd)), "bwipd"),
        kpi("Water cut", fmt_pct(wc)), kpi("Cum oil", fmt_num(np), "stb"),
        kpi("Intervals", if (is.null(ivw())) "–" else nrow(ivw()), if (!is.null(ivw())) paste(sum(ivw()$estado != "abierto"), "closed / partial"))))
  })
  output$w360_opps <- DT::renderDT({
    s <- ops()
    if (!nrow(s)) return(dt_dark(data.table::data.table(Message = "No opportunities on this well at this date"), dom = "t"))
    dt_dark(s[, .(Action = paste(action, "·", action_label(action)), Unit = sand, Interval = interval, Lenses = lenses, Evidence = families,
                  Status = as.character(status), `Gain bopd` = round(gain), `Oil 12 m` = round(np12), Score = score)], dom = "t")
  })
  shiny::observeEvent(input$w360_opps_rows_selected, { ctx$open_opp(ops()$key[input$w360_opps_rows_selected]); shiny::removeModal() })
  output$w360_hist <- plotly::renderPlotly({
    iv <- c(ctx$res()$ds$interventions[well == wsel(), date], store_interventions(ctx$con)[well == wsel(), date])
    p <- plot_well_history(hist(), 30)
    if (length(iv)) p <- plotly::layout(p, shapes = lapply(iv, vline_shape, col = pal$warn))
    p
  })
  shiny::observeEvent(input$w360_job, {
    s <- ops()[as.character(status) %in% c("screening_only", "candidate", "validated_candidate")]
    shiny::removeModal(); ctx$job_req(list(well = wsel(), pre = character(), t = Sys.time()))
  })
  output$w360_strip <- plotly::renderPlotly(plot_interval_strip(ivw()))
  output$w360_iv <- DT::renderDT({
    x <- ivw(); if (is.null(x) || !nrow(x)) return(dt_dark(data.table::data.table(Message = "No interval analysis for this well"), dom = "t"))
    dt_dark(x[, .(Unit = sand, Interval = interval_id, Top = top_ft, Base = base_ft, State = estado, `h net` = h_net_ft, kh = kh_md_ft,
                  `Sw LAS` = sw_las, `Sw act` = sw_act, `Np/OOIP` = round(np_ooip, 2), `BSW %` = round(bsw0_pct), qo = qo0, qw = qw0,
                  `EUR stb` = eur_stb, QA = qa)], dom = "t", pageLength = 30)
  })
  fc_keys <- shiny::reactive({
    pf <- ctx$res()$ds$profiles; if (is.null(pf)) return(character())
    unique(pf[well == wsel(), profile_key(well, sand, interval_id)])
  })
  output$w360_fc_pick <- shiny::renderUI({
    k <- fc_keys(); if (!length(k)) return(htmltools::div(class = "wf-muted", "No Bajo / Base / Alto profiles for this well"))
    shiny::selectInput("w360_fc_key", NULL, k, width = "320px")
  })
  output$w360_fc <- plotly::renderPlotly({
    shiny::req(input$w360_fc_key %in% fc_keys())
    plot_forecast(profile_rows(ctx$res()$ds$profiles, input$w360_fc_key))
  })
  output$w360_pat_note <- shiny::renderUI({
    if (nrow(wp()[flooded == TRUE])) NULL else htmltools::p(class = "wf-muted", "This well is in no waterflood pattern under injection: it is produced on primary. Pattern support evidence does not apply.")
  })
  output$w360_pats <- DT::renderDT({
    x <- wp(); if (!nrow(x)) return(dt_dark(data.table::data.table(Message = "No pattern allocation"), dom = "t"))
    u <- ctx$units_snap()
    d <- merge(x[, .(pattern, coeff, mechanism, flooded)], u[, .(pattern, sand, dwi, tp12)], by = "pattern", all.x = TRUE)
    dt_dark(d[, .(Pattern = pattern, Allocation = round(coeff, 3), Mechanism = mechanism, Unit = sand, `Unit DWI` = round(dwi, 2), `Unit TP %/yr` = round(tp12, 1))],
            dom = "tip", pageLength = 15)
  })
  shiny::observeEvent(input$w360_pats_rows_selected, {
    x <- wp(); if (nrow(x)) { p <- x$pattern[1]; shiny::removeModal(); ctx$open_p360(p) }
  })
  output$w360_jobs <- DT::renderDT({
    ctx$store_tick()
    a <- ctx$res()$ds$interventions; a <- if (!is.null(a)) a[well == wsel(), .(source = "table", date, type, sand, status, notes)] else NULL
    b <- store_interventions(ctx$con)
    b <- if (nrow(b)) b[well == wsel()][, job := data.table::fcoalesce(job, paste0("APP-", id))][, .(source = "app", date = date[1], type = paste(unique(type), collapse = " + "),
           sand = paste(stats::na.omit(sand), collapse = ", "), interval_id = paste(stats::na.omit(interval_id), collapse = ", "), status = status[1], notes = notes[1], opportunities = .N), by = job] else NULL
    x <- data.table::rbindlist(list(a, b), fill = TRUE)
    if (!nrow(x)) x <- data.table::data.table(Message = "No jobs on this well")
    dt_dark(x, dom = "t")
  })
}
