# STEP 3 - OPPORTUNITIES ---------------------------------------------------------------------

opportunities_ui <- function() {
  htmltools::tagList(
    htmltools::div(class = "wf-section-intro",
      htmltools::div(class = "wf-step", "Step 3 of 3"), htmltools::h2("Opportunities"),
      htmltools::p("Candidates are decision records built from maturity, velocity, unit, spatial and operations evidence.",
                   "A signal from a single family stays at screening_only; engineers confirm validation items and move the status forward.")),
    shiny::uiOutput("opp_funnel"),
    bslib::navset_card_underline(id = "opp_tabs",
      bslib::nav_panel("Candidates", value = "cand",
        bslib::layout_columns(col_widths = c(5, 7),
          htmltools::div(
            htmltools::div(class = "wf-inline-tools",
              shiny::checkboxGroupInput("opp_types", NULL, choices = stats::setNames(names(opp_types), paste(names(opp_types), opp_types)),
                                        selected = names(opp_types), inline = TRUE)),
            DT::DTOutput("opp_list")),
          shiny::uiOutput("opp_record"))),
      bslib::nav_panel("Board", value = "board", shiny::uiOutput("opp_board")),
      bslib::nav_panel("Conformance ranking", value = "rank",
        bslib::layout_columns(col_widths = c(7, 5),
          htmltools::div(htmltools::h6(class = "wf-h6", "Method 1: points against the area average (higher = stronger conformance candidate)"), DT::DTOutput("opp_rank")),
          htmltools::div(htmltools::h6(class = "wf-h6", "Method 2: volumetric efficiency ratio vs Loss"), plotly::plotlyOutput("opp_m2", height = "420px")))),
      bslib::nav_panel("Interventions & outcomes", value = "iv",
        bslib::layout_columns(col_widths = c(6, 6),
          htmltools::div(htmltools::h6(class = "wf-h6", "Interventions (your table + logged in the app)"), DT::DTOutput("opp_iv")),
          htmltools::div(htmltools::h6(class = "wf-h6", "Before / after the selected job, with the waterflood-fit baseline"), DT::DTOutput("opp_eval"),
                         plotly::plotlyOutput("opp_eval_plot", height = "300px")))),
      bslib::nav_panel("Rules & weights", value = "rules", shiny::uiOutput("opp_rules")))
  )
}

opportunities_server <- function(input, output, session, ctx) {
  status_filter <- shiny::reactiveVal(c("screening_only", "candidate", "validated_candidate", "executed", "outcome_evaluated"))

  output$opp_funnel <- shiny::renderUI({
    s <- ctx$opps()$summary
    n <- if (nrow(s)) table(factor(as.character(s$status), status_levels)) else stats::setNames(rep(0, length(status_levels)), status_levels)
    htmltools::div(class = "wf-funnel", lapply(status_levels, function(k)
      shiny::actionLink(paste0("opp_f_", k), label = htmltools::tagList(htmltools::div(class = "l", k), htmltools::tags$b(n[[k]])),
        class = paste("wf-fs", if (length(status_filter()) == 1 && status_filter() == k) "on"), style = sprintf("--fc:%s", status_colors[[k]]))),
      shiny::actionLink("opp_f_all", "show all", class = "wf-fs-all"))
  })
  for (k in status_levels) local({
    kk <- k
    shiny::observeEvent(input[[paste0("opp_f_", kk)]], { status_filter(kk); bslib::nav_select("opp_tabs", "cand") }, ignoreInit = TRUE)
  })
  shiny::observeEvent(input$opp_f_all, status_filter(status_levels))

  listed <- shiny::reactive({
    s <- ctx$opps()$summary
    if (!nrow(s)) return(s)
    s[type %in% input$opp_types & as.character(status) %in% status_filter()]
  })
  fam_chips <- function(f) paste(vapply(names(families), function(k)
    sprintf('<span class="wf-fam %s">%s</span>', if (grepl(k, f)) "y" else "", k), ""), collapse = "")
  output$opp_list <- DT::renderDT({
    s <- listed()
    if (!nrow(s)) return(dt_dark(data.table::data.table(Message = "No opportunities for this filter")))
    d <- s[, .(` ` = sprintf('<span class="wf-ot" style="--tc:%s">%s</span>', opp_colors[type], type),
               Target = sprintf("<b>%s</b>%s%s<br><small>%s</small>", pattern, ifelse(is.na(sand), "", paste(" · unit", sand)),
                                ifelse(is.na(well), "", paste(" ·", well)), opp_types[type]),
               Evidence = vapply(families, fam_chips, ""), Status = badge_html(as.character(status), status_colors[as.character(status)]),
               `Gain bopd` = round(gain), Score = score)]
    dt_dark(d, escape = FALSE, pageLength = 12, dom = "tip", ordering = FALSE)
  })
  shiny::observeEvent(input$opp_list_rows_selected, ctx$sel_opp(listed()$key[input$opp_list_rows_selected]))

  sel <- shiny::reactive({
    k <- ctx$sel_opp(); op <- ctx$opps()
    if (is.null(k) || !k %in% op$summary$key) { if (nrow(op$summary)) k <- op$summary$key[1] else return(NULL) }
    list(rec = op$records[[k]], row = op$summary[key == k])
  })

  blk <- function(label, txt) if (is.null(txt) || !nzchar(txt) || identical(txt, "-")) NULL else
    htmltools::div(class = "wf-blk", htmltools::div(class = "l", label), htmltools::p(txt))
  lst <- function(label, x) if (!length(x)) NULL else htmltools::div(class = "wf-blk", htmltools::div(class = "l", label), htmltools::tags$ul(lapply(x, htmltools::tags$li)))

  output$opp_record <- shiny::renderUI({
    s <- sel(); if (is.null(s)) return(htmltools::div(class = "wf-muted", "No opportunities at this date."))
    r <- s$rec; row <- s$row; tx <- r$text; key <- r$key
    checks <- store_checks(ctx$con, key)
    ai <- store_ai(ctx$con, key)
    htmltools::div(class = "wf-record",
      htmltools::div(class = "wf-rec-head",
        htmltools::span(class = "wf-ot big", style = sprintf("--tc:%s", opp_colors[[r$type]]), r$type),
        htmltools::h4(sprintf("%s · %s%s%s", opp_types[[r$type]], r$pattern, if (is.na(r$sand)) "" else paste(" · unit", r$sand), if (is.na(r$well)) "" else paste(" ·", r$well))),
        badge(as.character(row$status), status_colors[[as.character(row$status)]]),
        badge(sprintf("evidence %d of 5: %s", row$n_fam, row$families), "#fbbf24"),
        htmltools::span(class = "wf-muted small", paste("detected", fmt_month(ctx$asof()))),
        shiny::actionLink("opp_open360", "Pattern 360 →", class = "wf-link")),
      bslib::layout_columns(col_widths = c(4, 4, 4),
        plotly::plotlyOutput("opp_m_a", height = "190px"), plotly::plotlyOutput("opp_m_b", height = "190px"), plotly::plotlyOutput("opp_m_c", height = "190px")),
      htmltools::div(class = "wf-blk-grid",
        blk("Maturity evidence", tx$maturity), blk("Velocity / balance evidence", tx$velocity), blk("Vertical evidence", tx$vertical),
        blk("Spatial evidence", tx$spatial), blk("Operations", tx$ops), blk("Suspected mechanism", tx$mechanism),
        lst("Alternative explanations", tx$alternatives), lst("Data gaps", tx$gaps),
        blk("Proposed action", tx$action), blk(sprintf("Expected response (%s)", tx$window), tx$outcome)),
      htmltools::h6(class = "wf-h6", "OpportunityEvidence"),
      DT::DTOutput("opp_evidence"),
      bslib::layout_columns(col_widths = c(6, 6),
        htmltools::div(class = "wf-blk",
          htmltools::div(class = "l", "Validation before execution"),
          shiny::checkboxGroupInput("opp_checks", NULL, choices = tx$validation, selected = intersect(checks, tx$validation))),
        htmltools::div(class = "wf-blk",
          htmltools::div(class = "l", "Notes"),
          shiny::textAreaInput("opp_notes", NULL, value = store_get_state(ctx$con, key)$notes %||% "", rows = 3, width = "100%"),
          shiny::actionButton("opp_save_notes", "Save notes", class = "btn-sm btn-outline-info"))),
      htmltools::div(class = "wf-btn-row",
        shiny::actionButton("opp_validate", "Mark validated", class = "btn-sm btn-primary"),
        shiny::actionButton("opp_log", "Log intervention", class = "btn-sm btn-outline-info"),
        shiny::actionButton("opp_outcome", "Record outcome", class = "btn-sm btn-outline-info"),
        shiny::actionButton("opp_dismiss", "Dismiss with reason", class = "btn-sm btn-outline-light"),
        shiny::actionButton("opp_reset", "Reset status", class = "btn-sm btn-outline-light"),
        htmltools::span(class = "wf-muted small", sprintf("Priority basis: indicative gain %s bopd; remaining WF oil %s stb", fmt_num(r$gain), fmt_num(r$stake)))),
      htmltools::div(class = "wf-ai",
        htmltools::div(class = "l", "AI draft of the decision record"),
        if (ai_available()) shiny::actionButton("opp_ai", if (is.null(ai)) "Draft with AI" else "Redraft with AI", class = "btn-sm btn-outline-info")
        else htmltools::span(class = "wf-muted small", "Not configured: set ANTHROPIC_API_KEY on the server to enable drafting (optional model: WF_AI_MODEL)."),
        if (!is.null(ai)) htmltools::div(class = "wf-ai-text", htmltools::div(class = "wf-muted small", sprintf("%s · %s · draft, review before use", ai$model, ai$ts)),
                                         shiny::markdown(ai$text))),
      htmltools::h6(class = "wf-h6", "History"),
      DT::DTOutput("opp_hist"))
  })

  output$opp_evidence <- DT::renderDT({
    s <- sel(); shiny::req(s)
    e <- data.table::copy(s$rec$evidence)
    e[, `:=`(value = signif(value, 3), reference = signif(reference, 3))]
    dt_dark(e, dom = "t", pageLength = 20, ordering = FALSE)
  })
  output$opp_hist <- DT::renderDT({
    s <- sel(); shiny::req(s); ctx$store_tick()
    h <- store_history(ctx$con, s$rec$key)
    if (!nrow(h)) h <- data.table::data.table(Message = "No decisions recorded yet")
    dt_dark(h, dom = "t", pageLength = 20, ordering = FALSE)
  })

  mini <- function(kind) {
    s <- sel(); shiny::req(s); r <- s$rec; sn <- ctx$snap()
    p <- switch(kind,
      secrf = { x <- sn[flooded == TRUE]; pr <- main_proto(ctx$res(), x)
        q <- add_proto(plotly::plot_ly(), pr, "sec_rf", max(1.2, max(x$dwi) * 1.05))
        q <- pattern_scatter(x, "dwi", "sec_rf", color = "opr", size = NULL, shape = FALSE, hl = r$pattern, p = q)
        plotly::layout(pl_theme(q, "DWI", "Sec RF", legend = FALSE), yaxis = axis_style("Sec RF", tickformat = ".0%")) },
      utiltp = { x <- sn[flooded == TRUE & is.finite(util)]
        q <- pattern_scatter(x, "util", "tp12", color = "iwr12", size = NULL, shape = FALSE, hl = r$pattern)
        plotly::layout(pl_theme(q, "Utilization", "Inj TP %/yr", legend = FALSE), shapes = list(hline_shape(ctx$settings()$target_tp, pal$warn))) },
      third = {
        if (r$type %in% c("A", "D", "B")) {
          u <- ctx$units_snap()[pattern == r$pattern]
          q <- plotly::plot_ly(u, x = ~sand, y = ~dwi, type = "bar", name = "DWI", marker = list(color = ifelse(u$sand %in% r$sand, pal$crit, "#3b82f6")))
          q <- plotly::add_lines(q, x = ~sand, y = ~tp12 / 10, name = "TP/10", yaxis = "y", line = list(color = pal$warn))
          pl_theme(q, "Unit", "DWI (bars) · TP/10 (line)", legend = FALSE)
        } else {
          x <- sn[flooded == TRUE & is.finite(evr) & is.finite(loss)]
          q <- pattern_scatter(x, "loss", "evr", color = "opr", size = NULL, shape = FALSE, hl = r$pattern)
          st <- ctx$settings()
          plotly::layout(pl_theme(q, "Loss", "Evol(MB)/Evol(FF)", legend = FALSE),
                         shapes = list(list(type = "line", x0 = st$loss_high, x1 = st$loss_high, yref = "paper", y0 = 0, y1 = 1, line = list(color = pal$muted, dash = "dot")),
                                       hline_shape(st$evr_low, pal$muted, "dot")))
        }
      })
    plotly::layout(p, margin = list(l = 45, r = 5, t = 10, b = 35), font = list(size = 9))
  }
  output$opp_m_a <- plotly::renderPlotly(mini("secrf"))
  output$opp_m_b <- plotly::renderPlotly(mini("utiltp"))
  output$opp_m_c <- plotly::renderPlotly(mini("third"))

  key_now <- function() { s <- sel(); if (is.null(s)) NULL else s$rec$key }
  bump <- function() ctx$store_tick(ctx$store_tick() + 1)

  shiny::observeEvent(input$opp_checks, {
    k <- key_now(); shiny::req(k)
    if (!identical(sort(input$opp_checks), sort(intersect(store_checks(ctx$con, k), sel()$rec$text$validation))))
      store_set_state(ctx$con, k, checks = input$opp_checks)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  shiny::observeEvent(input$opp_save_notes, { store_set_state(ctx$con, key_now(), notes = input$opp_notes); shiny::showNotification("Notes saved") })
  shiny::observeEvent(input$opp_open360, ctx$open_p360(sel()$rec$pattern))

  shiny::observeEvent(input$opp_validate, {
    s <- sel(); v <- s$rec$text$validation
    if (!all(v %in% input$opp_checks)) { shiny::showNotification("Confirm every validation item first", type = "warning"); return() }
    if (s$row$n_fam < 2) { shiny::showNotification("Screening signals need a second family of evidence before validation", type = "warning"); return() }
    store_set_state(ctx$con, s$rec$key, status = "validated_candidate", checks = input$opp_checks, comment = "all validation items confirmed"); bump()
  })
  shiny::observeEvent(input$opp_reset, { store_set_state(ctx$con, key_now(), status = sel()$row$auto_status, comment = "reset"); bump() })

  shiny::observeEvent(input$opp_dismiss, shiny::showModal(shiny::modalDialog(title = "Dismiss opportunity",
    shiny::textAreaInput("opp_dismiss_reason", "Reason", rows = 3, width = "100%"),
    footer = htmltools::tagList(shiny::modalButton("Cancel"), shiny::actionButton("opp_dismiss_ok", "Dismiss", class = "btn-primary")))))
  shiny::observeEvent(input$opp_dismiss_ok, {
    store_set_state(ctx$con, key_now(), status = "dismissed", notes = input$opp_dismiss_reason, comment = input$opp_dismiss_reason)
    shiny::removeModal(); bump()
  })

  shiny::observeEvent(input$opp_log, {
    r <- sel()$rec
    wells <- unique(ctx$res()$alloc[pattern == r$pattern & coeff > 0, well])
    type0 <- c(A = "ISOLATION", B = "STIM", C = "LIFT", D = "ADPERF", E = "CONFORMANCE", F = "RATE")[[r$type]]
    shiny::showModal(shiny::modalDialog(title = "Log intervention", easyClose = TRUE,
      shiny::selectInput("iv_well", "Well", wells, selected = if (!is.na(r$well)) r$well else wells[1]),
      shiny::selectInput("iv_type", "Type", c("STIM", "ISOLATION", "ADPERF", "RATE", "LIFT", "CONFORMANCE"), selected = type0),
      shiny::selectInput("iv_sand", "Unit", c("(none)" = "", sort(unique(ctx$res()$sand_props$sand))), selected = if (is.na(r$sand)) "" else r$sand),
      shiny::dateInput("iv_date", "Date", value = Sys.Date()),
      shiny::radioButtons("iv_status", "Status", c("EXECUTED", "PLANNED"), inline = TRUE),
      shiny::textAreaInput("iv_notes", "Notes", rows = 2, width = "100%"),
      footer = htmltools::tagList(shiny::modalButton("Cancel"), shiny::actionButton("iv_ok", "Save", class = "btn-primary"))))
  })
  shiny::observeEvent(input$iv_ok, {
    r <- sel()$rec
    store_add_intervention(ctx$con, input$iv_well, r$pattern, input$iv_sand, input$iv_date, input$iv_type, input$iv_status, input$iv_notes, r$key)
    if (input$iv_status == "EXECUTED") store_set_state(ctx$con, r$key, status = "executed", comment = paste(input$iv_type, input$iv_well, format(input$iv_date)))
    shiny::removeModal(); bump(); shiny::showNotification("Intervention logged")
  })

  shiny::observeEvent(input$opp_outcome, {
    r <- sel()$rec
    shiny::showModal(shiny::modalDialog(title = "Record outcome", easyClose = TRUE,
      htmltools::p(class = "wf-muted", paste("Expected:", r$text$outcome)),
      shiny::radioButtons("oc_verdict", "Verdict", c("met", "partly met", "not met"), inline = TRUE),
      shiny::textAreaInput("oc_actual", "Observed response", rows = 3, width = "100%"),
      footer = htmltools::tagList(shiny::modalButton("Cancel"), shiny::actionButton("oc_ok", "Save", class = "btn-primary"))))
  })
  shiny::observeEvent(input$oc_ok, {
    r <- sel()$rec
    store_add_outcome(ctx$con, r$key, input$oc_verdict, r$text$outcome, input$oc_actual, "")
    store_set_state(ctx$con, r$key, status = "outcome_evaluated", comment = input$oc_verdict)
    shiny::removeModal(); bump()
  })

  shiny::observeEvent(input$opp_ai, {
    s <- sel(); r <- s$rec
    pr <- ctx$snap()[entity == r$pattern]
    shiny::withProgress(message = "Drafting with AI", value = 0.4, {
      out <- tryCatch(ai_draft(ai_payload(r, s$row, pr, ctx$settings())), error = function(e) e)
    })
    if (inherits(out, "error")) { shiny::showNotification(conditionMessage(out), type = "error", duration = 10); return() }
    store_save_ai(ctx$con, r$key, out$model, out$text); bump()
  })

  # ---- board ----
  output$opp_board <- shiny::renderUI({
    s <- ctx$opps()$summary
    if (!nrow(s)) return(htmltools::div(class = "wf-muted", "No opportunities"))
    htmltools::div(class = "wf-board", lapply(status_levels, function(k) {
      x <- s[as.character(status) == k]
      htmltools::div(class = "wf-col", htmltools::div(class = "wf-col-h", style = sprintf("--fc:%s", status_colors[[k]]), sprintf("%s (%d)", k, nrow(x))),
        lapply(seq_len(nrow(x)), function(i) htmltools::tags$a(class = "wf-bcard", href = "#",
          onclick = sprintf("Shiny.setInputValue('board_pick', '%s', {priority: 'event'}); return false;", x$key[i]),
          htmltools::span(class = "wf-ot", style = sprintf("--tc:%s", opp_colors[[x$type[i]]]), x$type[i]),
          htmltools::span(sprintf("%s%s", x$pattern[i], if (is.na(x$sand[i])) "" else paste(" \u00b7", x$sand[i]))),
          htmltools::span(class = "wf-muted small", paste("score", x$score[i])))))
    }))
  })
  shiny::observeEvent(input$board_pick, { ctx$sel_opp(input$board_pick); bslib::nav_select("opp_tabs", "cand") })

  # ---- conformance ranking ----
  rank <- shiny::reactive(conformance_ranking(ctx$snap()))
  output$opp_rank <- DT::renderDT({
    r <- rank(); shiny::req(nrow(r))
    d <- r[, .(Pattern = entity, Area = area, Score = round(score, 1), Util = round(util, 1), `Util cum` = round(util_cum, 1),
               WOR = round(wor6, 1), `WC %` = round(100 * wc6, 1), DWI = round(dwi, 2), `Vert. eff.` = round(ve, 2), OPR = round(opr, 2),
               `Method 2` = method2)]
    dt_dark(d, pageLength = 16)
  })
  shiny::observeEvent(input$opp_rank_rows_selected, ctx$open_p360(rank()$entity[input$opp_rank_rows_selected]))
  output$opp_m2 <- plotly::renderPlotly({
    sn <- ctx$snap()[flooded == TRUE & is.finite(evr) & is.finite(loss)]
    if (!nrow(sn)) return(empty_plot("Needs water cut and rel-perm"))
    st <- ctx$settings(); xm <- max(0.4, max(sn$loss) * 1.1); xl <- min(0, min(sn$loss) * 1.1); ym <- max(1.1, max(sn$evr) * 1.1)
    p <- pattern_scatter(sn, "loss", "evr", color = "opr", hl = ctx$focus())
    a <- function(x, y, t, c) list(x = x, y = y, text = t, showarrow = FALSE, font = list(size = 9, color = c))
    p <- pl_theme(p, "Loss = DWI − DTP", "Evol(MB) / Evol(FF)", legend = FALSE)
    plotly::event_register(plotly::layout(p, xaxis = axis_style("Loss = DWI − DTP", range = c(xl, xm)), yaxis = axis_style("Evol(MB)/Evol(FF)", range = c(0, ym)),
      shapes = list(rect_shape(xl, st$loss_high, 0, st$evr_low, "rgba(251,191,36,0.10)"), rect_shape(st$loss_high, xm, 0, st$evr_low, "rgba(248,113,113,0.12)"),
                    rect_shape(st$loss_high, xm, st$evr_low, ym, "rgba(251,191,36,0.06)"), rect_shape(xl, st$loss_high, st$evr_low, ym, "rgba(52,211,153,0.08)")),
      annotations = list(a((xl + st$loss_high) / 2, 0.05, "thief zone", pal$warn), a((st$loss_high + xm) / 2, 0.05, "reservoir & well", pal$crit),
                         a((st$loss_high + xm) / 2, ym * 0.96, "out of zone / area", pal$warn), a((xl + st$loss_high) / 2, ym * 0.96, "efficient", pal$ok))), "plotly_click")
  })

  # ---- interventions ----
  ivs <- shiny::reactive({
    ctx$store_tick()
    a <- ctx$res()$ds$interventions
    a <- if (!is.null(a) && nrow(a)) a[, .(source = "table", well, date, type, sand, status, notes, opp_key = NA_character_)] else NULL
    b <- store_interventions(ctx$con)
    b <- if (nrow(b)) b[, .(source = "app", well, date, type, sand, status, notes, opp_key)] else NULL
    x <- data.table::rbindlist(list(a, b), fill = TRUE)
    if (nrow(x)) data.table::setorder(x, -date)
    x
  })
  output$opp_iv <- DT::renderDT({
    x <- ivs(); if (!nrow(x)) x <- data.table::data.table(Message = "No interventions yet")
    dt_dark(x, pageLength = 10)
  })
  ev <- shiny::reactive({
    x <- ivs(); i <- input$opp_iv_rows_selected
    if (!nrow(x) || is.null(i)) return(NULL)
    evaluate_intervention(ctx$res(), ctx$series_all(), x[i], ctx$asof())
  })
  output$opp_eval <- DT::renderDT({
    e <- ev(); if (is.null(e) || !nrow(e)) e <- data.table::data.table(Message = "Select an executed job with at least one month of data after it")
    else e <- e[, .(Pattern = pattern, Metric = metric, Before = signif(before, 3), After = signif(after, 3),
                    `Change %` = round(100 * (after / before - 1)), `Incremental oil vs fit (stb)` = round(incremental_oil_stb), Months = months_after)]
    dt_dark(e, dom = "t", pageLength = 20, ordering = FALSE)
  })
  output$opp_eval_plot <- plotly::renderPlotly({
    x <- ivs(); i <- input$opp_iv_rows_selected
    if (!nrow(x) || is.null(i)) return(empty_plot("Select a job"))
    j <- x[i]; pats <- unique(ctx$res()$alloc[well == j$well & coeff > 0, pattern])
    s <- ctx$series_all()[entity %in% pats & date >= j$date - 3 * 365 & date <= ctx$asof()]
    p <- plotly::plot_ly(s, x = ~date, y = ~tp, color = ~entity, type = "scatter", mode = "lines", colors = cluster_palette)
    p <- plotly::add_lines(p, data = s, x = ~date, y = ~qo / 10, color = ~entity, line = list(dash = "dot"), showlegend = FALSE)
    plotly::layout(pl_theme(p, NULL, "Inj TP %/yr (solid) · oil/10 bopd (dotted)"), shapes = list(vline_shape(j$date, pal$warn)))
  })

  output$opp_rules <- shiny::renderUI({
    st <- ctx$settings()
    rules <- data.table::data.table(
      Type = paste(names(opp_types), opp_types),
      `Screening signal` = c(sprintf("Unit DWI in the area's top %d %% and above 1.2x the pattern DWI", round(100 * (1 - st$unit_dwi_q))),
                             sprintf("Inj TP down more than %d %% in 12 months, or below half the target", round(100 * st$tp_drop)),
                             sprintf("IWR 12 m above %.1f", st$iwr_high), "ADPERF in the last 3 years in a unit",
                             "DWI at or above the area median with OPR below 1", sprintf("Inj TP more than %d %% off target", round(100 * st$tp_off))),
      `Corroborating evidence` = c("Unit TP above target (V); rate above Cobb (O); high pattern utilization or water cut (M)",
                                   "Efficient or immature (M); units losing injectivity (U); VRF open but rate below 60 % of Cobb (O)",
                                   "Utilization at or below area median (M); fluid level above threshold (O); low Loss (S)",
                                   "Unit TP below half the target (V); unit immature (U)",
                                   "High utilization (V); injection concentrated in one unit (U); producer HI or Evol ratio (S)",
                                   "Maturity agrees: immature and efficient to raise, mature and cycling to cut (M); IWR (S)"))
    htmltools::tagList(
      htmltools::p(class = "wf-muted", "Candidate = two or more evidence families including maturity (M) or velocity (V). Thresholds and score weights are in Data & reference › Settings."),
      DT::renderDT(dt_dark(rules, dom = "t", ordering = FALSE)),
      htmltools::p(class = "wf-muted small", sprintf("Priority score = %d %% evidence count + %d %% indicative oil gain (waterflood fit) + %d %% remaining waterflood oil, each scaled to the largest candidate.",
                                                     round(100 * st$w_evidence), round(100 * st$w_gain), round(100 * st$w_stake))))
  })
}
