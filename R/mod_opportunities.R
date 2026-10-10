# STEP 3 - OPPORTUNITIES (well interventions) --------------------------------------------------

opportunities_ui <- function() {
  htmltools::tagList(
    htmltools::div(class = "wf-section-intro",
      htmltools::div(class = "wf-step", "Step 3 of 4"), htmltools::h2("Opportunities"),
      htmltools::p("Every opportunity is a job on a well (action · well · unit · interval). Pattern rules, the single-well analysis and other",
                   "analyses add evidence to the same target. A single family stays at screening_only; engineers confirm validation items and move the status forward.")),
    shiny::uiOutput("opp_funnel"),
    # loads the date-picker assets used by the job modal
    htmltools::div(style = "display:none", shiny::dateInput("opp_date_assets", NULL)),
    htmltools::div(class = "wf-opp-filters",
      shiny::radioButtons("opp_lens", NULL, c("All lenses" = "", "Pattern" = "PATTERN", "Well" = "WELL", "Other analyses" = "OTHER"), inline = TRUE),
      shiny::selectInput("opp_drive", NULL, c("All drives" = ""), width = "150px"),
      shiny::selectizeInput("opp_actions", NULL, choices = NULL, multiple = TRUE, width = "260px", options = list(placeholder = "All actions")),
      shiny::selectInput("opp_lvl", NULL, c("Orgunit" = "orgunit", "Contract" = "contract", "Field" = "field", "Structure" = "structure", "Area" = "area"),
                         selected = "field", width = "120px"),
      shiny::selectizeInput("opp_lvl_val", NULL, choices = NULL, multiple = TRUE, width = "220px", options = list(placeholder = "all"))),
    bslib::navset_card_underline(id = "opp_tabs",
      bslib::nav_panel("Portfolio", value = "cand",
        bslib::layout_columns(col_widths = c(5, 7),
          htmltools::div(shiny::uiOutput("opp_count"), DT::DTOutput("opp_list"), shiny::uiOutput("opp_unassigned")),
          shiny::uiOutput("opp_record"))),
      bslib::nav_panel("Portfolio map", value = "map",
        htmltools::p(class = "wf-muted small", "Oil in the first 12 months (Base profile, or an indicative 12 months from the gain rate) against the water lifted in the same period. Size = priority score. Click a point to open its record."),
        plotly::plotlyOutput("opp_bubble", height = "520px")),
      bslib::nav_panel("Board", value = "board", shiny::uiOutput("opp_board")),
      bslib::nav_panel("ADPERF workbench", value = "wb", workbench_ui()),
      bslib::nav_panel("Conformance ranking", value = "rank",
        bslib::layout_columns(col_widths = c(7, 5),
          htmltools::div(htmltools::h6(class = "wf-h6", "Method 1: points against the area average (higher = stronger conformance candidate)"), DT::DTOutput("opp_rank")),
          htmltools::div(htmltools::h6(class = "wf-h6", "Method 2: volumetric efficiency ratio vs Loss"), plotly::plotlyOutput("opp_m2", height = "420px")))),
      bslib::nav_panel("Rules & weights", value = "rules", shiny::uiOutput("opp_rules")))
  )
}

opportunities_server <- function(input, output, session, ctx) {
  status_filter <- shiny::reactiveVal(c("screening_only", "candidate", "validated_candidate", "executed", "outcome_evaluated"))

  # ---- filters ----
  shiny::observe({
    s <- ctx$opps()$summary
    if (!nrow(s)) return()
    dr <- sort(unique(stats::na.omit(s$drive)))
    shiny::updateSelectInput(session, "opp_drive", choices = c("All drives" = "", stats::setNames(dr, tools::toTitleCase(dr))), selected = shiny::isolate(input$opp_drive))
    ac <- unique(s$action)
    shiny::updateSelectizeInput(session, "opp_actions", choices = stats::setNames(ac, action_label(ac)), selected = shiny::isolate(input$opp_actions))
  })
  shiny::observe({
    s <- ctx$opps()$summary; lv <- input$opp_lvl %||% "field"
    if (!nrow(s) || !lv %in% names(s)) return()
    shiny::updateSelectizeInput(session, "opp_lvl_val", choices = sort(unique(stats::na.omit(s[[lv]]))), selected = character())
  })

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

  filtered <- shiny::reactive({
    s <- ctx$opps()$summary
    if (!nrow(s)) return(s)
    if (nzchar(input$opp_lens %||% "")) s <- s[vapply(strsplit(lenses, ", "), function(l) any(lens_group(l) == input$opp_lens), TRUE)]
    if (nzchar(input$opp_drive %||% "")) s <- s[drive == input$opp_drive]
    if (length(input$opp_actions)) s <- s[action %in% input$opp_actions]
    lv <- input$opp_lvl %||% "field"
    if (length(input$opp_lvl_val) && lv %in% names(s)) s <- s[get(lv) %in% input$opp_lvl_val]
    s
  })
  listed <- shiny::reactive({
    s <- filtered()
    if (!nrow(s)) return(s)
    s <- s[as.character(status) %in% status_filter()]
    # grouped by well: wells ordered by their best score, opportunities by score inside the well
    s[, wbest := max(score), by = well]
    data.table::setorder(s, -wbest, well, -score)
    s
  })

  fam_chips <- function(f) paste(vapply(names(families), function(k)
    sprintf('<span class="wf-fam %s">%s</span>', if (grepl(k, f)) "y" else "", k), ""), collapse = "")
  lens_chips <- function(l) paste(vapply(strsplit(l, ", ")[[1]], function(x)
    sprintf('<span class="wf-lens" style="--lc:%s">%s</span>', lens_colors[[lens_group(x)]], if (lens_group(x) == "OTHER") "OTHER" else x), ""), collapse = "")
  target_txt <- function(s) paste0(s$well, ifelse(is.na(s$sand), "", paste(" ·", s$sand)), ifelse(is.na(s$interval), "", paste(" ·", s$interval)))

  output$opp_count <- shiny::renderUI({
    s <- listed()
    htmltools::div(class = "wf-muted small", sprintf("%d opportunities on %d wells", nrow(s), data.table::uniqueN(s$well)),
      if (econ_available()) htmltools::span(" · economics plugged in") else htmltools::span(" · ranked on volumes (economics not plugged in)"))
  })
  output$opp_list <- DT::renderDT({
    s <- listed()
    if (!nrow(s)) return(dt_dark(data.table::data.table(Message = "No opportunities for this filter")))
    first <- !duplicated(s$well)
    d <- s[, .(Well = ifelse(first, sprintf("<b>%s</b><br><small>%s · %s</small>", well, tolower(data.table::fcoalesce(well_type, "")), drive), ""),
               Action = sprintf('<span class="wf-act" style="--tc:%s">%s</span> %s<br><small>%s</small>', action_color(action), action,
                                ifelse(is.na(sand), "", paste("unit", sand, ifelse(is.na(interval), "", interval))), action_label(action)),
               Lenses = vapply(lenses, lens_chips, ""), Evidence = vapply(families, fam_chips, ""),
               Status = badge_html(as.character(status), status_colors[as.character(status)]),
               `Gain bopd` = round(gain), Score = score)]
    dt_dark(d, escape = FALSE, pageLength = 14, dom = "tip", ordering = FALSE)
  })
  shiny::observeEvent(input$opp_list_rows_selected, ctx$sel_opp(listed()$key[input$opp_list_rows_selected]))

  output$opp_unassigned <- shiny::renderUI({
    u <- ctx$opps()$unassigned
    if (is.null(u) || !nrow(u)) return(NULL)
    htmltools::div(class = "wf-blk", htmltools::div(class = "l", sprintf("Findings without a target well (%d)", nrow(u))),
      htmltools::tags$ul(lapply(seq_len(nrow(u)), function(i) htmltools::tags$li(sprintf("%s rule %s on %s (%s): no qualifying well. %s",
        u$lens[i], u$rule[i], data.table::fcoalesce(u$pattern[i], "-"), u$families[i], u$note[i])))))
  })

  sel <- shiny::reactive({
    k <- ctx$sel_opp(); op <- ctx$opps()
    l <- listed()
    # follow the filters: a record hidden by them gives way to the first listed one
    if (is.null(k) || !k %in% op$summary$key || (nrow(l) && !k %in% l$key)) { if (nrow(l)) k <- l$key[1] else if (nrow(op$summary)) k <- op$summary$key[1] else return(NULL) }
    list(rec = op$records[[k]], row = op$summary[key == k])
  })
  sel_profile <- shiny::reactive({ s <- sel(); shiny::req(s); profile_rows(ctx$res()$ds$profiles, s$row$fkey) })
  sel_hist <- shiny::reactive({ s <- sel(); shiny::req(s); well_history(ctx$res(), s$rec$well, ctx$asof()) })

  blk <- function(label, txt) if (is.null(txt) || !length(txt) || !nzchar(txt) || identical(txt, "-")) NULL else
    htmltools::div(class = "wf-blk", htmltools::div(class = "l", label), htmltools::p(txt))
  lst <- function(label, x) if (!length(x)) NULL else htmltools::div(class = "wf-blk", htmltools::div(class = "l", label), htmltools::tags$ul(lapply(x, htmltools::tags$li)))

  auto_checks <- function(s) {
    r <- s$rec; row <- s$row; out <- list()
    qa <- r$meta$qa
    if (!is.null(qa)) out[["QA of the interval estimate"]] <- if (qa_ok(qa)) "ok" else paste("corrected:", qa)
    if (identical(row$gain_src, "PROFILE")) {
      iv <- ctx$res()$ds$intervals
      b <- sel_profile()[scenario == "Base" & month == 1]
      i <- if (!is.null(iv)) iv[well == r$well & sand == r$sand & interval_id == r$interval] else NULL
      if (!is.null(i) && nrow(i) && nrow(b)) out[["Base profile = initial rates"]] <- if (abs(b$qo - i$qo0) <= 0.01 * max(i$qo0, 1)) "ok" else sprintf("differs (%s vs %s bopd)", fmt_int(b$qo), fmt_int(i$qo0))
      out[["Forecast"]] <- "Bajo / Base / Alto profiles"
    } else out[["Forecast"]] <- switch(data.table::fcoalesce(row$gain_src, ""), SF_FIT = "waterflood fit (indicative)", ARPS = "well decline fit (indicative)",
                                         SOURCE = "gain given by the source analysis", INTERVAL = "initial rate of the interval only", "none: ranked on evidence")
    out
  }

  output$opp_record <- shiny::renderUI({
    s <- sel(); if (is.null(s)) return(htmltools::div(class = "wf-muted", "No opportunities at this date."))
    r <- s$rec; row <- s$row; tx <- r$text; key <- r$key
    checks <- store_checks(ctx$con, key)
    ai <- store_ai(ctx$con, key)
    ac <- auto_checks(s)
    depth <- if (is.finite(r$meta$top %||% NA) && is.finite(r$meta$base %||% NA)) sprintf("%s-%s ft", fmt_int(r$meta$top), fmt_int(r$meta$base)) else NULL
    htmltools::div(class = "wf-record",
      htmltools::div(class = "wf-rec-head",
        htmltools::span(class = "wf-act big", style = sprintf("--tc:%s", action_color(r$action)), r$action),
        htmltools::h4(sprintf("%s · %s%s%s", action_label(r$action), r$well, if (is.na(r$sand)) "" else paste(" · unit", r$sand), if (is.na(r$interval)) "" else paste(" ·", r$interval))),
        badge(as.character(row$status), status_colors[[as.character(row$status)]]),
        badge(sprintf("evidence %d of 5: %s", row$n_fam, row$families), "#fbbf24"),
        badge(row$drive %||% "primary", if (identical(row$drive, "primary")) "#94a3b8" else "#60a5fa"),
        htmltools::HTML(lens_chips(row$lenses)),
        htmltools::span(class = "wf-muted small", paste(stats::na.omit(c(row$orgunit, row$contract, row$field, row$structure)), collapse = " › ")),
        { jb <- store_interventions(ctx$con); jb <- if (nrow(jb)) jb[opp_key == key] else jb
          if (nrow(jb)) badge(sprintf("in %s", jb$job[nrow(jb)]), "#a78bfa") },
        { n_open <- ctx$opps()$summary[well == r$well & as.character(status) %in% open_statuses, .N]
          if (n_open > 1) badge(sprintf("%d open opportunities on %s", n_open, r$well), "#94a3b8") },
        shiny::actionLink("opp_openw360", "Well 360 →", class = "wf-link"),
        if (!is.na(row$pattern)) shiny::actionLink("opp_open360", paste("Pattern", row$pattern, "360 →"), class = "wf-link")),
      htmltools::div(class = "wf-kpi-row compact",
        if (!is.null(depth)) kpi("Interval, ft", sub(" ft", "", depth)),
        kpi("Gain, bopd", fmt_int(row$gain), data.table::fcoalesce(row$gain_src, "no estimate")),
        if (is.finite(row$qo1_Base)) kpi("Bajo / Base / Alto", sprintf("%s / %s / %s", fmt_int(row$qo1_Bajo), fmt_int(row$qo1_Base), fmt_int(row$qo1_Alto)), "initial oil, bopd"),
        kpi("Oil 12 m", fmt_num(row$np12), if (identical(row$gain_src, "PROFILE")) "stb, Base" else "stb, indicative"),
        if (is.finite(row$wp12)) kpi("Water 12 m", fmt_num(row$wp12), "bbl, Base"),
        kpi("EUR / remaining", fmt_num(row$stake), "stb"), kpi("Score", row$score)),
      bslib::layout_columns(col_widths = c(4, 4, 4),
        plotly::plotlyOutput("opp_m_a", height = "210px"), plotly::plotlyOutput("opp_m_b", height = "210px"), plotly::plotlyOutput("opp_m_c", height = "210px")),
      htmltools::div(class = "wf-blk-grid",
        blk("Maturity evidence", tx$maturity), blk("Velocity / support evidence", tx$velocity), blk("Vertical evidence", tx$vertical),
        blk("Spatial evidence", tx$spatial), blk("Operations", tx$ops), blk("Suspected mechanism", tx$mechanism),
        lst("Alternative explanations", tx$alternatives), lst("Data gaps", tx$gaps),
        blk("Proposed action", tx$action), blk(sprintf("Expected response (%s)", tx$window), tx$outcome),
        lst("Also found by", tx$others)),
      htmltools::h6(class = "wf-h6", "Evidence"),
      DT::DTOutput("opp_evidence"),
      bslib::layout_columns(col_widths = c(6, 6),
        htmltools::div(class = "wf-blk",
          htmltools::div(class = "l", "Validation before execution"),
          htmltools::div(class = "wf-auto-checks", lapply(names(ac), function(n) htmltools::div(
            htmltools::span(class = if (identical(ac[[n]], "ok") || grepl("profiles", ac[[n]])) "ok" else "warn", if (identical(ac[[n]], "ok")) "✓" else "•"),
            htmltools::span(paste0(n, ": ", ac[[n]]))))),
          shiny::checkboxGroupInput("opp_checks", NULL, choices = tx$validation, selected = intersect(checks, tx$validation))),
        htmltools::div(class = "wf-blk",
          htmltools::div(class = "l", "Notes"),
          shiny::textAreaInput("opp_notes", NULL, value = store_get_state(ctx$con, key)$notes %||% "", rows = 3, width = "100%"),
          shiny::actionButton("opp_save_notes", "Save notes", class = "btn-sm btn-outline-info"))),
      htmltools::div(class = "wf-btn-row",
        shiny::actionButton("opp_validate", "Mark validated", class = "btn-sm btn-primary"),
        shiny::actionButton("opp_log", "Propose job on this well", class = "btn-sm btn-outline-info"),
        shiny::actionButton("opp_outcome", "Record outcome", class = "btn-sm btn-outline-info"),
        shiny::actionButton("opp_dismiss", "Dismiss with reason", class = "btn-sm btn-outline-light"),
        shiny::actionButton("opp_reset", "Reset status", class = "btn-sm btn-outline-light"),
        htmltools::span(class = "wf-muted small", "Validating freezes the forecast used for the decision; the job is later compared with it.")),
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
    if (!nrow(e)) return(dt_dark(data.table::data.table(Message = "No evidence rows"), dom = "t"))
    e[, `:=`(value = signif(value, 3), reference = signif(reference, 3))]
    data.table::setcolorder(e, intersect(c("lens", "metric", "value", "reference", "unit", "comment"), names(e)))
    dt_dark(e, dom = "t", pageLength = 30, ordering = FALSE)
  })
  output$opp_hist <- DT::renderDT({
    s <- sel(); shiny::req(s); ctx$store_tick()
    h <- store_history(ctx$con, s$rec$key)
    if (!nrow(h)) h <- data.table::data.table(Message = "No decisions recorded yet")
    dt_dark(h, dom = "t", pageLength = 20, ordering = FALSE)
  })

  # ---- the three small plots adapt to what the target has ----
  small <- function(p) plotly::layout(p, margin = list(l = 45, r = 40, t = 10, b = 35), font = list(size = 9))
  pattern_mini <- function(kind, r) {
    sn <- ctx$snap(); pt <- r$pattern
    if (is.na(pt) || !pt %in% sn$entity) return(NULL)
    switch(kind,
      secrf = { x <- sn[flooded == TRUE]; if (!nrow(x)) return(NULL); pr <- main_proto(ctx$res(), x)
        q <- add_proto(plotly::plot_ly(), pr, "sec_rf", max(1.2, max(x$dwi) * 1.05))
        q <- pattern_scatter(x, "dwi", "sec_rf", color = "opr", size = NULL, shape = FALSE, hl = pt, p = q)
        plotly::layout(pl_theme(q, "DWI", "Sec RF", legend = FALSE), yaxis = axis_style("Sec RF", tickformat = ".0%")) },
      units = { u <- ctx$units_snap()[pattern == pt]; if (!nrow(u)) return(NULL)
        q <- plotly::plot_ly(u, x = ~sand, y = ~dwi, type = "bar", name = "DWI", marker = list(color = ifelse(u$sand %in% r$sand, pal$crit, "#3b82f6")))
        q <- plotly::add_lines(q, x = ~sand, y = ~tp12 / 10, name = "TP/10", line = list(color = pal$warn))
        pl_theme(q, paste("Unit in", pt), "DWI (bars) · TP/10 (line)", legend = FALSE) },
      utiltp = { x <- sn[flooded == TRUE & is.finite(util)]; if (!nrow(x)) return(NULL)
        q <- pattern_scatter(x, "util", "tp12", color = "iwr12", size = NULL, shape = FALSE, hl = pt)
        plotly::layout(pl_theme(q, "Utilization", "Inj TP %/yr", legend = FALSE), shapes = list(hline_shape(ctx$settings()$target_tp, pal$warn))) })
  }
  plots <- shiny::reactive({
    s <- sel(); shiny::req(s); r <- s$rec
    ivw <- ctx$res()$ds$intervals; ivw <- if (!is.null(ivw)) ivw[well == r$well] else NULL
    has_prof <- identical(s$row$gain_src, "PROFILE")
    cand <- list(
      if (has_prof) function() plot_forecast(sel_profile()),
      function() plot_well_history(sel_hist(), 5),
      if (!is.null(ivw) && nrow(ivw)) function() plot_interval_strip(ivw, r$interval),
      if (!is.na(r$pattern)) function() pattern_mini(if (!is.na(r$sand)) "units" else "secrf", r),
      if (!is.na(r$pattern)) function() pattern_mini("utiltp", r))
    cand <- Filter(Negate(is.null), cand)
    cand[seq_len(min(3, length(cand)))]
  })
  slot <- function(i) plotly::renderPlotly({ f <- plots(); if (length(f) < i) return(empty_plot("")); p <- f[[i]](); if (is.null(p)) p <- empty_plot("No pattern context"); small(p) })
  output$opp_m_a <- slot(1); output$opp_m_b <- slot(2); output$opp_m_c <- slot(3)

  key_now <- function() { s <- sel(); if (is.null(s)) NULL else s$rec$key }
  bump <- function() ctx$store_tick(ctx$store_tick() + 1)

  shiny::observeEvent(input$opp_checks, {
    k <- key_now(); shiny::req(k)
    if (!identical(sort(input$opp_checks), sort(intersect(store_checks(ctx$con, k), sel()$rec$text$validation))))
      store_set_state(ctx$con, k, checks = input$opp_checks)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  shiny::observeEvent(input$opp_save_notes, { store_set_state(ctx$con, key_now(), notes = input$opp_notes); shiny::showNotification("Notes saved") })
  shiny::observeEvent(input$opp_open360, ctx$open_p360(sel()$row$pattern))
  shiny::observeEvent(input$opp_openw360, ctx$open_w360(sel()$rec$well))

  shiny::observeEvent(input$opp_validate, {
    s <- sel(); v <- s$rec$text$validation
    if (!all(v %in% input$opp_checks)) { shiny::showNotification("Confirm every validation item first", type = "warning"); return() }
    if (s$row$n_fam < 2) { shiny::showNotification("Screening signals need a second family of evidence before validation", type = "warning"); return() }
    pr <- if (identical(s$row$gain_src, "PROFILE")) sel_profile() else NULL
    store_freeze_forecast(ctx$con, s$rec$key, pr)
    store_set_state(ctx$con, s$rec$key, status = "validated_candidate", checks = input$opp_checks,
                    comment = paste("all validation items confirmed", if (!is.null(pr)) "; Bajo/Base/Alto forecast frozen" else ""))
    bump()
  })
  shiny::observeEvent(input$opp_reset, { store_set_state(ctx$con, key_now(), status = sel()$row$auto_status, comment = "reset"); bump() })

  shiny::observeEvent(input$opp_dismiss, shiny::showModal(shiny::modalDialog(title = "Dismiss opportunity",
    shiny::textAreaInput("opp_dismiss_reason", "Reason", rows = 3, width = "100%"),
    footer = htmltools::tagList(shiny::modalButton("Cancel"), shiny::actionButton("opp_dismiss_ok", "Dismiss", class = "btn-primary")))))
  shiny::observeEvent(input$opp_dismiss_ok, {
    store_set_state(ctx$con, key_now(), status = "dismissed", notes = input$opp_dismiss_reason, comment = input$opp_dismiss_reason)
    shiny::removeModal(); bump()
  })

  # ---- job builder: a job is the combination of the opportunities the engineer picks on one well ----
  # Only the picked opportunities move to executed; every other opportunity on the well stays as identified.
  open_statuses <- c("screening_only", "candidate", "validated_candidate")
  job_choices <- function(w) {
    s <- ctx$opps()$summary[well == w & as.character(status) %in% open_statuses]
    s[order(-score)]
  }
  # forecast of one opportunity: the one frozen at validation, else the current profile
  opp_forecast <- function(k) {
    fr <- store_frozen(ctx$con, k)
    if (!is.null(fr)) return(fr)
    row <- ctx$opps()$summary[key == k]
    if (!nrow(row) || !identical(row$gain_src, "PROFILE")) return(NULL)
    profile_rows(ctx$res()$ds$profiles, row$fkey)
  }
  show_job_modal <- function(w, pre = character()) {
    s <- job_choices(w)
    if (!nrow(s)) { shiny::showNotification(sprintf("No open opportunities on %s", w), type = "warning"); return() }
    lab <- sprintf("%s %s · %s · %s bopd%s", s$action, ifelse(is.na(s$sand), "", paste("unit", s$sand, data.table::fcoalesce(s$interval, ""))),
                   as.character(s$status), fmt_int(s$gain), ifelse(s$gain_src %in% "PROFILE", " (profile)", ""))
    shiny::showModal(shiny::modalDialog(title = sprintf("Propose a job on %s", w), easyClose = TRUE, size = "l",
      htmltools::p(class = "wf-muted small", sprintf("%d open opportunities on this well. Tick the ones this job will execute; the others stay as identified. A lead approves the job in Jobs.", nrow(s))),
      shiny::checkboxGroupInput("iv_keys", NULL, choices = stats::setNames(s$key, lab), selected = intersect(pre, s$key), width = "100%"),
      shiny::uiOutput("iv_preview"),
      plotly::plotlyOutput("iv_preview_plot", height = "230px"),
      bslib::layout_columns(col_widths = c(6, 6),
        shiny::checkboxInput("iv_als", "Change the artificial lift in this rig visit", FALSE),
        shiny::textInput("iv_job", "Job name (optional)", "")),
      shiny::textAreaInput("iv_notes", "Notes", rows = 2, width = "100%"),
      footer = htmltools::tagList(shiny::modalButton("Cancel"), shiny::actionButton("iv_ok", "Propose job for approval", class = "btn-primary"))))
  }
  job_fc <- shiny::reactive({
    keys <- input$iv_keys
    if (!length(keys)) return(list(fc = NULL, n = 0, with = 0))
    fcs <- lapply(keys, opp_forecast)
    list(fc = combine_forecasts(fcs), n = length(keys), with = sum(!vapply(fcs, is.null, TRUE)))
  })
  output$iv_preview <- shiny::renderUI({
    j <- job_fc()
    if (!j$n) return(htmltools::div(class = "wf-muted small", "Select at least one opportunity."))
    if (is.null(j$fc)) return(htmltools::div(class = "wf-muted small", sprintf("%d opportunities selected; none has a Bajo / Base / Alto profile.", j$n)))
    m1 <- j$fc[month == 1]; g <- function(sc, v) { x <- m1[scenario == sc][[v]]; if (length(x)) x else NA_real_ }
    np12 <- j$fc[scenario == "Base" & month <= 12, sum(qo) * days_per_month]
    htmltools::div(class = "wf-kpi-row compact",
      kpi("Selected", j$n, sprintf("%d with a profile", j$with)),
      kpi("Job oil, initial", sprintf("%s / %s / %s", fmt_int(g("Bajo", "qo")), fmt_int(g("Base", "qo")), fmt_int(g("Alto", "qo"))), "Bajo / Base / Alto, bopd"),
      kpi("Job water, initial", fmt_int(g("Base", "qw")), "Base, bwpd"), kpi("Job liquid", fmt_int(g("Base", "qf")), "Base, bfpd"),
      kpi("Oil 12 m", fmt_num(np12), "stb, Base"))
  })
  output$iv_preview_plot <- plotly::renderPlotly({
    j <- job_fc(); if (is.null(j$fc)) return(empty_plot("No combined profile"))
    plotly::layout(plot_forecast(j$fc), margin = list(l = 45, r = 45, t = 10, b = 35), font = list(size = 9))
  })
  shiny::observeEvent(input$opp_log, show_job_modal(sel()$rec$well, sel()$rec$key))
  if (!is.null(ctx$job_req)) shiny::observeEvent(ctx$job_req(), { q <- ctx$job_req(); show_job_modal(q$well, q$pre %||% character()) }, ignoreInit = TRUE)
  shiny::observeEvent(input$iv_ok, {
    keys <- input$iv_keys
    if (!length(keys)) { shiny::showNotification("Tick at least one opportunity", type = "warning"); return() }
    id <- tryCatch(job_propose(ctx$con, ctx$opps(), keys, ctx$res(), ctx$asof(), ctx$settings(), ctx$user(), input$iv_job, input$iv_notes, isTRUE(input$iv_als)),
                   error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(id)) return()
    shiny::removeModal(); bump()
    shiny::showNotification(sprintf("Job #%s proposed with %d opportunit%s; a lead approves it in Jobs", id, length(keys), if (length(keys) > 1) "ies" else "y"))
  })

  shiny::observeEvent(input$opp_outcome, {
    r <- sel()$rec
    shiny::showModal(shiny::modalDialog(title = "Record outcome", easyClose = TRUE,
      htmltools::p(class = "wf-muted", paste("Expected:", r$text$outcome)),
      shiny::radioButtons("oc_verdict", "Verdict", c("above Alto", "within range", "below Bajo", "met", "partly met", "not met"), inline = TRUE),
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
    pr <- if (!is.na(s$row$pattern)) ctx$snap()[entity == s$row$pattern] else NULL
    shiny::withProgress(message = "Drafting with AI", value = 0.4, {
      out <- tryCatch(ai_draft(ai_payload(r, s$row, pr, ctx$settings())), error = function(e) e)
    })
    if (inherits(out, "error")) { shiny::showNotification(conditionMessage(out), type = "error", duration = 10); return() }
    store_save_ai(ctx$con, r$key, out$model, out$text); bump()
  })

  # ---- portfolio map ----
  output$opp_bubble <- plotly::renderPlotly({
    s <- filtered()[as.character(status) %in% status_filter() & is.finite(np12)]
    if (!nrow(s)) return(empty_plot("No opportunity with a volume estimate"))
    s[, wp := data.table::fcoalesce(wp12, 0)]
    s[, sz := 8 + 26 * score / max(c(score, 1))]
    p <- plotly::plot_ly(source = "oppmap")
    for (a in unique(s$action)) {
      d <- s[action == a]
      p <- plotly::add_markers(p, data = d, x = ~np12, y = ~wp, name = action_label(a), customdata = ~key,
        marker = list(color = action_color(a), size = ~sz, opacity = 0.8, line = list(color = pal$bg, width = 1)),
        text = ~sprintf("%s %s<br>%s · %s<br>oil 12 m %s stb · water 12 m %s bbl<br>score %s", action, target_txt(d), lenses, drive, fmt_num(np12), fmt_num(wp), score),
        hoverinfo = "text")
    }
    plotly::event_register(pl_theme(p, "Oil in 12 months, stb", "Water in 12 months, bbl"), "plotly_click")
  })
  shiny::observeEvent(suppressWarnings(plotly::event_data("plotly_click", source = "oppmap")), {
    k <- suppressWarnings(plotly::event_data("plotly_click", source = "oppmap"))$customdata
    if (length(k)) { ctx$sel_opp(unlist(k)[1]); bslib::nav_select("opp_tabs", "cand") }
  }, ignoreInit = TRUE)

  # ---- board ----
  output$opp_board <- shiny::renderUI({
    s <- filtered()
    if (!nrow(s)) return(htmltools::div(class = "wf-muted", "No opportunities"))
    htmltools::div(class = "wf-board", lapply(status_levels, function(k) {
      x <- s[as.character(status) == k]
      htmltools::div(class = "wf-col", htmltools::div(class = "wf-col-h", style = sprintf("--fc:%s", status_colors[[k]]), sprintf("%s (%d)", k, nrow(x))),
        lapply(seq_len(nrow(x)), function(i) htmltools::tags$a(class = "wf-bcard", href = "#",
          onclick = sprintf("Shiny.setInputValue('board_pick', '%s', {priority: 'event'}); return false;", x$key[i]),
          htmltools::span(class = "wf-act", style = sprintf("--tc:%s", action_color(x$action[i])), x$action[i]),
          htmltools::span(target_txt(x[i])),
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

  output$opp_rules <- shiny::renderUI({
    st <- ctx$settings()
    pr <- data.table::data.table(
      Rule = paste(names(opp_types), opp_types), `Well action` = paste(pattern_rule_action, action_label(pattern_rule_action)),
      Target = c("dominant injector, unit", "dominant injector", "producer with the largest pattern share (high fluid level first)",
                 "dominant injector, unit", "dominant injector, top unit", "dominant injector"),
      `Screening signal` = c(sprintf("Unit DWI in the area's top %d %% and above 1.2x the pattern DWI", round(100 * (1 - st$unit_dwi_q))),
                             sprintf("Inj TP down more than %d %% in 12 months, or below half the target", round(100 * st$tp_drop)),
                             sprintf("IWR 12 m above %.1f", st$iwr_high), "Logged ADPERF in the last 3 years in a unit with little injection",
                             "DWI at or above the area median with OPR below 1", sprintf("Inj TP more than %d %% off target", round(100 * st$tp_off))))
    wr <- data.table::data.table(
      Rule = paste(names(well_rules), well_rules), `Well action` = c("ADPERF", "WSO", "STIM_PROD or LIFT", "REACTIVATE"),
      Source = c("Intervals (closed or partly open) + Profiles", "Intervals (open)", "Wells monthly rates, WellStatus", "Wells monthly rates"),
      `Evidence` = c(sprintf("M: Np/OOIP <= %s or Sw actual <= %s · U: kh >= unit median and BSW < %s %% · S: Voronoi area >= unit median · V: injection support of the unit in the well's waterflood patterns (TP >= half the target)",
                             st$int_npooip_max, st$int_sw_max, st$int_bsw_max),
                     sprintf("U: initial BSW >= %s %% · M: Sw actual >= %s · O: well water cut near the limit", st$wso_bsw, st$int_sw_max),
                     sprintf("V: oil %d %% below the well's own decline over 6 months · O: fluid level >= %s ft (then LIFT) · M: Sw actual of the open intervals", round(100 * st$decline_drop), st$dfl_high),
                     sprintf("O: no production for %d months · M: last rate above field median or Sw actual of the open intervals", st$shutin_months)))
    htmltools::tagList(
      htmltools::p(class = "wf-muted", "Candidate = two or more evidence families including maturity (M) or velocity (V). Pattern rules run only on waterflood patterns;",
                   "well rules run in every drive. A well outside the waterflood patterns is primary. Thresholds and weights are in Data & reference › Settings."),
      htmltools::h6(class = "wf-h6", "Pattern lens (waterflood, other injection methods)"), DT::renderDT(dt_dark(pr, dom = "t", ordering = FALSE)),
      htmltools::h6(class = "wf-h6", "Well lens (all drives)"), DT::renderDT(dt_dark(wr, dom = "t", ordering = FALSE)),
      htmltools::p(class = "wf-muted small", "Other analyses: table Findings (Source, Well, Unit, Interval_ID, Action, Family, Metric, Value, Reference, Comment, Gain_bopd). Each source is its own lens; rows on the same target merge with the app's findings, and Profiles with the same keys give their forecast."),
      htmltools::p(class = "wf-muted small", sprintf("Priority score = %d %% evidence count + %d %% gain rate + %d %% EUR / remaining oil − %d %% Bajo-Alto spread, each scaled to the largest candidate. Volumes only: set WF_ECON_FILE to plug in the economic function.",
                                                     round(100 * st$w_evidence), round(100 * st$w_gain), round(100 * st$w_stake), round(100 * st$w_unc))))
  })
}
