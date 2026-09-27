# DATA & REFERENCE: tables, reconciliation, prototypes, analytics (ML), settings, method ------

data_ui <- function() {
  htmltools::tagList(
    htmltools::div(class = "wf-section-intro", htmltools::h2("Data"),
      htmltools::p("Load your tables (sheet or file names as in the schema: Wells, Alloc, Vol, Fluids, InjSand, InjSand_status, plus optional Hierarchy, Baseline, Prototypes, Prototype_Assign, Interventions, WellStatus).",
                   "Derived tables (Patterns, Patterns_Vel, Patterns_Mat, InjSand_calc) are only used for reconciliation.")),
    bslib::layout_columns(col_widths = c(4, 8), fill = FALSE,
      bslib::card(card_title("Load data"), bslib::card_body(
        shiny::fileInput("data_files", "Excel workbook or CSV files", multiple = TRUE, accept = c(".xlsx", ".csv")),
        htmltools::div(class = "wf-btn-row",
          shiny::actionButton("data_demo", "Reload demo field", class = "btn-outline-info btn-sm"),
          shiny::downloadButton("data_template", "Excel template", class = "btn-outline-info btn-sm"),
          shiny::downloadButton("data_export", "Export results", class = "btn-primary btn-sm")),
        htmltools::p(class = "wf-muted small", "Uploading a single table replaces only that table, so a new Alloc or InjSand can be swapped in."))),
      bslib::card(card_title("Table status", "role: source, reference, optional, derived"), bslib::card_body(shiny::uiOutput("data_status")))),
    bslib::layout_columns(col_widths = c(6, 6), fill = FALSE,
      bslib::card(card_title("Validation"), bslib::card_body(fillable = FALSE, DT::DTOutput("data_issues"))),
      bslib::card(card_title("Data dictionary"), bslib::card_body(fillable = FALSE, DT::DTOutput("data_dict"))))
  )
}

reconcile_ui <- function() {
  htmltools::tagList(
    htmltools::div(class = "wf-section-intro", htmltools::h2("Reconciliation"),
      htmltools::p("The app's results compared with the derived tables you uploaded. Gaps point at the pattern and month behind them, to agree on windows, baselines and prototype versions.")),
    bslib::card(card_title("Summary", tag = "new"), bslib::card_body(DT::DTOutput("rec_summary"))),
    bslib::card(card_title("Largest gaps"), bslib::card_body(DT::DTOutput("rec_gaps"))))
}

prototypes_ui <- function() {
  htmltools::tagList(
    htmltools::div(class = "wf-section-intro", htmltools::h2("Prototypes"),
      htmltools::p("Expected performance vs DWI. Uploaded curves (simulation or analog), analogs built here from patterns you choose, and a Buckley-Leverett reference.",
                   "Versions are kept: saving an analog under a new version never rewrites earlier readings.")),
    bslib::layout_columns(col_widths = c(8, 4), fill = FALSE,
      bslib::card(height = 460, card_title("Curves", NULL, shiny::selectInput("proto_var", NULL, c("Sec RF" = "sec_rf", "DWP" = "dwp", "Utilization" = "util", "WOR" = "wor"), width = "140px")),
        bslib::card_body(plotly::plotlyOutput("proto_plot", height = "100%"))),
      bslib::card(height = 460, card_title("Comparison basis"), bslib::card_body(
        shiny::selectInput("proto_override", "Compare every pattern with", c("Assigned prototype (per pattern)" = ""), width = "100%"),
        htmltools::p(class = "wf-muted small", "The override is for what-if comparisons; assignments from Prototype_Assign stay the reference."),
        DT::DTOutput("proto_assign")))),
    bslib::card(card_title("Build an analog prototype", "median trajectory of the chosen patterns", tag = "new"), bslib::card_body(
      bslib::layout_columns(col_widths = c(6, 3, 3),
        shiny::selectizeInput("proto_pats", "Analog patterns", choices = NULL, multiple = TRUE, width = "100%", options = list(placeholder = "pick 3 or more mature, well-behaved patterns")),
        shiny::textInput("proto_name", "Name", "App analog"), shiny::textInput("proto_ver", "Version", "v1")),
      htmltools::div(class = "wf-btn-row", shiny::actionButton("proto_suggest", "Suggest analogs (ML)", class = "btn-sm btn-outline-info"),
                     shiny::actionButton("proto_save", "Save prototype", class = "btn-sm btn-primary")))))
}

analytics_ui <- function() {
  htmltools::tagList(
    htmltools::div(class = "wf-section-intro", htmltools::h2("Analytics"),
      htmltools::p("Cluster analysis on the dimensionless variables groups patterns by behaviour. Choose 'Cluster' under Colour points by in the sidebar to see the groups on every plot.",
                   "Outlier scores flag patterns far from their group; nearest neighbours suggest analogs.")),
    bslib::layout_columns(col_widths = c(3, 9), fill = FALSE,
      bslib::card(card_title("Settings", tag = "new"), bslib::card_body(
        shiny::checkboxGroupInput("ml_features", "Variables", stats::setNames(names(ml_features), ml_features), selected = c("dwi", "sec_rf", "opr", "wpr", "util", "tp12", "iwr12", "lwor")),
        shiny::radioButtons("ml_method", "Method", c("k-means" = "kmeans", "Hierarchical (Ward)" = "hclust"), inline = TRUE),
        shiny::sliderInput("ml_k", "Clusters (0 = best silhouette)", 0, 6, 0, step = 1))),
      bslib::layout_columns(col_widths = c(6, 6),
        bslib::card(height = 400, card_title("Patterns on the first two principal components"), bslib::card_body(plotly::plotlyOutput("ml_pca", height = "100%"))),
        bslib::card(height = 400, card_title("Cluster profiles", "mean standardized value"), bslib::card_body(plotly::plotlyOutput("ml_profile", height = "100%"))),
        bslib::card(height = 360, card_title("Choosing k", "mean silhouette"), bslib::card_body(plotly::plotlyOutput("ml_scan", height = "100%"))),
        bslib::card(card_title("Assignments, outliers and nearest analogs"), bslib::card_body(fillable = FALSE, DT::DTOutput("ml_table"))))))
}

settings_ui <- function() {
  st <- default_settings
  num <- function(id, label, value, step, lci = FALSE) htmltools::div(class = "wf-set",
    shiny::numericInput(paste0("set_", id), htmltools::tagList(label, if (lci) htmltools::span(class = "wf-lci", "LCI reference")), value, step = step, width = "100%"))
  htmltools::tagList(
    htmltools::div(class = "wf-section-intro", htmltools::h2("Settings"),
      htmltools::p("Operating targets and screening limits. Values labelled LCI reference come from the La Cira-Infantas methodology and are not universal.")),
    bslib::card(bslib::card_body(htmltools::div(class = "wf-settings-grid",
      num("target_tp", "Target injection TP (%HCPV/yr)", st$target_tp, 0.5, TRUE), num("iwr_low", "Under-balanced IWR below", st$iwr_low, 0.05, TRUE),
      num("iwr_high", "Over-balanced IWR above", st$iwr_high, 0.05, TRUE),
      htmltools::div(class = "wf-set", shiny::selectInput("set_util_window", "Utilization window (months)", c(3, 6, 12), st$util_window)),
      num("judge_dwi", "OPR / WPR judged from DWI", st$judge_dwi, 0.05),
      htmltools::div(class = "wf-set", shiny::selectInput("set_baseline_method", "Secondary baseline", c("Np at flood start" = "start", "+ primary decline" = "decline"), st$baseline_method)),
      num("tp_drop", "Injectivity loss: TP drop in 12 m", st$tp_drop, 0.05), num("tp_off", "Rate change: TP off target by", st$tp_off, 0.05),
      num("unit_dwi_q", "Swept unit: DWI quantile", st$unit_dwi_q, 0.05), num("loss_high", "Loss threshold (method 2)", st$loss_high, 0.05),
      num("evr_low", "Evol ratio threshold (method 2)", st$evr_low, 0.05), num("dfl_high", "High fluid level (ft)", st$dfl_high, 50),
      num("early_dwi", "Stage: developing from DWI", st$early_dwi, 0.05), num("mature_dwi", "Stage: mature from DWI", st$mature_dwi, 0.1),
      num("late_dwi", "Stage: late life from DWI", st$late_dwi, 0.1), num("w_evidence", "Score weight: evidence", st$w_evidence, 0.05),
      num("w_gain", "Score weight: oil gain", st$w_gain, 0.05), num("w_stake", "Score weight: remaining oil", st$w_stake, 0.05)))))
}

method_ui <- function() {
  htmltools::tagList(htmltools::div(class = "wf-section-intro", htmltools::h2("Method")),
    bslib::card(bslib::card_body(class = "wf-method", shiny::HTML(paste0(
      "<h5>Lineage</h5><p>Wells + Alloc &rarr; pattern rates &middot; + Vol, Fluids, Baseline, Prototypes &rarr; maturity &middot; + windows &rarr; velocity &middot; InjSand + Alloc + Vol &rarr; unit metrics &middot; all of it &rarr; opportunities &rarr; interventions &rarr; outcomes. Derived tables are recalculated on every load.</p>",
      "<h5>Variables (reservoir volumes)</h5><ul>",
      "<li>DWI = &Sigma;Wi&middot;Bw / HCPV; RF = &Sigma;Np&middot;Bo / HCPV; Sec RF = (&Sigma;Np &minus; Np at baseline)&middot;Bo / HCPV</li>",
      "<li>DWP = &Sigma;Wp&middot;Bw / HCPV; DTP = &Sigma;(Np&middot;Bo + Wp&middot;Bw) / HCPV; Loss = DWI &minus; DTP (withdrawals since flood start)</li>",
      "<li>Inj TP = monthly Wi&middot;Bw / HCPV &times; 365 / days (%/yr); TP 12 m = mean of the last 12 months; Prod TP likewise; IWR 12 m = &Sigma;12 Wi&middot;Bw / &Sigma;12 (Np&middot;Bo + Wp&middot;Bw)</li>",
      "<li>Utilization = &Sigma;w Wi&middot;Bw / &Sigma;w Np&middot;Bo over 3, 6 or 12 months; cumulative since flood start</li>",
      "<li>OPR = Sec RF / expected Sec RF, WPR = DWP / expected DWP, at the same DWI, from the prototype valid at that month; not judged below the DWI threshold</li>",
      "<li>Unit injection = InjSand profile shares (held until the next profile) &times; Wells.BWIPD, allocated with Alloc; unit DWI and TP use the unit HCPV. VRF and Cobb are shown and used as operations evidence.</li>",
      "<li>Heterogeneity index = cumulative well volume / area average &minus; 1 (oil and water)</li>",
      "<li>Waterflood fit (SPE-96469): Sec RF = A&middot;(1 &minus; e<sup>&minus;C&middot;DWI</sup>); remaining = A&middot;e<sup>&minus;C&middot;DWI</sup>; forecasts at any TP</li>",
      "<li>Method 2 (SPE-190314 fig. 25): Evol(MB)/Evol(FF) = Sec RF / displacement implied by the current water cut (Welge), against Loss</li></ul>",
      "<h5>Opportunities</h5><p>Evidence families: maturity, velocity, unit, spatial, operations. One family = screening_only; two or more including maturity or velocity = candidate; validated, executed and evaluated are set by engineers and stored with history.</p>")))))
}

data_server <- function(input, output, session, ctx) {
  output$data_status <- shiny::renderUI({
    ds <- ctx$ds(); iss <- ds$issues
    htmltools::div(class = "wf-table-grid", lapply(names(wf_schema), function(k) {
      d <- ds[[k]]; sp <- wf_schema[[k]]
      ne <- if (nrow(iss)) iss[table == k & severity == "error", .N] else 0
      nw <- if (nrow(iss)) iss[table == k & severity == "warning", .N] else 0
      tone <- if (is.null(d)) (if (k %in% required_tables) "crit" else "none") else if (ne) "crit" else if (nw) "warn" else "ok"
      htmltools::div(class = paste("wf-table-box", paste0("tone-", tone)),
        htmltools::div(class = "wf-table-name", sp$title), htmltools::div(class = "wf-table-role", sp$role),
        htmltools::div(class = "wf-table-rows", if (is.null(d)) "not loaded" else paste(fmt_num(nrow(d)), "rows")))
    }), htmltools::div(class = "wf-muted small", paste("Dataset:", ds$name)))
  })
  output$data_issues <- DT::renderDT({
    iss <- ctx$ds()$issues
    if (!nrow(iss)) iss <- data.table::data.table(table = "all", severity = "info", message = "No issues found")
    sc <- c(error = pal$crit, warning = pal$warn, info = pal$muted)
    dt_dark(iss[, .(Severity = badge_html(severity, sc[severity]), Table = table, Issue = message)], escape = FALSE, pageLength = 8)
  })
  output$data_dict <- DT::renderDT({
    rows <- list()
    for (k in names(wf_schema)) for (c in wf_schema[[k]]$cols)
      rows[[length(rows) + 1]] <- data.table::data.table(Table = k, Role = wf_schema[[k]]$role, Column = c$name, Required = if (c$required) "yes" else "",
                                                          Unit = c$unit, Aliases = paste(c$aliases, collapse = ", "))
    dt_dark(data.table::rbindlist(rows), pageLength = 8)
  })
  output$data_template <- shiny::downloadHandler(filename = function() "floodpulse_v2_template.xlsx", content = function(file) write_template(file))
  output$data_export <- shiny::downloadHandler(
    filename = function() paste0("floodpulse_results_", format(ctx$asof(), "%Y%m"), ".xlsx"),
    content = function(file) {
      sn <- data.table::copy(ctx$snap()); sn[, stage := as.character(stage)]
      op <- data.table::copy(ctx$opps()$summary); if (nrow(op)) op[, status := as.character(status)]
      ev <- data.table::rbindlist(lapply(ctx$opps()$records, function(r) cbind(key = r$key, r$evidence)), fill = TRUE)
      s <- ctx$series_all()
      writexl::write_xlsx(list(
        PatternMaturity = s[, .(pattern = entity, date, np = cum_oil, nw = cum_water, nwi = cum_winj, dwi, rf, sec_rf, dwp, dtp, loss, iwr_cum, opr, wpr, proto_key)],
        PatternVelocity = s[, .(pattern = entity, date, tp, tp12, prod_tp12, iwr, iwr12, util3, util6, util12, wor, wc)],
        PatternSandMetrics = ctx$res()$units$pattern_sand[, .(pattern, sand, date, winj, cum_winj, dwi, tp, tp12, rate, cobb)],
        Snapshot = sn[, !sapply(sn, is.list), with = FALSE], Opportunity = op, OpportunityEvidence = ev,
        Interventions = store_interventions(ctx$con), Prototypes = ctx$res()$protos), file)
    })

  rec <- shiny::reactive(reconcile(ctx$res(), ctx$series_all()))
  output$rec_summary <- DT::renderDT({
    r <- rec()$summary
    if (!nrow(r)) return(dt_dark(data.table::data.table(Message = "Upload Patterns, Patterns_Vel, Patterns_Mat or InjSand_calc to reconcile")))
    dt_dark(r[, .(Table = c(pm = "Patterns_Mat", pv = "Patterns_Vel", p = "Patterns")[table], Variable = variable, Rows = rows,
                  `Within tolerance` = sprintf("%.1f %%", 100 * within), `Largest gap` = largest)], dom = "t", pageLength = 30)
  })
  output$rec_gaps <- DT::renderDT({
    g <- rec()$gaps; if (!nrow(g)) return(dt_dark(data.table::data.table(Message = "No gaps")))
    dt_dark(g[, .(Variable = variable, Pattern = entity, Month = format(date, "%b %Y"), Yours = signif(theirs, 4), App = signif(ours, 4), Difference = signif(diff, 3))], pageLength = 10)
  })

  # prototypes
  shiny::observe({
    r <- ctx$res(); shiny::req(r)
    shiny::updateSelectInput(session, "proto_override", choices = c("Assigned prototype (per pattern)" = "", unique(r$protos$pkey)), selected = ctx$settings()$prototype_override)
    shiny::updateSelectizeInput(session, "proto_pats", choices = sort(r$props$pattern), server = TRUE)
  })
  output$proto_plot <- plotly::renderPlotly({
    pr <- ctx$res()$protos; v <- input$proto_var
    p <- plotly::plot_ly()
    for (k in unique(pr$pkey)) { d <- pr[pkey == k & is.finite(get(v))]; if (nrow(d)) p <- plotly::add_lines(p, x = d$dwi, y = d[[v]], name = k) }
    if (length(input$proto_pats) >= 3) {
      a <- analog_prototype(ctx$series_all()[date <= ctx$asof()], "preview", "draft", input$proto_pats)
      if (nrow(a)) p <- plotly::add_lines(p, x = a$dwi, y = a[[v]], name = "Draft analog", line = list(dash = "dash", width = 3, color = "#fde047"))
    }
    plotly::layout(pl_theme(p, "DWI", v), yaxis = axis_style(v, type = if (v == "wor") "log" else "linear"))
  })
  output$proto_assign <- DT::renderDT({
    a <- ctx$res()$assign
    dt_dark(a[, .(Pattern = pattern, Prototype = pkey, `Valid from` = ifelse(valid_from < as.Date("1900-01-01"), "start", format(valid_from)))], dom = "tp", pageLength = 8)
  })
  shiny::observeEvent(input$proto_suggest, {
    ml <- ctx$ml(); sn <- ctx$snap()[flooded == TRUE]
    if (is.null(ml)) { shiny::showNotification("Not enough patterns to cluster", type = "warning"); return() }
    best <- merge(ml$assign, sn[, .(entity, opr, dwi)], by = "entity")[, .(m = stats::median(opr, na.rm = TRUE), d = stats::median(dwi)), by = cluster][order(-m)][1]
    pats <- ml$assign[cluster == best$cluster, entity]
    shiny::updateSelectizeInput(session, "proto_pats", selected = pats)
    shiny::showNotification(sprintf("Suggested the cluster with the highest median OPR (%s)", paste(pats, collapse = ", ")))
  })
  shiny::observeEvent(input$proto_save, {
    if (length(input$proto_pats) < 3) { shiny::showNotification("Pick at least 3 patterns", type = "warning"); return() }
    a <- analog_prototype(ctx$series_all()[date <= ctx$asof()], input$proto_name, input$proto_ver, input$proto_pats)
    if (!nrow(a)) { shiny::showNotification("Those patterns do not overlap enough in DWI", type = "warning"); return() }
    store_save_prototype(ctx$con, a); ctx$proto_tick(ctx$proto_tick() + 1)
    shiny::showNotification(sprintf("Saved %s | %s", input$proto_name, input$proto_ver))
  })

  # analytics
  output$ml_pca <- plotly::renderPlotly({
    ml <- ctx$ml(); if (is.null(ml)) return(empty_plot("Need at least 4 flooded patterns"))
    a <- ml$assign
    p <- plotly::plot_ly(source = "wf")
    lv <- sort(unique(a$cluster_name)); cols <- stats::setNames(rep(cluster_palette, length.out = length(lv)), lv)
    for (g in lv) { d <- a[cluster_name == g]
      p <- plotly::add_markers(p, x = d$pc1, y = d$pc2, name = g, customdata = d$entity, text = sprintf("%s<br>outlier %.2f", d$entity, d$outlier), hoverinfo = "text",
                               marker = list(size = 10 + 6 * pmin(d$outlier, 2), color = cols[[g]], line = list(color = "#0b1220", width = 1))) }
    p <- plotly::add_text(p, x = a$pc1, y = a$pc2, text = a$entity, textposition = "top center", textfont = list(size = 9, color = pal$muted), showlegend = FALSE, hoverinfo = "skip")
    L <- ml$pca$rotation[, 1:2] * max(abs(ml$pca$x[, 1:2])) * 0.8
    for (i in seq_len(nrow(L))) p <- plotly::add_annotations(p, x = L[i, 1], y = L[i, 2], ax = 0, ay = 0, axref = "x", ayref = "y", text = ml$labels[[rownames(L)[i]]],
                                                              showarrow = TRUE, arrowcolor = "#64748b", arrowhead = 0, font = list(size = 9, color = "#94a3b8"))
    p <- pl_theme(p, sprintf("PC1 (%.0f %%)", 100 * ml$var_explained[1]), sprintf("PC2 (%.0f %%)", 100 * ml$var_explained[2]))
    plotly::event_register(p, "plotly_click")
  })
  output$ml_profile <- plotly::renderPlotly({
    ml <- ctx$ml(); shiny::req(ml)
    lab <- unname(ml$labels[colnames(ml$centers)])
    p <- plotly::plot_ly(x = lab, y = paste0("C", rownames(ml$centers)), z = ml$centers, type = "heatmap",
                         colorscale = list(list(0, "#1d4ed8"), list(0.5, "#1e293b"), list(1, "#f87171")), zmid = 0,
                         hovertemplate = "%{y} · %{x}: %{z:.2f} sd<extra></extra>")
    pl_theme(p, legend = FALSE)
  })
  output$ml_scan <- plotly::renderPlotly({
    ml <- ctx$ml(); shiny::req(ml)
    p <- plotly::plot_ly(ml$scan, x = ~k, y = ~silhouette, type = "scatter", mode = "lines+markers", line = list(color = pal$accent))
    plotly::layout(pl_theme(p, "Number of clusters", "Mean silhouette", legend = FALSE), shapes = list(vline_shape(ml$k, pal$warn)))
  })
  output$ml_table <- DT::renderDT({
    ml <- ctx$ml(); shiny::req(ml)
    a <- data.table::copy(ml$assign)
    a[, analogs := vapply(entity, function(e) { x <- ml_analogs(ml, e); paste(x$entity, collapse = ", ") }, "")]
    dt_dark(a[, .(Pattern = entity, Cluster = cluster_name, Silhouette = round(silhouette, 2), `Outlier score` = round(outlier, 2), `Nearest analogs` = analogs)], pageLength = 8)
  })
}

read_settings <- function(input) {
  st <- default_settings
  for (nm in names(st)) {
    v <- input[[paste0("set_", nm)]]
    if (is.null(v)) next
    if (is.numeric(st[[nm]])) { v <- suppressWarnings(as.numeric(v)); if (length(v) && is.finite(v)) st[[nm]] <- v }
    else if (is.character(st[[nm]]) && length(st[[nm]]) == 1) st[[nm]] <- as.character(v)
  }
  st$prototype_override <- input$proto_override %||% ""
  st
}
