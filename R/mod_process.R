# PROCESS: how the flood is being run month to month -------------------------

process_ui <- function() {
  htmltools::tagList(
    htmltools::div(class = "wf-section-intro",
      htmltools::h2("Process"),
      htmltools::p("How the flood is being operated: injection vs withdrawals, voidage replacement,",
                   "water handling efficiency and the exceptions that need attention this month.")
    ),
    shiny::uiOutput("pro_kpis"),
    bslib::layout_columns(col_widths = c(7, 5), fill = FALSE,
      bslib::card(full_screen = TRUE, height = 430,
        card_title("Production & injection", "allocated rates for the selected scope"),
        bslib::card_body(plotly::plotlyOutput("pro_rates", height = "100%"))
      ),
      bslib::card(full_screen = TRUE, height = 430,
        card_title("Voidage balance", "reservoir barrels in vs out"),
        bslib::card_body(plotly::plotlyOutput("pro_voidage", height = "100%"))
      )
    ),
    bslib::card(full_screen = TRUE, height = 520,
      card_title("Pulse board", "every pattern, every month; click a cell to drill down",
                 shiny::selectInput("pro_pulse_metric", NULL, width = "230px",
                   metric_choices(c("vrr", "wc", "qo", "qwi", "inj_eff", "hcpvi", "ev", "rf")), "vrr"),
                 shiny::selectInput("pro_pulse_window", NULL, width = "150px",
                   c("Last 3 years" = 36, "Last 5 years" = 60, "Last 10 years" = 120, "All history" = 9999), 60)),
      bslib::card_body(plotly::plotlyOutput("pro_pulse", height = "100%"))
    ),
    bslib::layout_columns(col_widths = c(7, 5), fill = FALSE,
      bslib::card(full_screen = TRUE, height = 480,
        card_title("Exception feed", "automated diagnostics at the as-of month",
                   shiny::checkboxGroupInput("pro_sev", NULL, inline = TRUE,
                     choices = c("critical", "warning", "opportunity", "info"), selected = c("critical", "warning", "opportunity"))),
        bslib::card_body(DT::DTOutput("pro_flags"))
      ),
      bslib::card(full_screen = TRUE, height = 480,
        card_title("Water utilisation", "bbl injected per bbl of incremental oil (lower is better)"),
        bslib::card_body(plotly::plotlyOutput("pro_wuf", height = "100%"))
      )
    )
  )
}

process_server <- function(input, output, session, ctx) {

  output$pro_kpis <- shiny::renderUI({
    s <- ctx$scope_series(); a <- ctx$asof()
    shiny::req(nrow(s))
    s <- s[date <= a]
    last <- s[.N]; l6 <- utils::tail(s, 6); prev <- if (nrow(s) > 12) s[.N - 12] else NULL
    vrr6 <- sum(l6$winj_rb) / max(sum(l6$oil_rb + l6$water_rb), 1e-9)
    delta <- function(now, old, f = fmt_num) if (is.null(old) || !is.finite(old)) NULL else paste0("vs 12 m ago: ", f(old))
    htmltools::div(class = "wf-kpi-row",
      kpi("Oil rate · bopd", fmt_num(last$qo), delta(last$qo, prev$qo), tone = "oil"),
      kpi("Water rate · bwpd", fmt_num(last$qw), delta(last$qw, prev$qw)),
      kpi("Injection · bwipd", fmt_num(last$qwi), delta(last$qwi, prev$qwi), tone = "inj"),
      kpi("Water cut", fmt_pct(last$wc), delta(last$wc, prev$wc, fmt_pct)),
      kpi("VRR", fmt_num(last$vrr, 2), paste("6-month", fmt_num(vrr6, 2)),
          tone = if (is.finite(vrr6) && vrr6 < ctx$settings()$vrr_low) "warn" else "ok"),
      kpi("VRR since flood start", fmt_num(last$vrr_wf, 2), paste("all-time", fmt_num(last$cum_vrr, 2))),
      kpi("Water utilisation", fmt_num(last$wuf, 1), "bbl inj / bbl WF oil"),
      kpi("Injection efficiency", fmt_num(1000 * last$inj_eff, 0), "stb oil per 1000 bbl inj")
    )
  })

  vline <- function(a) list(type = "line", x0 = a, x1 = a, yref = "paper", y0 = 0, y1 = 1,
                            line = list(color = pal$accent, width = 1, dash = "dot"))

  output$pro_rates <- plotly::renderPlotly({
    s <- ctx$scope_series(); shiny::req(nrow(s))
    p <- plotly::plot_ly(s, x = ~date)
    p <- plotly::add_lines(p, y = ~qo, name = "Oil (bopd)", line = list(color = pal$oil, width = 2))
    p <- plotly::add_lines(p, y = ~qw, name = "Water (bwpd)", line = list(color = pal$water, width = 1.5))
    p <- plotly::add_lines(p, y = ~qwi, name = "Injection (bwipd)", line = list(color = pal$inj, width = 1.5))
    p <- plotly::add_lines(p, y = ~wc, name = "Water cut", yaxis = "y2", line = list(color = "#e2e8f0", width = 1, dash = "dot"))
    p <- pl_theme(p, NULL, "stb/d")
    plotly::layout(p, shapes = list(vline(ctx$asof())), hovermode = "x unified",
      yaxis2 = axis_style("Water cut", overlaying = "y", side = "right", tickformat = ".0%", range = c(0, 1), showgrid = FALSE))
  })

  output$pro_voidage <- plotly::renderPlotly({
    s <- data.table::copy(ctx$scope_series()); shiny::req(nrow(s))
    s[hcpvi < 0.02, vrr_wf := NA]  # too early after flood start to be meaningful
    p <- plotly::plot_ly(s, x = ~date)
    p <- plotly::add_bars(p, y = ~winj_rb / days, name = "Injection (rb/d)", marker = list(color = pal$inj))
    p <- plotly::add_bars(p, y = ~-(oil_rb + water_rb) / days, name = "Withdrawals (rb/d)", marker = list(color = pal$oil))
    p <- plotly::add_lines(p, y = ~vrr_wf, name = "VRR since flood start", yaxis = "y2", line = list(color = "#fde047", width = 2))
    p <- plotly::add_lines(p, y = ~cum_vrr, name = "VRR cumulative", yaxis = "y2", line = list(color = "#e2e8f0", width = 1, dash = "dot"))
    p <- pl_theme(p, NULL, "rb/d")
    plotly::layout(p, barmode = "relative", bargap = 0, hovermode = "x unified", shapes = list(vline(ctx$asof()),
      list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y2", y0 = 1, y1 = 1, line = list(color = "#fde047", width = 0.8, dash = "dash"))),
      yaxis2 = axis_style("VRR", overlaying = "y", side = "right", range = c(0, 2), showgrid = FALSE))
  })

  output$pro_pulse <- plotly::renderPlotly({
    key <- input$pro_pulse_metric
    s <- ctx$series(); a <- ctx$asof()
    shiny::req(nrow(s))
    win <- as.numeric(input$pro_pulse_window)
    months <- sort(unique(s$date)); months <- months[months <= a]
    months <- utils::tail(months, win)
    s <- s[date %in% months]
    z <- data.table::dcast(s, entity ~ date, value.var = key)
    mat <- as.matrix(z[, -1])
    ents <- z$entity
    if (key %in% c("vrr")) {
      zc <- pmin(mat, 2)
      cs <- list(list(0, "#b91c1c"), list(0.35, "#f87171"), list(0.5, "#1e293b"), list(0.65, "#60a5fa"), list(1, "#1d4ed8"))
      p <- plotly::plot_ly(x = as.Date(colnames(mat)), y = ents, z = zc, type = "heatmap", colorscale = cs, zmin = 0, zmax = 2,
        source = "wf", customdata = matrix(ents, nrow = length(ents), ncol = ncol(mat)),
        hovertemplate = "%{y} · %{x|%b %Y}<br>VRR %{z:.2f}<extra></extra>", colorbar = list(thickness = 10, tickfont = list(color = pal$muted)))
    } else {
      p <- plotly::plot_ly(x = as.Date(colnames(mat)), y = ents, z = mat, type = "heatmap", colorscale = cscale(key),
        source = "wf", customdata = matrix(ents, nrow = length(ents), ncol = ncol(mat)),
        hovertemplate = paste0("%{y} · %{x|%b %Y}<br>", metric_catalog[[key]]$label, " %{z:.3g}<extra></extra>"),
        colorbar = list(thickness = 10, tickfont = list(color = pal$muted)))
    }
    p <- pl_theme(p, NULL, NULL, legend = FALSE)
    p <- plotly::layout(p, yaxis = axis_style(NULL, type = "category", autorange = "reversed"), margin = list(l = 70))
    plotly::event_register(p, "plotly_click")
  })

  flags_data <- shiny::reactive({
    f <- ctx$snap()$flags
    if (!nrow(f)) return(data.table::data.table())
    f <- f[severity %in% input$pro_sev]
    f[, rank := sev_rank[severity]]
    data.table::setorder(f, rank, entity)
    f[, .(Severity = sev_badge_html(severity), Entity = entity, Diagnosis = message, `Recommended action` = action)]
  })
  output$pro_flags <- DT::renderDT({
    d <- flags_data()
    if (!nrow(d)) d <- data.table::data.table(Message = "No exceptions for this selection")
    dt_dark(d, escape = FALSE, pageLength = 8)
  })
  shiny::observeEvent(input$pro_flags_rows_selected, {
    d <- flags_data()
    if (nrow(d)) ctx$open_drill(d$Entity[input$pro_flags_rows_selected])
  })

  output$pro_wuf <- plotly::renderPlotly({
    sn <- data.table::copy(ctx$snap()$snap)[is.finite(wuf) & np_wf > 0]
    if (!nrow(sn)) return(empty_plot("No incremental waterflood oil yet"))
    data.table::setorder(sn, wuf)
    sn[, entity := factor(entity, entity)]
    med <- stats::median(sn$wuf)
    p <- plotly::plot_ly(sn, source = "wf", x = ~wuf, y = ~entity, type = "bar", orientation = "h", customdata = ~as.character(entity),
      marker = list(color = ramp_colors(sn$wuf, c("#2dd4bf", "#fde047", "#f87171"))),
      hovertemplate = "%{y}: %{x:.1f} bbl injected per bbl WF oil<extra></extra>")
    p <- pl_theme(p, "bbl water injected / bbl incremental oil", NULL, legend = FALSE)
    p <- plotly::layout(p, shapes = list(list(type = "line", x0 = med, x1 = med, yref = "paper", y0 = 0, y1 = 1,
                                              line = list(color = "#e2e8f0", dash = "dot", width = 1))),
                        annotations = list(list(x = med, y = 1.02, yref = "paper", text = "median", showarrow = FALSE,
                                                font = list(size = 9, color = pal$muted))))
    plotly::event_register(p, "plotly_click")
  })
}
