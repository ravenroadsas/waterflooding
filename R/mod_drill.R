# Pattern / block drill-down modal ------------------------------------------

drill_modal <- function(entity, level) {
  shiny::modalDialog(
    title = NULL, size = "xl", easyClose = TRUE, footer = NULL,
    htmltools::div(class = "wf-drill",
      htmltools::div(class = "wf-drill-head",
        htmltools::div(htmltools::span(class = "wf-drill-level", tools::toTitleCase(level)), htmltools::h3(entity)),
        shiny::uiOutput("drill_badges"),
        shiny::modalButton("Close")
      ),
      shiny::uiOutput("drill_kpis"),
      bslib::navset_card_underline(
        bslib::nav_panel("Performance",
          bslib::layout_columns(col_widths = c(7, 5),
            plotly::plotlyOutput("drill_rates", height = "360px"),
            plotly::plotlyOutput("drill_vrr", height = "360px"))),
        bslib::nav_panel("Diagnostics",
          bslib::layout_columns(col_widths = c(4, 4, 4),
            htmltools::div(htmltools::h6("Chan plot"), plotly::plotlyOutput("drill_chan", height = "340px")),
            htmltools::div(htmltools::h6("WOR vs cumulative oil (EUR)"), plotly::plotlyOutput("drill_wor", height = "340px")),
            htmltools::div(htmltools::h6("Recovery vs Buckley-Leverett"), plotly::plotlyOutput("drill_bl", height = "340px")))),
        bslib::nav_panel("Wells & allocation", DT::DTOutput("drill_wells")),
        bslib::nav_panel("Sands",
          bslib::layout_columns(col_widths = c(5, 7),
            plotly::plotlyOutput("drill_sand_plot", height = "320px"), DT::DTOutput("drill_sands"))),
        bslib::nav_panel("Rock & fluid",
          bslib::layout_columns(col_widths = c(6, 6),
            plotly::plotlyOutput("drill_kr", height = "320px"), plotly::plotlyOutput("drill_fw", height = "320px"))),
        bslib::nav_panel("Exceptions", DT::DTOutput("drill_flags"))
      )
    )
  )
}

drill_server <- function(input, output, session, ctx, entity) {
  ser <- shiny::reactive({
    e <- entity(); shiny::req(e)
    aggregate_level(ctx$res(), ctx$level(), ctx$sands(), entities = e)
  })
  ser_asof <- shiny::reactive(ser()[date <= ctx$asof()])
  patterns <- shiny::reactive({
    e <- entity()
    if (ctx$level() == "pattern") e else ctx$res()$pat_map[get(entity_col(ctx$level())) == e, pattern]
  })
  snap_row <- shiny::reactive({
    sn <- ctx$snap()$snap
    sn[entity == entity()]
  })

  output$drill_badges <- shiny::renderUI({
    r <- snap_row(); shiny::req(nrow(r))
    htmltools::div(class = "wf-drill-badges", stage_badge(r$stage),
      htmltools::span(class = "wf-badge", style = sprintf("--c:%s", quadrant_colors[as.character(r$quadrant)]), as.character(r$quadrant)),
      htmltools::span(class = "wf-drill-action", r$action))
  })

  output$drill_kpis <- shiny::renderUI({
    r <- snap_row(); shiny::req(nrow(r))
    htmltools::div(class = "wf-kpi-row compact",
      kpi("STOOIP", fmt_mm(r$stooip)), kpi("Np", fmt_mm(r$cum_oil), paste("RF", fmt_pct(r$rf))),
      kpi("WF RF", fmt_pct(r$rf_wf)), kpi("HCPVI", fmt_num(r$hcpvi, 2)), kpi("Ev", fmt_num(r$ev, 2)),
      kpi("Oil · bopd", fmt_num(r$qo)), kpi("Inj · bwipd", fmt_num(r$qwi)),
      kpi("WC 6m", fmt_pct(r$wc6)), kpi("VRR 6m", fmt_num(r$vrr6, 2)), kpi("EUR RF", fmt_pct(r$rf_eur)))
  })

  output$drill_rates <- plotly::renderPlotly({
    s <- ser()
    p <- plotly::plot_ly(s, x = ~date)
    p <- plotly::add_lines(p, y = ~qo, name = "Oil", line = list(color = pal$oil, width = 2))
    p <- plotly::add_lines(p, y = ~qw, name = "Water", line = list(color = pal$water))
    p <- plotly::add_lines(p, y = ~qwi, name = "Injection", line = list(color = pal$inj))
    p <- plotly::add_lines(p, y = ~wc, name = "WC", yaxis = "y2", line = list(color = "#e2e8f0", dash = "dot", width = 1))
    p <- pl_theme(p, NULL, "stb/d")
    plotly::layout(p, hovermode = "x unified", yaxis2 = axis_style("WC", overlaying = "y", side = "right", range = c(0, 1), tickformat = ".0%", showgrid = FALSE))
  })

  output$drill_vrr <- plotly::renderPlotly({
    s <- data.table::copy(ser())[cum_winj > 0]
    if (!nrow(s)) return(empty_plot("No injection yet"))
    s[hcpvi < 0.02, vrr_wf := NA]
    p <- plotly::plot_ly(s, x = ~date)
    p <- plotly::add_bars(p, y = ~pmin(vrr, 3), name = "VRR month", marker = list(color = ifelse(s$vrr < 1, "#f87171aa", "#60a5faaa")))
    p <- plotly::add_lines(p, y = ~vrr_wf, name = "VRR since flood", line = list(color = "#fde047", width = 2))
    p <- pl_theme(p, NULL, "VRR")
    plotly::layout(p, shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 1, y1 = 1, line = list(color = "#e2e8f0", dash = "dash", width = 1))))
  })

  output$drill_chan <- plotly::renderPlotly({
    cs <- chan_series(ser_asof())
    cs <- cs[is.finite(wor) & wor > 0]
    if (nrow(cs) < 3) return(empty_plot("Not enough water production"))
    p <- plotly::plot_ly(cs, x = ~t)
    p <- plotly::add_markers(p, y = ~wor, name = "WOR", marker = list(color = pal$water, size = 5))
    p <- plotly::add_markers(p, data = cs[is.finite(dwor) & dwor > 0], y = ~dwor, name = "WOR'", marker = list(color = "#f97316", size = 5, symbol = "diamond"))
    p <- pl_theme(p, "Days on production", "WOR, WOR' (1/d)")
    plotly::layout(p, xaxis = axis_style("Days on production", type = "log"), yaxis = axis_style("WOR, WOR'", type = "log", exponentformat = "power"))
  })

  output$drill_wor <- plotly::renderPlotly({
    s <- ser_asof()[oil > 0 & is.finite(wor) & wor > 0]
    if (nrow(s) < 3) return(empty_plot("Not enough water production"))
    r <- snap_row()
    st <- ctx$settings()
    p <- plotly::plot_ly(s, x = ~cum_oil / 1e6)
    p <- plotly::add_markers(p, y = ~wor, name = "WOR", marker = list(color = pal$water, size = 5))
    if (nrow(r) && is.finite(r$wor_b)) {
      x0 <- utils::tail(s$cum_oil, st$wor_fit_months)[1]
      xe <- (log10(st$wor_limit) - r$wor_a) / r$wor_b
      xs <- seq(x0, max(xe, x0), length.out = 40)
      p <- plotly::add_lines(p, x = xs / 1e6, y = 10^(r$wor_a + r$wor_b * xs), name = "Trend", line = list(color = "#fde047", dash = "dash"))
      p <- plotly::add_markers(p, x = r$eur / 1e6, y = st$wor_limit, name = "EUR", marker = list(color = "#fde047", size = 11, symbol = "star"))
    }
    p <- pl_theme(p, "Cumulative oil (MMstb)", "WOR")
    plotly::layout(p, yaxis = axis_style("WOR", type = "log", tickvals = 10^(-3:3), ticktext = c("0.001", "0.01", "0.1", "1", "10", "100", "1000")),
      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = st$wor_limit, y1 = st$wor_limit, line = list(color = "#f87171", dash = "dot", width = 1))))
  })

  output$drill_bl <- plotly::renderPlotly({
    s <- ser_asof()[hcpvi > 0]
    if (!nrow(s)) return(empty_plot("No injection yet"))
    cv <- theoretical_curve(ctx$res(), ctx$sands(), patterns())
    cv <- cv[hcpvi <= max(1.2, max(s$hcpvi) * 1.2)]
    p <- plotly::plot_ly()
    for (e in c(1, 0.75, 0.5, 0.25))
      p <- plotly::add_lines(p, x = cv$hcpvi, y = cv$rf * e, name = if (e == 1) "Ideal" else paste("Ev", e),
        line = list(color = if (e == 1) "#e2e8f0" else "#475569", dash = if (e == 1) "solid" else "dot", width = 1), showlegend = e == 1)
    p <- plotly::add_lines(p, data = s, x = ~hcpvi, y = ~rf_wf, name = "Actual WF RF", line = list(color = pal$oil, width = 2.5))
    p <- plotly::add_lines(p, data = s, x = ~hcpvi, y = ~rf_ideal, name = "Ideal at this HCPVI", line = list(color = pal$accent, width = 1, dash = "dash"))
    p <- pl_theme(p, "HCPVI", "WF recovery factor")
    plotly::layout(p, yaxis = axis_style("WF recovery factor", tickformat = ".0%"))
  })

  output$drill_wells <- DT::renderDT({
    if (ctx$level() != "pattern") return(dt_dark(data.table::data.table(Message = "Open a single pattern to see its wells")))
    w <- pattern_wells(ctx$res(), entity(), ctx$asof())
    shiny::req(nrow(w))
    d <- w[, .(Well = well, Type = well_type, `Coefficient (as-of)` = round(coefficient, 3),
               `Allocated cum oil (Mstb)` = round(cum_oil / 1e3, 1), `Allocated cum water (Mstb)` = round(cum_water / 1e3, 1),
               `Allocated cum inj (Mbbl)` = round(cum_winj / 1e3, 1), `Last oil (bopd)` = round(last_qo, 1),
               `Last inj (bwipd)` = round(last_qwi, 1), `Share of well cum oil %` = round(100 * cum_oil / pmax(well_cum_oil, 1), 1))]
    dt_dark(d, pageLength = 15)
  })

  sand_tab <- shiny::reactive({
    m <- pattern_sand_snapshot(ctx$res(), ctx$asof(), patterns())
    if (length(ctx$sands())) m <- m[sand %in% ctx$sands()]
    m[, c(lapply(.SD, sum), list(days = days[1])), by = .(entity = sand, date), .SDcols = c(additive_cols, static_cols)] |> derive_metrics()
  })
  output$drill_sands <- DT::renderDT({
    m <- sand_tab()
    dt_dark(m[, .(Sand = entity, `STOOIP MMstb` = round(stooip / 1e6, 2), `Np MMstb` = round(cum_oil / 1e6, 3), `RF %` = round(100 * rf, 1),
                  `WF RF %` = round(100 * rf_wf, 1), HCPVI = round(hcpvi, 2), Ev = round(ev, 2), `WC %` = round(100 * wc, 1),
                  `Remaining movable MMstb` = round(remaining_mov / 1e6, 2))], pageLength = 10)
  })
  output$drill_sand_plot <- plotly::renderPlotly({
    m <- sand_tab()
    p <- plotly::plot_ly(m, x = ~entity, y = ~rf, type = "bar", name = "RF", marker = list(color = pal$oil))
    p <- plotly::add_bars(p, y = ~rf_ideal + (rf - rf_wf), name = "Primary + ideal WF", marker = list(color = "rgba(226,232,240,0.25)"))
    p <- pl_theme(p, "Sand", "Recovery factor")
    plotly::layout(p, barmode = "overlay", yaxis = axis_style("Recovery factor", tickformat = ".0%"))
  })

  props <- shiny::reactive(aggregate_props(ctx$res(), ctx$sands(), patterns()))
  output$drill_kr <- plotly::renderPlotly({
    p <- props()
    sw <- seq(p$swc, 1 - p$sor, length.out = 100)
    kr <- corey_kr(sw, p)
    pl <- plotly::plot_ly(x = sw)
    pl <- plotly::add_lines(pl, y = kr$krw, name = "krw", line = list(color = pal$water, width = 2))
    pl <- plotly::add_lines(pl, y = kr$kro, name = "kro", line = list(color = pal$oil, width = 2))
    pl <- pl_theme(pl, "Sw", "kr")
    plotly::layout(pl, annotations = list(list(x = 0.5, y = 1.0, xref = "paper", yref = "paper", showarrow = FALSE,
      text = sprintf("M = %.2f · Swi %.2f · Sor %.2f · μo %.1f cp", mobility_ratio(p), p$swi, p$sor, p$mu_o),
      font = list(size = 11, color = pal$muted))))
  })
  output$drill_fw <- plotly::renderPlotly({
    p <- props()
    sw <- seq(p$swc, 1 - p$sor, length.out = 200)
    cv <- welge_curve(p, p$swi)
    pl <- plotly::plot_ly(x = sw, y = frac_flow(sw, p), type = "scatter", mode = "lines", name = "fw", line = list(color = pal$water, width = 2))
    pl <- plotly::add_lines(pl, x = cv$hcpvi[cv$hcpvi <= 5], y = cv$ed[cv$hcpvi <= 5], name = "ED vs HCPVI", xaxis = "x2", yaxis = "y2",
                            line = list(color = pal$oil, width = 2))
    pl <- pl_theme(pl, "Sw", "fw")
    plotly::layout(pl, xaxis = axis_style("Sw", domain = c(0, 0.47)), xaxis2 = axis_style("HCPVI", domain = c(0.53, 1), anchor = "y2"),
                   yaxis2 = axis_style("ED", anchor = "x2", range = c(0, 1)))
  })

  output$drill_flags <- DT::renderDT({
    f <- ctx$snap()$flags
    f <- if (nrow(f)) f[entity == entity()] else f
    if (!nrow(f)) return(dt_dark(data.table::data.table(Message = "No exceptions")))
    dt_dark(f[, .(Severity = sev_badge_html(severity), Diagnosis = message, `Recommended action` = action)], escape = FALSE)
  })
}
