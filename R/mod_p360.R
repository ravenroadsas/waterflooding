# PATTERN 360: side-panel drill-down (Field > Area > Pattern > Well > Unit) ------------------

p360_modal <- function(pattern, area, field) {
  shiny::modalDialog(title = NULL, size = "xl", easyClose = TRUE, footer = NULL,
    htmltools::div(class = "wf-drawer-mark wf-p360",
      htmltools::div(class = "wf-crumb", htmltools::span(field), "›", htmltools::span(area), "›", htmltools::tags$b(pattern),
                     shiny::uiOutput("p360_crumb_tail", inline = TRUE), htmltools::span(style = "margin-left:auto"), shiny::modalButton("Close")),
      shiny::uiOutput("p360_head"),
      bslib::navset_card_underline(id = "p360_tabs",
        bslib::nav_panel("Evidence", DT::DTOutput("p360_opps")),
        bslib::nav_panel("Maturity", bslib::layout_columns(col_widths = c(6, 6),
          plotly::plotlyOutput("p360_secrf", height = "340px"), plotly::plotlyOutput("p360_wor", height = "340px"))),
        bslib::nav_panel("Velocity", bslib::layout_columns(col_widths = c(6, 6),
          plotly::plotlyOutput("p360_tp", height = "340px"), plotly::plotlyOutput("p360_util", height = "340px"))),
        bslib::nav_panel("Units", bslib::layout_columns(col_widths = c(6, 6),
          plotly::plotlyOutput("p360_units", height = "300px"), plotly::plotlyOutput("p360_unit_tp", height = "300px")),
          htmltools::h6(class = "wf-h6", "Injector intervals: valve (VRF), design rate (Cobb) and actual rate"), DT::DTOutput("p360_mandrels")),
        bslib::nav_panel("Wells", bslib::layout_columns(col_widths = c(7, 5), DT::DTOutput("p360_wells"), plotly::plotlyOutput("p360_hi", height = "320px"))),
        bslib::nav_panel("Forecast", htmltools::div(class = "wf-inline-tools",
            shiny::sliderInput("p360_tp_fc", "Injection TP for the forecast (%HCPV/yr)", min = 1, max = 30, value = 11.5, step = 0.5, width = "380px")),
          bslib::layout_columns(col_widths = c(6, 6), plotly::plotlyOutput("p360_sf", height = "330px"), plotly::plotlyOutput("p360_fc", height = "330px")),
          shiny::uiOutput("p360_sf_text")),
        bslib::nav_panel("Interventions", DT::DTOutput("p360_iv")),
        bslib::nav_panel("Performance", bslib::layout_columns(col_widths = c(7, 5),
          plotly::plotlyOutput("p360_rates", height = "330px"), plotly::plotlyOutput("p360_chan", height = "330px"))),
        bslib::nav_panel("Rock & fluid", bslib::layout_columns(col_widths = c(6, 6),
          plotly::plotlyOutput("p360_kr", height = "300px"), plotly::plotlyOutput("p360_proto", height = "300px"))))))
}

p360_server <- function(input, output, session, ctx, pat) {
  ser <- shiny::reactive({ p <- pat(); shiny::req(p); ctx$series_all()[entity == p] })
  sa <- shiny::reactive(ser()[date <= ctx$asof()])
  row <- shiny::reactive(ctx$snap()[entity == pat()])

  output$p360_crumb_tail <- shiny::renderUI({
    inj <- dominant_injector(ctx$res(), pat(), ctx$asof()); u <- row()$top_sand
    htmltools::span(if (!is.na(inj)) paste(" ›", inj), if (length(u) && !is.na(u)) htmltools::span(class = "wf-acc", paste(" › unit", u)))
  })
  output$p360_head <- shiny::renderUI({
    r <- row(); shiny::req(nrow(r)); op <- ctx$opps()$summary[pattern == pat()]
    htmltools::div(class = "wf-p360-head",
      htmltools::h3(paste("Pattern", pat())), badge(as.character(r$stage), stage_colors[[as.character(r$stage)]]),
      if (is.finite(r$opr)) badge(sprintf("OPR %.2f · WPR %.2f", r$opr, r$wpr), if (r$opr < 1) pal$crit else pal$ok),
      if (nrow(op)) badge(sprintf("%d opportunities (%s)", nrow(op), paste(unique(op$type), collapse = ", ")), pal$accent),
      if ("cluster_name" %in% names(r) && !is.na(r$cluster_name)) badge(r$cluster_name, "#f472b6"),
      htmltools::div(class = "wf-kpi-row compact",
        kpi("HCPV", fmt_mm(r$hcpv, 2)), kpi("DWI", fmt_num(r$dwi, 2)), kpi("Sec RF", fmt_pct(r$sec_rf)), kpi("Util", fmt_num(r$util, 1)),
        kpi("Inj TP 12m", paste0(fmt_num(r$tp12, 1), "%")), kpi("IWR 12m", fmt_num(r$iwr12, 2)), kpi("WC", fmt_pct(r$wc6)),
        kpi("Loss", fmt_num(r$loss, 2)), kpi("Remaining WF", fmt_mm(r$remaining_stb, 2))))
  })

  output$p360_opps <- DT::renderDT({
    op <- ctx$opps()$summary[pattern == pat()]
    if (!nrow(op)) return(dt_dark(data.table::data.table(Message = "No opportunities for this pattern at this date")))
    dt_dark(op[, .(Type = paste(type, opp_types[type]), Unit = sand, Well = well, Evidence = families, Status = as.character(status), Score = score, Key = key)], dom = "t")
  })
  shiny::observeEvent(input$p360_opps_rows_selected, {
    op <- ctx$opps()$summary[pattern == pat()]
    ctx$open_opp(op$key[input$p360_opps_rows_selected]); shiny::removeModal()
  })

  output$p360_secrf <- plotly::renderPlotly({
    s <- sa()[dwi > 0]; if (!nrow(s)) return(empty_plot("No injection yet"))
    pr <- ctx$res()$protos[pkey == s$proto_key[nrow(s)]]
    p <- add_proto(plotly::plot_ly(), pr, "sec_rf", max(1.2, max(s$dwi) * 1.3))
    p <- plotly::add_lines(p, data = s, x = ~dwi, y = ~sec_rf, name = "Sec RF", line = list(color = pal$oil, width = 2.5))
    p <- plotly::add_lines(p, data = s, x = ~dwi, y = ~dwp / 5, name = "DWP / 5", line = list(color = pal$water, width = 1.5, dash = "dot"))
    plotly::layout(pl_theme(p, "DWI", "Sec RF"), yaxis = axis_style("Sec RF (fraction HCPV)", tickformat = ".0%"))
  })
  output$p360_wor <- plotly::renderPlotly({
    s <- sa()[dwi > 0 & wor > 0]; if (!nrow(s)) return(empty_plot("No water"))
    pr <- ctx$res()$protos[pkey == s$proto_key[nrow(s)]]
    p <- add_proto(plotly::plot_ly(), pr, "wor", max(1.2, max(s$dwi) * 1.1), band = 1, log = TRUE)
    p <- plotly::add_lines(p, data = s, x = ~dwi, y = ~wor, name = "WOR", line = list(color = pal$water, width = 2))
    plotly::layout(pl_theme(p, "DWI", "WOR"), yaxis = axis_style("WOR (log)", type = "log"))
  })
  output$p360_tp <- plotly::renderPlotly({
    s <- sa(); p <- plotly::plot_ly(s, x = ~date)
    p <- plotly::add_lines(p, y = ~tp, name = "Inj TP month", line = list(color = "rgba(167,139,250,0.5)", width = 1))
    p <- plotly::add_lines(p, y = ~tp12, name = "Inj TP 12 m", line = list(color = pal$inj, width = 2))
    p <- plotly::add_lines(p, y = ~prod_tp12, name = "Prod TP 12 m", line = list(color = pal$oil, width = 2))
    p <- plotly::add_lines(p, y = ~iwr12 * 10, name = "IWR x10", line = list(color = "#fde047", width = 1, dash = "dot"))
    plotly::layout(pl_theme(p, NULL, "%HCPV/yr"), hovermode = "x unified", shapes = list(hline_shape(ctx$settings()$target_tp, pal$warn)))
  })
  output$p360_util <- plotly::renderPlotly({
    s <- sa()[dwi > 0]; if (!nrow(s)) return(empty_plot("No injection yet"))
    pr <- ctx$res()$protos[pkey == s$proto_key[nrow(s)]]
    p <- add_proto(plotly::plot_ly(), pr, "util", max(1.2, max(s$dwi) * 1.1), band = 0.3)
    p <- plotly::add_lines(p, data = s, x = ~dwi, y = ~util, name = "Utilization", line = list(color = pal$warn, width = 2))
    pl_theme(p, "DWI", "Utilization (rb/rb)")
  })

  us <- shiny::reactive({ u <- ctx$units_snap(); shiny::req(u); u[pattern == pat()] })
  output$p360_units <- plotly::renderPlotly({
    u <- us(); if (!nrow(u)) return(empty_plot("No unit data"))
    p <- plotly::plot_ly(u, x = ~sand, y = ~dwi, type = "bar", name = "DWI", marker = list(color = ifelse(u$dwi >= 1.2, "#3b82f6", ifelse(u$dwi >= 0.5, "#22d3ee", "#34d399"))))
    p <- plotly::add_bars(p, x = ~sand, y = ~hcpv / sum(hcpv), name = "HCPV share", marker = list(color = "#3f6f5a"))
    p <- plotly::add_bars(p, x = ~sand, y = ~cum_winj_rb / sum(cum_winj_rb), name = "Injection share", marker = list(color = "#475f86"))
    plotly::layout(pl_theme(p, "Unit", "DWI · shares"), barmode = "group")
  })
  output$p360_unit_tp <- plotly::renderPlotly({
    u <- ctx$res()$units$pattern_sand[pattern == pat() & date <= ctx$asof() & date >= ctx$asof() - 6 * 365]
    p <- plotly::plot_ly(u, x = ~date, y = ~tp12, color = ~sand, type = "scatter", mode = "lines", colors = cluster_palette)
    plotly::layout(pl_theme(p, NULL, "Unit Inj TP 12 m (%/yr)"), shapes = list(hline_shape(ctx$settings()$target_tp, pal$warn)))
  })
  output$p360_mandrels <- DT::renderDT({
    ws <- ctx$res()$units$well_sand; inj <- unique(ctx$res()$alloc[pattern == pat() & coeff > 0, well])
    x <- ws[well %in% inj & date == max(date[date <= ctx$asof()])]
    if (!nrow(x)) return(dt_dark(data.table::data.table(Message = "No injector data")))
    d <- x[, .(Well = well, Unit = sand, `VRF (valve)` = vrf, `Cobb (bbl/d)` = round(cobb), `Actual (bbl/d)` = round(winj_s / days),
               `Actual / Cobb` = round(winj_s / days / cobb, 2), Share = round(share, 3), `Last profile` = format(pdate, "%b %Y"), Split = method)]
    dt <- dt_dark(d, dom = "t", pageLength = 30)
    DT::formatStyle(dt, "Actual / Cobb", color = DT::styleInterval(c(0.6, 1.1), c(pal$warn, pal$text, pal$crit)))
  })

  output$p360_wells <- DT::renderDT({
    r <- ctx$res(); a <- ctx$asof()
    al <- r$alloc[pattern == pat() & date == max(date[date <= a])]
    pw <- r$pattern_well[pattern == pat() & date <= a, .(cum_oil = sum(oil), cum_water = sum(water), cum_winj = sum(winj)), by = well]
    ws <- r$ds$well_status; dfl <- if (!is.null(ws)) ws[date <= a][, .SD[.N], by = well][, .(well, dfl)] else data.table::data.table(well = character(), dfl = numeric())
    hi <- r$hi[date == max(date[date <= a]), .(well, hi_oil, hi_water)]
    d <- Reduce(function(x, y) merge(x, y, by = "well", all.x = TRUE), list(al[, .(well, coeff)], pw, unique(r$wells[, .(well, well_type)], by = "well"), dfl, hi))
    dt_dark(d[, .(Well = well, Type = well_type, Coeff = round(coeff, 3), `Alloc. Np (Mstb)` = round(cum_oil / 1e3, 1), `Alloc. Wp (Mbbl)` = round(cum_water / 1e3, 1),
                  `Alloc. Wi (Mbbl)` = round(cum_winj / 1e3, 1), `HI oil` = round(hi_oil, 2), `HI water` = round(hi_water, 2), `Fluid level ft` = dfl)], dom = "t", pageLength = 20)
  })
  output$p360_hi <- plotly::renderPlotly({
    r <- ctx$res(); wells <- unique(r$alloc[pattern == pat() & coeff > 0, well])
    h <- r$hi[well %in% wells & date <= ctx$asof() & date > ctx$asof() - 5 * 365 & format(date, "%m") %in% c("01", "07")]
    if (!nrow(h)) return(empty_plot("No producers"))
    p <- plotly::plot_ly(h, x = ~hi_water, y = ~hi_oil, color = ~well, type = "scatter", mode = "lines+markers", colors = cluster_palette)
    plotly::layout(pl_theme(p, "HI water", "HI oil"), shapes = list(list(type = "line", x0 = 0, x1 = 0, yref = "paper", y0 = 0, y1 = 1, line = list(color = pal$muted)), hline_shape(0, pal$muted, "solid")))
  })

  fit <- shiny::reactive({ s <- sa()[flooding %in% TRUE]; sf_fit(s$dwi, s$sec_rf) })
  output$p360_sf <- plotly::renderPlotly({
    s <- sa()[flooding %in% TRUE]; f <- fit()
    if (is.null(f)) return(empty_plot("Not enough waterflood history for the fit"))
    xs <- seq(0, max(s$dwi) * 1.8, length.out = 80)
    p <- plotly::plot_ly(s, x = ~dwi, y = ~sec_rf, type = "scatter", mode = "markers", name = "Actual", marker = list(color = pal$oil, size = 4))
    p <- plotly::add_lines(p, x = xs, y = sf_predict(f, xs), name = sprintf("Fit A = %.1f %%, C = %.2f", 100 * f$A, f$C), line = list(color = pal$warn, dash = "dash"), inherit = FALSE)
    plotly::layout(pl_theme(p, "DWI", "Sec RF"), yaxis = axis_style("Sec RF", tickformat = ".0%"))
  })
  fc <- shiny::reactive({
    f <- fit(); r <- row(); shiny::req(f, nrow(r))
    bo <- r$cum_oil_rb / max(r$cum_oil, 1)
    a <- sf_forecast(f, r$dwi, r$sec_rf, input$p360_tp_fc, r$hcpv, bo, ctx$asof())
    b <- sf_forecast(f, r$dwi, r$sec_rf, r$tp12, r$hcpv, bo, ctx$asof())
    list(target = a, current = b, bo = bo, r = r, f = f)
  })
  output$p360_fc <- plotly::renderPlotly({
    x <- fc(); h <- sa()[date >= ctx$asof() - 5 * 365]
    p <- plotly::plot_ly(h, x = ~date, y = ~qo, type = "scatter", mode = "lines", name = "History", line = list(color = pal$oil))
    p <- plotly::add_lines(p, data = x$current, x = ~date, y = ~qo, name = sprintf("At current TP %.1f %%", x$r$tp12), line = list(color = pal$muted, dash = "dot"), inherit = FALSE)
    p <- plotly::add_lines(p, data = x$target, x = ~date, y = ~qo, name = sprintf("At %.1f %%", input$p360_tp_fc), line = list(color = pal$warn, dash = "dash"), inherit = FALSE)
    pl_theme(p, NULL, "Oil rate (bopd)")
  })
  output$p360_sf_text <- shiny::renderUI({
    x <- fc(); d5 <- sum(x$target$qo[1:60] - x$current$qo[1:60]) * 30.44
    htmltools::p(class = "wf-muted", sprintf("Remaining waterflood oil on this trend: %s stb (%.1f %% of HCPV). Changing injection TP from %.1f to %.1f %%/yr changes 5-year oil by %s stb (fit R² %.2f). Throughput-based: remaining oil depends on injected volume, not time.",
      fmt_num(sf_remaining(x$f, x$r$dwi) * x$r$hcpv / x$bo), 100 * sf_remaining(x$f, x$r$dwi), x$r$tp12, input$p360_tp_fc, fmt_num(d5), x$f$r2))
  })

  output$p360_iv <- DT::renderDT({
    ctx$store_tick(); r <- ctx$res(); wells <- unique(r$alloc[pattern == pat() & coeff > 0, well])
    a <- r$ds$interventions; a <- if (!is.null(a)) a[well %in% wells, .(source = "table", well, date, type, sand, status, notes)] else NULL
    b <- store_interventions(ctx$con); b <- if (nrow(b)) b[pattern == pat() | well %in% wells, .(source = "app", well, date, type, sand, status, notes)] else NULL
    x <- data.table::rbindlist(list(a, b), fill = TRUE)
    if (!nrow(x)) x <- data.table::data.table(Message = "No interventions")
    dt_dark(x, dom = "t")
  })

  output$p360_rates <- plotly::renderPlotly({
    s <- ser(); p <- plotly::plot_ly(s, x = ~date)
    p <- plotly::add_lines(p, y = ~qo, name = "Oil", line = list(color = pal$oil, width = 2))
    p <- plotly::add_lines(p, y = ~qw, name = "Water", line = list(color = pal$water))
    p <- plotly::add_lines(p, y = ~qwi, name = "Injection", line = list(color = pal$inj))
    p <- plotly::add_lines(p, y = ~wc, name = "WC", yaxis = "y2", line = list(color = "#e2e8f0", dash = "dot", width = 1))
    plotly::layout(pl_theme(p, NULL, "bbl/d"), hovermode = "x unified", shapes = list(vline_shape(ctx$asof())),
                   yaxis2 = axis_style("WC", overlaying = "y", side = "right", range = c(0, 1), tickformat = ".0%", showgrid = FALSE))
  })
  output$p360_chan <- plotly::renderPlotly({
    cs <- chan_series(sa())[is.finite(wor) & wor > 0]
    if (nrow(cs) < 3) return(empty_plot("Not enough water production"))
    p <- plotly::plot_ly(cs, x = ~t)
    p <- plotly::add_markers(p, y = ~wor, name = "WOR", marker = list(color = pal$water, size = 4))
    p <- plotly::add_markers(p, data = cs[is.finite(dwor) & dwor > 0], y = ~dwor, name = "WOR'", marker = list(color = "#f97316", size = 4, symbol = "diamond"))
    plotly::layout(pl_theme(p, "Days on production", "WOR, WOR'"), xaxis = axis_style("Days", type = "log"), yaxis = axis_style("WOR, WOR'", type = "log"))
  })
  output$p360_kr <- plotly::renderPlotly({
    x <- ctx$res()$sand_props[pattern == pat()]; w <- x$hcpv / sum(x$hcpv)
    p <- list(swc = sum(w * x$swc), sor = sum(w * x$sor), krw_or = sum(w * x$krw), kro_wc = sum(w * x$kro), nw = sum(w * x$nw), no = sum(w * x$no), mu_o = sum(w * x$mu_o), mu_w = sum(w * x$mu_w))
    sw <- seq(p$swc, 1 - p$sor, length.out = 100); kr <- corey_kr(sw, p)
    q <- plotly::plot_ly(x = sw); q <- plotly::add_lines(q, y = kr$krw, name = "krw", line = list(color = pal$water))
    q <- plotly::add_lines(q, y = kr$kro, name = "kro", line = list(color = pal$oil)); q <- plotly::add_lines(q, y = frac_flow(sw, p), name = "fw", line = list(color = "#e2e8f0", dash = "dot"))
    plotly::layout(pl_theme(q, "Sw", "kr, fw"), annotations = list(list(x = 0.5, y = 1.05, xref = "paper", yref = "paper", showarrow = FALSE,
      text = sprintf("M = %.2f%s", mobility_ratio(p), if (isTRUE(ctx$res()$ds$fluids_relperm_given)) "" else " · default rel-perm"), font = list(size = 10, color = pal$muted))))
  })
  output$p360_proto <- plotly::renderPlotly({
    pr <- ctx$res()$protos; k <- sa()$proto_key[nrow(sa())]
    p <- plotly::plot_ly()
    for (kk in unique(pr$pkey)) { d <- pr[pkey == kk]; p <- plotly::add_lines(p, x = d$dwi, y = d$sec_rf, name = kk, line = list(width = if (kk == k) 2.5 else 1)) }
    plotly::layout(pl_theme(p, "DWI", "Expected Sec RF"), yaxis = axis_style("Expected Sec RF", tickformat = ".0%"))
  })
}
