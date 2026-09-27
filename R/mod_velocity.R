# STEP 2 - PROCESS VELOCITY ---------------------------------------------------------------

velocity_ui <- function() {
  htmltools::tagList(
    htmltools::div(class = "wf-section-intro",
      htmltools::div(class = "wf-step", "Step 2 of 3"), htmltools::h2("Process velocity"),
      htmltools::p("Is water going in and fluid coming out at the right speed for each pattern's maturity, and is the pattern balanced?",
                   "Speed against efficiency first, then the change over a year, then balance, then throughput on maturity by unit.")),
    shiny::uiOutput("vel_kpis"),
    bslib::layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
      bslib::card(full_screen = TRUE, height = 420, card_title("Utilization vs Inj TP", "colour and size IWR; target line", tag = "new"),
        bslib::card_body(plotly::plotlyOutput("vel_utiltp", height = "100%"))),
      bslib::card(full_screen = TRUE, height = 420, card_title("Inj TP now vs 12 months ago", "1:1 and ±25 / 50 %", tag = "new"),
        bslib::card_body(plotly::plotlyOutput("vel_tp12", height = "100%"))),
      bslib::card(full_screen = TRUE, height = 420, card_title("IWR vs utilization", "screening bands", tag = "new"),
        bslib::card_body(plotly::plotlyOutput("vel_iwr", height = "100%")))),
    bslib::layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
      bslib::card(full_screen = TRUE, height = 440, card_title("Pattern balance", NULL,
          shiny::radioButtons("vel_bal", NULL, c("Inj vs Prod TP" = "tp", "DWI vs DTP" = "dtp"), inline = TRUE), tag = "extend"),
        bslib::card_body(plotly::plotlyOutput("vel_bal_plot", height = "100%"))),
      bslib::card(full_screen = TRUE, height = 440, card_title("DWI + TP by unit", "background DWI, bubbles unit TP",
          shiny::selectInput("vel_map_unit", NULL, character(), width = "90px"), tag = "new"),
        bslib::card_body(plotly::plotlyOutput("vel_map", height = "100%"))),
      bslib::card(full_screen = TRUE, height = 440, card_title("Rate gap to target", "bwipd",
          shiny::radioButtons("vel_gap_basis", NULL, c("Target TP" = "tp", "Cobb" = "cobb"), inline = TRUE), tag = "new"),
        bslib::card_body(plotly::plotlyOutput("vel_gap", height = "100%")))),
    bslib::layout_columns(col_widths = c(6, 6), fill = FALSE,
      bslib::card(full_screen = TRUE, height = 420, card_title("Inj TP by unit", NULL, shiny::uiOutput("vel_tphist_scope", inline = TRUE), tag = "new"),
        bslib::card_body(plotly::plotlyOutput("vel_tphist", height = "100%"))),
      bslib::card(full_screen = TRUE, height = 420, card_title("Pulse board", "every pattern, every month",
          shiny::selectInput("vel_pulse_metric", NULL, metric_choices(c("iwr", "tp", "util", "wc6", "opr", "dwi")), "iwr", width = "140px"),
          shiny::selectInput("vel_pulse_window", NULL, c("3 years" = 36, "5 years" = 60, "10 years" = 120, "All" = 9999), 36, width = "95px"), tag = "keep"),
        bslib::card_body(plotly::plotlyOutput("vel_pulse", height = "100%")))),
    bslib::layout_columns(col_widths = c(7, 5), fill = FALSE,
      bslib::card(full_screen = TRUE, height = 420, card_title("Production, injection and voidage", "allocated rates, selected scope", tag = "keep"),
        bslib::card_body(plotly::plotlyOutput("vel_rates", height = "100%"))),
      bslib::card(full_screen = TRUE, height = 420, card_title("Screening signals", "single-family evidence, sent to step 3", tag = "extend"),
        bslib::card_body(fillable = FALSE, DT::DTOutput("vel_signals"))))
  )
}

velocity_server <- function(input, output, session, ctx) {
  T <- function() ctx$settings()$target_tp

  output$vel_kpis <- shiny::renderUI({
    s <- ctx$scope_series(); a <- ctx$asof(); shiny::req(nrow(s)); l <- s[date <= a][.N]; sn <- ctx$snap()[flooded == TRUE]
    st <- ctx$settings()
    gap <- sum(pmax((st$target_tp - sn$tp12) / 100 * sn$hcpv / 365 / 1.02, 0), na.rm = TRUE)
    ch <- if (is.finite(l$tp12_ago) && l$tp12_ago > 0) l$tp12 / l$tp12_ago - 1 else NA
    htmltools::div(class = "wf-kpi-row",
      kpi("Inj TP 12 m", paste0(fmt_num(l$tp12, 1), " %/yr"), paste0("target ", st$target_tp, " (LCI)"), tone = "inj"),
      kpi("Prod TP 12 m", paste0(fmt_num(l$prod_tp12, 1), " %/yr"), "HCPV per year"),
      kpi("IWR 12 m", fmt_num(l$iwr12, 2), sprintf("band %.1f–%.1f", st$iwr_low, st$iwr_high),
          tone = if (is.finite(l$iwr12) && (l$iwr12 < st$iwr_low || l$iwr12 > st$iwr_high)) "warn" else "ok"),
      kpi(sprintf("Utilization %d m", st$util_window), fmt_num(l$util, 1), "rb/rb"),
      kpi("TP vs 12 m ago", fmt_pct(ch, 0), sprintf("%d patterns down >%d%%", sum(sn$tp12 < (1 - st$tp_drop) * sn$tp12_ago, na.rm = TRUE), round(100 * st$tp_drop))),
      kpi("Rate gap", paste0("+", fmt_num(gap), " bwipd"), sprintf("%d patterns below target", sum(sn$tp12 < st$target_tp, na.rm = TRUE))),
      kpi("Under-balanced", sum(sn$iwr12 < st$iwr_low, na.rm = TRUE), paste("IWR <", st$iwr_low)),
      kpi("Over-balanced", sum(sn$iwr12 > st$iwr_high, na.rm = TRUE), paste("IWR >", st$iwr_high)))
  })

  iwr_col <- shiny::reactive(if (ctx$color_by() %in% c("cluster", "area", "stage")) ctx$color_by() else "iwr12")

  output$vel_utiltp <- plotly::renderPlotly({
    sn <- ctx$snap()[flooded == TRUE & is.finite(util) & is.finite(tp12)]; shiny::req(nrow(sn))
    xm <- max(sn$util) * 1.1; ym <- max(T() * 1.6, max(sn$tp12) * 1.15); med <- stats::median(sn$util)
    p <- pattern_scatter(sn, "util", "tp12", color = iwr_col(), hl = ctx$focus())
    p <- pl_theme(p, "Utilization (rb/rb)", "Inj TP (%HCPV/yr)", legend = FALSE)
    p <- plotly::layout(p, xaxis = axis_style("Utilization", range = c(0, xm)), yaxis = axis_style("Inj TP 12 m (%/yr)", range = c(0, ym)),
      shapes = list(rect_shape(0, med, 0, T(), "rgba(45,212,191,0.09)"), rect_shape(med, xm, T(), ym, "rgba(248,113,113,0.08)"), hline_shape(T(), pal$warn)),
      annotations = list(list(x = med / 2, y = T() * 0.06, text = "ACCELERATE", showarrow = FALSE, font = list(size = 9, color = pal$ok)),
                         list(x = (med + xm) / 2, y = ym * 0.96, text = "CONTROL", showarrow = FALSE, font = list(size = 9, color = pal$crit)),
                         list(x = xm, y = T(), xanchor = "right", yanchor = "bottom", text = paste0("target ", T(), " %/yr"), showarrow = FALSE, font = list(size = 9, color = pal$warn))))
    plotly::event_register(p, "plotly_click")
  })

  output$vel_tp12 <- plotly::renderPlotly({
    sn <- ctx$snap()[flooded == TRUE & is.finite(tp12_ago) & is.finite(tp12)]; if (!nrow(sn)) return(empty_plot("Needs 24 months of injection"))
    m <- max(c(sn$tp12, sn$tp12_ago)) * 1.1
    ln <- function(k, col, dash, lab) list(type = "line", x0 = 0, y0 = 0, x1 = m, y1 = m * k, line = list(color = col, width = 1, dash = dash))
    p <- pattern_scatter(sn, "tp12_ago", "tp12", color = iwr_col(), hl = ctx$focus())
    p <- pl_theme(p, "Inj TP 12 months ago", "Inj TP now", legend = FALSE)
    p <- plotly::layout(p, xaxis = axis_style("Inj TP 12 months ago (%/yr)", range = c(0, m)), yaxis = axis_style("Inj TP now (%/yr)", range = c(0, m)),
      shapes = list(ln(1, pal$proto, "solid"), ln(1.25, pal$muted, "dot"), ln(0.75, pal$muted, "dot"), ln(0.5, pal$crit, "dot")),
      annotations = list(list(x = m * 0.97, y = m * 0.5, text = "−50 %", showarrow = FALSE, font = list(size = 9, color = pal$crit)),
                         list(x = m * 0.97, y = m * 0.75, text = "−25 %", showarrow = FALSE, font = list(size = 9, color = pal$muted))))
    plotly::event_register(p, "plotly_click")
  })

  output$vel_iwr <- plotly::renderPlotly({
    sn <- ctx$snap()[flooded == TRUE & is.finite(util) & is.finite(iwr12)]; shiny::req(nrow(sn)); st <- ctx$settings()
    xm <- max(sn$util) * 1.1; ym <- max(2, max(sn$iwr12) * 1.1)
    col <- if (ctx$color_by() %in% c("cluster", "area", "stage")) ctx$color_by() else "util"
    p <- pattern_scatter(sn, "util", "iwr12", color = col, hl = ctx$focus())
    p <- pl_theme(p, "Utilization", "IWR 12 m", legend = FALSE)
    p <- plotly::layout(p, xaxis = axis_style("Utilization (rb/rb)", range = c(0, xm)), yaxis = axis_style("IWR 12 m", range = c(0, ym)),
      shapes = list(rect_shape(0, xm, st$iwr_low, st$iwr_high, "rgba(45,212,191,0.08)"), hline_shape(st$iwr_low, pal$crit, "dot"), hline_shape(st$iwr_high, pal$inj, "dot")),
      annotations = list(list(x = xm, y = st$iwr_high, xanchor = "right", yanchor = "bottom", text = "over-balanced", showarrow = FALSE, font = list(size = 9, color = pal$inj)),
                         list(x = xm, y = st$iwr_low, xanchor = "right", yanchor = "top", text = "under-balanced", showarrow = FALSE, font = list(size = 9, color = pal$crit))))
    plotly::event_register(p, "plotly_click")
  })

  output$vel_bal_plot <- plotly::renderPlotly({
    sn <- ctx$snap()[flooded == TRUE]; shiny::req(nrow(sn))
    if (identical(input$vel_bal, "dtp")) { x <- "dtp_wf"; y <- "dwi"; xl <- "DTP since flood start"; yl <- "DWI" } else { x <- "prod_tp12"; y <- "tp12"; xl <- "Prod TP 12 m (%/yr)"; yl <- "Inj TP 12 m (%/yr)" }
    sn <- sn[is.finite(get(x)) & is.finite(get(y))]; m <- max(c(sn[[x]], sn[[y]])) * 1.1
    p <- pattern_scatter(sn, x, y, color = iwr_col(), hl = ctx$focus(), xlab = xl, ylab = yl)
    p <- pl_theme(p, xl, yl, legend = FALSE); st <- ctx$settings()
    ln <- function(k, col) list(type = "line", x0 = 0, y0 = 0, x1 = m, y1 = m * k, line = list(color = col, width = 1, dash = if (k == 1) "solid" else "dot"))
    p <- plotly::layout(p, xaxis = axis_style(xl, range = c(0, m)), yaxis = axis_style(yl, range = c(0, m)),
                        shapes = if (x == "prod_tp12") list(ln(1, pal$proto), ln(st$iwr_high, pal$inj), ln(st$iwr_low, pal$crit)) else list(ln(1, pal$proto)))
    plotly::event_register(p, "plotly_click")
  })

  shiny::observe({ r <- ctx$res(); shiny::req(r); s <- sort(unique(r$sand_props$sand)); shiny::updateSelectInput(session, "vel_map_unit", choices = s, selected = s[1]) })
  output$vel_map <- plotly::renderPlotly({
    u <- ctx$units_snap(); shiny::req(u, input$vel_map_unit)
    u <- u[sand == input$vel_map_unit]
    inj <- ctx$res()$wells[well_type == "INJECTOR", .(pattern, well)]
    b <- merge(inj, u[, .(pattern, tp = tp12)], by = "pattern")
    pattern_map_plot(ctx$res(), u[, .(pattern, value = dwi)], paste("DWI", input$vel_map_unit), lim = c(0, max(2, max(u$dwi))), bubbles = b, hl = ctx$focus())
  })

  output$vel_gap <- plotly::renderPlotly({
    st <- ctx$settings()
    if (identical(input$vel_gap_basis, "cobb")) {
      u <- ctx$units_snap(); shiny::req(u)
      g <- u[is.finite(cobb), .(gap = sum(cobb - rate)), by = pattern]
      lab <- "Cobb design rate − actual (bwipd)"
    } else {
      sn <- ctx$snap()[flooded == TRUE]
      g <- sn[, .(pattern = entity, gap = (st$target_tp - tp12) / 100 * hcpv / 365 / 1.02)]
      lab <- "Target-TP rate − actual (bwipd)"
    }
    if (!nrow(g)) return(empty_plot("No design rates (Cobb) available"))
    data.table::setorder(g, gap); g[, pattern := factor(pattern, pattern)]
    p <- plotly::plot_ly(g, y = ~pattern, x = ~gap, type = "bar", orientation = "h", source = "wf", customdata = ~as.character(pattern),
                         marker = list(color = ifelse(g$gap > 0, pal$ok, pal$inj)), hovertemplate = "%{y}: %{x:+,.0f} bwipd<extra></extra>")
    p <- pl_theme(p, lab, NULL, legend = FALSE)
    plotly::event_register(plotly::layout(p, yaxis = axis_style(NULL, type = "category")), "plotly_click")
  })

  output$vel_tphist_scope <- shiny::renderUI(htmltools::span(class = "wf-card-hint", paste("pattern", ctx$focus() %||% "(select one)", "vs target")))
  output$vel_tphist <- plotly::renderPlotly({
    r <- ctx$res(); f <- ctx$focus(); shiny::req(r$units)
    if (is.null(f)) return(empty_plot("Select a pattern"))
    u <- r$units$pattern_sand[pattern == f & date <= ctx$asof() & date >= ctx$asof() - 6 * 365]
    if (length(ctx$sands())) u <- u[sand %in% ctx$sands()]
    p <- plotly::plot_ly(u, x = ~date, y = ~tp12, color = ~sand, type = "scatter", mode = "lines", colors = cluster_palette)
    p <- pl_theme(p, NULL, "Unit Inj TP 12 m (%/yr)")
    plotly::layout(p, shapes = list(hline_shape(T(), pal$warn), vline_shape(ctx$asof())), hovermode = "x unified")
  })

  output$vel_pulse <- plotly::renderPlotly({
    key <- input$vel_pulse_metric; s <- ctx$series(); a <- ctx$asof(); shiny::req(nrow(s))
    months <- utils::tail(sort(unique(s$date[s$date <= a])), as.numeric(input$vel_pulse_window))
    z <- data.table::dcast(s[date %in% months], entity ~ date, value.var = key)
    mat <- as.matrix(z[, -1])
    if (key %in% c("iwr", "opr")) {
      cs <- list(list(0, "#b91c1c"), list(0.35, "#f87171"), list(0.5, "#1e293b"), list(0.65, "#60a5fa"), list(1, "#1d4ed8"))
      p <- plotly::plot_ly(x = as.Date(colnames(mat)), y = z$entity, z = pmin(mat, 2), type = "heatmap", colorscale = cs, zmin = 0, zmax = 2, source = "wf",
                           customdata = matrix(z$entity, nrow(mat), ncol(mat)), hovertemplate = "%{y} · %{x|%b %Y}<br>%{z:.2f}<extra></extra>")
    } else {
      p <- plotly::plot_ly(x = as.Date(colnames(mat)), y = z$entity, z = mat, type = "heatmap", colorscale = num_scale(key != "tp"), source = "wf",
                           customdata = matrix(z$entity, nrow(mat), ncol(mat)), hovertemplate = "%{y} · %{x|%b %Y}<br>%{z:.3g}<extra></extra>")
    }
    p <- pl_theme(p, legend = FALSE)
    plotly::event_register(plotly::layout(p, yaxis = axis_style(NULL, type = "category", autorange = "reversed"), margin = list(l = 50)), "plotly_click")
  })

  output$vel_rates <- plotly::renderPlotly({
    s <- ctx$scope_series(); shiny::req(nrow(s))
    p <- plotly::plot_ly(s, x = ~date)
    p <- plotly::add_lines(p, y = ~qo, name = "Oil (bopd)", line = list(color = pal$oil, width = 2))
    p <- plotly::add_lines(p, y = ~qw, name = "Water (bwpd)", line = list(color = pal$water, width = 1.4))
    p <- plotly::add_lines(p, y = ~qwi, name = "Injection (bwipd)", line = list(color = pal$inj, width = 1.4))
    p <- plotly::add_lines(p, y = ~iwr12, name = "IWR 12 m", yaxis = "y2", line = list(color = "#fde047", width = 1.4))
    p <- pl_theme(p, NULL, "bbl/d")
    plotly::layout(p, hovermode = "x unified", shapes = list(vline_shape(ctx$asof()), hline_shape(1, "#fde047", "dash", "y2")),
                   yaxis2 = axis_style("IWR", overlaying = "y", side = "right", range = c(0, 2.5), showgrid = FALSE))
  })

  sig <- shiny::reactive({
    op <- ctx$opps()$summary
    if (!nrow(op)) return(data.table::data.table())
    op[status == "screening_only", .(Type = sprintf('<b style="color:%s">%s</b> %s', opp_colors[type], type, opp_types[type]),
                                     Pattern = pattern, Unit = data.table::fcoalesce(sand, ""), Families = families, Key = key)]
  })
  output$vel_signals <- DT::renderDT({
    d <- sig(); if (!nrow(d)) d <- data.table::data.table(Message = "No single-family signals")
    dt_dark(d[, setdiff(names(d), "Key"), with = FALSE], escape = FALSE, pageLength = 8)
  })
  shiny::observeEvent(input$vel_signals_rows_selected, { d <- sig(); if (nrow(d)) ctx$open_opp(d$Key[input$vel_signals_rows_selected]) })
}
