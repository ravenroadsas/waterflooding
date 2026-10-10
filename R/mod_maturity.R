# STEP 1 - PATTERN MATURITY ----------------------------------------------------------------

maturity_ui <- function() {
  htmltools::tagList(
    htmltools::div(class = "wf-section-intro",
      htmltools::div(class = "wf-step", "Step 1 of 4"), htmltools::h2("Pattern maturity"),
      htmltools::p("Where each pattern and unit sits in its flood life, and whether it recovers what the prototype expects at that DWI.",
                   "Start with Sec RF vs DWI, separate inefficient water use, excess water and low oil response, then drill by unit and check spatially.")),
    shiny::uiOutput("mat_kpis"),
    bslib::layout_columns(col_widths = c(7, 5), fill = FALSE,
      bslib::card(full_screen = TRUE, height = 470,
        card_title("Sec RF vs DWI", "prototype band; colour, size = IWR, shape = start of flood",
                   shiny::checkboxInput("mat_traj", "Trajectories", FALSE), tag = "extend"),
        bslib::card_body(plotly::plotlyOutput("mat_secrf", height = "100%"))),
      bslib::card(full_screen = TRUE, height = 470,
        card_title("OPR vs WPR", "actual vs expected at the same DWI", tag = "new"),
        bslib::card_body(plotly::plotlyOutput("mat_oprwpr", height = "100%")))),
    bslib::layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
      bslib::card(full_screen = TRUE, height = 400,
        card_title("Utilization vs DWI", "colour Inj TP · size IWR", tag = "new"),
        bslib::card_body(plotly::plotlyOutput("mat_util", height = "100%"))),
      bslib::card(full_screen = TRUE, height = 400,
        card_title("WOR vs DWI", "log scale, prototype band", tag = "new"),
        bslib::card_body(plotly::plotlyOutput("mat_wor", height = "100%"))),
      bslib::card(full_screen = TRUE, height = 400,
        card_title("DWI by unit", NULL, shiny::uiOutput("mat_units_scope", inline = TRUE), tag = "new"),
        bslib::card_body(plotly::plotlyOutput("mat_units", height = "100%")))),
    bslib::layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
      bslib::card(full_screen = TRUE, height = 440,
        card_title("Maturity map", NULL, shiny::selectInput("mat_map_unit", NULL, c("All units" = ""), width = "120px"), tag = "extend"),
        bslib::card_body(plotly::plotlyOutput("mat_map", height = "100%"))),
      bslib::card(full_screen = TRUE, height = 440,
        card_title("Heterogeneity index", "producers vs area average, 5 years", tag = "new"),
        bslib::card_body(plotly::plotlyOutput("mat_hi", height = "100%"))),
      bslib::card(full_screen = TRUE, height = 440,
        card_title("Life-cycle stage", "HCPV by stage, from DWI and water cut", tag = "keep"),
        bslib::card_body(plotly::plotlyOutput("mat_stage", height = "100%")))),
    bslib::card(full_screen = TRUE,
      card_title("Maturity register", "click a row to open Pattern 360; next step carries the pattern to Process velocity", tag = "extend"),
      bslib::card_body(fillable = FALSE, DT::DTOutput("mat_table")))
  )
}

maturity_server <- function(input, output, session, ctx) {
  output$mat_kpis <- shiny::renderUI({
    s <- ctx$scope_series(); a <- ctx$asof(); shiny::req(nrow(s))
    l <- s[date <= a][.N]; sn <- ctx$snap()
    rem <- sum((sn$remaining_stb)[is.finite(sn$remaining_stb)])
    htmltools::div(class = "wf-kpi-row",
      kpi("HCPV", paste(fmt_num(l$hcpv / 1e6, 0), "MMrb"), sprintf("%d patterns", nrow(ctx$snap()))),
      kpi("DWI", fmt_num(l$dwi, 2), paste("DTP", fmt_num(l$dtp, 2))),
      kpi("Sec RF", fmt_pct(l$sec_rf), paste("total RF", fmt_pct(l$rf)), tone = "ok"),
      kpi("OPR", fmt_num(l$opr, 2), paste("expected Sec RF", fmt_pct(l$exp_sec_rf)), tone = if (isTRUE(l$opr < 0.9)) "warn" else "neutral"),
      kpi("WPR", fmt_num(l$wpr, 2), "water vs prototype", tone = if (isTRUE(l$wpr > 1.2)) "warn" else "neutral"),
      kpi("Loss", fmt_num(l$loss, 2), "DWI − DTP since flood start"),
      kpi(sprintf("Utilization %d m", ctx$settings()$util_window), fmt_num(l$util, 1), paste("prototype", fmt_num(l$exp_util, 1))),
      kpi("Remaining WF oil", paste(fmt_num(rem / 1e6, 1), "MMstb"), "waterflood fit, all patterns"))
  })

  proto_now <- shiny::reactive(main_proto(ctx$res(), ctx$snap()))

  output$mat_secrf <- plotly::renderPlotly({
    sn <- ctx$snap()[flooded == TRUE]; shiny::req(nrow(sn))
    xmax <- max(1.2, max(sn$dwi, na.rm = TRUE) * 1.08)
    p <- plotly::plot_ly(source = "wf")
    p <- add_proto(p, proto_now(), "sec_rf", xmax)
    if (isTRUE(input$mat_traj)) {
      tr <- ctx$series()[date <= ctx$asof() & dwi > 0]
      p <- plotly::add_trace(p, data = tr, x = ~dwi, y = ~sec_rf, split = ~entity, type = "scatter", mode = "lines",
                             line = list(width = 1, color = "rgba(148,163,184,0.35)"), hoverinfo = "skip", showlegend = FALSE)
    }
    p <- pattern_scatter(sn, "dwi", "sec_rf", color = ctx$color_by(), hl = ctx$focus(), p = p)
    p <- pl_theme(p, "DWI (HCPV injected)", "Secondary RF")
    ymax <- max(0.1, max(sn$sec_rf, na.rm = TRUE) * 1.25)
    p <- plotly::layout(p, yaxis = axis_style("Secondary RF (fraction of HCPV)", tickformat = ".0%", range = c(0, ymax)), xaxis = axis_style("DWI", range = c(0, xmax)),
      shapes = list(rect_shape(0, ctx$settings()$judge_dwi, 0, 1, "rgba(148,163,184,0.10)")),
      annotations = list(list(x = ctx$settings()$judge_dwi / 2, y = 0.98, yref = "paper", text = "too early<br>to judge", showarrow = FALSE,
                              font = list(size = 9, color = pal$muted))))
    plotly::event_register(p, "plotly_click")
  })

  output$mat_oprwpr <- plotly::renderPlotly({
    sn <- ctx$snap()[is.finite(opr) & is.finite(wpr)]
    if (!nrow(sn)) return(empty_plot("OPR / WPR need a prototype and DWI above the judging threshold"))
    xm <- max(2, max(sn$wpr) * 1.1); ym <- max(1.6, max(sn$opr) * 1.1)
    p <- pattern_scatter(sn, "wpr", "opr", color = ctx$color_by(), hl = ctx$focus())
    q <- function(x, y, t, c) list(x = x, y = y, text = t, showarrow = FALSE, font = list(size = 9, color = c))
    p <- pl_theme(p, "WPR (DWP / expected)", "OPR (Sec RF / expected)")
    p <- plotly::layout(p, xaxis = axis_style("WPR", range = c(0, xm)), yaxis = axis_style("OPR", range = c(0, ym)),
      shapes = list(rect_shape(0, 1, 1, ym, "rgba(45,212,191,0.08)"), rect_shape(1, xm, 0, 1, "rgba(248,113,113,0.08)"),
                    rect_shape(0, 1, 0, 1, "rgba(251,191,36,0.06)"), rect_shape(1, xm, 1, ym, "rgba(96,165,250,0.06)"),
                    list(type = "line", x0 = 1, x1 = 1, y0 = 0, y1 = ym, line = list(color = pal$muted, width = 1)),
                    list(type = "line", x0 = 0, x1 = xm, y0 = 1, y1 = 1, line = list(color = pal$muted, width = 1))),
      annotations = list(q(0.5, ym * 0.97, "BEST: more oil, less water", pal$ok), q((1 + xm) / 2, ym * 0.97, "more oil, more water", pal$water),
                         q(0.5, 0.04, "low response: out of zone?", pal$warn), q((1 + xm) / 2, 0.04, "WORST: low oil, excess water", pal$crit)))
    plotly::event_register(p, "plotly_click")
  })

  output$mat_util <- plotly::renderPlotly({
    sn <- ctx$snap()[flooded == TRUE & is.finite(util)]; shiny::req(nrow(sn))
    xmax <- max(1.2, max(sn$dwi) * 1.08)
    p <- add_proto(plotly::plot_ly(source = "wf"), proto_now(), "util", xmax, band = 0.3)
    col <- if (ctx$color_by() %in% c("cluster", "area", "stage")) ctx$color_by() else "tp12"
    p <- pattern_scatter(sn, "dwi", "util", color = col, hl = ctx$focus(), p = p)
    p <- pl_theme(p, "DWI", "Utilization (rb inj / rb oil)", legend = FALSE)
    plotly::event_register(plotly::layout(p, xaxis = axis_style("DWI", range = c(0, xmax))), "plotly_click")
  })

  output$mat_wor <- plotly::renderPlotly({
    sn <- ctx$snap()[flooded == TRUE & is.finite(wor6) & wor6 > 0]; shiny::req(nrow(sn))
    xmax <- max(1.2, max(sn$dwi) * 1.08)
    p <- add_proto(plotly::plot_ly(source = "wf"), proto_now(), "wor", xmax, band = 1, log = TRUE)
    p <- pattern_scatter(sn, "dwi", "wor6", color = ctx$color_by(), hl = ctx$focus(), p = p)
    p <- pl_theme(p, "DWI", "WOR (log)", legend = FALSE)
    plotly::event_register(plotly::layout(p, yaxis = axis_style("WOR (log)", type = "log"), xaxis = axis_style("DWI", range = c(0, xmax))), "plotly_click")
  })

  output$mat_units_scope <- shiny::renderUI(htmltools::span(class = "wf-card-hint", ctx$focus() %||% "select a pattern"))
  output$mat_units <- plotly::renderPlotly({
    u <- ctx$units_snap(); f <- ctx$focus()
    if (is.null(u) || is.null(f)) return(empty_plot("Select a pattern (sidebar or click a point)"))
    u <- u[pattern == f]
    if (length(ctx$sands())) u <- u[sand %in% ctx$sands()]
    if (!nrow(u)) return(empty_plot("No unit data"))
    cls <- ifelse(u$dwi >= 1.2, "#3b82f6", ifelse(u$dwi >= 0.5, "#22d3ee", "#34d399"))
    p <- plotly::plot_ly(y = u$sand, x = u$cum_winj / 1e6, type = "bar", orientation = "h", name = "Cum. injection (MMbbl)",
                         marker = list(color = "#475f86"), xaxis = "x", hovertemplate = "%{y}: %{x:.2f} MMbbl<extra></extra>")
    p <- plotly::add_bars(p, y = u$sand, x = u$hcpv / 1e6, name = "HCPV (MMrb)", marker = list(color = "#3f6f5a"), xaxis = "x2",
                          hovertemplate = "%{y}: %{x:.2f} MMrb<extra></extra>")
    p <- plotly::add_bars(p, y = u$sand, x = u$dwi, name = "DWI", marker = list(color = cls), xaxis = "x3",
                          text = fmt_num(u$dwi, 2), textposition = "outside", hovertemplate = "%{y}: DWI %{x:.2f}<extra></extra>")
    p <- pl_theme(p, legend = FALSE)
    plotly::layout(p, yaxis = axis_style(NULL, autorange = "reversed", type = "category"),
      xaxis = axis_style("Cum. inj. MMbbl", domain = c(0, 0.3)), xaxis2 = axis_style("HCPV MMrb", domain = c(0.35, 0.65)),
      xaxis3 = axis_style("DWI", domain = c(0.7, 1)), margin = list(l = 30))
  })

  shiny::observe({
    r <- ctx$res(); shiny::req(r)
    shiny::updateSelectInput(session, "mat_map_unit", choices = c("All units" = "", sort(unique(r$sand_props$sand))))
  })
  output$mat_map <- plotly::renderPlotly({
    if (!nzchar(input$mat_map_unit %||% "")) {
      sn <- ctx$snap(); vals <- sn[, .(pattern = entity, value = dwi)]
      pattern_map_plot(ctx$res(), vals, "DWI", lim = c(0, max(2, max(vals$value, na.rm = TRUE))), hl = ctx$focus())
    } else {
      u <- ctx$units_snap()[sand == input$mat_map_unit]
      pattern_map_plot(ctx$res(), u[, .(pattern, value = dwi)], paste("DWI unit", input$mat_map_unit), lim = c(0, max(2, max(u$dwi))), hl = ctx$focus())
    }
  })

  output$mat_hi <- plotly::renderPlotly({
    r <- ctx$res(); a <- ctx$asof(); shiny::req(r$hi)
    f <- ctx$focus()
    wells <- if (!is.null(f)) unique(r$alloc[pattern == f & coeff > 0, well]) else unique(r$hi$well)
    h <- r$hi[well %in% wells & date <= a & date > a - 5 * 365 & format(date, "%m") %in% c("01", "07")]
    h <- rbind(h, r$hi[well %in% wells & date == max(date[date <= a])])
    if (!nrow(h)) return(empty_plot("No producers"))
    lim <- max(1, max(abs(c(h$hi_oil, h$hi_water)), na.rm = TRUE) * 1.1)
    p <- plotly::plot_ly()
    for (w in unique(h$well)) {
      g <- h[well == w][order(date)]
      p <- plotly::add_trace(p, x = g$hi_water, y = g$hi_oil, type = "scatter", mode = "lines+markers", name = w,
                             marker = list(size = c(rep(4, nrow(g) - 1), 9)), line = list(width = 1.5),
                             text = sprintf("%s %s<br>HI water %.2f · HI oil %.2f", w, fmt_month(g$date), g$hi_water, g$hi_oil), hoverinfo = "text")
    }
    p <- pl_theme(p, "HI water", "HI oil")
    plotly::layout(p, xaxis = axis_style("HI water", range = c(-lim, lim)), yaxis = axis_style("HI oil", range = c(-lim, lim)),
      shapes = list(rect_shape(-lim, 0, 0, lim, "rgba(45,212,191,0.07)"), rect_shape(0, lim, -lim, 0, "rgba(248,113,113,0.08)"),
                    list(type = "line", x0 = 0, x1 = 0, y0 = -lim, y1 = lim, line = list(color = pal$muted, width = 1)),
                    list(type = "line", x0 = -lim, x1 = lim, y0 = 0, y1 = 0, line = list(color = pal$muted, width = 1))),
      annotations = list(list(x = -lim * 0.5, y = lim * 0.93, text = "more oil, less water", showarrow = FALSE, font = list(size = 9, color = pal$ok)),
                         list(x = lim * 0.5, y = -lim * 0.93, text = "less oil, more water", showarrow = FALSE, font = list(size = 9, color = pal$crit))))
  })

  output$mat_stage <- plotly::renderPlotly({
    sn <- ctx$snap(); shiny::req(nrow(sn))
    a <- sn[, .(hcpv = sum(hcpv), n = .N), by = stage]
    a <- merge(data.table::data.table(stage = factor(stage_levels, stage_levels)), a, by = "stage", all.x = TRUE)
    a[is.na(n), `:=`(hcpv = 0, n = 0L)]
    p <- plotly::plot_ly(a, y = ~stage, x = ~hcpv / 1e6, type = "bar", orientation = "h",
                         marker = list(color = unname(stage_colors[as.character(a$stage)])),
                         text = ~ifelse(n > 0, paste(n, "patterns"), ""), textposition = "auto",
                         hovertemplate = "%{y}: %{x:.1f} MMrb HCPV<extra></extra>")
    p <- pl_theme(p, "HCPV (MMrb)", NULL, legend = FALSE)
    plotly::layout(p, yaxis = axis_style(NULL, autorange = "reversed", type = "category"), margin = list(l = 100))
  })

  reg <- shiny::reactive({
    sn <- data.table::copy(ctx$snap()); op <- ctx$opps()$summary
    nxt <- if (nrow(op)) op[!is.na(pattern), .(nxt = paste(unique(paste(action_label(action), well)), collapse = "; ")), by = pattern] else data.table::data.table(pattern = character(), nxt = character())
    sn <- merge(sn, nxt, by.x = "entity", by.y = "pattern", all.x = TRUE)
    data.table::setorder(sn, -dwi)
    sn[, .(Pattern = entity, Area = area, Stage = as.character(stage), DWI = round(dwi, 2), `Sec RF %` = round(100 * sec_rf, 1),
           OPR = round(opr, 2), WPR = round(wpr, 2), Util = round(util, 1), `IWR 12m` = round(iwr12, 2), WOR = round(wor6, 1),
           `Loss` = round(loss, 2), Cluster = if ("cluster_name" %in% names(sn)) cluster_name else NA_character_,
           `Next step` = data.table::fcoalesce(nxt, ""))]
  })
  output$mat_table <- DT::renderDT({
    d <- reg()
    dt <- dt_dark(d, pageLength = 16)
    DT::formatStyle(dt, "Stage", color = DT::styleEqual(names(stage_colors), unname(stage_colors)), fontWeight = "600")
  })
  shiny::observeEvent(input$mat_table_rows_selected, ctx$open_p360(reg()$Pattern[input$mat_table_rows_selected]))
}
