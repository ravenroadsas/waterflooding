# MATURITY: where each pattern stands in its waterflood life ------------------

maturity_ui <- function() {
  htmltools::tagList(
    htmltools::div(class = "wf-section-intro",
      htmltools::h2("Maturity"),
      htmltools::p("Where each pattern stands in its flood life: throughput (HCPVI), incremental recovery against",
                   "the Buckley-Leverett ideal, remaining oil and what to do next.")
    ),
    shiny::uiOutput("mat_kpis"),
    bslib::layout_columns(col_widths = c(7, 5), fill = FALSE,
      bslib::card(full_screen = TRUE, height = 470,
        card_title("Flood maturity map", "incremental WF recovery vs throughput, against ideal sweep curves",
                   shiny::checkboxInput("mat_trails", "Trajectories", FALSE)),
        bslib::card_body(plotly::plotlyOutput("mat_map", height = "100%"))
      ),
      bslib::card(full_screen = TRUE, height = 470,
        card_title("Life-cycle stage", "STOOIP and patterns per stage"),
        bslib::card_body(plotly::plotlyOutput("mat_stage", height = "100%"))
      )
    ),
    bslib::layout_columns(col_widths = c(6, 6), fill = FALSE,
      bslib::card(full_screen = TRUE, height = 520,
        card_title("Reservoir map", "patterns coloured by metric; click to drill down",
                   shiny::selectInput("mat_map_metric", NULL, width = "210px",
                     metric_choices(c("rf", "rf_wf", "hcpvi", "ev", "mi", "wc", "vrr_wf", "remaining_mov", "qo")), "rf")),
        bslib::card_body(plotly::plotlyOutput("mat_geo", height = "100%"))
      ),
      bslib::card(full_screen = TRUE, height = 520,
        card_title("Opportunity quadrant", "maturity (x) vs sweep performance (y)"),
        bslib::card_body(plotly::plotlyOutput("mat_quad", height = "100%"))
      )
    ),
    bslib::layout_columns(col_widths = c(6, 6), fill = FALSE,
      bslib::card(full_screen = TRUE, height = 460,
        card_title("Pattern x sand matrix", "which sand is lagging",
                   shiny::selectInput("mat_ps_metric", NULL, width = "200px",
                     metric_choices(c("rf", "rf_wf", "hcpvi", "ev", "mi", "remaining_mov")), "rf")),
        bslib::card_body(plotly::plotlyOutput("mat_psmat", height = "100%"))
      ),
      bslib::card(full_screen = TRUE, height = 460,
        card_title("Oil left on the table", "produced, reserves to economic WOR, and movable oil beyond"),
        bslib::card_body(plotly::plotlyOutput("mat_remaining", height = "100%"))
      )
    ),
    bslib::card(full_screen = TRUE,
      card_title("Maturity & action register", "click a row to open the pattern"),
      bslib::card_body(DT::DTOutput("mat_table"))
    )
  )
}

maturity_server <- function(input, output, session, ctx) {

  output$mat_kpis <- shiny::renderUI({
    s <- ctx$scope_series(); a <- ctx$asof()
    shiny::req(nrow(s))
    last <- s[date <= a][.N]
    sn <- ctx$snap()$snap
    eur <- sum(sn$eur, na.rm = TRUE); eur_st <- sum(sn$stooip[is.finite(sn$eur)])
    htmltools::div(class = "wf-kpi-row",
      kpi("STOOIP", fmt_mm(last$stooip, 1), "stb, selected sands"),
      kpi("Cumulative oil", fmt_mm(last$cum_oil, 2), paste("RF", fmt_pct(last$rf))),
      kpi("Waterflood RF", fmt_pct(last$rf_wf), "incremental over primary decline", tone = "ok"),
      kpi("HCPV injected", fmt_num(last$hcpvi, 2), paste("PVI", fmt_num(last$pvi, 2))),
      kpi("Apparent sweep Ev", fmt_num(last$ev, 2), paste("ideal WF RF", fmt_pct(last$rf_ideal)),
          tone = if (isTRUE(last$ev < 0.35)) "warn" else "neutral"),
      kpi("Maturity index", fmt_pct(last$mi, 0), "Np / movable oil"),
      kpi("EUR (WOR trend)", if (eur_st > 0) fmt_pct(eur / eur_st) else "–", paste("remaining", fmt_mm(max(eur - sum(sn$cum_oil[is.finite(sn$eur)]), 0))))
    )
  })

  output$mat_map <- plotly::renderPlotly({
    sn <- ctx$snap()$snap
    shiny::req(nrow(sn))
    cv <- theoretical_curve(ctx$res(), ctx$sands(), ctx$scope_patterns())
    xmax <- max(1.2, max(sn$hcpvi, na.rm = TRUE) * 1.15)
    cv <- cv[hcpvi <= xmax]
    p <- plotly::plot_ly(source = "wf")
    for (e in c(1, 0.75, 0.5, 0.25)) {
      p <- plotly::add_lines(p, x = cv$hcpvi, y = cv$rf * e, name = if (e == 1) "Ideal (Ev = 1)" else paste0("Ev = ", e),
        line = list(color = if (e == 1) "#e2e8f0" else "#475569", width = if (e == 1) 1.6 else 1, dash = if (e == 1) "solid" else "dot"),
        hoverinfo = "skip", showlegend = e %in% c(1, 0.5))
    }
    if (isTRUE(input$mat_trails)) {
      tr <- ctx$series()[date <= ctx$asof() & hcpvi > 0]
      p <- plotly::add_trace(p, data = tr, x = ~hcpvi, y = ~rf_wf, split = ~entity, type = "scatter", mode = "lines",
        line = list(width = 1, color = "rgba(148,163,184,0.35)"), hoverinfo = "skip", showlegend = FALSE)
    }
    fl <- sn[hcpvi > 0]
    if (nrow(fl)) fl[, size := 12 + 30 * sqrt(stooip / max(stooip))]
    for (stg in intersect(stage_levels, as.character(fl$stage))) {
      g <- fl[stage == stg]
      p <- plotly::add_markers(p, data = g, x = ~hcpvi, y = ~rf_wf, name = stg,
        marker = list(size = ~size, color = stage_colors[[stg]], opacity = 0.85, line = list(color = "#0b1220", width = 1)),
        customdata = ~entity,
        hovertemplate = paste0("<b>%{customdata}</b><br>HCPVI %{x:.2f}<br>WF RF %{y:.1%}<br>",
                               "Ev ", fmt_num(g$ev, 2), " · WC ", fmt_pct(g$wc6, 0), "<extra>", stg, "</extra>"))
    }
    p <- plotly::add_text(p, data = fl, x = ~hcpvi, y = ~rf_wf, text = ~entity, textposition = "top center",
      textfont = list(size = 9, color = pal$muted), hoverinfo = "skip", showlegend = FALSE)
    p <- pl_theme(p, "HCPV injected (dimensionless)", "Waterflood RF (fraction of STOOIP)")
    p <- plotly::layout(p, yaxis = axis_style("Waterflood RF", tickformat = ".0%"), xaxis = axis_style("HCPV injected", range = c(0, xmax)))
    plotly::event_register(p, "plotly_click")
  })

  output$mat_stage <- plotly::renderPlotly({
    sn <- ctx$snap()$snap
    shiny::req(nrow(sn))
    agg <- sn[, .(stooip = sum(stooip), n = .N, cum = sum(cum_oil)), by = stage]
    agg <- merge(data.table::data.table(stage = factor(stage_levels, stage_levels)), agg, by = "stage", all.x = TRUE)
    agg[is.na(n), `:=`(stooip = 0, n = 0L, cum = 0)]
    agg[, label := ifelse(n > 0, paste0(n, " · ", fmt_mm(stooip, 1), " · RF ", fmt_pct(cum / pmax(stooip, 1), 0)), "")]
    p <- plotly::plot_ly(agg, y = ~stage, x = ~stooip / 1e6, type = "bar", orientation = "h", name = "STOOIP",
      marker = list(color = paste0(unname(stage_colors[as.character(agg$stage)]), "55"),
                    line = list(color = unname(stage_colors[as.character(agg$stage)]), width = 1.5)),
      hovertemplate = "%{y}: %{x:.1f} MMstb STOOIP<extra></extra>")
    p <- plotly::add_bars(p, data = agg, y = ~stage, x = ~cum / 1e6, name = "Produced", orientation = "h",
      marker = list(color = unname(stage_colors[as.character(agg$stage)])), hovertemplate = "%{y}: %{x:.2f} MMstb produced<extra></extra>")
    p <- plotly::add_annotations(p, data = agg, x = ~stooip / 1e6, y = ~stage, text = ~label, xanchor = "left", xshift = 6,
      showarrow = FALSE, font = list(size = 10, color = pal$muted))
    p <- pl_theme(p, "MMstb", NULL)
    plotly::layout(p, barmode = "overlay", yaxis = axis_style(NULL, autorange = "reversed", type = "category"),
                   xaxis = axis_style("MMstb", range = c(0, max(agg$stooip) / 1e6 * 1.9)), margin = list(l = 110))
  })

  output$mat_geo <- plotly::renderPlotly({
    res <- ctx$res()
    polys <- pattern_polygons(res$wells)
    if (is.null(polys) || ctx$level() != "pattern") return(empty_plot(
      if (is.null(polys)) "Add x / y columns to the hierarchy table to enable the map" else "Map available at pattern level"))
    key <- input$mat_map_metric
    sn <- ctx$snap()$snap
    polys <- merge(polys, sn[, c("entity", key, "stage"), with = FALSE], by.x = "pattern", by.y = "entity")
    shiny::req(nrow(polys))
    vals <- unique(polys[, c("pattern", key), with = FALSE])
    lim <- range(vals[[key]], na.rm = TRUE)
    rev_scale <- key %in% c("wc", "remaining_mov")
    cols <- c("#1e3a8a", "#0ea5e9", "#2dd4bf", "#fde047", "#f97316")
    if (rev_scale) cols <- rev(cols)
    vals[, col := ramp_colors(get(key), cols, lim)]
    p <- plotly::plot_ly(source = "wf")
    for (pt in vals$pattern) {
      g <- polys[pattern == pt]
      p <- plotly::add_polygons(p, x = g$px, y = g$py, fillcolor = paste0(vals[pattern == pt, col], "cc"),
        line = list(color = "#0b1220", width = 1.5), name = pt, customdata = rep(pt, nrow(g)),
        hoveron = "fills", text = paste0("<b>", pt, "</b><br>", metric_catalog[[key]]$label, ": ", fmt_metric(vals[pattern == pt][[key]], key),
                                         "<br>", g$stage[1]), hoverinfo = "text", showlegend = FALSE)
    }
    cen <- unique(polys[, .(pattern, cx, cy)])
    p <- plotly::add_text(p, data = cen, x = ~cx, y = ~cy, text = ~pattern, textposition = "top center", textfont = list(color = "#ffffff", size = 12, family = "Inter, system-ui, sans-serif"),
                          hoverinfo = "skip", showlegend = FALSE)
    w <- unique(res$wells[is.finite(x)], by = "well")
    p <- plotly::add_markers(p, data = w[well_type == "PRODUCER"], x = ~x, y = ~y, name = "Producer",
      marker = list(symbol = "circle", size = 7, color = "#0b1220", line = list(color = "#e2e8f0", width = 1.2)),
      text = ~well, hoverinfo = "text")
    p <- plotly::add_markers(p, data = w[well_type == "INJECTOR"], x = ~x, y = ~y, name = "Injector",
      marker = list(symbol = "triangle-down", size = 9, color = pal$water, line = list(color = "#0b1220", width = 1)),
      text = ~well, hoverinfo = "text")
    # colour bar via invisible trace
    p <- plotly::add_markers(p, x = mean(w$x), y = mean(w$y), marker = list(size = 0.1, opacity = 0,
      color = lim, cmin = lim[1], cmax = lim[2], colorscale = lapply(seq_along(cols), function(i) list((i - 1) / (length(cols) - 1), cols[i])),
      showscale = TRUE, colorbar = list(thickness = 10, len = 0.8, tickfont = list(color = pal$muted, size = 9),
        tickformat = if (metric_catalog[[key]]$fmt == "pct") ".0%" else NULL)), hoverinfo = "skip", showlegend = FALSE)
    p <- pl_theme(p, NULL, NULL)
    p <- plotly::layout(p, xaxis = axis_style(NULL, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = axis_style(NULL, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, scaleanchor = "x"),
                        margin = list(l = 10, r = 10, t = 30, b = 10))
    plotly::event_register(p, "plotly_click")
  })

  output$mat_quad <- plotly::renderPlotly({
    sn <- ctx$snap()$snap
    st <- ctx$settings()
    shiny::req(nrow(sn))
    fl <- sn[hcpvi > 0 & is.finite(ev)]
    if (!nrow(fl)) return(empty_plot("No flooded patterns yet"))
    xmax <- max(1.2, max(fl$hcpvi) * 1.1); ymax <- max(1, max(fl$ev) * 1.1)
    zones <- list(
      list(x0 = 0, x1 = st$quad_hcpvi, y0 = st$quad_ev, y1 = ymax, q = "Accelerate"),
      list(x0 = st$quad_hcpvi, x1 = xmax, y0 = st$quad_ev, y1 = ymax, q = "Harvest"),
      list(x0 = st$quad_hcpvi, x1 = xmax, y0 = 0, y1 = st$quad_ev, q = "Conformance"),
      list(x0 = 0, x1 = st$quad_hcpvi, y0 = 0, y1 = st$quad_ev, q = "Investigate"))
    shapes <- lapply(zones, function(z) list(type = "rect", x0 = z$x0, x1 = z$x1, y0 = z$y0, y1 = z$y1, layer = "below",
      fillcolor = paste0(quadrant_colors[[z$q]], "14"), line = list(width = 0)))
    ann <- lapply(zones, function(z) list(x = (z$x0 + z$x1) / 2, y = if (z$y0 > 0) z$y1 * 0.97 else z$y1 * 0.06, text = toupper(z$q),
      showarrow = FALSE, font = list(color = quadrant_colors[[z$q]], size = 11)))
    fl[, size := 12 + 26 * sqrt(remaining_mov / max(remaining_mov, 1))]
    p <- plotly::plot_ly(fl, source = "wf", x = ~hcpvi, y = ~ev, type = "scatter", mode = "markers+text", text = ~entity, textposition = "top center",
      textfont = list(size = 9, color = pal$muted), customdata = ~entity,
      marker = list(size = ~size, color = unname(quadrant_colors[as.character(fl$quadrant)]), opacity = 0.85,
                    line = list(color = "#0b1220", width = 1)),
      hovertemplate = paste0("<b>%{customdata}</b><br>HCPVI %{x:.2f} · Ev %{y:.2f}<br>Remaining movable ",
                             fmt_mm(fl$remaining_mov), "<br>", fl$action, "<extra></extra>"))
    p <- pl_theme(p, "HCPV injected (maturity)", "Apparent sweep Ev (performance)", legend = FALSE)
    p <- plotly::layout(p, shapes = shapes, annotations = ann, xaxis = axis_style("HCPV injected (maturity)", range = c(0, xmax)),
                        yaxis = axis_style("Apparent sweep Ev (performance)", range = c(0, ymax)))
    plotly::event_register(p, "plotly_click")
  })

  output$mat_psmat <- plotly::renderPlotly({
    if (ctx$level() != "pattern") return(empty_plot("Matrix available at pattern level"))
    key <- input$mat_ps_metric
    m <- pattern_sand_snapshot(ctx$res(), ctx$asof(), ctx$scope_patterns())
    if (length(ctx$sands())) m <- m[sand %in% ctx$sands()]
    shiny::req(nrow(m))
    z <- data.table::dcast(m, sand ~ pattern, value.var = key)
    mat <- as.matrix(z[, -1])
    txt <- matrix(fmt_metric(as.vector(mat), key), nrow = nrow(mat))
    p <- plotly::plot_ly(x = colnames(mat), y = z$sand, z = mat, type = "heatmap", colorscale = cscale(key),
      text = txt, texttemplate = if (ncol(mat) <= 14) "%{text}" else "", textfont = list(size = 9),
      hovertemplate = "Pattern %{x} · sand %{y}<br>%{text}<extra></extra>", colorbar = list(thickness = 10, tickfont = list(color = pal$muted)))
    p <- pl_theme(p, "Pattern", "Sand", legend = FALSE)
    plotly::layout(p, yaxis = axis_style("Sand", type = "category"), xaxis = axis_style("Pattern", type = "category"))
  })

  output$mat_remaining <- plotly::renderPlotly({
    sn <- data.table::copy(ctx$snap()$snap)
    shiny::req(nrow(sn))
    sn[, eur_c := ifelse(is.finite(eur), eur, cum_oil)]
    sn[, `:=`(reserves = pmax(eur_c - cum_oil, 0), beyond = pmax(mov - eur_c, 0))]
    data.table::setorder(sn, -beyond)
    sn[, entity := factor(entity, rev(entity))]
    p <- plotly::plot_ly(sn, source = "wf", y = ~entity, x = ~cum_oil / 1e6, type = "bar", orientation = "h", name = "Produced",
                         marker = list(color = pal$oil), customdata = ~as.character(entity),
                         hovertemplate = "%{y}: %{x:.2f} MMstb produced<extra></extra>")
    p <- plotly::add_bars(p, x = ~reserves / 1e6, name = "Reserves to WOR limit", marker = list(color = "#a3e635"),
                          hovertemplate = "%{y}: %{x:.2f} MMstb to economic WOR<extra></extra>")
    p <- plotly::add_bars(p, x = ~beyond / 1e6, name = "Movable oil beyond (target)", marker = list(color = "rgba(251,191,36,0.55)"),
                          hovertemplate = "%{y}: %{x:.2f} MMstb movable not recovered by current trend<extra></extra>")
    p <- pl_theme(p, "MMstb", NULL)
    p <- plotly::layout(p, barmode = "stack", yaxis = axis_style(NULL, type = "category"))
    plotly::event_register(p, "plotly_click")
  })

  mat_table_data <- shiny::reactive({
    sn <- data.table::copy(ctx$snap()$snap)
    shiny::req(nrow(sn))
    data.table::setorder(sn, quadrant, -remaining_mov)
    sn[, .(Entity = entity, Stage = as.character(stage), Quadrant = as.character(quadrant),
           `STOOIP MMstb` = round(stooip / 1e6, 2), RF = round(100 * rf, 1), `WF RF` = round(100 * rf_wf, 1),
           HCPVI = round(hcpvi, 2), Ev = round(ev, 2), `WC 6m %` = round(100 * wc6, 1), `VRR 6m` = round(vrr6, 2),
           `EUR RF` = round(100 * rf_eur, 1), `Remaining movable MMstb` = round(remaining_mov / 1e6, 2),
           Flags = n_flags, `Suggested action` = action)]
  })
  output$mat_table <- DT::renderDT({
    d <- mat_table_data()
    dt <- dt_dark(d, pageLength = 12)
    dt <- DT::formatStyle(dt, "Stage", color = DT::styleEqual(names(stage_colors), unname(stage_colors)), fontWeight = "600")
    DT::formatStyle(dt, "Quadrant", color = DT::styleEqual(names(quadrant_colors), unname(quadrant_colors)), fontWeight = "600")
  })
  shiny::observeEvent(input$mat_table_rows_selected, {
    i <- input$mat_table_rows_selected
    ctx$open_drill(mat_table_data()$Entity[i])
  })
}
