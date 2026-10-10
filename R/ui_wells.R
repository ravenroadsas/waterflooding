# Well-level plots shared by the decision record and Well 360 --------------------------------

scenario_colors <- c(Bajo = "#fbbf24", Base = "#34d399", Alto = "#60a5fa")
estado_colors <- c(abierto = "#34d399", parcial = "#fbbf24", cerrado = "#64748b")

# Bajo / Base / Alto oil profiles with the band between Bajo and Alto; Base water on the right axis.
plot_forecast <- function(prof) {
  if (is.null(prof) || !nrow(prof)) return(empty_plot("No Bajo / Base / Alto profile for this target"))
  w <- data.table::dcast(prof, month ~ scenario, value.var = "qo")
  p <- plotly::plot_ly()
  if (all(c("Bajo", "Alto") %in% names(w))) {
    p <- plotly::add_ribbons(p, data = w, x = ~month, ymin = ~Bajo, ymax = ~Alto, name = "Bajo-Alto", fillcolor = "rgba(34,211,238,0.12)",
                             line = list(width = 0), hoverinfo = "skip")
  }
  for (sc in intersect(c("Bajo", "Base", "Alto"), unique(prof$scenario))) {
    x <- prof[scenario == sc]
    p <- plotly::add_lines(p, data = x, x = ~month, y = ~qo, name = paste("qo", sc), line = list(color = scenario_colors[[sc]], width = if (sc == "Base") 2.6 else 1.4),
                           hovertemplate = paste0(sc, " month %{x}: %{y:.0f} bopd<extra></extra>"))
  }
  b <- prof[scenario == "Base"]
  if (nrow(b) && any(is.finite(b$qw))) p <- plotly::add_lines(p, data = b, x = ~month, y = ~qw, name = "qw Base", yaxis = "y2",
                                                            line = list(color = pal$water, width = 1.2, dash = "dot"))
  p <- pl_theme(p, "Month after the job", "Oil, bopd")
  plotly::layout(p, yaxis2 = axis_style("Water Base, bwpd", overlaying = "y", side = "right", showgrid = FALSE, rangemode = "tozero"),
                 yaxis = axis_style("Oil, bopd", rangemode = "tozero"))
}

# Monthly well rates; an optional decline fit (W3) and a job date.
plot_well_history <- function(h, years = 6, fit_note = NULL, job_date = NULL) {
  if (is.null(h) || !nrow(h)) return(empty_plot("No production for this well"))
  h <- h[date >= max(date) - years * 365]
  p <- plotly::plot_ly(h, x = ~date)
  if (any(h$bwipd > 0)) p <- plotly::add_lines(p, y = ~bwipd, name = "Injection", line = list(color = pal$inj, width = 2))
  if (any(h$bopd > 0)) p <- plotly::add_lines(p, y = ~bopd, name = "Oil", line = list(color = pal$oil, width = 2))
  if (any(h$bwpd > 0)) p <- plotly::add_lines(p, y = ~bwpd, name = "Water", line = list(color = pal$water, width = 1.4))
  shp <- if (!is.null(job_date)) list(vline_shape(job_date, pal$warn)) else list()
  plotly::layout(pl_theme(p, NULL, "bbl/d"), shapes = shp, yaxis = axis_style("bbl/d", type = "log"),
                 annotations = if (!is.null(fit_note)) list(list(x = 0.01, y = 0.98, xref = "paper", yref = "paper", text = fit_note, showarrow = FALSE,
                                                                  xanchor = "left", font = list(size = 10, color = pal$muted))) else list())
}

# Intervals of one well by depth: bar = kh, colour = opening status, label = Sw actual and BSW.
plot_interval_strip <- function(iv, sel = NULL) {
  if (is.null(iv) || !nrow(iv)) return(empty_plot("No interval analysis for this well"))
  x <- data.table::copy(iv)[order(data.table::fcoalesce(top_ft, 0))]
  x[, lab := sprintf("%s %s<br>%s ft", sand, interval_id, fmt_int(top_ft))]
  x[, lab := factor(lab, levels = rev(lab))]
  x[, txt := sprintf("Sw %s · BSW %s", fmtn(sw_act), fmtp(bsw0_pct, 0))]
  x[, line_w := ifelse(!is.null(sel) & interval_id %in% sel, 2.5, 0)]
  p <- plotly::plot_ly()
  for (e in unique(x$estado)) {
    d <- x[estado == e]
    p <- plotly::add_bars(p, data = d, y = ~lab, x = ~kh_md_ft, orientation = "h", name = e, text = ~txt, textposition = "auto",
                          marker = list(color = estado_colors[[e]] %||% pal$proto, line = list(color = pal$accent, width = d$line_w)),
                          hovertemplate = "%{y}<br>kh %{x:.0f} mD.ft<br>%{text}<extra></extra>", insidetextfont = list(color = "#0b1220"))
  }
  plotly::layout(pl_theme(p, "kh, mD.ft", NULL), barmode = "overlay", margin = list(l = 70), legend = list(orientation = "h", y = 1.08),
                 yaxis = axis_style(NULL, type = "category"))
}
