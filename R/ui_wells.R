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

# Well track for the ADPERF workbench: completion state, each log algorithm, agreement, potential,
# water share of open intervals and the job selection, against depth. Points carry the opportunity
# key in customdata so a click toggles it in the job.
plot_wellbore <- function(wb, sel = character()) {
  algs <- if (!is.null(wb$log) && nrow(wb$log)) sort(unique(wb$log$algorithm)) else character()
  cols <- c("Wellbore", algs, "Agree", "Potential", "Water share", "Job")
  pos <- stats::setNames(seq_along(cols) - 1, cols)
  shapes <- list(); pts <- list()
  rect <- function(col, t, b, fill, w = 0.38, line = NULL, dash = NULL, frac = 1) {
    x0 <- pos[[col]] - w; shapes[[length(shapes) + 1]] <<- list(type = "rect", x0 = x0, x1 = x0 + 2 * w * frac, y0 = t, y1 = b, fillcolor = fill,
      line = list(color = line %||% fill, width = if (is.null(line)) 0 else 1.5, dash = dash %||% "solid"), layer = "above") }
  pt <- function(col, t, b, txt, key = NA_character_) pts[[length(pts) + 1]] <<- data.table::data.table(x = pos[[col]], y = (t + b) / 2, txt = txt, okey = key)
  acol <- stats::setNames(c("#3b82f6", "#f97316", "#10b981", "#a855f7", "#eab308")[seq_along(algs)], algs)
  st <- wb$state
  for (i in seq_len(nrow(st$open))) { o <- st$open[i]; rect("Wellbore", o$top_ft, o$base_ft, "#e2e8f0"); pt("Wellbore", o$top_ft, o$base_ft, sprintf("Open perforation %s-%s ft", fmt_int(o$top_ft), fmt_int(o$base_ft))) }
  for (i in seq_len(nrow(st$squeeze))) { o <- st$squeeze[i]; rect("Wellbore", o$top_ft, o$base_ft, "rgba(148,163,184,0.25)", line = "#94a3b8", dash = "dot"); pt("Wellbore", o$top_ft, o$base_ft, sprintf("Squeezed %s-%s ft", fmt_int(o$top_ft), fmt_int(o$base_ft))) }
  dmax <- max(c(wb$depth_max, 0), na.rm = TRUE)
  for (i in seq_len(nrow(st$plug))) { o <- st$plug[i]; rect("Wellbore", o$top_ft, dmax, "rgba(100,116,139,0.55)"); pt("Wellbore", o$top_ft, o$top_ft + 2, sprintf("Plug at %s ft: everything below is isolated", fmt_int(o$top_ft))) }
  for (a in algs) { x <- wb$log[algorithm == a]; for (i in seq_len(nrow(x))) { rect(a, x$top_ft[i], x$base_ft[i], acol[[a]]); pt(a, x$top_ft[i], x$base_ft[i], sprintf("%s: %s-%s ft", a, fmt_int(x$top_ft[i]), fmt_int(x$base_ft[i]))) } }
  c <- wb$cands
  if (!is.null(c) && nrow(c)) for (i in seq_len(nrow(c))) {
    r <- c[i]; blocked <- nzchar(r$conflict)
    rect("Agree", r$top_ft, r$base_ft, if (blocked) "rgba(34,211,238,0.25)" else "#22d3ee", frac = r$n / r$n_of)
    pt("Agree", r$top_ft, r$base_ft, sprintf("%s · unit %s<br>found by %d of %d: %s%s", r$target, data.table::fcoalesce(r$sand, "n/a"), r$n, r$n_of, r$algorithms,
                                           if (blocked) paste0("<br>blocked: ", r$conflict) else ""), r$key)
    if (is.finite(r$qo_base)) { rect("Potential", r$top_ft, r$base_ft, "#34d399", frac = min(1, r$qo_base / wb$qmax)); pt("Potential", r$top_ft, r$base_ft, sprintf("%s Bajo / Base / Alto %s / %s / %s bopd", r$target, fmt_int(r$qo_bajo), fmt_int(r$qo_base), fmt_int(r$qo_alto)), r$key) }
    if (blocked) pt("Job", r$top_ft, r$base_ft, sprintf("%s blocked: %s", r$target, r$conflict))
    else if (!is.na(r$key)) { on <- r$key %in% sel; rect("Job", r$top_ft, r$base_ft, if (on) "#22d3ee" else "rgba(34,211,238,0.06)", line = "#22d3ee", dash = if (on) "solid" else "dash")
      pt("Job", r$top_ft, r$base_ft, sprintf("%s: %s", r$target, if (on) "in the job (click to remove)" else "click to add ADPERF"), r$key) }
  }
  o <- wb$offenders
  if (!is.null(o) && nrow(o)) for (i in seq_len(nrow(o))) {
    r <- o[i]; rect("Water share", r$top_ft, r$base_ft, "#60a5fa", frac = r$share)
    pt("Water share", r$top_ft, r$base_ft, sprintf("%s: %s of the water (%s bwpd, %s bopd, WC %s)", r$target, fmtp(100 * r$share, 0), fmt_int(r$qw), fmt_int(r$qo), fmtp(100 * r$wc, 0)), r$key)
    if (!is.na(r$key)) { on <- r$key %in% sel; rect("Job", r$top_ft, r$base_ft, if (on) "#f87171" else "rgba(248,113,113,0.06)", line = "#f87171", dash = if (on) "solid" else "dash")
      pt("Job", r$top_ft, r$base_ft, sprintf("%s: %s", r$target, if (on) "isolation in the job (click to remove)" else "click to add isolation"), r$key) }
  }
  g <- wb$gaps
  if (!is.null(g) && nrow(g)) for (i in seq_len(nrow(g))) {
    r <- g[i]; if (is.na(r$key)) next
    on <- r$key %in% sel; rect("Job", r$top_ft, r$base_ft, if (on) "#a78bfa" else "rgba(167,139,250,0.06)", line = "#a78bfa", dash = if (on) "solid" else "dash", w = 0.2)
    pt("Job", r$top_ft, r$base_ft, sprintf("%s: %s bopd below potential; %s", r$target, fmt_int(r$gap), if (on) "re-perforation in the job" else "click to add stimulation / re-perforation"), r$key)
  }
  p <- data.table::rbindlist(pts)
  dr <- range(c(wb$depth_min, wb$depth_max), na.rm = TRUE)
  fig <- plotly::plot_ly(source = "wbtrack")
  if (nrow(p)) fig <- plotly::add_markers(fig, data = p, x = ~x, y = ~y, customdata = ~okey, text = ~txt, hoverinfo = "text",
                                          marker = list(size = 16, opacity = 0), showlegend = FALSE)
  fig <- plotly::layout(pl_theme(fig, NULL, "Depth, ft", legend = FALSE), shapes = shapes,
    xaxis = axis_style(NULL, tickvals = unname(pos), ticktext = names(pos), range = c(-0.6, length(cols) - 0.4), side = "top", showgrid = FALSE, zeroline = FALSE),
    yaxis = axis_style("Depth, ft", range = c(dr[2] + 10, dr[1] - 10)), margin = list(l = 60, r = 10, t = 40, b = 10))
  plotly::event_register(fig, "plotly_click")
}

# Oil or water bridge from the current rate to the rate after the job (Base).
plot_job_bridge <- function(now, items, what = c("qo", "qw"), title = "") {
  what <- match.arg(what)
  v <- items[[paste0(what, "1")]]; v[!is.finite(v)] <- 0
  lab <- c("Now", items$label, "After job")
  vals <- c(now, v, now + sum(v))
  meas <- c("absolute", rep("relative", length(v)), "total")
  plotly::plot_ly(x = factor(lab, levels = lab), y = vals, measure = meas, type = "waterfall", textposition = "outside", text = fmt_int(vals),
                  increasing = list(marker = list(color = if (what == "qo") pal$oil else pal$water)), decreasing = list(marker = list(color = "#f97316")),
                  totals = list(marker = list(color = "#64748b")), connector = list(line = list(color = pal$line))) |>
    pl_theme(NULL, if (what == "qo") "Oil, bopd" else "Water, bwpd", legend = FALSE)
}
