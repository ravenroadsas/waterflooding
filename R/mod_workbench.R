# ADPERF WORKBENCH: candidates of the log algorithms against the wellbore, offenders, potential gaps,
# and the job composer (potential, uncertainty, cost, lift) that proposes the job for approval. ------

workbench_ui <- function() {
  htmltools::tagList(
    htmltools::div(class = "wf-inline-tools",
      shiny::selectizeInput("wb_well", NULL, choices = NULL, width = "220px", options = list(placeholder = "well")),
      shiny::uiOutput("wb_head", inline = TRUE)),
    bslib::layout_columns(col_widths = c(5, 7),
      htmltools::div(htmltools::h6(class = "wf-h6", "Wellbore, log algorithms and job · click an interval in the Job column to add or remove it"),
                     plotly::plotlyOutput("wb_track", height = "640px")),
      htmltools::div(
        htmltools::h6(class = "wf-h6", "Candidates merged across algorithms"), DT::DTOutput("wb_cands"),
        htmltools::h6(class = "wf-h6", "Open intervals: water offenders"), DT::DTOutput("wb_off"),
        htmltools::h6(class = "wf-h6", "Open intervals below their theoretical potential"), DT::DTOutput("wb_gap"),
        shiny::selectizeInput("wb_other", "Other opportunities on this well", choices = NULL, multiple = TRUE, width = "100%"))),
    htmltools::h6(class = "wf-h6", "Job composer"),
    shiny::uiOutput("wb_kpis"),
    bslib::layout_columns(col_widths = c(4, 4, 4),
      plotly::plotlyOutput("wb_oil", height = "260px"), plotly::plotlyOutput("wb_wat", height = "260px"), plotly::plotlyOutput("wb_fc", height = "260px")),
    bslib::layout_columns(col_widths = c(5, 7),
      htmltools::div(htmltools::h6(class = "wf-h6", "Cost (standard cost lookup by job type and well depth)"), DT::DTOutput("wb_cost")),
      htmltools::div(htmltools::h6(class = "wf-h6", "Propose the job"),
        shiny::checkboxInput("wb_als", "Change the artificial lift in this rig visit", FALSE),
        shiny::textInput("wb_name", "Job name (optional)", "", width = "100%"),
        shiny::textAreaInput("wb_notes", "Notes for the lead", rows = 2, width = "100%"),
        shiny::actionButton("wb_propose", "Propose job for approval", class = "btn-primary btn-sm"),
        shiny::uiOutput("wb_msg"))))
}

workbench_server <- function(input, output, session, ctx) {
  sel <- shiny::reactiveVal(character())
  shiny::observe({
    ds <- ctx$res()$ds; ws <- workbench_wells(ds)
    if (!length(ws)) return()
    # default: the well with the most wellbore inputs (algorithms, rates, potential)
    n <- vapply(ws, function(w) sum(c(w %in% ds$log_intervals$well, w %in% ds$interval_rates$well, w %in% ds$interval_potential$well)), 0)
    w0 <- shiny::isolate(input$wb_well)
    if (is.null(w0) || !w0 %in% ws) w0 <- ws[which.max(n)]
    shiny::updateSelectizeInput(session, "wb_well", choices = ws, selected = w0)
  })
  wb <- shiny::reactive({ shiny::req(input$wb_well); well_workbench(ctx$res(), ctx$opps(), input$wb_well, ctx$asof()) })
  open_st <- c("screening_only", "candidate", "validated_candidate")
  wopps <- shiny::reactive({ s <- ctx$opps()$summary; if (!nrow(s)) s else s[well == input$wb_well & as.character(status) %in% open_st] })
  # default selection: the agreed candidates with the best potential (up to 3) and the worst offender
  shiny::observeEvent(wb(), {
    w <- wb(); o <- wopps(); k <- character()
    if (!is.null(w$cands) && nrow(w$cands)) { c <- w$cands[!is.na(key) & key %in% o$key & conflict == ""][order(-data.table::fcoalesce(qo_base, 0))]; k <- utils::head(c$key, 3) }
    if (!is.null(w$offenders) && nrow(w$offenders)) k <- c(k, utils::head(w$offenders[!is.na(key) & key %in% o$key, key], 1))
    sel(k)
    other <- o[!key %in% c(w$cands$key, w$offenders$key, w$gaps$key)]
    shiny::updateSelectizeInput(session, "wb_other", choices = stats::setNames(other$key, paste(action_label(other$action), data.table::fcoalesce(other$sand, ""))), selected = character())
    lc <- lift_check(ctx$res()$ds, ctx$res(), input$wb_well, ctx$asof(), 0)
    shiny::updateCheckboxInput(session, "wb_als", value = isTRUE(is.finite(lc$runlife_used) && lc$runlife_used >= ctx$settings()$runlife_trigger))
  })
  toggle <- function(k) { if (is.na(k) || !nzchar(k) || !k %in% wopps()$key) return(); s <- sel(); sel(if (k %in% s) setdiff(s, k) else c(s, k)) }
  shiny::observeEvent(plotly::event_data("plotly_click", source = "wbtrack"), {
    k <- plotly::event_data("plotly_click", source = "wbtrack")$customdata
    if (length(k)) toggle(unlist(k)[1])
  })
  keys <- shiny::reactive(unique(c(sel(), input$wb_other)))
  prop <- shiny::reactive({
    k <- keys(); if (!length(k)) return(NULL)
    job_proposal(ctx$opps(), k, ctx$res(), ctx$asof(), ctx$settings(), store_outcomes(ctx$con), isTRUE(input$wb_als))
  })

  output$wb_head <- shiny::renderUI({
    w <- input$wb_well; shiny::req(w)
    h <- well_history(ctx$res(), w, ctx$asof()); l <- utils::tail(h, 3)
    tr <- job_triggers(ctx$res()$ds, ctx$res(), w, ctx$asof(), ctx$settings())
    dr <- well_patterns(ctx$res(), ctx$asof())[well == w & flooded == TRUE]
    htmltools::span(badge(if (nrow(dr)) paste(dr$mechanism[1], "·", paste(dr$pattern, collapse = ", ")) else "primary", "#60a5fa"),
      if (nrow(l)) badge(sprintf("now %s bopd · %s bwpd", fmt_int(mean(l$bopd)), fmt_int(mean(l$bwpd))), "#94a3b8"),
      lapply(tr, function(t) badge(paste("✕", t), pal$crit)),
      shiny::actionLink("wb_w360", "Well 360 →", class = "wf-link"))
  })
  shiny::observeEvent(input$wb_w360, ctx$open_w360(input$wb_well))
  output$wb_track <- plotly::renderPlotly({ w <- wb(); if (!is.finite(w$depth_min)) return(empty_plot("No wellbore data for this well")); plot_wellbore(w, sel()) })

  tbl_sel <- function(x) which(x$key %in% sel())
  output$wb_cands <- DT::renderDT({
    c <- wb()$cands
    if (is.null(c) || !nrow(c)) return(dt_dark(data.table::data.table(Message = "No candidates for this well"), dom = "t"))
    d <- c[, .(ID = target, `Depth ft` = sprintf("%s-%s", fmt_int(top_ft), fmt_int(base_ft)), Unit = data.table::fcoalesce(sand, ""),
               `Found by` = ifelse(n > 0, sprintf("%d of %d · %s", n, n_of, algorithms), algorithms),
               `Oil B / B / A` = ifelse(is.finite(qo_base), sprintf("%s / %s / %s", fmt_int(qo_bajo), fmt_int(qo_base), fmt_int(qo_alto)), "no potential yet"),
               Check = ifelse(nzchar(conflict), paste("✕", conflict), data.table::fcoalesce(status, "")), `In job` = ifelse(key %in% sel(), "✓ ADPERF", ""))]
    DT::datatable(d, rownames = FALSE, class = "compact", selection = list(mode = "multiple", selected = tbl_sel(c)),
                  options = list(dom = "t", pageLength = 50, ordering = FALSE))
  })
  output$wb_off <- DT::renderDT({
    o <- wb()$offenders
    if (is.null(o) || !nrow(o)) return(dt_dark(data.table::data.table(Message = "No interval rates for this well"), dom = "t"))
    d <- o[, .(Interval = target, `Depth ft` = sprintf("%s-%s", fmt_int(top_ft), fmt_int(base_ft)), `Water share` = fmtp(100 * share, 0),
               `qw bwpd` = round(qw), `qo bopd` = round(qo), WC = fmtp(100 * wc, 0), Isolation = ifelse(is.na(key), "below thresholds", "candidate"), `In job` = ifelse(key %in% sel(), "✓ isolate", ""))]
    DT::datatable(d, rownames = FALSE, class = "compact", selection = list(mode = "multiple", selected = tbl_sel(o)), options = list(dom = "t", ordering = FALSE))
  })
  output$wb_gap <- DT::renderDT({
    g <- wb()$gaps
    if (is.null(g) || !nrow(g)) return(dt_dark(data.table::data.table(Message = "No theoretical potential for this well"), dom = "t"))
    d <- g[, .(Interval = target, `Now bopd` = round(qo_now), `Theoretical bopd` = round(qo_theo), Gap = round(gap), Action = ifelse(is.na(key), "below thresholds", "re-perforate / stimulate"), `In job` = ifelse(key %in% sel(), "✓", ""))]
    DT::datatable(d, rownames = FALSE, class = "compact", selection = list(mode = "multiple", selected = tbl_sel(g)), options = list(dom = "t", ordering = FALSE))
  })
  sync <- function(id, part) shiny::observeEvent(input[[paste0(id, "_rows_selected")]], {
    x <- wb()[[part]]; if (is.null(x) || !nrow(x)) return()
    picked <- x$key[input[[paste0(id, "_rows_selected")]]]
    blocked <- if (part == "cands") x[nzchar(conflict) | is.na(key), key] else x[is.na(key), key]
    picked <- setdiff(stats::na.omit(picked), blocked)
    picked <- intersect(picked, wopps()$key)
    rest <- setdiff(sel(), x$key)
    if (!setequal(c(rest, picked), sel())) sel(c(rest, picked))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  sync("wb_cands", "cands"); sync("wb_off", "offenders"); sync("wb_gap", "gaps")

  output$wb_kpis <- shiny::renderUI({
    p <- prop()
    if (is.null(p)) return(htmltools::div(class = "wf-muted", "Select intervals in the track or the tables to compose the job."))
    lc <- p$lift
    htmltools::div(class = "wf-kpi-row compact",
      kpi("Items", nrow(p$items), paste(table(action_job(p$items$action)), names(table(action_job(p$items$action))), collapse = " + ")),
      kpi("Oil, initial", sprintf("%s / %s / %s", fmt_int(p$qo[["Bajo"]]), fmt_int(p$qo[["Base"]]), fmt_int(p$qo[["Alto"]])), "Bajo / Base / Alto, bopd"),
      kpi("Water change", fmt_int(p$qw), "bwpd, Base"),
      kpi("Liquid after job", fmt_int(lc$liquid_after), if (is.finite(lc$capacity)) sprintf("lift capacity %s%s", fmt_int(lc$capacity), if (lc$over) " · ✕ over" else "") else "no lift data",
          tone = if (isTRUE(lc$over)) "warn" else "neutral"),
      kpi("Oil 12 m", fmt_num(p$np12), sprintf("risked %s (P %s)", fmt_num(p$risked_np12), fmtn(p$ps))),
      kpi("Cost", sprintf("%s k", fmt_num(p$cost_usd / 1000)), "USD, standard costs"),
      kpi("Trigger", if (length(p$triggers)) paste(p$triggers, collapse = "; ") else "planned", if (is.finite(lc$runlife_used)) sprintf("lift run life %s", fmtp(100 * lc$runlife_used, 0)) else "",
          tone = if (length(p$triggers)) "warn" else "neutral"))
  })
  bridge_items <- shiny::reactive({ p <- prop(); shiny::req(p); it <- data.table::copy(p$items); it[, label := sprintf("%s %s", action_job(action), data.table::fcoalesce(interval, ""))]; it })
  now_rates <- shiny::reactive({ h <- utils::tail(well_history(ctx$res(), input$wb_well, ctx$asof()), 3); c(qo = mean(h$bopd), qw = mean(h$bwpd)) })
  output$wb_oil <- plotly::renderPlotly(plot_job_bridge(now_rates()[["qo"]], bridge_items(), "qo"))
  output$wb_wat <- plotly::renderPlotly(plot_job_bridge(now_rates()[["qw"]], bridge_items(), "qw"))
  output$wb_fc <- plotly::renderPlotly({ p <- prop(); if (is.null(p) || is.null(p$forecast)) return(empty_plot("No forecast")); plot_forecast(p$forecast) })
  output$wb_cost <- DT::renderDT({ p <- prop(); shiny::req(p); dt_dark(p$cost[, .(Item = item, `Depth ft` = round(depth_ft), `Cost USD` = fmt_num(cost_usd))], dom = "t", ordering = FALSE) })

  output$wb_msg <- shiny::renderUI(NULL)
  shiny::observeEvent(input$wb_propose, {
    k <- keys(); if (!length(k)) { shiny::showNotification("Select at least one item", type = "warning"); return() }
    id <- tryCatch(job_propose(ctx$con, ctx$opps(), k, ctx$res(), ctx$asof(), ctx$settings(), ctx$user(), input$wb_name, input$wb_notes, isTRUE(input$wb_als)),
                   error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(id)) return()
    ctx$store_tick(ctx$store_tick() + 1)
    output$wb_msg <- shiny::renderUI(htmltools::div(class = "wf-muted small", sprintf("Job #%s proposed by %s. It is waiting for a lead's approval in Jobs.", id, ctx$user())))
    shiny::showNotification(sprintf("Job #%s proposed for approval", id))
  })
}
