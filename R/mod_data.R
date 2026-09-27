# DATA hub: load tables, check quality, tune thresholds, export --------------

data_ui <- function() {
  st <- default_settings
  num <- function(id, label, value, step) shiny::numericInput(id, label, value, step = step, width = "100%")
  htmltools::tagList(
    htmltools::div(class = "wf-section-intro",
      htmltools::h2("Data & method"),
      htmltools::p("Load the six input tables, check their quality, tune the diagnostic thresholds and export results.")
    ),
    bslib::layout_columns(col_widths = c(4, 8), fill = FALSE,
      bslib::card(
        card_title("Load data"),
        bslib::card_body(
          shiny::fileInput("data_files", "Excel workbook (one sheet per table) or the six CSV files",
                           multiple = TRUE, accept = c(".xlsx", ".csv")),
          htmltools::div(class = "wf-btn-row",
            shiny::actionButton("data_demo", "Reload demo field", class = "btn-outline-info btn-sm"),
            shiny::downloadButton("data_template", "Excel template", class = "btn-outline-info btn-sm"),
            shiny::downloadButton("data_export", "Export results", class = "btn-primary btn-sm")),
          htmltools::p(class = "wf-muted small",
            "Sheet / file names are matched flexibly (production, hierarchy, stooip, fluids, petrophysics, allocation;",
            "Spanish names too). Columns accept common aliases: see the dictionary below.")
        )
      ),
      bslib::card(
        card_title("Table status"),
        bslib::card_body(shiny::uiOutput("data_status"))
      )
    ),
    bslib::layout_columns(col_widths = c(7, 5), fill = FALSE,
      bslib::card(height = 470,
        card_title("Validation", "issues found while loading"),
        bslib::card_body(DT::DTOutput("data_issues"))
      ),
      bslib::card(height = 470,
        card_title("Diagnostic thresholds"),
        bslib::card_body(
          htmltools::div(class = "wf-settings-grid",
            num("set_wor_limit", "Economic WOR limit", st$wor_limit, 1),
            num("set_wor_fit_months", "WOR fit window (months)", st$wor_fit_months, 1),
            num("set_early_hcpvi", "Early → developing HCPVI", st$early_hcpvi, 0.05),
            num("set_mature_hcpvi", "Mature from HCPVI", st$mature_hcpvi, 0.05),
            num("set_mature_wc", "Mature from WC", st$mature_wc, 0.05),
            num("set_late_hcpvi", "Late life from HCPVI", st$late_hcpvi, 0.1),
            num("set_late_wc", "Late life from WC", st$late_wc, 0.01),
            num("set_vrr_low", "Under-injection VRR <", st$vrr_low, 0.05),
            num("set_vrr_high", "Over-injection VRR >", st$vrr_high, 0.05),
            num("set_wc_jump", "WC jump in 12 m >", st$wc_jump, 0.05),
            num("set_ev_low", "Low sweep Ev <", st$ev_low, 0.05),
            num("set_quad_hcpvi", "Quadrant split HCPVI", st$quad_hcpvi, 0.05),
            num("set_quad_ev", "Quadrant split Ev", st$quad_ev, 0.05)
          )
        )
      )
    ),
    bslib::layout_columns(col_widths = c(6, 6), fill = FALSE,
      bslib::card(
        card_title("How the numbers are built"),
        bslib::card_body(class = "wf-method", shiny::HTML(method_html()))
      ),
      bslib::card(height = 520,
        card_title("Data dictionary"),
        bslib::card_body(DT::DTOutput("data_dict"))
      )
    )
  )
}

method_html <- function() {
  paste0(
    "<ol>",
    "<li><b>Monthly volumes</b> per well = rate × calendar days of the month.</li>",
    "<li><b>Areal allocation</b>: pattern volume = Σ<sub>wells</sub> volume × coefficient(pattern, well, month). ",
    "Undated coefficients are constant; dated rows step-change from their month on.</li>",
    "<li><b>Vertical split</b>: each well's pattern share is split between the pattern's sands by kh from petrophysics ",
    "(STOOIP share when a well has no petrophysics).</li>",
    "<li><b>Reservoir barrels</b>: oil × Bo, water × Bw, injection × Bw (per sand), summed after the split.</li>",
    "<li><b>Dimensionless variables</b>: RF = Np / STOOIP; HCPVI = W<sub>i</sub>B<sub>w</sub> / (STOOIP·B<sub>oi</sub>); ",
    "PVI = W<sub>i</sub>B<sub>w</sub> / PV; VRR = W<sub>i</sub>B<sub>w</sub> / (N<sub>p</sub>B<sub>o</sub> + W<sub>p</sub>B<sub>w</sub>); ",
    "WOR, WC, maturity index = Np / movable oil, movable oil = STOOIP·(1 − Swi − Sor)/(1 − Swi).</li>",
    "<li><b>Waterflood RF</b> = oil since first injection minus the primary decline extrapolated from the 24 months before injection.</li>",
    "<li><b>Ideal recovery</b>: Buckley-Leverett / Welge displacement efficiency E<sub>D</sub>(HCPVI) from Corey kr and viscosities, ",
    "per pattern & sand. <b>Apparent sweep</b> E<sub>v</sub> = WF oil / (STOOIP · E<sub>D</sub> · B<sub>oi</sub>/B<sub>o</sub>).</li>",
    "<li><b>EUR</b>: straight line log(WOR) vs Np over the last N months extrapolated to the economic WOR, capped at movable oil.</li>",
    "<li><b>Chan plot</b>: WOR and dWOR/dt vs days on production (log-log); a steep rising WOR' suggests channeling.</li>",
    "</ol>")
}

data_server <- function(input, output, session, ctx) {
  output$data_status <- shiny::renderUI({
    ds <- ctx$ds()
    iss <- ds$issues
    boxes <- lapply(names(wf_schema), function(k) {
      d <- ds[[k]]
      ni <- if (nrow(iss)) iss[table == k & severity == "error", .N] else 0
      nw <- if (nrow(iss)) iss[table == k & severity == "warning", .N] else 0
      tone <- if (is.null(d)) "crit" else if (ni) "crit" else if (nw) "warn" else "ok"
      extra <- if (!is.null(d) && k == "production" && nrow(d)) paste(fmt_month(min(d$date)), "–", fmt_month(max(d$date)))
               else if (!is.null(d) && "pattern" %in% names(d)) paste(data.table::uniqueN(d$pattern), "patterns")
               else if (!is.null(d) && "sand" %in% names(d)) paste(data.table::uniqueN(d$sand), "sands")
               else ""
      htmltools::div(class = paste("wf-table-box", paste0("tone-", tone)),
        htmltools::div(class = "wf-table-name", wf_schema[[k]]$title),
        htmltools::div(class = "wf-table-rows", if (is.null(d)) "missing" else paste(fmt_num(nrow(d)), "rows")),
        htmltools::div(class = "wf-table-extra", extra))
    })
    htmltools::div(class = "wf-table-grid", htmltools::tagList(boxes),
      htmltools::div(class = "wf-muted small", paste("Dataset:", ds$name)))
  })

  output$data_issues <- DT::renderDT({
    iss <- ctx$ds()$issues
    if (!nrow(iss)) iss <- data.table::data.table(table = "all", severity = "info", message = "No issues found")
    sevmap <- c(error = "critical", warning = "warning", info = "info")
    iss <- iss[, .(Severity = sev_badge_html(sevmap[severity]), Table = table, Issue = message)]
    dt_dark(iss, escape = FALSE, pageLength = 6)
  })

  output$data_dict <- DT::renderDT({
    rows <- list()
    for (k in names(wf_schema)) for (c in wf_schema[[k]]$cols)
      rows[[length(rows) + 1]] <- data.table::data.table(Table = k, Column = c$name, Required = if (c$required) "yes" else "",
                                                          Unit = c$unit, Aliases = paste(c$aliases, collapse = ", "), Notes = c$doc)
    dt_dark(data.table::rbindlist(rows), pageLength = 12)
  })

  output$data_template <- shiny::downloadHandler(
    filename = function() "waterflood_input_template.xlsx",
    content = function(file) write_template(file)
  )

  output$data_export <- shiny::downloadHandler(
    filename = function() paste0("waterflood_results_", format(ctx$asof(), "%Y%m"), ".xlsx"),
    content = function(file) {
      res <- ctx$res()
      snap <- data.table::copy(ctx$snap()$snap)
      snap[, `:=`(stage = as.character(stage), quadrant = as.character(quadrant))]
      pm <- aggregate_level(res, "pattern", ctx$sands())
      ps <- res$ps[, .(pattern, sand, date, oil, water, winj, cum_oil, cum_water, cum_winj, np_wf, ideal_np, stooip, hcpv, pv)]
      writexl::write_xlsx(list(
        snapshot = snap, flags = ctx$snap()$flags, pattern_monthly = pm,
        pattern_sand_monthly = ps, field_monthly = aggregate_level(res, "field", ctx$sands()),
        allocation_used = res$alloc[date == max(date)], sand_split = res$split), file)
    }
  )
}

read_settings <- function(input) {
  st <- default_settings
  for (nm in names(st)) {
    v <- input[[paste0("set_", nm)]]
    if (!is.null(v) && is.finite(v)) st[[nm]] <- v
  }
  st
}
