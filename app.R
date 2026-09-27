# FloodPulse v2: waterflood surveillance (LCI dimensionless methodology) -------------------
# Run with: shiny::runApp()  or  Rscript -e 'shiny::runApp(port = 3838)'
# Environment: WF_DATA_DIR (input folder, default data/demo), WF_DB (SQLite store),
#              ANTHROPIC_API_KEY / WF_AI_MODEL (optional AI drafting).

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(data.table); library(plotly); library(DT)
})
if (!exists("run_engine")) for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

data_dir <- Sys.getenv("WF_DATA_DIR", "data/demo")
init_raw <- read_dataset_dir(data_dir)
init_n <- tryCatch(data.table::uniqueN(standardize_table(init_raw$wells, "wells")$data$date), error = function(e) 2L)

wf_theme <- bs_theme(version = 5, bg = pal$bg, fg = pal$text, primary = "#22d3ee", secondary = "#334155",
  success = "#34d399", warning = "#fbbf24", danger = "#f87171", info = "#60a5fa",
  base_font = font_collection("Inter", "Segoe UI", "system-ui", "-apple-system", "sans-serif"),
  "border-radius" = "10px", "card-bg" = pal$panel, "card-border-color" = pal$line)

ui <- page_navbar(
  title = tags$span(class = "wf-brand", tags$span(class = "wf-logo", HTML("&#x1F4A7;")), "FloodPulse", tags$small("v2 · waterflood surveillance")),
  id = "nav", theme = wf_theme, fillable = FALSE, window_title = "FloodPulse v2",
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  sidebar = sidebar(width = 280, open = "desktop", class = "wf-sidebar",
    div(class = "wf-asof", div(class = "wf-side-label", "As of"), uiOutput("asof_label"),
        sliderInput("asof_idx", NULL, min = 1, max = init_n, value = init_n, step = 1, ticks = FALSE, width = "100%",
                    animate = animationOptions(interval = 1100, loop = FALSE))),
    div(class = "wf-side-label", "Area"), selectInput("scope_area", NULL, c("All areas" = ""), width = "100%"),
    div(class = "wf-side-label", "Units"), checkboxGroupInput("sands", NULL, choices = character(), inline = TRUE),
    div(class = "wf-side-label", "Colour points by"),
    selectInput("color_by", NULL, c("Utilization" = "util", "Cluster (ML)" = "cluster", "Stage" = "stage", "Area" = "area",
                                    "OPR" = "opr", "Inj TP 12 m" = "tp12", "IWR 12 m" = "iwr12"), width = "100%"),
    div(class = "wf-side-label", "Focus pattern"),
    div(class = "wf-inline", selectizeInput("focus_sel", NULL, choices = NULL, width = "100%", options = list(placeholder = "pattern")),
        actionButton("focus_go", "360", class = "btn-sm btn-primary", title = "Open Pattern 360")),
    uiOutput("side_signals"),
    div(class = "wf-side-foot", uiOutput("dataset_label"))),
  nav_panel(tags$span(tags$i(class = "wf-n", "1"), "Maturity"), value = "maturity", maturity_ui()),
  nav_panel(tags$span(tags$i(class = "wf-n", "2"), "Process velocity"), value = "velocity", velocity_ui()),
  nav_panel(tags$span(tags$i(class = "wf-n", "3"), "Opportunities"), value = "opportunities", opportunities_ui()),
  nav_spacer(),
  nav_menu("Data & reference", icon = icon("database"), align = "right",
    nav_panel("Data", value = "data", data_ui()),
    nav_panel("Reconciliation", value = "reconcile", reconcile_ui()),
    nav_panel("Prototypes", value = "prototypes", prototypes_ui()),
    nav_panel("Analytics (ML)", value = "analytics", analytics_ui()),
    nav_panel("Settings", value = "settings", settings_ui()),
    nav_panel("Method", value = "method", method_ui()))
)

server <- function(input, output, session) {
  con <- store_open()
  session$onSessionEnded(function() DBI::dbDisconnect(con))
  rv <- reactiveValues(raw = NULL, ds = NULL)
  store_tick <- reactiveVal(0); proto_tick <- reactiveVal(0)

  load_raw <- function(raw, name) {
    ds <- tryCatch(build_dataset(raw, name), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(ds)) return(invisible(FALSE))
    bad <- ds$issues[severity == "error" & table %in% required_tables]
    if (nrow(bad)) { showNotification(paste("Cannot compute:", paste(unique(bad$message), collapse = "; ")), type = "error", duration = 10); rv$ds <- ds; return(invisible(FALSE)) }
    rv$raw <- raw; rv$ds <- ds; invisible(TRUE)
  }
  load_raw(init_raw, basename(normalizePath(data_dir, mustWork = FALSE)))
  observeEvent(input$data_demo, { load_raw(read_dataset_dir("data/demo"), "demo"); showNotification("Demo field loaded") })
  observeEvent(input$data_files, {
    f <- input$data_files; raw <- list()
    for (i in seq_len(nrow(f))) {
      part <- if (grepl("xlsx$", f$name[i], ignore.case = TRUE)) read_dataset_xlsx(f$datapath[i]) else read_dataset_csv(f$datapath[i], f$name[i])
      for (k in names(part)) raw[[k]] <- part[[k]]
    }
    if (!length(raw)) { showNotification("No recognisable tables in the upload", type = "error"); return() }
    merged <- rv$raw; for (k in names(raw)) merged[[k]] <- raw[[k]]
    if (isTRUE(load_raw(merged, paste(f$name, collapse = ", ")))) showNotification(sprintf("Loaded %s", paste(names(raw), collapse = ", ")))
  })

  settings <- debounce(reactive(read_settings(input)), 700)
  engine_st <- reactive({ s <- settings(); list(baseline_method = s$baseline_method, prototype_override = s$prototype_override) })
  res <- reactive({
    ds <- rv$ds; req(ds, ds$wells, ds$alloc, ds$vol); proto_tick()
    st <- utils::modifyList(default_settings, engine_st())
    withProgress(message = "Computing surveillance variables", value = 0.5,
      tryCatch(run_engine(ds, st, extra_protos = store_prototypes(con)), error = function(e) { showNotification(paste("Engine error:", conditionMessage(e)), type = "error", duration = 15); NULL }))
  })
  observeEvent(res(), {
    r <- res(); req(r); n <- length(r$months)
    if (!identical(input$asof_idx, n)) updateSliderInput(session, "asof_idx", min = 1, max = n, value = n)
    s <- sort(unique(r$sand_props$sand)); updateCheckboxGroupInput(session, "sands", choices = s, selected = s, inline = TRUE)
    updateSelectInput(session, "scope_area", choices = c("All areas" = "", sort(unique(r$pat_map$area))))
    updateSelectizeInput(session, "focus_sel", choices = sort(r$props$pattern), server = TRUE)
  })

  asof <- reactive({ r <- res(); req(r); r$months[min(max(input$asof_idx, 1), length(r$months))] })
  area <- reactive(if (is.null(input$scope_area) || input$scope_area == "") NULL else input$scope_area)
  series_all <- reactive({ r <- res(); req(r); aggregate_level(r, "pattern", st = settings()) })
  series <- reactive({ s <- series_all(); a <- area(); if (is.null(a)) s else s[entity %in% res()$pat_map[area == a, pattern]] })
  scope_series <- reactive({ r <- res(); req(r); a <- area()
    if (is.null(a)) aggregate_level(r, "field", st = settings()) else aggregate_level(r, "area", entities = a, st = settings()) })
  snap_base <- reactive(pattern_snapshot(res(), series(), asof(), settings()))
  ml <- reactive({
    k <- if (is.null(input$ml_k) || input$ml_k == 0) NULL else input$ml_k
    f <- input$ml_features %||% c("dwi", "sec_rf", "opr", "wpr", "util", "tp12", "iwr12", "lwor")
    tryCatch(ml_cluster(snap_base(), f, k, input$ml_method %||% "kmeans"), error = function(e) NULL)
  })
  snap <- reactive({
    s <- snap_base(); m <- ml()
    if (!is.null(m)) s <- merge(s, m$assign[, .(entity, cluster, cluster_name)], by = "entity", all.x = TRUE) else s[, `:=`(cluster = NA_integer_, cluster_name = NA_character_)]
    s
  })
  units_snap <- reactive({ r <- res(); req(r$units); u <- r$units$pattern_sand; u[date == max(date[date <= asof()])] })
  opps <- reactive({
    store_tick()
    o <- tryCatch(generate_opportunities(res(), series(), asof(), settings()),
                  error = function(e) { showNotification(paste("Opportunity rules:", conditionMessage(e)), type = "error"); list(summary = data.table(), records = list()) })
    o$summary <- apply_states(o$summary, store_states(con))
    o
  })

  focus <- reactiveVal(NULL); sel_opp <- reactiveVal(NULL); p360_pat <- reactiveVal(NULL)
  open_p360 <- function(p) {
    if (is.null(p) || !length(p) || is.na(p)) return()
    p <- as.character(p); focus(p); p360_pat(p)
    pm <- res()$pat_map[pattern == p]
    showModal(p360_modal(p, pm$area[1] %||% "", pm$field[1] %||% ""))
  }
  open_opp <- function(k) { if (is.null(k) || !length(k)) return(); sel_opp(k); nav_select("nav", "opportunities"); nav_select("opp_tabs", "cand") }
  observeEvent(input$focus_sel, if (nzchar(input$focus_sel)) focus(input$focus_sel), ignoreInit = TRUE)
  observeEvent(focus(), if (!identical(input$focus_sel, focus())) updateSelectizeInput(session, "focus_sel", selected = focus()))
  observeEvent(input$focus_go, open_p360(input$focus_sel))
  observeEvent(event_data("plotly_click", source = "wf"), {
    cd <- event_data("plotly_click", source = "wf")$customdata
    if (is.list(cd)) cd <- unlist(cd)
    if (length(cd) && !is.na(cd[1]) && cd[1] %in% res()$props$pattern) open_p360(cd[1])
  })

  ctx <- list(ds = reactive(rv$ds), res = res, asof = asof, series = series, series_all = series_all, scope_series = scope_series,
              snap = snap, ml = ml, units_snap = units_snap, opps = opps, settings = settings, sands = reactive(input$sands),
              color_by = reactive(input$color_by %||% "util"), focus = focus, sel_opp = sel_opp, open_p360 = open_p360, open_opp = open_opp,
              con = con, store_tick = store_tick, proto_tick = proto_tick)

  output$asof_label <- renderUI(div(class = "wf-asof-value", fmt_month(asof())))
  output$dataset_label <- renderUI({
    r <- res(); ds <- rv$ds; req(r, ds)
    tagList(div(strong(ds$name)), div(sprintf("%d patterns · %d wells · %d units", nrow(r$props), uniqueN(r$well$well), uniqueN(r$sand_props$sand))),
            div(paste(fmt_month(min(r$months)), "–", fmt_month(max(r$months)))),
            div(class = "small", if (ai_available()) "AI drafting on" else "AI drafting off"))
  })
  output$side_signals <- renderUI({
    s <- opps()$summary
    n <- if (nrow(s)) table(factor(as.character(s$status), status_levels)) else setNames(rep(0, length(status_levels)), status_levels)
    div(class = "wf-side-alerts", lapply(c("screening_only", "candidate", "validated_candidate", "executed"), function(k)
      actionLink(paste0("side_", k), label = tagList(span(class = "n", n[[k]]), span(gsub("_", " ", k))), class = "wf-alert-pill", style = sprintf("--c:%s", status_colors[[k]]))))
  })
  for (k in status_levels) local({ kk <- k; observeEvent(input[[paste0("side_", kk)]], { nav_select("nav", "opportunities") }, ignoreInit = TRUE) })

  maturity_server(input, output, session, ctx)
  velocity_server(input, output, session, ctx)
  opportunities_server(input, output, session, ctx)
  p360_server(input, output, session, ctx, p360_pat)
  data_server(input, output, session, ctx)
}

shinyApp(ui, server)
