# FloodPulse: waterflood surveillance pilot ----------------------------------
# Run with: shiny::runApp()  (or Rscript -e 'shiny::runApp(port = 3838)')

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(data.table)
  library(plotly)
  library(DT)
})
# Files in R/ are sourced automatically by shiny::runApp(); source them here too
# so the app also works when launched via shinyApp() from another script.
if (!exists("run_engine")) for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

data_dir <- Sys.getenv("WF_DATA_DIR", "data/demo")
init_raw <- read_dataset_dir(data_dir)
init_n <- tryCatch(data.table::uniqueN(parse_month(standardize_table(init_raw$production, "production")$data$date)),
                   error = function(e) 2L)

wf_theme <- bs_theme(
  version = 5, bg = pal$bg, fg = pal$text, primary = "#22d3ee", secondary = "#334155",
  success = "#34d399", warning = "#fbbf24", danger = "#f87171", info = "#60a5fa",
  base_font = font_collection("Inter", "Segoe UI", "system-ui", "-apple-system", "sans-serif"),
  "border-radius" = "10px", "card-bg" = pal$panel, "card-border-color" = pal$line
)

ui <- page_navbar(
  title = tags$span(class = "wf-brand", tags$span(class = "wf-logo", HTML("&#x1F4A7;")), "FloodPulse",
                    tags$small("waterflood surveillance")),
  id = "nav", theme = wf_theme, fillable = FALSE, window_title = "FloodPulse · Waterflood surveillance",
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  sidebar = sidebar(width = 290, open = "desktop", class = "wf-sidebar",
    div(class = "wf-asof",
      div(class = "wf-side-label", "As of"),
      uiOutput("asof_label"),
      sliderInput("asof_idx", NULL, min = 1, max = init_n, value = init_n, step = 1, ticks = FALSE, width = "100%",
                  animate = animationOptions(interval = 900, loop = FALSE))
    ),
    div(class = "wf-side-label", "Aggregation"),
    radioButtons("level", NULL, c("Pattern" = "pattern", "Block" = "block"), inline = TRUE),
    div(class = "wf-side-label", "Scope"),
    selectInput("scope_block", NULL, choices = c("All blocks" = ""), width = "100%"),
    div(class = "wf-side-label", "Sands"),
    checkboxGroupInput("sands", NULL, choices = character(), inline = TRUE),
    div(class = "wf-side-label", "Drill into"),
    div(class = "wf-inline",
      selectizeInput("focus", NULL, choices = NULL, width = "100%", options = list(placeholder = "pattern / block")),
      actionButton("focus_go", icon("arrow-right"), class = "btn-sm btn-primary")),
    uiOutput("side_alerts"),
    div(class = "wf-side-foot", uiOutput("dataset_label"))
  ),
  nav_panel("Maturity", icon = icon("mountain-sun"), maturity_ui()),
  nav_panel("Process", icon = icon("gears"), process_ui()),
  nav_spacer(),
  nav_panel("Data", icon = icon("database"), data_ui())
)

server <- function(input, output, session) {
  rv <- reactiveValues(raw = NULL, ds = NULL)

  load_raw <- function(raw, name) {
    ds <- tryCatch(build_dataset(raw, name), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(ds)) return(invisible(FALSE))
    if (nrow(ds$issues) && any(ds$issues$severity == "error")) {
      bad <- ds$issues[severity == "error"]
      if (any(bad$table %in% c("production", "allocation", "stooip"))) {
        showNotification(paste("Cannot compute:", paste(unique(bad$message), collapse = "; ")), type = "error", duration = 10)
        rv$ds <- ds  # keep for the Data page
        return(invisible(FALSE))
      }
    }
    rv$raw <- raw; rv$ds <- ds
    invisible(TRUE)
  }
  load_raw(init_raw, basename(normalizePath(data_dir, mustWork = FALSE)))

  observeEvent(input$data_demo, {
    load_raw(read_dataset_dir("data/demo"), "demo")
    showNotification("Demo field loaded", type = "message")
  })
  observeEvent(input$data_files, {
    f <- input$data_files
    raw <- list()
    for (i in seq_len(nrow(f))) {
      part <- if (grepl("xlsx$", f$name[i], ignore.case = TRUE)) read_dataset_xlsx(f$datapath[i])
              else read_dataset_csv(f$datapath[i], f$name[i])
      for (k in names(part)) raw[[k]] <- part[[k]]
    }
    if (!length(raw)) { showNotification("No recognisable tables in the upload", type = "error"); return() }
    # tables not in the upload are kept, so a single new allocation table can be swapped in
    merged <- rv$raw
    for (k in names(raw)) merged[[k]] <- raw[[k]]
    ok <- load_raw(merged, paste(f$name, collapse = ", "))
    if (isTRUE(ok)) showNotification(sprintf("Loaded %s", paste(names(raw), collapse = ", ")), type = "message")
  })

  res <- reactive({
    ds <- rv$ds; req(ds, ds$production, ds$allocation, ds$stooip)
    withProgress(message = "Computing pattern volumes", value = 0.5,
      tryCatch(run_engine(ds), error = function(e) { showNotification(paste("Engine error:", conditionMessage(e)), type = "error"); NULL }))
  })

  observeEvent(res(), {
    r <- res(); req(r)
    n <- length(r$months)
    if (!identical(input$asof_idx, n)) updateSliderInput(session, "asof_idx", min = 1, max = n, value = n)
    sands <- sort(unique(r$props$sand))
    updateCheckboxGroupInput(session, "sands", choices = sands, selected = sands, inline = TRUE)
    blocks <- sort(unique(r$pat_map$block))
    updateSelectInput(session, "scope_block", choices = c("All blocks" = "", blocks))
  })

  settings <- reactive(read_settings(input))
  sands <- reactive({ s <- input$sands; r <- res(); req(r); if (!length(s)) unique(r$props$sand) else s })
  asof <- reactive({ r <- res(); req(r); r$months[min(max(input$asof_idx, 1), length(r$months))] })
  level <- reactive(input$level %||% "pattern")
  scope_patterns <- reactive({
    r <- res(); b <- input$scope_block
    if (is.null(b) || b == "") r$pat_map$pattern else r$pat_map[block == b, pattern]
  })
  series <- reactive({
    r <- res(); req(r)
    ents <- if (level() == "pattern") scope_patterns() else if (!is.null(input$scope_block) && input$scope_block != "") input$scope_block else NULL
    aggregate_level(r, level(), sands(), entities = ents)
  })
  scope_series <- reactive({
    r <- res(); req(r)
    b <- input$scope_block
    if (is.null(b) || b == "") aggregate_level(r, "field", sands()) else aggregate_level(r, "block", sands(), entities = b)
  })
  snap <- reactive(build_snapshot(series(), asof(), settings()))

  observe({
    s <- series()
    updateSelectizeInput(session, "focus", choices = sort(unique(s$entity)), server = TRUE)
  })

  drill_entity <- reactiveVal(NULL)
  open_drill <- function(e) {
    if (is.null(e) || !length(e) || is.na(e)) return()
    drill_entity(as.character(e))
    showModal(drill_modal(as.character(e), level()))
  }
  observeEvent(input$focus_go, open_drill(input$focus))
  observeEvent(event_data("plotly_click", source = "wf"), {
    ev <- event_data("plotly_click", source = "wf")
    cd <- ev$customdata
    if (is.list(cd)) cd <- unlist(cd)
    if (length(cd) && !is.na(cd[1]) && cd[1] %in% series()$entity) open_drill(cd[1])
  })

  ctx <- list(ds = reactive(rv$ds), res = res, sands = sands, asof = asof, level = level, series = series,
              scope_series = scope_series, scope_patterns = scope_patterns, snap = snap, settings = settings,
              open_drill = open_drill)

  output$asof_label <- renderUI(div(class = "wf-asof-value", fmt_month(asof())))
  output$dataset_label <- renderUI({
    r <- res(); ds <- rv$ds; req(r, ds)
    tagList(div(strong(ds$name)),
            div(sprintf("%d patterns · %d wells · %d sands", uniqueN(r$props$pattern), uniqueN(r$well$well), uniqueN(r$props$sand))),
            div(paste(fmt_month(min(r$months)), "–", fmt_month(max(r$months)))))
  })
  output$side_alerts <- renderUI({
    f <- snap()$flags
    n <- if (nrow(f)) table(factor(f$severity, names(sev_colors))) else setNames(rep(0, 4), names(sev_colors))
    div(class = "wf-side-alerts",
      lapply(names(n), function(s) div(class = "wf-alert-pill", style = sprintf("--c:%s", sev_colors[[s]]),
                                       span(class = "n", n[[s]]), span(s))))
  })

  maturity_server(input, output, session, ctx)
  process_server(input, output, session, ctx)
  data_server(input, output, session, ctx)
  drill_server(input, output, session, ctx, drill_entity)
}

shinyApp(ui, server)
