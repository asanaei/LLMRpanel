# gui.R -------------------------------------------------------------------------
# An optional Shiny GUI for LLMRpanel, launched with run_panel_studio(). Shiny,
# bslib, DT, and the shared LLMR.shiny substrate are Suggests, not Imports: a
# non-GUI user installs none of them, and R CMD check does not require them. Every
# call into those packages is fully qualified and the launcher guards on all four,
# so the analysis package stays lean and the app ships in the same box.

#' Launch the LLMRpanel Shiny GUI
#'
#' Starts a Shiny application that builds a persona panel, administers one
#' choice item, and compares response shares with an optional benchmark. The
#' application can download the responses and report in a zip file. It adds
#' the benchmark table when one is available.
#'
#' The GUI is optional. It needs the suggested packages `shiny`, `bslib`, `DT`,
#' and `LLMR.shiny`; install them first. Keys are read from environment variables
#' only, never pasted into the app; a deterministic demo mode runs offline.
#'
#' @param ... Passed to [shiny::runApp()] (e.g. `port`, `launch.browser`).
#' @return Invisibly, the value of [shiny::runApp()]; called for the side effect
#'   of starting the app.
#' @examples
#' if (interactive() &&
#'     requireNamespace("shiny", quietly = TRUE) &&
#'     requireNamespace("LLMR.shiny", quietly = TRUE)) {
#'   run_panel_studio()
#' }
#' @export
run_panel_studio <- function(...) {
  .panel_gui_require()
  app <- shiny::shinyApp(ui = .panel_gui_ui(), server = .panel_gui_server)
  shiny::runApp(app, ...)
}

# Guard: every Suggests dependency the GUI needs, named in one place.
.panel_gui_require <- function() {
  need <- c("shiny", "bslib", "DT", "LLMR.shiny")
  missing <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("The LLMRpanel GUI needs these packages: ",
         paste(missing, collapse = ", "),
         ". Install them with install.packages() / remotes::install_github(), then retry.",
         call. = FALSE)
  }
  invisible(TRUE)
}

# A demo responder for offline mode: parse the "Options: a | b | ..." line out of
# the rendered question and pick deterministically, so the panel has variation
# without a model.
.panel_gui_demo_responder <- function() {
  function(text) {
    text <- text %||% ""
    m <- regmatches(text, regexpr("Options:\\s*(.*)$", text))
    if (!length(m)) return("neutral")
    opts <- trimws(unlist(strsplit(sub("^Options:\\s*", "", m), "|", fixed = TRUE)))
    opts <- opts[nzchar(opts)]
    if (!length(opts)) return("")
    opts[[(sum(utf8ToInt(text)) %% length(opts)) + 1L]]
  }
}

# Bundle the run's artifacts into a zip for the download handler: responses as
# CSV, LLMR::report() text, and an optional comparison table as a second CSV. All
# writes go through tempfile(); in demo mode the CSV gains a demo_notice column
# and the report is prefixed, so deterministic offline output cannot pass as
# model output once it leaves the app.
.panel_gui_bundle_artifacts <- function(responses, file, demo = FALSE) {
  stopifnot(inherits(responses, "panel_responses"))
  file <- normalizePath(file, mustWork = FALSE)
  out_dir <- tempfile("llmrpanel-artifacts-")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  old <- getwd()
  on.exit({
    setwd(old)
    unlink(out_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  notice <- if (isTRUE(demo)) LLMR.shiny::demo_notice() else NULL

  resp_df <- as.data.frame(tibble::as_tibble(responses))
  if (!is.null(notice)) resp_df$demo_notice <- notice
  utils::write.csv(resp_df, file.path(out_dir, "responses.csv"),
                   row.names = FALSE)

  report_text <- paste(unclass(LLMR::report(responses)), collapse = "\n")
  if (!is.null(notice)) report_text <- paste(notice, report_text, sep = "\n\n")
  writeLines(report_text, file.path(out_dir, "report.txt"))

  files <- c("responses.csv", "report.txt")
  benchmark <- attr(responses, "benchmark")
  if (!is.null(benchmark)) {
    benchmark_df <- as.data.frame(benchmark$table)
    if (!is.null(notice)) benchmark_df$demo_notice <- notice
    utils::write.csv(benchmark_df, file.path(out_dir, "benchmark.csv"),
                     row.names = FALSE)
    files <- c(files, "benchmark.csv")
  }

  setwd(out_dir)
  utils::zip(zipfile = file, files = files)
  invisible(file)
}

.panel_gui_ui <- function() {
  bslib::page_navbar(
    title = "LLMRpanel",
    id = "main_nav",
    fillable = TRUE,
    theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
    sidebar = LLMR.shiny::shell_sidebar(),
    bslib::nav_panel("Silicon panel", .panel_gui_module_ui("panel"))
  )
}

.panel_gui_server <- function(input, output, session) {
  shared <- LLMR.shiny::shell_context(input, output, session)
  .panel_gui_module_server("panel", shared)
}

.panel_gui_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("module_ui"))
}

.panel_gui_module_server <- function(id, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    panel       <- shiny::reactiveVal(NULL)
    responses   <- shiny::reactiveVal(NULL)
    benchmark_result <- shiny::reactiveVal(NULL)
    run_is_demo <- shiny::reactiveVal(NULL)
    run_error   <- shiny::reactiveVal(NULL)

    output$module_ui <- shiny::renderUI({
      bslib::card(
        bslib::card_header("Silicon survey panel"),
        bslib::card_body(
          shiny::uiOutput(ns("run_error")),
          shiny::tags$strong("1. Where the personas come from"),
          shiny::radioButtons(ns("source"), NULL,
            choices = c("Population margins" = "margins",
                        "ANES 2024 personas" = "anes"),
            selected = "margins", inline = TRUE),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'margins'", ns("source")),
            shiny::tags$p(class = "text-muted",
              "One attribute per line as 'name: level=prob, level=prob'. You supply the margins; the report cites what you supply."),
            shiny::textAreaInput(ns("margins"), NULL, rows = 4,
              value = paste("cohort: young=0.3, middle=0.45, older=0.25",
                            "party: left=0.45, right=0.45, independent=0.10", sep = "\n")),
            shiny::textInput(ns("persona_tmpl"), "Persona template",
                             value = "A {cohort} voter who leans {party}.")),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'anes'", ns("source")),
            shiny::tags$p(class = "text-muted",
              "Pick respondents (the list runs from most liberal at the top to most conservative at the bottom). Select none to draw a sample of the panel size."),
            LLMR.shiny::persona_selector_ui(ns("personas"))),
          shiny::numericInput(ns("n"), "Panel size", value = 30, min = 2, max = 500, step = 1),
          shiny::actionButton(ns("build_panel"), "Build panel", class = "btn-primary"),
          shiny::uiOutput(ns("panel_status")),
          shiny::tags$hr(),
          shiny::tags$strong("2. Instrument (one choice item)"),
          shiny::textInput(ns("item_text"), "Question",
                           value = "Should the government increase public spending?"),
          shiny::textInput(ns("item_opts"), "Options (comma-separated)", value = "yes, no, unsure"),
          shiny::tags$hr(),
          shiny::tags$strong("3. Administer"),
          if (identical(shared$mode(), "demo")) LLMR.shiny::demo_banner_ui(),
          shiny::actionButton(ns("administer"), "Administer to panel", class = "btn-primary"),
          shiny::tags$hr(),
          shiny::tags$strong("4. Benchmark comparison (optional)"),
          shiny::tags$p(class = "text-muted",
            "Benchmark as 'level=share, level=share' for the item; leave blank to see the NOT BENCHMARKED result."),
          shiny::textInput(ns("benchmark"), NULL, value = "yes=0.5, no=0.4, unsure=0.1"),
          shiny::actionButton(ns("compare_benchmark"), "Compare with benchmark",
                              class = "btn-primary"),
          shiny::tags$hr(),
          shiny::uiOutput(ns("results"))
        )
      )
    })

    output$run_error <- shiny::renderUI(run_error())

    parse_margins <- function(txt) {
      lines <- trimws(unlist(strsplit(txt %||% "", "\n", fixed = TRUE)))
      lines <- lines[nzchar(lines)]
      out <- list()
      for (ln in lines) {
        nm <- trimws(sub(":.*$", "", ln))
        kv <- trimws(unlist(strsplit(sub("^[^:]*:", "", ln), ",", fixed = TRUE)))
        vals <- suppressWarnings(as.numeric(sub("^.*=", "", kv)))
        keys <- trimws(sub("=.*$", "", kv))
        ok <- nzchar(keys) & !is.na(vals)
        if (any(ok)) out[[nm]] <- stats::setNames(vals[ok], keys[ok])
      }
      out
    }
    parse_named_shares <- function(txt) {
      kv <- trimws(unlist(strsplit(txt %||% "", ",", fixed = TRUE)))
      vals <- suppressWarnings(as.numeric(sub("^.*=", "", kv)))
      keys <- trimws(sub("=.*$", "", kv))
      ok <- nzchar(keys) & !is.na(vals)
      stats::setNames(vals[ok], keys[ok])
    }
    parse_opts <- function(txt) {
      x <- trimws(unlist(strsplit(txt %||% "", ",", fixed = TRUE))); x[nzchar(x)]
    }
    warn_card <- function(msg) bslib::card(class = "border-warning", bslib::card_body(msg))

    # The ANES persona picker (shared module); returns the chosen row indices.
    persona_rows <- LLMR.shiny::persona_selector_server("personas",
      if (requireNamespace("LLMR", quietly = TRUE)) LLMR::anes_2024_personas else NULL)

    shiny::observeEvent(input$build_panel, {
      run_error(NULL)
      res <- if (identical(input$source %||% "margins", "anes")) {
        LLMR.shiny::safe_llmr_call({
          set.seed(110)
          rows <- persona_rows()
          panel_from_personas(LLMR::anes_2024_personas,
                              rows = if (length(rows)) rows else NULL,
                              n = if (length(rows)) NULL else as.integer(input$n %||% 30))
        }, shared$provider())
      } else {
        margins <- parse_margins(input$margins)
        if (!length(margins)) { run_error(warn_card("Could not parse any margins.")); return() }
        LLMR.shiny::safe_llmr_call({
          set.seed(110)
          panel_from_margins(margins, n = as.integer(input$n %||% 30),
                             persona_template = input$persona_tmpl)
        }, shared$provider())
      }
      if (!res$ok) { run_error(res$ui); return() }
      panel(res$value); responses(NULL); benchmark_result(NULL); run_is_demo(NULL)
    })

    output$panel_status <- shiny::renderUI({
      if (is.null(panel())) return(NULL)
      shiny::tags$p(class = "text-success", paste0("Panel of ", nrow(panel()), " personas built."))
    })

    shiny::observeEvent(input$administer, {
      run_error(NULL)
      if (is.null(panel())) { run_error(warn_card("Build a panel first.")); return() }
      opts <- parse_opts(input$item_opts)
      if (length(opts) < 2) { run_error(warn_card("Enter at least two options.")); return() }
      if (identical(shared$mode(), "live") && !shared$can_run()) {
        run_error(LLMR.shiny::live_run_blocker_ui(shared$key())); return()
      }
      res <- LLMR.shiny::safe_llmr_call({
        instrument <- panel_instrument(item_choice("q1", input$item_text, opts))
        cfg <- LLMR.shiny::build_llm_config(shared$provider(), shared$model(), temperature = 0)
        runner <- LLMR.shiny::build_runner(shared$mode(), .panel_gui_demo_responder())
        set.seed(110)
        panel_administer(panel(), instrument, cfg, .runner = runner)
      }, shared$provider())
      if (!res$ok) { run_error(res$ui); return() }
      responses(res$value); benchmark_result(NULL)
      run_is_demo(identical(shared$mode(), "demo"))
      shared$add_usage(list(calls = nrow(panel())))
    })

    shiny::observeEvent(input$compare_benchmark, {
      run_error(NULL)
      if (is.null(responses())) { run_error(warn_card("Administer the instrument first.")); return() }
      bench <- parse_named_shares(input$benchmark)
      if (!length(bench)) { run_error(warn_card("Enter a benchmark as level=share, ...")); return() }
      bench_df <- data.frame(item_id = "q1", response = names(bench),
                             share = as.numeric(bench), stringsAsFactors = FALSE)
      res <- LLMR.shiny::safe_llmr_call(
        panel_benchmark(responses(), bench_df, benchmark_name = "user benchmark"),
        shared$provider())
      if (!res$ok) { run_error(res$ui); return() }
      benchmark_result(res$value)
    })

    output$results <- shiny::renderUI({
      if (is.null(responses())) return(NULL)
      shiny::tagList(
        shiny::tags$h5("Responses"),
        DT::DTOutput(ns("responses_tbl")),
        shiny::tags$h5("Report"),
        shiny::verbatimTextOutput(ns("report")),
        shiny::downloadButton(ns("download_bundle"), "Download artifacts"),
        shiny::tags$p(class = "text-muted",
          "A zip: responses.csv, report.txt, and benchmark.csv once a benchmark has been run.")
      )
    })

    output$download_bundle <- shiny::downloadHandler(
      filename = function() paste0("llmrpanel_artifacts_", Sys.Date(), ".zip"),
      content = function(file) {
        r <- if (!is.null(benchmark_result())) benchmark_result() else responses()
        .panel_gui_bundle_artifacts(r, file, demo = isTRUE(run_is_demo()))
      }
    )

    output$responses_tbl <- DT::renderDT({
      shiny::req(responses())
      DT::datatable(LLMR.shiny::as_display_table(responses()),
                    options = list(scrollX = TRUE, pageLength = 5))
    })

    output$report <- shiny::renderText({
      shiny::req(responses())
      r <- if (!is.null(benchmark_result())) benchmark_result() else responses()
      LLMR.shiny::report_text(LLMR::report(r))
    })
  })
}

# %||% is in rlang (already imported by the package); make it available here.
`%||%` <- rlang::`%||%`
