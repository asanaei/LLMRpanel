# gui.R -------------------------------------------------------------------------
# An optional Shiny GUI for LLMRpanel, launched with run_panel_studio(). Shiny,
# bslib, DT, and the shared LLMR.shiny substrate are Suggests, not Imports: a
# non-GUI user installs none of them, and R CMD check does not require them. Every
# call into those packages is fully qualified and the launcher guards on all four,
# so the analysis package stays lean and the app ships in the same box.

#' Launch the LLMRpanel Shiny GUI
#'
#' Starts a Shiny application that builds a persona panel, administers a choice
#' item or conjoint instrument, and presents the package's diagnostics and
#' design analyses. Choice-item response shares can be compared with an
#' optional benchmark. The application can download the responses and report
#' in a zip file, with the benchmark table when one is available.
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

# Live runs above this request count require explicit confirmation in the GUI.
.panel_gui_large_run_threshold <- 100L

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

.panel_gui_combine_runs <- function(run_results) {
  stopifnot(length(run_results) >= 1L,
            all(vapply(run_results, inherits, logical(1), "panel_responses")))
  if (length(run_results) == 1L) return(run_results[[1]])

  data_parts <- lapply(seq_along(run_results), function(run) {
    tibble::add_column(run_results[[run]]$data, run = run, .before = 1)
  })
  usage_parts <- Filter(Negate(is.null), lapply(seq_along(run_results), function(run) {
    usage <- run_results[[run]]$usage
    if (is.null(usage)) return(NULL)
    tibble::add_column(usage, run = run, .before = 1)
  }))

  out <- run_results[[1]]
  out$data <- tibble::as_tibble(do.call(rbind, data_parts))
  out$usage <- if (length(usage_parts)) {
    tibble::as_tibble(do.call(rbind, usage_parts))
  } else {
    NULL
  }
  out$benchmark <- NULL
  out
}

.panel_gui_run_shares <- function(responses) {
  stopifnot(inherits(responses, "panel_responses"))
  data <- as.data.frame(responses$data)
  if (!"run" %in% names(data)) data$run <- 1L
  data$run <- as.integer(data$run)
  data$response <- as.character(data$response)
  valid <- !(data$success %in% FALSE) & !is.na(data$response) &
    data$type != "open"
  data <- data[valid, , drop = FALSE]

  items <- responses$instrument$items
  rows <- lapply(items, function(item) {
    if (is.null(item$options)) return(NULL)
    runs <- sort(unique(as.integer(
      if ("run" %in% names(responses$data)) responses$data$run else 1L
    )))
    observed <- data$response[data$item_id == item$id]
    levels <- unique(c(as.character(item$options), observed))
    grid <- expand.grid(
      run = runs, item_id = item$id, response = levels,
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
    )
    item_data <- data[data$item_id == item$id, , drop = FALSE]
    if (nrow(item_data)) {
      counts <- stats::aggregate(
        persona_id ~ run + item_id + response,
        data = item_data, FUN = length
      )
      names(counts)[names(counts) == "persona_id"] <- "n"
      totals <- stats::aggregate(
        persona_id ~ run + item_id, data = item_data, FUN = length
      )
      names(totals)[names(totals) == "persona_id"] <- "n_valid"
      grid <- merge(grid, counts,
                    by = c("run", "item_id", "response"), all.x = TRUE)
      grid <- merge(grid, totals, by = c("run", "item_id"), all.x = TRUE)
    } else {
      grid$n <- 0L
      grid$n_valid <- 0L
    }
    grid$n[is.na(grid$n)] <- 0L
    grid$n_valid[is.na(grid$n_valid)] <- 0L
    grid$share <- ifelse(grid$n_valid > 0L, grid$n / grid$n_valid, NA_real_)
    grid
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(tibble::tibble(
      run = integer(), item_id = character(), response = character(),
      n = integer(), n_valid = integer(), share = numeric()
    ))
  }
  out <- tibble::as_tibble(do.call(rbind, rows))
  out[order(out$item_id, out$response, out$run), ]
}

.panel_gui_response_shares <- function(responses) {
  shares <- .panel_gui_run_shares(responses)
  if (!nrow(shares)) {
    return(tibble::tibble(
      item_id = character(), response = character(), n = integer(),
      n_valid = integer(), share = numeric()
    ))
  }
  out <- stats::aggregate(
    cbind(n, n_valid) ~ item_id + response, data = shares, FUN = sum
  )
  out$share <- ifelse(out$n_valid > 0L, out$n / out$n_valid, NA_real_)
  tibble::as_tibble(out[order(out$item_id, out$response), ])
}

.panel_gui_run_summary <- function(responses) {
  shares <- .panel_gui_run_shares(responses)
  if (!nrow(shares) || length(unique(shares$run)) < 2L) {
    return(tibble::tibble(
      item_id = character(), response = character(), mean_share = numeric(),
      sd_share = numeric(), min_share = numeric(), max_share = numeric()
    ))
  }
  groups <- split(shares, paste(shares$item_id, shares$response, sep = "\r"))
  rows <- lapply(groups, function(group) {
    values <- group$share[is.finite(group$share)]
    tibble::tibble(
      item_id = group$item_id[[1]],
      response = group$response[[1]],
      mean_share = if (length(values)) mean(values) else NA_real_,
      sd_share = if (length(values) > 1L) stats::sd(values) else NA_real_,
      min_share = if (length(values)) min(values) else NA_real_,
      max_share = if (length(values)) max(values) else NA_real_
    )
  })
  tibble::as_tibble(do.call(rbind, rows))
}

.panel_gui_timing_summary <- function(responses, wall_seconds = NULL) {
  stopifnot(inherits(responses, "panel_responses"))
  usage <- responses$usage
  if (is.null(usage) || !"duration" %in% names(usage)) return(NULL)
  duration <- suppressWarnings(as.numeric(usage$duration))
  duration <- duration[is.finite(duration) & duration >= 0]
  if (!length(duration)) return(NULL)
  tibble::tibble(
    metric = c(
      "Total wall time", "Recorded calls", "Total call time",
      "Mean per call", "Median per call", "90th percentile per call"
    ),
    seconds = c(
      if (length(wall_seconds) && is.finite(wall_seconds)) wall_seconds else NA_real_,
      NA_real_, sum(duration), mean(duration), stats::median(duration),
      as.numeric(stats::quantile(duration, 0.9, names = FALSE))
    ),
    count = c(NA_integer_, length(duration), rep(NA_integer_, 4L))
  )
}

.panel_gui_encode_profiles <- function(profiles) {
  vapply(profiles, function(profile) {
    if (is.null(profile) || !nrow(profile)) return("")
    rows <- vapply(seq_len(nrow(profile)), function(row) {
      values <- vapply(profile, function(column) {
        encodeString(as.character(column[[row]]), quote = "\"")
      }, character(1))
      paste(sprintf("%s=%s", names(values), values), collapse = "; ")
    }, character(1))
    paste(rows, collapse = " || ")
  }, character(1))
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
  if ("profiles" %in% names(resp_df)) {
    resp_df$profiles <- .panel_gui_encode_profiles(resp_df$profiles)
  }
  if (!is.null(notice)) resp_df$demo_notice <- notice
  utils::write.csv(resp_df, file.path(out_dir, "responses.csv"),
                   row.names = FALSE)

  report_text <- paste(unclass(LLMR::report(responses)), collapse = "\n")
  if (!is.null(notice)) report_text <- paste(notice, report_text, sep = "\n\n")
  writeLines(report_text, file.path(out_dir, "report.txt"))

  files <- c("responses.csv", "report.txt")
  benchmark <- responses$benchmark
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
    fillable = FALSE,
    theme = LLMR.shiny::llmr_theme("panel"),
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
    panel <- shiny::reactiveVal(NULL)
    responses <- shiny::reactiveVal(NULL)
    benchmark_result <- shiny::reactiveVal(NULL)
    power_result <- shiny::reactiveVal(NULL)
    amce_result <- shiny::reactiveVal(NULL)
    run_elapsed <- shiny::reactiveVal(NULL)
    run_is_demo <- shiny::reactiveVal(NULL)
    run_error <- shiny::reactiveVal(NULL)
    analysis_error <- shiny::reactiveVal(NULL)
    persona_source <- LLMR::anes_2024_personas

    output$module_ui <- shiny::renderUI({
      bslib::card(
        bslib::card_header("Silicon survey panel"),
        bslib::card_body(
          shiny::uiOutput(ns("run_error")),
          shiny::tags$strong("1. Where the personas come from"),
          shiny::radioButtons(ns("source"), "Persona source",
            choices = c("Population margins" = "margins",
                        "ANES 2024 personas" = "anes"),
            selected = "margins", inline = TRUE),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'margins'", ns("source")),
            shiny::textAreaInput(
              ns("margins"),
              shiny::tagList(
                "Population margins ",
                LLMR.shiny::help_tip(
                  "Enter one attribute per line as name: level=probability, level=probability."
                )
              ),
              rows = 4,
              value = paste("cohort: young=0.3, middle=0.45, older=0.25",
                            "party: left=0.45, right=0.45, independent=0.10",
                            sep = "\n")
            ),
            shiny::textInput(
              ns("persona_tmpl"),
              shiny::tagList(
                "Persona template ",
                LLMR.shiny::help_tip(
                  "Use braces around a margin name where its sampled level should appear."
                )
              ),
              value = "A {cohort} voter who leans {party}."
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'anes'", ns("source")),
            shiny::tags$p(class = "text-muted",
              "Pick respondents (the list runs from most liberal at the top to most conservative at the bottom). Select none to draw a sample of the panel size."),
            LLMR.shiny::persona_selector_ui(ns("personas")),
            bslib::accordion(
              bslib::accordion_panel(
                "Persona fields and full text",
                shiny::tags$p(
                  class = "text-muted",
                  "All fields are included by default. The first selected row is shown below."
                ),
                shiny::tags$div(
                  style = "max-height: 16rem; overflow-y: auto;",
                  shiny::checkboxGroupInput(
                    ns("persona_columns"),
                    shiny::tagList(
                      "Fields rendered into each persona ",
                      LLMR.shiny::help_tip(
                        "Clear a field to omit it from the persona text sent with each request."
                      )
                    ),
                    choices = names(persona_source),
                    selected = names(persona_source)
                  )
                ),
                shiny::tags$h6("Full selected persona"),
                LLMR.shiny::text_block_output(ns("persona_text"), height = "12rem")
              ),
              open = FALSE
            )
          ),
          shiny::numericInput(
            ns("n"),
            shiny::tagList(
              "Panel size ",
              LLMR.shiny::help_tip(
                "This is the number of personas administered each instrument item."
              )
            ),
            value = 30, min = 2, max = 500, step = 1
          ),
          shiny::actionButton(ns("build_panel"), "Build panel", class = "btn-primary"),
          shiny::uiOutput(ns("panel_status")),
          shiny::tags$hr(),
          shiny::tags$strong("2. Instrument"),
          shiny::radioButtons(
            ns("instrument_type"), "Instrument type",
            choices = c("Choice item" = "choice", "Conjoint tasks" = "conjoint"),
            selected = "choice", inline = TRUE
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'choice'", ns("instrument_type")),
            shiny::textInput(
              ns("item_text"), "Question",
              value = "Should the government increase public spending?"
            ),
            shiny::textInput(
              ns("item_opts"), "Options (comma-separated)",
              value = "yes, no, unsure"
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'conjoint'", ns("instrument_type")),
            shiny::textAreaInput(
              ns("conjoint_attributes"),
              shiny::tagList(
                "Conjoint attributes ",
                LLMR.shiny::help_tip(
                  "Enter one attribute per line as name: level, level."
                )
              ),
              rows = 4,
              value = paste(
                "tax rate: lower, unchanged, higher",
                "public services: fewer, unchanged, more",
                sep = "\n"
              )
            ),
            shiny::numericInput(
              ns("conjoint_tasks"),
              shiny::tagList(
                "Tasks per persona ",
                LLMR.shiny::help_tip(
                  "Each task presents a new randomized set of profiles to each persona."
                )
              ),
              value = 6, min = 2, max = 20, step = 1
            ),
            shiny::numericInput(
              ns("conjoint_profiles"),
              shiny::tagList(
                "Profiles per task ",
                LLMR.shiny::help_tip(
                  "Each persona chooses one of this many profiles in every task."
                )
              ),
              value = 2, min = 2, max = 5, step = 1
            ),
            shiny::textInput(
              ns("conjoint_question"), "Conjoint question",
              value = "Which policy package do you prefer?"
            )
          ),
          shiny::tags$hr(),
          shiny::tags$strong("3. Administer"),
          shiny::numericInput(
            ns("runs"),
            shiny::tagList(
              "Runs ",
              LLMR.shiny::help_tip(
                "Repeat the same persona-item requests to assess response consistency."
              )
            ),
            value = 1, min = 1, max = 20, step = 1
          ),
          if (identical(shared$mode(), "demo")) LLMR.shiny::demo_banner_ui(),
          shiny::uiOutput(ns("administer_plan")),
          shiny::actionButton(ns("administer"), "Administer to panel", class = "btn-primary"),
          shiny::tags$hr(),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'choice'", ns("instrument_type")),
            shiny::tags$strong("4. Benchmark comparison (optional)"),
            shiny::textInput(
              ns("benchmark"),
              shiny::tagList(
                "Benchmark shares ",
                LLMR.shiny::help_tip(
                  "Enter the human response distribution as level=share pairs separated by commas."
                )
              ),
              value = "yes=0.5, no=0.4, unsure=0.1"
            ),
            shiny::textInput(
              ns("benchmark_name"), "Benchmark name",
              value = "user benchmark"
            ),
            shiny::actionButton(
              ns("compare_benchmark"), "Compare with benchmark",
              class = "btn-primary"
            )
          ),
          shiny::tags$hr(),
          shiny::uiOutput(ns("results"))
        )
      )
    })

    output$run_error <- shiny::renderUI(run_error())
    output$analysis_error <- shiny::renderUI(analysis_error())

    parse_margins <- function(txt) {
      lines <- trimws(unlist(strsplit(txt %||% "", "\n", fixed = TRUE)))
      lines <- lines[nzchar(lines)]
      out <- list()
      for (ln in lines) {
        nm <- trimws(sub(":.*$", "", ln))
        kv <- trimws(unlist(strsplit(sub("^[^:]*:", "", ln), ",", fixed = TRUE)))
        vals <- suppressWarnings(as.numeric(sub("^.*=", "", kv)))
        keys <- trimws(sub("=.*$", "", kv))
        ok <- nzchar(nm) & nzchar(keys) & !is.na(vals)
        if (any(ok)) out[[nm]] <- stats::setNames(vals[ok], keys[ok])
      }
      out
    }
    parse_attributes <- function(txt) {
      lines <- trimws(unlist(strsplit(txt %||% "", "\n", fixed = TRUE)))
      lines <- lines[nzchar(lines)]
      out <- list()
      for (ln in lines) {
        nm <- trimws(sub(":.*$", "", ln))
        values <- trimws(unlist(
          strsplit(sub("^[^:]*:", "", ln), ",", fixed = TRUE)
        ))
        values <- unique(values[nzchar(values)])
        if (nzchar(nm) && length(values)) out[[nm]] <- values
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
    administer_warning <- function(message, ui = NULL) {
      if (is.null(ui)) ui <- warn_card(message)
      run_error(ui)
      shiny::showNotification(message, type = "warning", session = session)
      invisible(NULL)
    }
    reset_results <- function() {
      responses(NULL)
      benchmark_result(NULL)
      power_result(NULL)
      amce_result(NULL)
      run_elapsed(NULL)
      run_is_demo(NULL)
      analysis_error(NULL)
    }
    plan_label <- function(n_calls) {
      sprintf(
        "Administer panel (%d expected response rows; retries excluded)",
        n_calls
      )
    }

    # The ANES persona picker (shared module); returns the chosen row indices.
    persona_rows <- LLMR.shiny::persona_selector_server("personas",
      persona_source)

    output$persona_text <- shiny::renderText({
      rows <- persona_rows()
      if (!length(rows)) {
        return("Select a persona row above to inspect its full rendered text.")
      }
      row <- rows[[1]]
      paste0(
        "Source row ", row,
        if (length(rows) > 1L) " (the first selected row)" else "",
        "\n\n",
        .render_persona_text(persona_source, row)
      )
    })

    shiny::observeEvent(input$build_panel, {
      run_error(NULL)
      panel_n <- suppressWarnings(as.integer(input$n %||% 30L))
      if (length(panel_n) != 1L || is.na(panel_n) ||
          panel_n < 2L || panel_n > 500L) {
        administer_warning("Panel size must be an integer from 2 through 500.")
        return()
      }
      res <- if (identical(input$source %||% "margins", "anes")) {
        columns <- input$persona_columns
        if (is.null(columns)) columns <- names(persona_source)
        if (!length(columns)) {
          administer_warning("Select at least one persona field.")
          return()
        }
        LLMR.shiny::safe_llmr_call({
          set.seed(110)
          rows <- persona_rows()
          frame <- if (setequal(columns, names(persona_source))) {
            persona_source
          } else {
            .restrict_persona_frame(persona_source, columns)
          }
          panel_from_personas(
            frame,
            rows = if (length(rows)) rows else NULL,
            n = if (length(rows)) NULL else panel_n
          )
        }, shared$provider())
      } else {
        margins <- parse_margins(input$margins)
        if (!length(margins)) {
          administer_warning("Could not parse any population margins.")
          return()
        }
        template <- trimws(input$persona_tmpl %||% "")
        if (!nzchar(template)) {
          administer_warning("Enter a persona template.")
          return()
        }
        LLMR.shiny::safe_llmr_call({
          set.seed(110)
          panel_from_margins(margins, n = panel_n,
                             persona_template = template)
        }, shared$provider())
      }
      if (!res$ok) { run_error(res$ui); return() }
      panel(res$value)
      reset_results()
    })

    output$panel_status <- shiny::renderUI({
      if (is.null(panel())) return(NULL)
      shiny::tags$p(class = "text-success", paste0("Panel of ", nrow(panel()), " personas built."))
    })

    planned_runs <- shiny::reactive({
      value <- suppressWarnings(as.integer(input$runs %||% 1L))
      if (length(value) != 1L || is.na(value) || value < 1L) 1L else value
    })
    planned_items <- shiny::reactive({
      if (!identical(input$instrument_type %||% "choice", "conjoint")) {
        return(1L)
      }
      value <- suppressWarnings(as.integer(input$conjoint_tasks %||% 6L))
      if (length(value) != 1L || is.na(value) || value < 1L) 6L else value
    })
    planned_calls <- shiny::reactive({
      if (is.null(panel())) return(0L)
      as.integer(nrow(panel()) * planned_items() * planned_runs())
    })

    output$administer_plan <- shiny::renderUI({
      if (is.null(panel())) {
        return(shiny::tags$p(
          class = "text-muted",
          "Build a panel to preview the administration scale."
        ))
      }
      n_calls <- planned_calls()
      runs <- planned_runs()
      if (identical(shared$mode(), "demo")) {
        return(shiny::tags$p(
          class = "text-muted",
          sprintf(
            paste0(
              "%d run(s) produce %d deterministic response rows and make no ",
              "API calls."
            ),
            runs, n_calls
          )
        ))
      }
      shiny::tags$p(
        class = "text-muted",
        sprintf(
          paste0(
            "%d run(s), %d planned API calls, and %d expected response rows; ",
            "retries are excluded. Live runs above %d calls require confirmation."
          ),
          runs, n_calls, n_calls, .panel_gui_large_run_threshold
        )
      )
    })

    shiny::observe({
      if (is.null(panel()) || !identical(shared$mode(), "live")) {
        shared$set_plan(0L)
        return()
      }
      n_calls <- planned_calls()
      shared$set_plan(n_calls, plan_label(n_calls))
    })

    administer_inputs <- function() {
      if (is.null(panel())) {
        administer_warning("Build a panel before administering the instrument.")
        return(NULL)
      }
      runs <- suppressWarnings(as.integer(input$runs %||% 1L))
      if (length(runs) != 1L || is.na(runs) || runs < 1L || runs > 20L) {
        administer_warning("Runs must be an integer from 1 through 20.")
        return(NULL)
      }
      instrument_type <- input$instrument_type %||% "choice"
      if (identical(instrument_type, "conjoint")) {
        attributes <- parse_attributes(input$conjoint_attributes)
        if (length(attributes) < 2L ||
            any(vapply(attributes, length, integer(1)) < 2L)) {
          administer_warning(
            "Enter at least two conjoint attributes with at least two levels each."
          )
          return(NULL)
        }
        tasks <- suppressWarnings(as.integer(input$conjoint_tasks %||% 6L))
        profiles <- suppressWarnings(as.integer(input$conjoint_profiles %||% 2L))
        question <- trimws(input$conjoint_question %||% "")
        if (!nzchar(question)) {
          administer_warning("Enter a conjoint question.")
          return(NULL)
        }
        if (is.na(tasks) || tasks < 2L || tasks > 20L ||
            is.na(profiles) || profiles < 2L || profiles > 5L) {
          administer_warning(
            "Conjoint tasks must be 2 through 20 and profiles must be 2 through 5."
          )
          return(NULL)
        }
        item_text <- NULL
        opts <- NULL
        n_items <- tasks
      } else {
        item_text <- trimws(input$item_text %||% "")
        if (!nzchar(item_text)) {
          administer_warning("Enter a question before administering the instrument.")
          return(NULL)
        }
        opts <- parse_opts(input$item_opts)
        if (length(opts) < 2L) {
          administer_warning(
            "Enter at least two response options before administering the instrument."
          )
          return(NULL)
        }
        attributes <- NULL
        tasks <- NULL
        profiles <- NULL
        question <- NULL
        n_items <- 1L
      }
      mode <- shared$mode()
      if (identical(mode, "live") && !nzchar(trimws(shared$model() %||% ""))) {
        administer_warning("Enter a model before starting a live run.")
        return(NULL)
      }
      if (identical(mode, "live") && !shared$can_run()) {
        administer_warning(
          "Set the provider API key in the environment before starting a live run.",
          LLMR.shiny::live_run_blocker_ui(shared$key())
        )
        return(NULL)
      }
      list(
        panel = panel(),
        instrument_type = instrument_type,
        item_text = item_text,
        opts = opts,
        attributes = attributes,
        tasks = tasks,
        profiles = profiles,
        conjoint_question = question,
        runs = runs,
        mode = mode,
        provider = shared$provider(),
        model = shared$model(),
        temperature = shared$temperature(),
        max_tokens = shared$max_tokens(),
        reasoning_effort = shared$reasoning_effort(),
        n_calls = as.integer(nrow(panel()) * n_items * runs)
      )
    }

    administer_panel <- function(job) {
      run_error(NULL)
      analysis_error(NULL)
      run_call <- function(show_progress = FALSE) {
        set.seed(110)
        instrument <- if (identical(job$instrument_type, "conjoint")) {
          design <- conjoint_design(
            job$attributes,
            n_tasks = job$tasks,
            profiles_per_task = job$profiles
          )
          conjoint_instrument(design, question = job$conjoint_question)
        } else {
          panel_instrument(item_choice("q1", job$item_text, job$opts))
        }
        cfg <- LLMR.shiny::build_llm_config(
          job$provider,
          job$model,
          temperature = job$temperature,
          max_tokens = job$max_tokens,
          reasoning_effort = job$reasoning_effort
        )
        runner <- LLMR.shiny::build_runner(
          job$mode, .panel_gui_demo_responder()
        )
        completed <- 0L
        if (isTRUE(show_progress)) {
          base_runner <- runner
          runner <- function(experiments, ...) {
            total_calls <- nrow(experiments)
            pieces <- vector("list", total_calls)
            for (i in seq_len(total_calls)) {
              pieces[[i]] <- base_runner(
                experiments[i, , drop = FALSE], ...
              )
              completed <<- completed + 1L
              shiny::incProgress(
                1 / job$n_calls,
                detail = sprintf(
                  "%d of %d calls complete", completed, job$n_calls
                )
              )
            }
            do.call(rbind, pieces)
          }
        }
        run_results <- lapply(seq_len(job$runs), function(run) {
          # Identical randomization across runs isolates model response variation.
          set.seed(110)
          panel_administer(
            job$panel, instrument, cfg,
            confirm = TRUE, .runner = runner
          )
        })
        .panel_gui_combine_runs(run_results)
      }

      started <- proc.time()[["elapsed"]]
      if (identical(job$mode, "live")) {
        shared$set_plan(job$n_calls, plan_label(job$n_calls))
        res <- LLMR.shiny::safe_llmr_call(
          shiny::withProgress(
            message = "Administering panel",
            detail = sprintf("0 of %d calls complete", job$n_calls),
            value = 0,
            run_call(show_progress = TRUE)
          ),
          job$provider
        )
      } else {
        res <- LLMR.shiny::safe_llmr_call(
          run_call(show_progress = FALSE),
          job$provider
        )
      }
      elapsed <- proc.time()[["elapsed"]] - started
      if (!res$ok) {
        run_elapsed(NULL)
        run_error(res$ui)
        return()
      }
      responses(res$value)
      benchmark_result(NULL)
      power_result(NULL)
      amce_result(NULL)
      run_elapsed(elapsed)
      run_is_demo(identical(job$mode, "demo"))
      usage <- if (identical(job$mode, "demo")) {
        list(result_rows = nrow(res$value$data))
      } else {
        out <- LLMR.shiny::extract_token_counts(
          res$value$usage, fallback_calls = job$n_calls
        )
        out$calls <- job$n_calls
        out$result_rows <- nrow(res$value$data)
        out
      }
      shared$add_usage(usage)
    }

    shiny::observeEvent(input$administer, {
      run_error(NULL)
      job <- administer_inputs()
      if (is.null(job)) return()
      if (identical(job$mode, "live") &&
          job$n_calls > .panel_gui_large_run_threshold) {
        shiny::showModal(shiny::modalDialog(
          title = "Confirm Large Live Run",
          shiny::tags$p(sprintf(
            paste0(
              "This administration will make %d planned API calls across ",
              "%d run(s) and produce %d expected response rows."
            ),
            job$n_calls, job$runs, job$n_calls
          )),
          shiny::tags$p(
            "Retries are excluded from these counts and may add API requests."
          ),
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("confirm_administer"),
              sprintf("Administer %d Calls", job$n_calls),
              class = "btn-primary"
            )
          )
        ))
        return()
      }
      administer_panel(job)
    })

    shiny::observeEvent(input$confirm_administer, {
      shiny::removeModal()
      run_error(NULL)
      job <- administer_inputs()
      if (is.null(job)) return()
      administer_panel(job)
    })

    shiny::observeEvent(input$compare_benchmark, {
      run_error(NULL)
      if (is.null(responses())) {
        administer_warning("Administer the instrument first.")
        return()
      }
      if (!is.null(responses()$instrument$conjoint)) {
        administer_warning(
          "Benchmark shares are available for the choice-item instrument."
        )
        return()
      }
      bench <- parse_named_shares(input$benchmark)
      if (!length(bench)) {
        administer_warning("Enter benchmark shares as level=share pairs.")
        return()
      }
      benchmark_name <- trimws(input$benchmark_name %||% "")
      if (!nzchar(benchmark_name)) {
        administer_warning("Enter a benchmark name.")
        return()
      }
      bench_df <- data.frame(item_id = "q1", response = names(bench),
                             share = as.numeric(bench), stringsAsFactors = FALSE)
      res <- LLMR.shiny::safe_llmr_call(
        panel_benchmark(responses(), bench_df, benchmark_name = benchmark_name),
        shared$provider())
      if (!res$ok) { run_error(res$ui); return() }
      benchmark_result(res$value)
    })

    active_responses <- shiny::reactive({
      if (!is.null(benchmark_result())) benchmark_result() else responses()
    })

    shiny::observeEvent(input$calculate_power, {
      analysis_error(NULL)
      r <- active_responses()
      if (is.null(r)) return()
      item <- r$instrument$items[[1]]
      effect <- suppressWarnings(as.numeric(input$power_effect))
      alpha <- suppressWarnings(as.numeric(input$power_alpha))
      target <- suppressWarnings(as.numeric(input$power_target))
      focal <- input$power_focal
      res <- LLMR.shiny::safe_llmr_call(
        panel_power(
          r, effect = effect, items = item$id,
          focal = stats::setNames(focal, item$id),
          alpha = alpha, power = target
        ),
        shared$provider()
      )
      if (!res$ok) {
        analysis_error(res$ui)
        power_result(NULL)
        return()
      }
      power_result(res$value)
    })

    shiny::observeEvent(input$calculate_amce, {
      analysis_error(NULL)
      r <- active_responses()
      if (is.null(r)) return()
      res <- LLMR.shiny::safe_llmr_call(
        conjoint_amce(r),
        shared$provider()
      )
      if (!res$ok) {
        analysis_error(res$ui)
        amce_result(NULL)
        return()
      }
      amce_result(res$value)
    })

    output$results <- shiny::renderUI({
      shiny::validate(shiny::need(
        !is.null(responses()),
        "Administer the instrument to view responses and the report."
      ))
      r <- active_responses()
      runs <- if ("run" %in% names(r$data)) {
        length(unique(r$data$run))
      } else {
        1L
      }
      shiny::tagList(
        shiny::uiOutput(ns("result_status")),
        shiny::tags$h5("Response shares"),
        DT::DTOutput(ns("response_shares_tbl")),
        if (runs > 1L) shiny::tagList(
          shiny::tags$h5("Consistency across runs"),
          shiny::tags$p(
            class = "text-muted",
            "The table and plot compare response shares from identical persona-item requests."
          ),
          DT::DTOutput(ns("run_summary_tbl")),
          shiny::uiOutput(ns("variation_ui"))
        ),
        shiny::uiOutput(ns("timing_ui")),
        shiny::tags$h5("Bias audit"),
        shiny::tags$p(
          class = "text-muted",
          "Parse and execution failures are counts. The order-effect p-value tests sensitivity to the first option shown."
        ),
        DT::DTOutput(ns("bias_tbl")),
        shiny::tags$h5("Diagnostics"),
        DT::DTOutput(ns("diagnostics_tbl")),
        shiny::uiOutput(ns("analysis_controls")),
        shiny::uiOutput(ns("analysis_error")),
        shiny::uiOutput(ns("benchmark_results")),
        bslib::accordion(
          bslib::accordion_panel(
            "Technical details",
            shiny::tags$h6("Response-level data"),
            DT::DTOutput(ns("responses_tbl")),
            shiny::tags$h6("Report"),
            LLMR.shiny::text_block_output(ns("report"), height = "20rem")
          ),
          open = FALSE
        ),
        shiny::downloadButton(ns("download_bundle"), "Download artifacts"),
        shiny::tags$p(class = "text-muted",
          "A zip: responses.csv, report.txt, and benchmark.csv once a benchmark has been run.")
      )
    })

    output$result_status <- shiny::renderUI({
      r <- active_responses()
      shiny::req(r)
      data <- r$data
      n_runs <- if ("run" %in% names(data)) length(unique(data$run)) else 1L
      failures <- sum(data$success %in% FALSE)
      parse_failures <- sum(
        is.na(data$response) & data$type != "open" &
          !(data$success %in% FALSE)
      )
      benchmark_state <- if (is.null(r$benchmark)) {
        "not benchmarked"
      } else if (r$benchmark$items_covered < r$benchmark$items_total) {
        "partially benchmarked"
      } else {
        "benchmarked"
      }
      bslib::card(
        class = if (failures || parse_failures) "border-warning" else "border-success",
        bslib::card_body(
          shiny::tags$p(
            class = "mb-0",
            sprintf(
              paste0(
                "%d response row(s) from %d run(s). %d execution failure(s), ",
                "%d parse failure(s); %s."
              ),
              nrow(data), n_runs, failures, parse_failures, benchmark_state
            )
          )
        )
      )
    })

    output$response_shares_tbl <- DT::renderDT({
      shiny::req(active_responses())
      tab <- .panel_gui_response_shares(active_responses())
      tab$share <- round(tab$share, 3)
      DT::datatable(
        tab, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE, pageLength = 10)
      )
    })

    output$run_summary_tbl <- DT::renderDT({
      shiny::req(active_responses())
      tab <- .panel_gui_run_summary(active_responses())
      numeric <- vapply(tab, is.numeric, logical(1))
      tab[numeric] <- lapply(tab[numeric], function(value) round(value, 3))
      DT::datatable(
        tab, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE, pageLength = 10)
      )
    })

    output$variation_ui <- shiny::renderUI({
      shares <- .panel_gui_run_shares(active_responses())
      if (!nrow(shares) || length(unique(shares$run)) < 2L) return(NULL)
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        return(LLMR.shiny::install_guidance_ui(
          "ggplot2", "Response-share variation plot"
        ))
      }
      shiny::plotOutput(ns("variation_plot"), height = "360px")
    })

    output$variation_plot <- shiny::renderPlot({
      shiny::req(requireNamespace("ggplot2", quietly = TRUE))
      shares <- .panel_gui_run_shares(active_responses())
      shiny::req(length(unique(shares$run)) > 1L)
      ggplot2::ggplot(
        shares,
        ggplot2::aes(
          x = run, y = share, colour = response, group = response
        )
      ) +
        ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE) +
        ggplot2::geom_point(size = 2.4, na.rm = TRUE) +
        ggplot2::facet_wrap(ggplot2::vars(item_id)) +
        ggplot2::scale_x_continuous(
          breaks = sort(unique(shares$run))
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 1)) +
        ggplot2::labs(
          x = "run", y = "share of valid responses", colour = "response"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          legend.position = "bottom",
          axis.text = ggplot2::element_text(size = 11),
          strip.text = ggplot2::element_text(size = 11)
        )
    })

    output$timing_ui <- shiny::renderUI({
      timing <- .panel_gui_timing_summary(
        active_responses(), run_elapsed()
      )
      if (is.null(timing)) return(NULL)
      shiny::tagList(
        shiny::tags$h5("Timing"),
        shiny::tableOutput(ns("timing_tbl")),
        if (requireNamespace("ggplot2", quietly = TRUE)) {
          shiny::plotOutput(ns("timing_plot"), height = "300px")
        } else {
          LLMR.shiny::install_guidance_ui("ggplot2", "Call timing plot")
        }
      )
    })

    output$timing_tbl <- shiny::renderTable({
      timing <- .panel_gui_timing_summary(
        active_responses(), run_elapsed()
      )
      shiny::req(timing)
      data.frame(
        Metric = timing$metric,
        Value = ifelse(
          is.na(timing$count),
          ifelse(
            is.na(timing$seconds), "not recorded",
            sprintf("%.3f seconds", timing$seconds)
          ),
          as.character(timing$count)
        ),
        check.names = FALSE
      )
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$timing_plot <- shiny::renderPlot({
      shiny::req(requireNamespace("ggplot2", quietly = TRUE))
      usage <- active_responses()$usage
      duration <- suppressWarnings(as.numeric(usage$duration))
      keep <- is.finite(duration) & duration >= 0
      timing <- data.frame(
        call = seq_len(sum(keep)),
        duration = duration[keep],
        run = if ("run" %in% names(usage)) usage$run[keep] else 1L
      )
      ggplot2::ggplot(
        timing,
        ggplot2::aes(x = call, y = duration, colour = factor(run))
      ) +
        ggplot2::geom_point(size = 2.2) +
        ggplot2::labs(
          x = "recorded call", y = "seconds", colour = "run"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          legend.position = "bottom",
          axis.text = ggplot2::element_text(size = 11)
        )
    })

    output$bias_tbl <- DT::renderDT({
      shiny::req(active_responses())
      tab <- panel_bias_audit(active_responses())
      DT::datatable(
        tab, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE, pageLength = 10)
      )
    })

    output$diagnostics_tbl <- DT::renderDT({
      shiny::req(active_responses())
      tab <- LLMR.shiny::diagnostics_table(active_responses())
      DT::datatable(
        tab, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE, pageLength = 10)
      )
    })

    output$analysis_controls <- shiny::renderUI({
      r <- active_responses()
      shiny::req(r)
      if (!is.null(r$instrument$conjoint)) {
        return(shiny::tagList(
          shiny::tags$h5("Conjoint AMCE"),
          shiny::tags$p(
            class = "text-muted",
            "Estimate average marginal component effects from the recorded profile assignments."
          ),
          shiny::actionButton(
            ns("calculate_amce"), "Estimate conjoint AMCE",
            class = "btn-primary"
          ),
          DT::DTOutput(ns("amce_tbl"))
        ))
      }
      item <- r$instrument$items[[1]]
      shiny::tagList(
        shiny::tags$h5("Power calculation"),
        shiny::numericInput(
          ns("power_effect"),
          shiny::tagList(
            "Minimum detectable difference ",
            LLMR.shiny::help_tip(
              "For a choice item, enter the difference in focal-response proportions between two arms."
            )
          ),
          value = 0.10, min = 0.001, max = 0.99, step = 0.01
        ),
        shiny::selectInput(
          ns("power_focal"),
          shiny::tagList(
            "Focal response ",
            LLMR.shiny::help_tip(
              "Power is calculated for the share choosing this response."
            )
          ),
          choices = item$options, selected = item$options[[1]]
        ),
        shiny::numericInput(
          ns("power_alpha"), "Two-sided alpha",
          value = 0.05, min = 0.001, max = 0.2, step = 0.01
        ),
        shiny::numericInput(
          ns("power_target"), "Target power",
          value = 0.80, min = 0.5, max = 0.99, step = 0.01
        ),
        shiny::actionButton(
          ns("calculate_power"), "Calculate power",
          class = "btn-primary"
        ),
        DT::DTOutput(ns("power_tbl"))
      )
    })

    output$power_tbl <- DT::renderDT({
      shiny::req(power_result())
      DT::datatable(
        power_result(), rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)
      )
    })

    output$amce_tbl <- DT::renderDT({
      shiny::req(amce_result())
      tab <- as.data.frame(amce_result())
      DT::datatable(
        tab, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE, pageLength = 10)
      )
    })

    output$benchmark_results <- shiny::renderUI({
      if (is.null(benchmark_result())) return(NULL)
      shiny::tagList(
        shiny::tags$h5("Benchmark comparison"),
        DT::DTOutput(ns("benchmark_tbl")),
        if (requireNamespace("ggplot2", quietly = TRUE)) {
          shiny::plotOutput(ns("benchmark_plot"), height = "360px")
        } else {
          LLMR.shiny::install_guidance_ui(
            "ggplot2", "Benchmark comparison plot"
          )
        }
      )
    })

    output$benchmark_tbl <- DT::renderDT({
      shiny::req(benchmark_result())
      tab <- as.data.frame(benchmark_result()$benchmark$table)
      DT::datatable(
        tab, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE, pageLength = 10)
      )
    })

    output$benchmark_plot <- shiny::renderPlot({
      shiny::req(
        benchmark_result(),
        requireNamespace("ggplot2", quietly = TRUE)
      )
      plot(benchmark_result())
    })

    output$download_bundle <- shiny::downloadHandler(
      filename = function() paste0("llmrpanel_artifacts_", Sys.Date(), ".zip"),
      content = function(file) {
        .panel_gui_bundle_artifacts(
          active_responses(), file, demo = isTRUE(run_is_demo())
        )
      }
    )

    output$responses_tbl <- DT::renderDT({
      shiny::req(responses())
      DT::datatable(LLMR.shiny::as_display_table(
        tibble::as_tibble(responses())),
                    options = list(scrollX = TRUE, pageLength = 5))
    })

    output$report <- shiny::renderText({
      shiny::req(active_responses())
      LLMR.shiny::report_text(active_responses())
    })
  })
}

# %||% is in rlang (already imported by the package); make it available here.
`%||%` <- rlang::`%||%`
