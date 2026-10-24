# The optional GUI's launcher and helpers. The Shiny machinery is a Suggests
# concern; here we test the dependency guard and the offline demo responder,
# skipping when the GUI packages are absent.

test_that("run_panel_studio errors helpfully when GUI packages are missing", {
  skip_if_not_installed("LLMR.shiny", "0.1.2")
  # If everything is installed the guard passes; simulate absence by checking the
  # message path only when something is genuinely missing.
  need <- c("shiny", "bslib", "DT", "LLMR.shiny")
  have_all <- all(vapply(need, requireNamespace, logical(1), quietly = TRUE))
  if (have_all) {
    expect_true(isTRUE(LLMRpanel:::.panel_gui_require()))
  } else {
    expect_error(LLMRpanel:::.panel_gui_require(), "GUI needs these packages")
  }
})

test_that("the demo responder picks a valid option from the rendered item", {
  resp <- LLMRpanel:::.panel_gui_demo_responder()
  ans <- resp("Increase spending?\nOptions: yes | no | unsure")
  expect_true(ans %in% c("yes", "no", "unsure"))
  expect_identical(resp("Increase spending?\nOptions: yes | no | unsure"), ans)
})

test_that("the GUI assembles when its suggested packages are present", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("LLMR.shiny", "0.1.2")
  ui <- LLMRpanel:::.panel_gui_ui()
  expect_s3_class(ui, "bslib_page")
  ui_code <- paste(deparse(body(LLMRpanel:::.panel_gui_ui)), collapse = " ")
  expect_match(ui_code, "fillable = FALSE", fixed = TRUE)
  ui_html <- paste(
    as.character(LLMRpanel:::.panel_gui_sidebar()), collapse = " "
  )
  expect_match(
    ui_html, "Leave blank to use the LLMRpanel default of 512.", fixed = TRUE
  )
  expect_match(ui_html, 'placeholder = "LLMRpanel default: 512"', fixed = TRUE)
  expect_false(grepl(
    "Leave blank to use the model default.", ui_html, fixed = TRUE
  ))
})

gui_demo_shared <- function(usage_seen = NULL, max_tokens = NULL) {
  max_tokens_setting <- max_tokens
  list(
    mode = shiny::reactive("demo"),
    provider = shiny::reactive("groq"),
    model = shiny::reactive(""),
    temperature = shiny::reactive(0.7),
    max_tokens = shiny::reactive(max_tokens_setting),
    reasoning_effort = shiny::reactive(""),
    can_run = shiny::reactive(TRUE),
    key = shiny::reactive(list()),
    set_plan = function(calls, label = "Next run") NULL,
    add_usage = function(tokens) {
      if (!is.null(usage_seen)) usage_seen$value <- tokens
    }
  )
}

gui_live_shared <- function(max_tokens = NULL) {
  shared <- gui_demo_shared(max_tokens = max_tokens)
  shared$mode <- shiny::reactive("live")
  shared$model <- shiny::reactive("fake-model")
  shared$can_run <- shiny::reactive(TRUE)
  shared$key <- shiny::reactive(list(found = TRUE))
  shared
}

gui_action_tag <- function(html, input_id) {
  pattern <- paste0('<button[^>]*id="', input_id, '"[^>]*>')
  hit <- regexpr(pattern, html, perl = TRUE)
  if (hit[[1]] < 0L) return("")
  regmatches(html, hit)
}

gui_action_is_disabled <- function(html, input_id) {
  grepl("[[:space:]]disabled(?:[[:space:]]|=|>)",
        gui_action_tag(html, input_id), perl = TRUE)
}

test_that("the module UI renders labeled controls and scrollable text", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny", "0.1.2")
  skip_if_not(
    "help_tip" %in% getNamespaceExports("LLMR.shiny"),
    "installed LLMR.shiny predates the shared display helpers"
  )

  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = gui_demo_shared()),
    {
      session$flushReact()
      html <- paste(as.character(output$module_ui), collapse = "\n")
      expect_match(html, "Persona source", fixed = TRUE)
      expect_match(html, "Instrument type", fixed = TRUE)
      expect_match(html, "1. Build the persona panel", fixed = TRUE)
      expect_match(html, "2. Define the instrument", fixed = TRUE)
      expect_match(html, "3. Set administration", fixed = TRUE)
      expect_match(html, "4. Compare with a benchmark", fixed = TRUE)
      expect_match(html, "Question wording", fixed = TRUE)
      expect_match(
        html, "Should the government increase public spending?", fixed = TRUE
      )
      expect_match(
        html, "A {cohort} voter who leans {party}.", fixed = TRUE
      )
      expect_match(html, "Draw a sample of personas", fixed = TRUE)
      expect_match(html, "Use the respondents I select", fixed = TRUE)
      expect_match(html, "sampling with replacement", fixed = TRUE)
      expect_match(html, "personas-selection_count", fixed = TRUE)
      expect_match(html, "llmr-text-block", fixed = TRUE)
      decoded_html <- gsub("&#39;", "'", html, fixed = TRUE)
      decoded_html <- gsub("&amp;", "&", decoded_html, fixed = TRUE)
      expect_match(
        decoded_html,
        sprintf(
          "(input['%s'] == 'anes' && input['%s'] == 'draw')",
          session$ns("source"), session$ns("anes_mode")
        ),
        fixed = TRUE
      )
      expect_false(grepl("<label[^>]*>[[:space:]]*</label>", html))
    }
  )
})

test_that("blank max output tokens use the visible panel default", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny", "0.1.2")

  build_job <- function(shared, expected, phrase) {
    shiny::testServer(
      LLMRpanel:::.panel_gui_module_server,
      args = list(shared = shared),
      {
        session$setInputs(
          source = "margins",
          margins = "party: left=0.5, right=0.5",
          persona_tmpl = "A {party} voter.",
          n = 2,
          instrument_type = "choice",
          item_text = "Increase spending?",
          item_opts = "yes, no",
          runs = 1,
          build_panel = 1
        )
        session$flushReact()
        expect_identical(administer_inputs()$max_tokens, expected)
        status <- paste(
          as.character(output$max_tokens_status), collapse = "\n"
        )
        expect_match(status, as.character(expected), fixed = TRUE)
        expect_match(status, phrase, fixed = TRUE)
      }
    )
  }

  build_job(
    gui_demo_shared(), LLMRpanel:::.panel_gui_default_max_tokens,
    "sidebar field is blank"
  )
  build_job(gui_demo_shared(max_tokens = 768L), 768L, "from the sidebar")
})

test_that("administration and benchmark actions follow preconditions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny", "0.1.2")

  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = gui_demo_shared()),
    {
      session$flushReact()
      administer_id <- session$ns("administer")
      administer_html <- paste(
        as.character(output$administer_action), collapse = "\n"
      )
      expect_true(gui_action_is_disabled(administer_html, administer_id))
      expect_match(
        administer_html,
        "Build a panel before administering the instrument.",
        fixed = TRUE
      )
      benchmark_id <- session$ns("compare_benchmark")
      benchmark_html <- paste(
        as.character(output$benchmark_action), collapse = "\n"
      )
      expect_true(gui_action_is_disabled(benchmark_html, benchmark_id))
      expect_match(
        benchmark_html,
        "Administer a choice item before comparing it with a benchmark.",
        fixed = TRUE
      )

      session$setInputs(
        source = "margins",
        margins = "party: left=0.5, right=0.5",
        persona_tmpl = "A {party} voter.",
        n = 6,
        instrument_type = "choice",
        item_text = "Increase spending?",
        item_opts = "yes, no",
        runs = 1
      )
      session$flushReact()
      build_id <- session$ns("build_panel")
      build_html <- paste(
        as.character(output$build_panel_action), collapse = "\n"
      )
      expect_false(gui_action_is_disabled(build_html, build_id))

      session$setInputs(margins = "")
      session$flushReact()
      build_html <- paste(
        as.character(output$build_panel_action), collapse = "\n"
      )
      expect_true(gui_action_is_disabled(build_html, build_id))
      expect_match(
        build_html, "Enter at least one population margin.", fixed = TRUE
      )

      session$setInputs(
        margins = "party: left=0.5, right=0.5",
        build_panel = 1
      )
      session$flushReact()
      administer_html <- paste(
        as.character(output$administer_action), collapse = "\n"
      )
      expect_false(gui_action_is_disabled(administer_html, administer_id))

      session$setInputs(item_opts = "yes")
      session$flushReact()
      administer_html <- paste(
        as.character(output$administer_action), collapse = "\n"
      )
      expect_true(gui_action_is_disabled(administer_html, administer_id))
      expect_match(
        administer_html, "Enter at least two response options", fixed = TRUE
      )

      session$setInputs(
        item_opts = "yes, no",
        benchmark = "yes=0.5, no=0.5",
        benchmark_name = "toy benchmark"
      )
      session$flushReact()
      expect_false(gui_action_is_disabled(
        paste(as.character(output$administer_action), collapse = "\n"),
        administer_id
      ))
      session$setInputs(administer = 1)
      session$flushReact()
      expect_s3_class(responses(), "panel_responses")

      benchmark_html <- paste(
        as.character(output$benchmark_action), collapse = "\n"
      )
      expect_false(gui_action_is_disabled(benchmark_html, benchmark_id))
      session$setInputs(benchmark = "")
      session$flushReact()
      benchmark_html <- paste(
        as.character(output$benchmark_action), collapse = "\n"
      )
      expect_true(gui_action_is_disabled(benchmark_html, benchmark_id))
      expect_match(
        benchmark_html, "Enter benchmark shares as level=share pairs.",
        fixed = TRUE
      )
    }
  )
})

test_that("panel tables wrap text, round doubles, and retain hidden identifiers", {
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny", "0.1.2")
  skip_if_not(
    "digits" %in% names(formals(LLMR.shiny::as_display_table)),
    "installed LLMR.shiny predates display rounding"
  )

  tab <- data.frame(
    persona_id = 1:2,
    response_text = c(
      "A response with enough words to exercise wrapping.",
      "Another response with enough words to exercise wrapping."
    ),
    share = c(1 / 3, 2 / 3),
    response_id = c("r1", "r2"),
    stringsAsFactors = FALSE
  )
  widget <- LLMRpanel:::.panel_gui_datatable(
    tab,
    wide_column = "response_text",
    identifier_columns = c("persona_id", "response_id"),
    hide_identifiers = TRUE,
    digits = 3L
  )

  expect_s3_class(widget, "datatables")
  expect_identical(
    names(widget$x$data),
    c("response_text", "share", "persona_id", "response_id")
  )
  expect_equal(widget$x$data$share, c(0.333, 0.667))
  expect_true(isTRUE(widget$x$options$autoWidth))
  expect_identical(widget$x$options$buttons[[1]]$extend, "colvis")
  defs <- widget$x$options$columnDefs
  expect_true(any(vapply(
    defs,
    function(def) identical(def$width, "55%"),
    logical(1)
  )))
  renderers <- Filter(function(def) !is.null(def$render), defs)
  expect_true(any(vapply(
    renderers,
    function(def) grepl("textContent", as.character(def$render),
                        fixed = TRUE),
    logical(1)
  )))
  cell_styles <- Filter(function(def) !is.null(def$createdCell), defs)
  expect_true(any(vapply(
    cell_styles,
    function(def) grepl("whiteSpace", as.character(def$createdCell),
                        fixed = TRUE),
    logical(1)
  )))
})

test_that("demo administration records rows without API calls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny", "0.1.2")
  usage_seen <- new.env(parent = emptyenv())
  usage_seen$value <- NULL
  shared <- gui_demo_shared(usage_seen)

  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = shared),
    {
      session$setInputs(
        source = "margins",
        margins = "party: left=0.5, right=0.5",
        persona_tmpl = "A {party} voter.",
        n = 6,
        item_text = "Increase spending?",
        item_opts = "yes, no",
        build_panel = 1
      )
      session$flushReact()
      expect_true(all(grepl("^A (left|right) voter[.]$", panel()$persona)))
      session$setInputs(administer = 1)
      session$flushReact()
      expect_s3_class(responses(), "panel_responses")
      expect_identical(
        responses()$instrument$items[[1]]$text,
        "Increase spending?"
      )
      expect_true(isTRUE(run_is_demo()))
      expect_false("run" %in% names(responses()$data))
      if ("text_block_output" %in% getNamespaceExports("LLMR.shiny")) {
        results_html <- paste(as.character(output$results), collapse = "\n")
        expect_match(results_html, "Technical details", fixed = TRUE)
        expect_match(results_html, "llmr-text-block", fixed = TRUE)
      }
      session$setInputs(
      )
      session$flushReact()
    }
  )

  expect_identical(usage_seen$value$result_rows, 6L)
  expect_null(usage_seen$value$calls)
  expect_null(usage_seen$value$sent)
  expect_null(usage_seen$value$received)
})

# An offline fixture for the artifact bundle: one item, deterministic runner.
gui_fixture_responses <- function(benchmarked = FALSE) {
  set.seed(110)
  panel <- panel_from_margins(list(party = c(left = .5, right = .5)), n = 6,
                              persona_template = "A voter who leans {party}.")
  instr <- panel_instrument(
    item_choice("q1", "Increase spending?", c("yes", "no")),
    randomize = character(0))
  det <- function(experiments, ...) {
    experiments$response_text <-
      rep(c("yes", "no"), length.out = nrow(experiments))
    experiments
  }
  r <- panel_administer(panel, instr, LLMR::llm_config("groq", "fake-model"),
                        .runner = det)
  if (!benchmarked) return(r)
  bench <- data.frame(item_id = "q1", response = c("yes", "no"),
                      share = c(.5, .5))
  panel_benchmark(r, bench, "toy benchmark")
}

gui_fixture_conjoint <- function() {
  set.seed(110)
  panel <- panel_from_margins(list(group = c(A = .5, B = .5)), n = 4)
  design <- conjoint_design(
    list(price = c("low", "high"), service = c("basic", "expanded")),
    n_tasks = 2
  )
  instrument <- conjoint_instrument(design, "Choose a package.")
  runner <- function(experiments, ...) {
    experiments$response_text <- "Profile 1"
    experiments
  }
  set.seed(110)
  panel_administer(
    panel, instrument, LLMR::llm_config("groq", "fake-model"),
    .runner = runner
  )
}

test_that("the AMCE action is disabled when conjoint responses cannot be used", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny", "0.1.2")

  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = gui_demo_shared()),
    {
      usable <- gui_fixture_conjoint()
      responses(usable)
      session$flushReact()
      amce_id <- session$ns("calculate_amce")
      analysis_html <- paste(
        as.character(output$amce_action), collapse = "\n"
      )
      expect_false(gui_action_is_disabled(analysis_html, amce_id))

      failed <- usable
      failed$data$success <- FALSE
      responses(failed)
      session$flushReact()
      analysis_html <- paste(
        as.character(output$amce_action), collapse = "\n"
      )
      expect_true(gui_action_is_disabled(analysis_html, amce_id))
      expect_match(
        analysis_html,
        "every conjoint administration failed",
        fixed = TRUE
      )
    }
  )
})

test_that("repeated responses retain run identity and summarize shares", {
  one <- gui_fixture_responses()
  expect_identical(
    LLMRpanel:::.panel_gui_combine_runs(list(one)),
    one
  )

  repeated <- LLMRpanel:::.panel_gui_combine_runs(list(one, one, one))
  expect_s3_class(repeated, "panel_responses")
  expect_equal(nrow(repeated$data), 18L)
  expect_setequal(unique(repeated$data$run), 1:3)

  shares <- LLMRpanel:::.panel_gui_run_shares(repeated)
  expect_setequal(unique(shares$run), 1:3)
  expect_equal(shares$share, rep(0.5, nrow(shares)))
  summary <- LLMRpanel:::.panel_gui_run_summary(repeated)
  expect_equal(summary$mean_share, c(0.5, 0.5))
  expect_equal(summary$sd_share, c(0, 0))
})

test_that("demo repeated runs and conjoint instruments use the held panel", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny", "0.1.2")
  usage_seen <- new.env(parent = emptyenv())
  usage_seen$value <- NULL

  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = gui_demo_shared(usage_seen)),
    {
      session$setInputs(
        source = "margins",
        margins = "group: A=0.5, B=0.5",
        persona_tmpl = "A respondent in group {group}.",
        n = 6,
        instrument_type = "conjoint",
        conjoint_attributes = paste(
          "price: low, high",
          "service: basic, expanded",
          sep = "\n"
        ),
        conjoint_tasks = 4,
        conjoint_profiles = 2,
        conjoint_question = "Which package do you prefer?",
        runs = 2,
        build_panel = 1
      )
      session$flushReact()
      plan_html <- paste(
        as.character(output$administer_plan), collapse = "\n"
      )
      expect_match(
        plan_html,
        "2 run(s) produce 48 deterministic response rows",
        fixed = TRUE
      )
      session$setInputs(administer = 1)
      session$flushReact()
      expect_s3_class(responses(), "panel_responses")
      expect_s3_class(responses()$instrument$conjoint, "conjoint_design")
      expect_true(all(vapply(
        responses()$instrument$items,
        function(item) identical(item$text, "Which package do you prefer?"),
        logical(1)
      )))
      expect_equal(nrow(responses()$data), 48L)
      expect_setequal(unique(responses()$data$run), 1:2)
      session$setInputs(calculate_amce = 1)
      session$flushReact()
      expect_s3_class(amce_result(), "conjoint_amce")
    }
  )

  expect_identical(usage_seen$value$result_rows, 48L)
})

test_that("ANES field selection preserves the default and restricts on request", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny", "0.1.2")

  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = gui_demo_shared()),
    {
      session$setInputs(
        source = "anes", anes_mode = "draw", n = 2, seed = 110,
        build_panel = 1
      )
      session$flushReact()
      set.seed(110)
      expected <- panel_from_personas(LLMRpanel::anes_2024_personas, n = 2)
      expect_identical(panel()$persona, expected$persona)

      session$setInputs(
        persona_columns = "demo_age",
        build_panel = 2
      )
      session$flushReact()
      expect_setequal(names(panel()), c("persona_id", "demo_age", "persona"))
      expect_false(any(grepl("Party identification", panel()$persona)))
    }
  )
})

test_that("ANES construction modes ignore the inactive row control", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny", "0.1.2")

  expect_true(LLMRpanel:::.panel_gui_uses_panel_size("margins", "selected"))
  expect_true(LLMRpanel:::.panel_gui_uses_panel_size("anes", "draw"))
  expect_false(LLMRpanel:::.panel_gui_uses_panel_size("anes", "selected"))

  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = gui_demo_shared()),
    {
      session$setInputs(source = "anes", anes_mode = "selected", n = 1)
      session$flushReact()
      build_id <- session$ns("build_panel")
      build_html <- paste(
        as.character(output$build_panel_action), collapse = "\n"
      )
      expect_true(gui_action_is_disabled(build_html, build_id))
      expect_match(
        build_html, "Select at least one respondent.", fixed = TRUE
      )
      expect_identical(
        output[["personas-selection_count"]], "0 of 100 selected."
      )

      selected_rows <- c(2L, 7L, 11L)
      session$setInputs(
        `personas-table_rows_selected` = selected_rows,
        build_panel = 1
      )
      session$flushReact()
      expect_identical(
        output[["personas-selection_count"]], "3 of 100 selected."
      )
      expect_equal(nrow(panel()), 3L)
      expected <- panel_from_personas(
        LLMRpanel::anes_2024_personas, rows = selected_rows
      )
      expect_identical(panel()$persona, expected$persona)

      session$setInputs(anes_mode = "draw", n = 4, seed = 110, build_panel = 2)
      session$flushReact()
      set.seed(110)
      expected <- panel_from_personas(LLMRpanel::anes_2024_personas, n = 4)
      expect_equal(nrow(panel()), 4L)
      expect_identical(panel()$persona, expected$persona)
    }
  )
})

test_that("large ANES draws drive the scale preview and confirmation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny", "0.1.2")

  modal_seen <- new.env(parent = emptyenv())
  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = gui_live_shared()),
    {
      root_session <- .subset2(session, "parent")
      root_session$sendModal <- function(type, message) {
        modal_seen$type <- type
        modal_seen$message <- message
        invisible(NULL)
      }
      suppressWarnings({
        session$setInputs(
          source = "anes",
          anes_mode = "draw",
          n = 250,
          instrument_type = "conjoint",
          conjoint_attributes = paste(
            "price: low, high",
            "service: basic, expanded",
            sep = "\n"
          ),
          conjoint_tasks = 4,
          conjoint_profiles = 2,
          conjoint_question = "Which package do you prefer?",
          runs = 2,
          build_panel = 1
        )
        session$flushReact()
      })
      expect_equal(nrow(panel()), 250L)
      expect_identical(planned_calls(), 2000L)
      plan_html <- paste(
        as.character(output$administer_plan), collapse = "\n"
      )
      expect_match(plan_html, "2000 planned API calls", fixed = TRUE)
      expect_identical(administer_inputs()$n_calls, 2000L)

      session$setInputs(administer = 1)
      session$flushReact()
      expect_identical(modal_seen$type, "show")
      modal_html <- as.character(modal_seen$message$html)
      expect_match(modal_html, "2000 planned API calls", fixed = TRUE)
      expect_match(modal_html, "Administer 2000 Calls", fixed = TRUE)
      expect_null(responses())
    }
  )
})

test_that("recorded call durations reach usage and timing summaries", {
  set.seed(110)
  panel <- panel_from_margins(list(group = c(A = 1)), n = 2)
  instrument <- panel_instrument(
    item_choice("q1", "Choose.", c("yes", "no")),
    randomize = character(0)
  )
  runner <- function(experiments, ...) {
    experiments$response_text <- "yes"
    experiments$success <- TRUE
    experiments$duration <- c(0.2, 0.4)
    experiments
  }
  responses <- panel_administer(
    panel, instrument, LLMR::llm_config("groq", "fake-model"),
    .runner = runner
  )

  expect_equal(responses$usage$duration, c(0.2, 0.4))
  expect_equal(panel_usage(responses)$duration_s, 0.6)
  timing <- LLMRpanel:::.panel_gui_timing_summary(
    responses, wall_seconds = 0.5
  )
  expect_equal(timing$seconds[c(1, 3, 4, 5)], c(0.5, 0.6, 0.3, 0.3))

  repeated <- LLMRpanel:::.panel_gui_combine_runs(
    list(responses, responses)
  )
  expect_equal(repeated$usage$duration, rep(c(0.2, 0.4), 2))
  expect_equal(repeated$usage$run, rep(1:2, each = 2))
  expect_equal(panel_usage(repeated)$duration_s, 1.2)

  without_duration <- gui_fixture_responses()
  expect_null(LLMRpanel:::.panel_gui_timing_summary(without_duration))
})

skip_if_no_zip <- function() {
  cmd <- Sys.getenv("R_ZIPCMD", "zip")
  skip_if(!nzchar(Sys.which(cmd)), "no zip binary available")
}

test_that("the artifact bundle holds the responses CSV and the report", {
  skip_if_no_zip()
  zipfile <- tempfile(fileext = ".zip")
  LLMRpanel:::.panel_gui_bundle_artifacts(gui_fixture_responses(), zipfile)
  expect_true(file.exists(zipfile))
  entries <- utils::unzip(zipfile, list = TRUE)$Name
  expect_setequal(entries, c("responses.csv", "report.txt"))

  ex_dir <- tempfile("bundle-")
  utils::unzip(zipfile, exdir = ex_dir)
  resp <- utils::read.csv(file.path(ex_dir, "responses.csv"))
  expect_equal(nrow(resp), 6L)
  expect_true(all(c("persona_id", "item_id", "response") %in% names(resp)))
  report <- readLines(file.path(ex_dir, "report.txt"))
  expect_true(any(grepl("NOT BENCHMARKED", report)))
})

test_that("a conjoint bundle preserves recorded profile assignments", {
  skip_if_no_zip()
  zipfile <- tempfile(fileext = ".zip")
  LLMRpanel:::.panel_gui_bundle_artifacts(
    gui_fixture_conjoint(), zipfile
  )

  ex_dir <- tempfile("bundle-")
  utils::unzip(zipfile, exdir = ex_dir)
  resp <- utils::read.csv(file.path(ex_dir, "responses.csv"))
  expect_true("profiles" %in% names(resp))
  expect_true(all(nzchar(resp$profiles)))
  expect_true(all(grepl('task="', resp$profiles, fixed = TRUE)))
  expect_true(all(grepl('profile="', resp$profiles, fixed = TRUE)))
  expect_true(all(grepl('price="', resp$profiles, fixed = TRUE)))
  expect_true(all(grepl('service="', resp$profiles, fixed = TRUE)))
})

test_that("a benchmarked run adds the benchmark table to the bundle", {
  skip_if_no_zip()
  zipfile <- tempfile(fileext = ".zip")
  LLMRpanel:::.panel_gui_bundle_artifacts(gui_fixture_responses(benchmarked = TRUE),
                                          zipfile)
  entries <- utils::unzip(zipfile, list = TRUE)$Name
  expect_setequal(entries, c("responses.csv", "report.txt", "benchmark.csv"))

  ex_dir <- tempfile("bundle-")
  utils::unzip(zipfile, exdir = ex_dir)
  bm <- utils::read.csv(file.path(ex_dir, "benchmark.csv"))
  expect_true(all(c("item_id", "response", "share_silicon", "share_human",
                    "deviation") %in% names(bm)))
  report <- readLines(file.path(ex_dir, "report.txt"))
  expect_true(any(grepl("toy benchmark", report)))
})

test_that("demo mode stamps the bundle so offline output cannot pass as model output", {
  skip_if_no_zip()
  skip_if_not_installed("LLMR.shiny", "0.1.2")
  zipfile <- tempfile(fileext = ".zip")
  LLMRpanel:::.panel_gui_bundle_artifacts(
    gui_fixture_responses(benchmarked = TRUE), zipfile, demo = TRUE)
  ex_dir <- tempfile("bundle-")
  utils::unzip(zipfile, exdir = ex_dir)
  resp <- utils::read.csv(file.path(ex_dir, "responses.csv"))
  expect_true("demo_notice" %in% names(resp))
  bm <- utils::read.csv(file.path(ex_dir, "benchmark.csv"))
  expect_true("demo_notice" %in% names(bm))
  expect_true(all(bm$demo_notice == LLMR.shiny::demo_notice()))
  report <- readLines(file.path(ex_dir, "report.txt"))
  expect_identical(report[1], LLMR.shiny::demo_notice())
})

test_that("the results card wires its download control to the artifact bundle", {
  code <- paste(deparse(body(LLMRpanel:::.panel_gui_module_server)),
                collapse = " ")
  code <- gsub("[[:space:]]+", " ", code)
  expect_true(grepl(
    'downloadButton(ns("download_bundle"), "Download artifacts")',
    code, fixed = TRUE))
  expect_true(grepl(
    "output$download_bundle <- shiny::downloadHandler(", code, fixed = TRUE))
  expect_true(grepl(
    '.panel_gui_primary_action(ns("compare_benchmark"), "Compare with benchmark"',
    code, fixed = TRUE))
  expect_true(grepl(
    "panel_bias_audit(active_responses())",
    code, fixed = TRUE))
  expect_true(grepl(
    "conjoint_amce(r)",
    code, fixed = TRUE))
  expect_true(grepl(
    "temperature = job$temperature, max_tokens = job$max_tokens",
    code, fixed = TRUE))
  expect_true(grepl(
    "reasoning_effort = job$reasoning_effort",
    code, fixed = TRUE))
  expect_true(grepl(
    ".panel_gui_bundle_artifacts(active_responses(), file, demo = isTRUE(run_is_demo()))",
    code, fixed = TRUE))
  expect_true(grepl(
    'text_block_output(ns("report"), height = "20rem")',
    code, fixed = TRUE))
  expect_false(grepl("verbatimTextOutput", code, fixed = TRUE))
  expect_false(grepl(
    paste0(
      "(radioButtons|textInput|textAreaInput|numericInput|selectInput|",
      "checkboxGroupInput)\\(ns\\(\"[^\"]+\"\\), NULL"
    ),
    code
  ))
})

test_that("the Studio seeds only when a seed is supplied", {
  expect_true(is.na(LLMRpanel:::.panel_gui_seed(NULL)))
  expect_true(is.na(LLMRpanel:::.panel_gui_seed(NA)))
  expect_true(is.na(LLMRpanel:::.panel_gui_seed("")))
  expect_identical(LLMRpanel:::.panel_gui_seed(110), 110L)
  expect_identical(LLMRpanel:::.panel_gui_seed("110"), 110L)

  code <- paste(deparse(LLMRpanel:::.panel_gui_module_server), collapse = "\n")
  expect_false(grepl("set.seed(110)", code, fixed = TRUE))
  expect_true(grepl("is.na(draw_seed)", code, fixed = TRUE))
  expect_true(grepl("set.seed(draw_seed)", code, fixed = TRUE))
  expect_true(grepl("set.seed(run_seed)", code, fixed = TRUE))
})

test_that("a blank seed draws the panel from the session's own stream", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny", "0.1.2")

  set.seed(4242)
  expected <- panel_from_personas(LLMRpanel::anes_2024_personas, n = 3)

  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = gui_demo_shared()),
    {
      set.seed(4242)
      session$setInputs(
        source = "anes", anes_mode = "draw", n = 3, build_panel = 1
      )
      session$flushReact()
      expect_identical(panel()$persona, expected$persona)
    }
  )
})

test_that("invalid margins disable construction and forced events preserve the panel", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny", "0.1.2")

  cases <- list(
    c(" \n ", "Enter at least one population margin."),
    c("age young=0.3, old=0.7",
      "Population margin line 2 ('age young=0.3, old=0.7') needs ':' after the margin name."),
    c(": young=0.3, old=0.7",
      "Population margin line 2 (': young=0.3, old=0.7') has an empty margin name."),
    c("age: young=1\n age : old=1",
      "Population margin line 3 repeats the name 'age'."),
    c("age:", "Margin 'age' has an empty token at position 1."),
    c("age: , young=1", "Margin 'age' has an empty token at position 1."),
    c("age: young=1, , old=1", "Margin 'age' has an empty token at position 2."),
    c("age: young=1, old=1, ", "Margin 'age' has an empty token at position 3."),
    c("age: young=0.3, old",
      paste("Margin 'age' has token 'old' without '='.",
            "Use level=share pairs and a decimal point for fractional shares.")),
    c("age: young=0,3, old=0.7",
      paste("Margin 'age' has token '3' without '='.",
            "Use level=share pairs and a decimal point for fractional shares.")),
    c("age: =0.3, old=0.7", "Margin 'age' has an empty level in token '=0.3'."),
    c("age: young=0.3, young =0.7",
      "Margin 'age' repeats level 'young' in token 'young =0.7'."),
    c("age: young=0, old=0", "Margin 'age' must have a positive, finite total weight."),
    c("age: young=1e308, old=1e308",
      "Margin 'age' must have a positive, finite total weight.")
  )
  tokens <- c(
    "young=bad=0.3", "young=0.3=0.7", "35-64=0.45x", "young=30%",
    "young=", "young=1 2", "young=NA", "young=NaN", "young=Inf",
    "young=-Inf", "young=1e309", "young=-0.3"
  )
  cases <- c(cases, lapply(tokens, function(token) {
    c(paste0("age: ", token, ", old=0.7"), sprintf(
      paste("Margin 'age' has invalid share token '%s'.",
            "Enter one finite, nonnegative number after '='."), token
    ))
  }))

  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = gui_demo_shared()),
    {
      session$setInputs(
        source = "margins", margins = "age: young=3, old=7",
        persona_tmpl = "A {age} voter.", n = 6, seed = 110,
        instrument_type = "choice", item_text = "Increase spending?",
        item_opts = "yes, no", runs = 1, build_panel = 1,
        benchmark = "yes=0.5, no=0.5", benchmark_name = "toy benchmark"
      )
      session$setInputs(administer = 1)
      session$setInputs(compare_benchmark = 1)
      held_panel <- panel()
      held_responses <- responses()
      held_result <- benchmark_result()
      expect_s3_class(held_panel, "silicon_panel")
      expect_s3_class(held_result, "panel_responses")
      expect_identical(parse_margins(NULL), list(
        value = NULL, error = "Enter at least one population margin."
      ))

      for (i in seq_along(cases)) {
        text <- cases[[i]][[1]]
        message <- cases[[i]][[2]]
        if (nzchar(trimws(text))) text <- paste("group: A=1", text, sep = "\n")
        expect_identical(parse_margins(text), list(value = NULL, error = message))
        session$setInputs(margins = text)
        expect_identical(build_panel_action_state(), list(enabled = FALSE, reason = message))
        html <- paste(as.character(output$build_panel_action), collapse = "\n")
        expect_true(gui_action_is_disabled(html, session$ns("build_panel")), info = text)
        expect_match(gsub("&#39;", "'", html, fixed = TRUE), message, fixed = TRUE)

        session$setInputs(build_panel = i + 1L)
        warning <- paste(as.character(run_error()), collapse = "\n")
        expect_match(gsub("&#39;", "'", warning, fixed = TRUE), message, fixed = TRUE)
        expect_identical(panel(), held_panel)
        expect_identical(responses(), held_responses)
        expect_identical(benchmark_result(), held_result)
      }
    }
  )
})

test_that("unnormalized Studio margins preserve seeded draws and supplied weights", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny", "0.1.2")

  margins <- list(
    age = c(young = 30, middle = 45, older = 25, unused = 0),
    party = c(left = 4.5, right = 4.5, independent = 1)
  )
  template <- "A {age} voter who leans {party}."
  set.seed(110)
  expected <- panel_from_margins(margins, n = 12, persona_template = template)
  text <- paste(
    "", " age : young=3e1, middle =45, older=+25, unused=0 ", "",
    " party: left=4.5, right=4.5, independent=1 ", "", sep = "\n"
  )
  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = gui_demo_shared()),
    {
      expect_identical(parse_margins(text), list(value = margins, error = NULL))
      session$setInputs(
        source = "margins", margins = text, persona_tmpl = template,
        n = 12, seed = 110
      )
      expect_true(build_panel_action_state()$enabled)
      session$setInputs(build_panel = 1)
      expect_identical(panel(), expected)
      expect_identical(attr(panel(), "margins"), margins)
      expect_null(run_error())
    }
  )
})

test_that("invalid benchmark tokens disable comparison and forced events preserve results", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny", "0.1.2")

  cases <- list(
    c("", "Enter benchmark shares as level=share pairs."),
    c(", yes=1", "Benchmark shares have an empty token at position 1."),
    c("yes=0.5, , no=0.5", "Benchmark shares have an empty token at position 2."),
    c("yes=0.5, no=0.5, ", "Benchmark shares have an empty token at position 3."),
    c("yes=0.5, no", paste("Benchmark token 'no' needs '='.",
      "Use level=share pairs and a decimal point for fractional shares.")),
    c("yes=0,5, no=0.5", paste("Benchmark token '5' needs '='.",
      "Use level=share pairs and a decimal point for fractional shares.")),
    c("yes=0.5, =0.5", "Benchmark token '=0.5' has an empty level."),
    c("yes=0.5, yes =0.5", "Benchmark level 'yes' is repeated in token 'yes =0.5'.")
  )
  tokens <- c(
    "no=0.005x", "no=bad=0.005", "no=0.005=0.005", "no=30%", "no=",
    "no=1 2", "no=NA", "no=NaN", "no=Inf", "no=-Inf", "no=1e309", "no=-0.005"
  )
  cases <- c(cases, lapply(tokens, function(token) {
    c(paste0("yes=0.995, ", token), sprintf(
      paste("Benchmark share token '%s' is invalid.",
            "Enter one finite, nonnegative number after '='."), token
    ))
  }))

  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = gui_demo_shared()),
    {
      session$setInputs(
        source = "margins", margins = "group: A=1", persona_tmpl = "A {group} voter.",
        n = 6, seed = 110, instrument_type = "choice", item_text = "Increase spending?",
        item_opts = "yes, no", runs = 1, build_panel = 1,
        benchmark = "yes=0.5, no=0.5", benchmark_name = "toy benchmark"
      )
      session$setInputs(administer = 1)
      session$setInputs(compare_benchmark = 1)
      held_responses <- responses()
      held_result <- benchmark_result()
      expect_s3_class(held_result, "panel_responses")
      expect_identical(parse_named_shares(NULL), list(
        value = NULL, error = "Enter benchmark shares as level=share pairs."
      ))

      for (i in seq_along(cases)) {
        text <- cases[[i]][[1]]
        message <- cases[[i]][[2]]
        expect_identical(parse_named_shares(text), list(value = NULL, error = message))
        session$setInputs(benchmark = text)
        expect_identical(benchmark_action_state(), list(enabled = FALSE, reason = message))
        html <- paste(as.character(output$benchmark_action), collapse = "\n")
        expect_true(gui_action_is_disabled(html, session$ns("compare_benchmark")), info = text)
        expect_match(gsub("&#39;", "'", html, fixed = TRUE), message, fixed = TRUE)

        session$setInputs(compare_benchmark = i + 1L)
        warning <- paste(as.character(run_error()), collapse = "\n")
        expect_match(gsub("&#39;", "'", warning, fixed = TRUE), message, fixed = TRUE)
        expect_identical(responses(), held_responses)
        expect_identical(benchmark_result(), held_result)
      }

      session$setInputs(benchmark = " yes = 9.95e-1, no = .005 ")
      expect_true(benchmark_action_state()$enabled)
      expect_identical(parse_named_shares(input$benchmark), list(
        value = c(yes = .995, no = .005), error = NULL
      ))
      session$setInputs(compare_benchmark = length(cases) + 2L)
      expect_null(run_error())
      table <- benchmark_result()$benchmark$table
      expect_equal(table$share_human[match(c("yes", "no"), table$response)], c(.995, .005))
      expect_identical(responses(), held_responses)
    }
  )
})

test_that("benchmark probability, total, and offered-label checks remain in the API", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny", "0.1.2")

  cases <- list(
    c("yes=1.2, no=0", "probabilities in [0, 1]"),
    c("yes=0.8, no=0.3", "must sum to 1"),
    c("yes=0, no=0", "must sum to 1"),
    c("yes=1e308, no=1e308", "probabilities in [0, 1]"),
    c("yes=0.5, typo=0.5", "not among its offered options")
  )
  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = gui_demo_shared()),
    {
      responses(gui_fixture_responses())
      benchmark_result(gui_fixture_responses(benchmarked = TRUE))
      held_result <- benchmark_result()
      session$setInputs(benchmark_name = "toy benchmark")
      for (i in seq_along(cases)) {
        text <- cases[[i]][[1]]
        parsed <- parse_named_shares(text)
        expect_null(parsed$error)
        session$setInputs(benchmark = text)
        expect_true(benchmark_action_state()$enabled)
        benchmark <- data.frame(
          item_id = "q1", response = names(parsed$value), share = unname(parsed$value)
        )
        expect_error(panel_benchmark(responses(), benchmark), cases[[i]][[2]], fixed = TRUE)
        session$setInputs(compare_benchmark = i)
        expect_match(paste(as.character(run_error()), collapse = "\n"), cases[[i]][[2]], fixed = TRUE)
        expect_identical(benchmark_result(), held_result)
      }
      expect_identical(parse_named_shares("yes=0.995"), list(
        value = c(yes = .995), error = NULL
      ))
      session$setInputs(benchmark = "yes=0.995", compare_benchmark = length(cases) + 1L)
      expect_null(run_error())
      table <- benchmark_result()$benchmark$table
      expect_equal(table$share_human[match("yes", table$response)], .995)
    }
  )
})

test_that("the Studio uses the package's own personas and previews the text it sends", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny", "0.1.2")

  shiny::testServer(
    LLMRpanel:::.panel_gui_module_server,
    args = list(shared = gui_demo_shared()),
    {
      expect_identical(persona_source, LLMRpanel::anes_2024_personas)
      expect_true("demo_rural_urban_identity" %in% persona_fields)
      expect_false("ideology_score" %in% persona_fields)
      session$flushReact()
      html <- paste(as.character(output$module_ui), collapse = "\n")
      expect_match(html, "Selected persona as sent", fixed = TRUE)
      expect_match(html, 'value="demo_rural_urban_identity"', fixed = TRUE)
      expect_false(grepl('value="ideology_score"', html, fixed = TRUE))
      expect_false(grepl('value="demo_community_type"', html, fixed = TRUE))

      demographics <- LLMR::llm_persona_demographic_fields(persona_source)
      session$setInputs(
        source = "anes", anes_mode = "selected",
        persona_columns = demographics,
        `personas-table_rows_selected` = 4L
      )
      session$flushReact()
      preview <- output$persona_text
      expect_match(preview, "^Source row 4\n\n")
      expect_false(grepl("On a questionnaire", preview, fixed = TRUE))
      suppressMessages(session$setInputs(build_panel = 1))
      session$flushReact()
      expect_identical(sub("^Source row 4\n\n", "", preview), panel()$persona)

      # one demographic and one answer: the preview names exactly those two
      session$setInputs(
        persona_columns = c("demo_sex", "att_id_party_identification")
      )
      session$flushReact()
      preview <- sub("^Source row 4\n\n", "", output$persona_text)
      suppressMessages(session$setInputs(build_panel = 2))
      session$flushReact()
      expect_identical(preview, panel()$persona)
      expect_match(preview, "^You are this person: sex ")
      expect_match(preview, "On a questionnaire you gave these answers: Party identification -- ")
      expect_false(grepl("age ", preview, fixed = TRUE))
      expect_false(grepl(";", preview, fixed = TRUE))

      session$setInputs(persona_columns = character(0))
      session$flushReact()
      expect_identical(output$persona_text, "Select at least one persona field.")
    }
  )
})
