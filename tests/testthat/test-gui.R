# The optional GUI's launcher and helpers. The Shiny machinery is a Suggests
# concern; here we test the dependency guard and the offline demo responder,
# skipping when the GUI packages are absent.

test_that("run_panel_studio errors helpfully when GUI packages are missing", {
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
  skip_if_not_installed("LLMR.shiny")
  expect_s3_class(LLMRpanel:::.panel_gui_ui(), "bslib_page")
})

# An offline fixture for the artifact bundle: one item, deterministic runner.
gui_fixture_responses <- function(calibrated = FALSE) {
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
  if (!calibrated) return(r)
  bench <- data.frame(item_id = "q1", response = c("yes", "no"),
                      share = c(.5, .5))
  panel_calibrate(r, bench, "toy benchmark")
}

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
  expect_true(any(grepl("UNCALIBRATED", report)))
})

test_that("a calibrated run adds the calibration table to the bundle", {
  skip_if_no_zip()
  zipfile <- tempfile(fileext = ".zip")
  LLMRpanel:::.panel_gui_bundle_artifacts(gui_fixture_responses(calibrated = TRUE),
                                          zipfile)
  entries <- utils::unzip(zipfile, list = TRUE)$Name
  expect_setequal(entries, c("responses.csv", "report.txt", "calibration.csv"))

  ex_dir <- tempfile("bundle-")
  utils::unzip(zipfile, exdir = ex_dir)
  cal <- utils::read.csv(file.path(ex_dir, "calibration.csv"))
  expect_true(all(c("item_id", "response", "share_silicon", "share_human",
                    "deviation") %in% names(cal)))
  report <- readLines(file.path(ex_dir, "report.txt"))
  expect_true(any(grepl("toy benchmark", report)))
})

test_that("demo mode stamps the bundle so offline output cannot pass as model output", {
  skip_if_no_zip()
  skip_if_not_installed("LLMR.shiny")
  zipfile <- tempfile(fileext = ".zip")
  LLMRpanel:::.panel_gui_bundle_artifacts(
    gui_fixture_responses(calibrated = TRUE), zipfile, demo = TRUE)
  ex_dir <- tempfile("bundle-")
  utils::unzip(zipfile, exdir = ex_dir)
  resp <- utils::read.csv(file.path(ex_dir, "responses.csv"))
  expect_true("demo_notice" %in% names(resp))
  cal <- utils::read.csv(file.path(ex_dir, "calibration.csv"))
  expect_true("demo_notice" %in% names(cal))
  expect_true(all(cal$demo_notice == LLMR.shiny::demo_notice()))
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
    "r <- if (!is.null(calibration())) calibration() else responses()",
    code, fixed = TRUE))
  expect_true(grepl(
    ".panel_gui_bundle_artifacts(r, file, demo = isTRUE(run_is_demo()))",
    code, fixed = TRUE))
})
