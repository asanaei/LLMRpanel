# The AI usage capsule (inst/ai/LLMRpanel.md) must never drift from the real
# API: every function it mentions in call position must exist, either as an
# export of this package, an LLMR export, or a base/stats/utils function.

test_that("the AI capsule mentions only real functions", {
  path <- system.file("ai", "LLMRpanel.md", package = "LLMRpanel")
  expect_true(nzchar(path))
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  hits <- regmatches(txt, gregexpr(
    "(?<![$:A-Za-z0-9_.])([A-Za-z_][A-Za-z0-9_.]*)\\(", txt, perl = TRUE))[[1]]
  fns <- unique(sub("\\($", "", hits))
  known <- function(f) {
    f %in% getNamespaceExports("LLMRpanel") ||
      f %in% getNamespaceExports("LLMR") ||
      exists(f, envir = baseenv()) ||
      f %in% getNamespaceExports("stats") ||
      f %in% getNamespaceExports("utils")
  }
  unknown <- fns[!vapply(fns, known, logical(1))]
  expect_identical(unknown, character(0))
})

test_that("the AI capsule keeps the expanded core signatures exact", {
  path <- system.file("ai", "LLMRpanel.md", package = "LLMRpanel")
  txt <- paste(readLines(path, warn = FALSE), collapse = " ")
  txt <- gsub("[[:space:]]+", " ", txt)

  signature <- function(name) {
    fml <- paste(deparse(args(getExportedValue("LLMRpanel", name)),
                         width.cutoff = 500L), collapse = " ")
    fml <- gsub("[[:space:]]+", " ", fml)
    paste0(name, "(", sub("^function \\((.*)\\) NULL$", "\\1", fml), ")")
  }

  expanded <- c(
    "panel_from_personas", "as_persona_frame", "panel_administer",
    "panel_batch_submit", "panel_batch_fetch", "panel_batch_status",
    "panel_usage", "run_panel_studio")
  for (name in expanded) {
    expected <- signature(name)
    expect_true(grepl(expected, txt, fixed = TRUE), info = expected)
  }
})

test_that("the public surface is the intended initial-release surface", {
  expected <- c(
    "as_persona_frame", "conjoint_amce", "conjoint_design",
    "conjoint_instrument", "item_choice", "item_likert", "item_open",
    "panel_administer", "panel_batch_fetch", "panel_batch_status",
    "panel_batch_submit", "panel_benchmark", "panel_bias_audit",
    "panel_from_data", "panel_from_margins", "panel_from_personas",
    "panel_instrument", "panel_power", "panel_usage", "run_panel_studio")

  exports <- getNamespaceExports("LLMRpanel")
  expect_setequal(exports, expected)
  expect_false("panel_report" %in% exports)
  expect_true(exists("panel_report", envir = asNamespace("LLMRpanel"),
                     inherits = FALSE))
})

test_that("administration signatures use instrument and put .runner last", {
  administer <- names(formals(panel_administer))
  expect_true("instrument" %in% administer)
  expect_false("instr" %in% administer)
  expect_identical(tail(administer, 2), c(".runner", "..."))

  submit <- names(formals(panel_batch_submit))
  expect_true("instrument" %in% submit)
  expect_false("instr" %in% submit)
})
