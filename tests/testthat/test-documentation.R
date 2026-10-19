# These tests read the help sources and the vignette, which exist only in
# the package source tree.
skip_without_sources <- function() {
  skip_on_cran()
  skip_if_not(dir.exists(test_path("..", "..", "man")),
              "Package sources are not available.")
}

test_that("help, the vignette, and the AI capsule contain no affirmative claims about removed features", {
  skip_without_sources()
  man <- test_path("..", "..", "man")
  files <- c(
    list.files(man, pattern = "[.]Rd$", full.names = TRUE),
    test_path("..", "..", "vignettes", "design.Rmd"),
    test_path("..", "..", "inst", "ai", "LLMRpanel.md")
  )
  expect_gt(length(files), 1L)
  expect_true(all(file.exists(files)))
  claims <- paste(c(
    "randomiz[a-z]* item[ -](and option )?order",
    "item (and option )?order (can be|is) randomiz",
    "item orders, option orders",
    "records the item and option order",
    "(calculate|plan|estimate).{0,80}sample[ -]sizes?.{0,80}(pilot|dispersion)",
    "dispersion.{0,80}(study planning|planning human data)"
  ), collapse = "|")
  stale <- vapply(files, function(path) {
    text <- paste(readLines(path, warn = FALSE), collapse = " ")
    grepl(claims, gsub("[[:space:]]+", " ", text), ignore.case = TRUE)
  }, logical(1))
  expect_identical(basename(files[stale]), character(0))
})

test_that("help describes independent requests and probability of scale reversal", {
  skip_without_sources()
  read_rd <- function(name) {
    text <- readLines(test_path("..", "..", "man", paste0(name, ".Rd")))
    gsub("[[:space:]]+", " ", paste(text, collapse = " "))
  }
  package <- read_rd("LLMRpanel-package")
  expect_match(package, "independent request for each persona-item pair", fixed = TRUE)
  expect_match(package, "fixed instrument position", fixed = TRUE)
  for (name in c("panel_administer", "panel_instrument")) {
    text <- read_rd(name)
    expect_match(text, "independent request", fixed = TRUE)
    expect_match(text, "probability one-half", fixed = TRUE)
    expect_match(text, "fixed, 1-based", fixed = TRUE)
    expect_false(grepl("random half", text, fixed = TRUE))
  }
  expect_match(read_rd("panel_instrument"), "is therefore refused", fixed = TRUE)
  conjoint <- read_rd("conjoint_instrument")
  expect_match(conjoint, "Each persona-task pair is a separate request", fixed = TRUE)
  expect_match(conjoint, "does not describe an order of exposure", fixed = TRUE)
  benchmark <- read_rd("panel_benchmark")
  expect_match(benchmark, "raises an error", fixed = TRUE)
  expect_match(benchmark, "differs from 1 by more than 0.01", fixed = TRUE)
  expect_false(grepl("draws a warning", benchmark, fixed = TRUE))
})
