# panel_from_personas() draws the respondents that `rows` selects, including a
# single selected row, and accepts only whole-number selectors and sizes.

drawn_rows <- function(...) {
  testthat::local_mocked_bindings(
    .render_persona_text = function(data, r) paste0("row ", r),
    .package = "LLMRpanel")
  p <- panel_from_personas(LLMR::anes_2024_personas, ...)
  as.integer(sub("row ", "", p$persona))
}

test_that("a single selected row is the row that enters the panel", {
  skip_if_not_installed("LLMR")
  set.seed(110)
  expect_identical(drawn_rows(rows = 5, n = 1), 5L)
  set.seed(110)
  expect_identical(suppressWarnings(drawn_rows(rows = 7, n = 3)), c(7L, 7L, 7L))
  set.seed(110)
  expect_identical(drawn_rows(rows = 9, n = 1, weights = 2), 9L)
})

test_that("draws from several rows match sample() exactly", {
  skip_if_not_installed("LLMR")
  pick <- c(2L, 4L, 6L, 8L, 10L)
  set.seed(110)
  got <- drawn_rows(rows = pick, n = 3)
  set.seed(110)
  expect_identical(got, sample(pick, 3))
  w <- c(5, 1, 1, 1, 2)
  set.seed(110)
  got <- drawn_rows(rows = pick, n = 4, weights = w)
  set.seed(110)
  expect_identical(got, sample(pick, 4, prob = w / sum(w)))
})

test_that("fractional, missing, and out-of-range selectors are refused", {
  skip_if_not_installed("LLMR")
  d <- LLMR::anes_2024_personas
  expect_error(panel_from_personas(d, rows = c(1.9, 3.7)), "whole numbers")
  expect_error(panel_from_personas(d, rows = c(1, NA)), "whole numbers")
  expect_error(panel_from_personas(d, rows = 0), "whole numbers")
  expect_error(panel_from_personas(d, rows = "3"), "whole numbers")
  expect_error(panel_from_personas(d, n = 2.5), "`n` must be one whole number")
  expect_error(panel_from_personas(d, n = 0), "`n` must be one whole number")
  expect_error(panel_from_personas(d, n = c(2, 3)), "`n` must be one whole number")
})
