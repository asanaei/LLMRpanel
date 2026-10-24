# panel_from_personas() draws the respondents that `rows` selects, including a
# single selected row, and accepts only whole-number selectors and sizes.

drawn_rows <- function(...) {
  testthat::local_mocked_bindings(
    .render_persona_text = function(data, r, ...) paste0("row ", r),
    .package = "LLMRpanel")
  p <- panel_from_personas(LLMRpanel::anes_2024_personas, ...)
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
  d <- LLMRpanel::anes_2024_personas
  expect_error(panel_from_personas(d, rows = c(1.9, 3.7)), "whole numbers")
  expect_error(panel_from_personas(d, rows = c(1, NA)), "whole numbers")
  expect_error(panel_from_personas(d, rows = 0), "whole numbers")
  expect_error(panel_from_personas(d, rows = "3"), "whole numbers")
  expect_error(panel_from_personas(d, n = 2.5), "`n` must be one whole number")
  expect_error(panel_from_personas(d, n = 0), "`n` must be one whole number")
  expect_error(panel_from_personas(d, n = c(2, 3)), "`n` must be one whole number")
})

test_that("character and factor selectors are refused, not read as positions", {
  skip_if_not_installed("LLMR")
  d <- LLMRpanel::anes_2024_personas
  expect_error(panel_from_personas(d, rows = c("3", "5")), "whole numbers")
  expect_error(panel_from_personas(d, rows = factor(c(10, 20))), "whole numbers")
})

traced_panel <- function(...) {
  testthat::local_mocked_bindings(
    .render_persona_text = function(data, r, ...) paste0("row ", r),
    .package = "LLMRpanel")
  panel_from_personas(LLMRpanel::anes_2024_personas, ...)
}

test_that("source_rows traces every persona to the row it was drawn from", {
  skip_if_not_installed("LLMR")
  one <- function(df) seq_len(nrow(df)) == 5L
  set.seed(110)
  p <- traced_panel(rows = one, n = 1)
  expect_identical(attr(p, "source_rows"), 5L)
  set.seed(110)
  p <- suppressWarnings(traced_panel(rows = one, n = 3))
  expect_identical(attr(p, "source_rows"), c(5L, 5L, 5L))

  p <- traced_panel(rows = c(30, 2, 17))
  expect_identical(attr(p, "source_rows"), c(30L, 2L, 17L))

  set.seed(110)
  p <- traced_panel(rows = seq(2, 40, by = 2), n = 12,
                    weights = rep(c(3, 1), 10))
  drawn <- as.integer(sub("row ", "", p$persona))
  expect_identical(attr(p, "source_rows"), drawn)
  expect_true(all(drawn %% 2L == 0L))

  sub <- p[c(9, 4), ]
  expect_identical(attr(sub, "source_rows")[sub$persona_id], drawn[c(9, 4)])
  expect_null(attr(tibble::as_tibble(p), "source_rows"))
})

test_that("panel_from_data records the microdata rows it drew", {
  src <- data.frame(id = sprintf("case%02d", 1:12),
                    group = rep(c("a", "b", "c"), 4),
                    w = c(rep(0, 6), rep(1, 6)))
  set.seed(110)
  p <- panel_from_data(src, n = 8, columns = c("id", "group"), weights = "w")
  rows <- attr(p, "source_rows")
  expect_type(rows, "integer")
  expect_length(rows, 8L)
  expect_true(all(rows > 6L))
  expect_identical(p$id, src$id[rows])
  expect_identical(p$group, src$group[rows])
})
