# Regression tests for the 0.6.1 corrections: honest randomization, grid
# authority over runners, validated human references, and safe construction.

.rf_panel <- function(n = 8) {
  set.seed(110)
  panel_from_margins(list(g = c(a = .5, b = .5)), n = n,
                     persona_template = "A person of group {g}.")
}
.rf_cfg <- function() LLMR::llm_config("groq", "fake-model")

test_that("likert scales reverse rather than permute under option_order", {
  instr <- panel_instrument(list(
    item_likert("lk", "Agree?", scale = c("low", "mid", "high"))))
  runner <- function(experiments, ...) {
    experiments$response_text <- "low"
    experiments$success <- TRUE
    experiments
  }
  set.seed(110)
  r <- panel_administer(.rf_panel(40), instr, .rf_cfg(), .runner = runner)
  shown <- unique(r$data$option_order)
  expect_true(all(shown %in% c("low|mid|high", "high|mid|low")))
  expect_length(shown, 2L)
})

test_that("a runner cannot rewrite experimental assignments", {
  instr <- panel_instrument(list(
    item_choice("pick", "Choose.", c("A", "B"))), randomize = character(0))
  tampering <- function(experiments, ...) {
    experiments$response_text <- "A"
    experiments$success <- TRUE
    experiments$persona_id <- rev(experiments$persona_id)
    experiments$item_id <- "forged"
    experiments$option_order <- "B|A"
    experiments
  }
  p <- .rf_panel(4)
  r <- panel_administer(p, instr, .rf_cfg(), .runner = tampering)
  expect_setequal(r$data$persona_id, p$persona_id)
  expect_true(all(r$data$item_id == "pick"))
  expect_true(all(r$data$option_order == "A|B"))
})

test_that("the human reference is validated before comparison", {
  instr <- panel_instrument(list(
    item_choice("pick", "Choose.", c("A", "B"))), randomize = character(0))
  runner <- function(experiments, ...) {
    experiments$response_text <- "A"
    experiments$success <- TRUE
    experiments
  }
  r <- panel_administer(.rf_panel(6), instr, .rf_cfg(), .runner = runner)
  expect_error(panel_benchmark(r, data.frame(
    item_id = "pick", response = c("A", "B"), share = c(.6, .6)),
    "bad sums"), "sum")
  expect_error(panel_benchmark(r, data.frame(
    item_id = c("pick", "pick", "pick"),
    response = c("A", "A", "B"), share = c(.3, .3, .4)),
    "dup rows"), "exactly once")
  expect_error(panel_benchmark(r, data.frame(
    item_id = "pick", response = c("A", "B"), share = c(1.4, -0.4)),
    "bad range"), "\\[0, 1\\]")
})

test_that("panel_from_personas requires data and honest weights", {
  expect_error(panel_from_personas(), "required")
  df <- data.frame(x = c("p", "q", "r"), w = c(1, NA, 2))
  expect_error(
    panel_from_personas(df, n = 2, weights = "w"),
    "nonnegative with no NA")
  df$f <- factor(c("lo", "hi", "lo"))
  expect_error(
    panel_from_personas(df, n = 2, weights = "f"),
    "numeric column")
  expect_warning(
    panel_from_personas(data.frame(x = c("p", "q")), weights = c(1, 2)),
    "ignored")
  expect_error(
    panel_from_personas(data.frame(x = c("p", "q")), rows = c(1L, 9L)),
    "between 1 and")
})

test_that("placeholder filling is single-pass", {
  out <- LLMRpanel:::.fill("{a} and {b}", list(a = "{b}", b = "safe"))
  expect_identical(out, "{b} and safe")
})

test_that("an empty reply from an exhausted budget is named, not just counted", {
  instr <- panel_instrument(list(item_choice("p", "Pick.", c("A", "B"))),
                            randomize = character(0))
  starving <- function(experiments, ...) {
    experiments$response_text <- ""
    experiments$success <- TRUE
    experiments$finish_reason <- "length"
    experiments
  }
  expect_warning(
    panel_administer(.rf_panel(2), instr, .rf_cfg(), .runner = starving),
    "budget ran out before any visible text")
  # a genuine unparseable answer is not blamed on the budget
  refusing <- function(experiments, ...) {
    experiments$response_text <- "I would rather not say"
    experiments$success <- TRUE
    experiments$finish_reason <- "stop"
    experiments
  }
  expect_no_warning(
    r <- panel_administer(.rf_panel(2), instr, .rf_cfg(), .runner = refusing))
  expect_true(all(is.na(as.data.frame(tibble::as_tibble(r))$response)))
})
