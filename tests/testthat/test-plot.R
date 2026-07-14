# The calibration plot: structure assertions only (a ggplot comes back; the
# refusals are informative). All offline, through the .runner seam.

plot_fixture <- function(calibrated = TRUE) {
  set.seed(110)
  panel <- panel_from_margins(
    list(party = c(left = .5, right = .5)), n = 10,
    persona_template = "A voter who leans {party}.")
  instr <- panel_instrument(list(
    item_likert("wk4", "A four-day work week would benefit society.",
                scale = c("disagree", "neutral", "agree")),
    item_choice("plan", "Which plan do you prefer?", c("Plan A", "Plan B"))))
  by_party <- function(experiments, ...) {
    experiments$response_text <- vapply(seq_len(nrow(experiments)), function(i) {
      sys <- experiments$messages[[i]][["system"]]
      usr <- experiments$messages[[i]][["user"]]
      right <- grepl("leans right", sys)
      if (grepl("work week", usr)) { if (right) "agree" else "disagree" }
      else { if (right) "Plan A" else "Plan B" }
    }, character(1))
    experiments
  }
  r <- panel_administer(panel, instr, LLMR::llm_config("groq", "fake-model"),
                        .runner = by_party)
  if (!calibrated) return(r)
  bench <- rbind(
    data.frame(item_id = "plan", response = c("Plan A", "Plan B"),
               share = c(.5, .5)),
    data.frame(item_id = "wk4", response = c("disagree", "neutral", "agree"),
               share = c(.4, .2, .4)))
  panel_calibrate(r, bench, "toy human study")
}

test_that("plot() on a calibrated result returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  p <- plot(plot_fixture())
  expect_s3_class(p, "ggplot")
})

test_that("plot() refuses an uncalibrated result and names the way forward", {
  r <- plot_fixture(calibrated = FALSE)
  expect_error(plot(r), "UNCALIBRATED")
  expect_error(plot(r), "panel_calibrate")
})

test_that("plot() errors informatively when ggplot2 is absent", {
  rc <- plot_fixture()
  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "ggplot2")) return(FALSE)
      base::requireNamespace(package, ...)
    })
  expect_error(plot(rc), "ggplot2")
})
