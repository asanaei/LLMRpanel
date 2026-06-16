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
