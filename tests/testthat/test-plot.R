# The benchmark plot: structure assertions only (a ggplot comes back; the
# refusals are informative). All offline, through the .runner seam.

plot_fixture <- function(benchmarked = TRUE) {
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
  if (!benchmarked) return(r)
  bench <- rbind(
    data.frame(item_id = "plan", response = c("Plan A", "Plan B"),
               share = c(.5, .5)),
    data.frame(item_id = "wk4", response = c("disagree", "neutral", "agree"),
               share = c(.4, .2, .4)))
  panel_benchmark(r, bench, "toy human study")
}

test_that("plot() on a benchmarked result returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  p <- plot(plot_fixture())
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$geom, "GeomSegment")
  expect_s3_class(p$layers[[2]]$geom, "GeomPoint")
  expect_identical(rlang::as_label(p$layers[[2]]$mapping$colour), "series")
  expect_s3_class(p$facet, "FacetGrid")
  expect_true(isTRUE(p$facet$params$free$y))
  expect_true(p$theme$text$size >= 12)
  expect_true(p$theme$axis.text$size >= 10)
  expect_true(p$theme$strip.text.y.left$size >= 10)
  expect_identical(p$theme$strip.text.y.left$angle, 0)
  expect_identical(p$theme$legend.position, "bottom")
  expect_identical(p$theme$plot.title.position, "plot")
  expect_match(p$labels$title, "Benchmark")

  built <- ggplot2::ggplot_build(p)
  expect_setequal(unique(built$data[[2]]$colour), c("grey25", "#2C7FB8"))
})

test_that("plot() preserves response order within each item", {
  skip_if_not_installed("ggplot2")
  set.seed(110)
  panel <- panel_from_margins(list(group = c(left = .5, right = .5)), n = 6)
  instr <- panel_instrument(list(
    item_choice("first", "First item.", c("A", "B")),
    item_choice("second", "Second item.", c("B", "A"))),
    randomize = character(0))
  alternate <- function(experiments, ...) {
    odd <- experiments$persona_id %% 2L == 1L
    experiments$response_text <- ifelse(
      experiments$item_id == "first",
      ifelse(odd, "A", "B"),
      ifelse(odd, "B", "A"))
    experiments
  }
  responses <- panel_administer(
    panel, instr, LLMR::llm_config("groq", "fake-model"), .runner = alternate)
  benchmark <- rbind(
    data.frame(item_id = "first", response = c("A", "B"), share = c(.5, .5)),
    data.frame(item_id = "second", response = c("B", "A"), share = c(.5, .5)))

  p <- plot(panel_benchmark(responses, benchmark, "order benchmark"))
  expect_identical(
    levels(p$layers[[2]]$data$response_key),
    c("first\rA", "first\rB", "second\rB", "second\rA"))
  y_labels <- unname(p$scales$get_scales("y")$labels)
  expect_false(any(grepl("\r", y_labels, fixed = TRUE)))
})

test_that("plot() wraps a long benchmark citation outside the legend", {
  skip_if_not_installed("ggplot2")
  responses <- plot_fixture()
  bm <- responses$benchmark
  bm$benchmark_name <- paste(
    paste(rep("long benchmark citation", 8), collapse = " "),
    paste0("https://example.org/", strrep("a", 100)))
  responses$benchmark <- bm

  p <- plot(responses)
  subtitle_lines <- strsplit(p$labels$subtitle, "\n", fixed = TRUE)[[1]]
  expect_gte(length(subtitle_lines), 3L)
  expect_lte(max(nchar(subtitle_lines)), 76L)
  expect_setequal(levels(p$layers[[2]]$data$series), c("human", "silicon"))
})

test_that("plot() refuses a result without a benchmark and names the way forward", {
  r <- plot_fixture(benchmarked = FALSE)
  expect_error(plot(r), "NOT BENCHMARKED")
  expect_error(plot(r), "panel_benchmark")
})

test_that("plot() errors informatively when ggplot2 is absent", {
  rc <- plot_fixture()
  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "ggplot2")) return(FALSE)
      base::requireNamespace(package, ...)
    },
    .package = "LLMRpanel"
  )
  expect_error(plot(rc), "ggplot2")
})
