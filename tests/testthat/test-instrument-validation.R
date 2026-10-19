# Instrument definitions are checked before any request is built: items need
# one id and one question, labels a reply can be matched to, and conjoint
# designs need unique attributes and whole-number counts.

test_that("item ids and question texts must be one nonempty string", {
  bad_ids <- list(c("a", "b"), NA_character_, "", "   ", 1, character(0))
  for (id in bad_ids) {
    expect_error(item_choice(id, "Which?", c("A", "B")),
                 "`id` must be one nonempty character string", info = deparse(id))
    expect_error(item_open(id, "Why?"), "`id` must be one nonempty", info = deparse(id))
    expect_error(item_likert(id, "Agree?"), "`id` must be one nonempty", info = deparse(id))
  }
  expect_error(item_choice("q", c("Which?", "Really?"), c("A", "B")),
               "`text` must be one nonempty character string, not c\\(\"Which\\?\"")
  expect_error(item_open("q", NA_character_), "`text` must be one nonempty")
  expect_error(item_likert("q", ""), "`text` must be one nonempty")
})

test_that("labels that are missing, empty, or indistinguishable are refused", {
  expect_error(item_choice("q", "Which?", c("A", "A")),
               "cannot tell apart: \"A\", \"A\"", fixed = TRUE)
  expect_error(item_choice("q", "Which?", c("Yes", "yes", "No")),
               "cannot tell apart: \"Yes\", \"yes\"", fixed = TRUE)
  expect_error(item_choice("q", "Which?", c("No", " No ")),
               "cannot tell apart", fixed = TRUE)
  expect_error(item_choice("q", "Which?", c("A", NA)),
               "missing or empty label at position 2", fixed = TRUE)
  expect_error(item_choice("q", "Which?", c("", "B", " ")),
               "missing or empty label at position 1, 3", fixed = TRUE)
  expect_error(item_choice("q", "Which?", "A"), "at least two labels")
  expect_error(item_choice("q", "Which?", 1:2), "at least two labels, not 1:2",
               fixed = TRUE)
  expect_error(item_likert("q", "Agree?", c("agree", "neutral", "agree")),
               "`scale` has labels that replies cannot tell apart: \"agree\", \"agree\"",
               fixed = TRUE)
  expect_error(item_likert("q", "Agree?", c("low", NA, "high")),
               "`scale` has a missing or empty label at position 2", fixed = TRUE)
})

test_that("valid items are built exactly as before", {
  expect_identical(
    item_choice("vote", "Which proposal?", c("A", " B")),
    structure(list(id = "vote", text = "Which proposal?", type = "choice",
                   options = c("A", " B")), class = "panel_item"))
  expect_identical(
    item_likert("wk4", "It helps."),
    structure(list(id = "wk4", text = "It helps.", type = "likert",
                   options = c("strongly disagree", "disagree", "neutral",
                               "agree", "strongly agree")),
              class = "panel_item"))
  expect_identical(
    item_open("why", "Why?"),
    structure(list(id = "why", text = "Why?", type = "open", options = NULL),
              class = "panel_item"))
})

test_that("a repeated item id is named", {
  expect_error(
    panel_instrument(list(item_open("q", "One?"), item_open("q", "Two?"),
                          item_open("r", "Three?"))),
    "Item ids must be unique; repeated: \"q\".", fixed = TRUE)
})

test_that("conjoint counts must be whole numbers of at least one", {
  attrs <- list(a = c("x", "y"), b = c("p", "q"))
  for (n in list(2.5, 0, -1, NA, NA_real_, Inf, c(2, 3), "3", TRUE)) {
    expect_error(conjoint_design(attrs, n_tasks = n),
                 "`n_tasks` must be one whole number of at least 1",
                 info = deparse(n))
  }
  for (k in list(0, 1.5, NA_integer_, "2")) {
    expect_error(conjoint_design(attrs, profiles_per_task = k),
                 "`profiles_per_task` must be one whole number of at least 1",
                 info = deparse(k))
  }
  expect_error(conjoint_design(attrs, n_tasks = 2.5), "not 2.5", fixed = TRUE)
})

test_that("conjoint attributes need unique names and usable levels", {
  expect_error(conjoint_design(list(a = c("x", "y"), a = c("p", "q"))),
               "Attribute names must be unique; repeated: \"a\".", fixed = TRUE)
  for (nm in c("task", "profile", "persona_id", "chosen")) {
    attrs <- stats::setNames(list(c("x", "y"), c("p", "q")), c(nm, "b"))
    expect_error(conjoint_design(attrs),
                 sprintf("Attribute name \"%s\" is reserved", nm), fixed = TRUE)
  }
  expect_error(conjoint_design(stats::setNames(list("x", c("p", "q")), c("", "b"))),
               "named list")
  expect_error(conjoint_design(list(a = c("x", NA), b = c("p", "q"))),
               "Attribute \"a\" has a missing or empty level at position 2",
               fixed = TRUE)
  expect_error(conjoint_design(list(a = c("x", ""), b = c("p", "q"))),
               "missing or empty level", fixed = TRUE)
  expect_error(conjoint_design(list(a = c("x", "x", "y"), b = c("p", "q"))),
               "Attribute \"a\" repeats the level(s) \"x\".", fixed = TRUE)
  expect_error(conjoint_design(list(a = character(0), b = c("p", "q"))),
               "at least one level", fixed = TRUE)
})

test_that("valid designs draw the same profiles as before", {
  attrs <- list(price = c("$10", "$20"), speed = c("slow", "fast"))
  set.seed(110)
  design <- conjoint_design(attrs, n_tasks = 4)
  set.seed(110)
  expected <- tibble::as_tibble(do.call(rbind, lapply(1:4, function(tk) {
    LLMRpanel:::.draw_conjoint_task(attrs, tk, 1:2)
  })))
  expect_identical(design$profiles, expected)
  set.seed(110)
  numeric_levels <- conjoint_design(list(n = c(1, 2), m = c(10, 20)), n_tasks = 2L)
  expect_identical(nrow(numeric_levels$profiles), 4L)
})

test_that("the duplicate-profile warning says whether the space or the redraws ran out", {
  set.seed(110)
  expect_warning(
    conjoint_design(list(a = "x", b = c("p", "q")), n_tasks = 2,
                    profiles_per_task = 3),
    "too small for distinct profiles: it holds 2 distinct profiles and each task shows 3")
  testthat::local_mocked_bindings(
    .draw_conjoint_task = function(attributes, task, profiles) {
      tibble::tibble(task = task, profile = profiles, a = "x", b = "p")
    })
  expect_warning(
    conjoint_design(list(a = c("x", "y"), b = c("p", "q")), n_tasks = 2),
    "25 redraws found no distinct one, although the attribute space holds 4")
})
