# instrument.R --------------------------------------------------------------------
# Instruments: items with response options and conjoint task builders.
# Option order is data, not noise: panel_administer() randomizes it per response
# and records what each respondent saw.

#' Survey items
#'
#' Three item types cover most quantitative instruments: a Likert item (an
#' agree-disagree battery row), a forced choice, and an open item (free
#' text, returned verbatim). Likert responses also get a numeric `score`
#' (position on the scale as given, 1-based).
#'
#' @param id Item identifier (unique within an instrument).
#' @param text The question text.
#' @param scale For [item_likert()]: response options from low to high.
#' @param options For [item_choice()]: the choice options.
#' @return An object of class `panel_item`.
#' @examples
#' item_likert("wk4", "A four-day work week would benefit society.")
#' item_choice("vote", "Which proposal do you prefer?", c("A", "B"))
#' item_open("why", "In one sentence, why?")
#' @name panel_items
NULL

#' @rdname panel_items
#' @export
item_likert <- function(id, text,
                        scale = c("strongly disagree", "disagree", "neutral",
                                  "agree", "strongly agree")) {
  stopifnot(is.character(id), nzchar(id), is.character(text), nzchar(text),
            is.character(scale), length(scale) >= 2L)
  structure(list(id = id, text = text, type = "likert", options = scale),
            class = "panel_item")
}

#' @rdname panel_items
#' @export
item_choice <- function(id, text, options) {
  stopifnot(is.character(id), nzchar(id), is.character(text), nzchar(text),
            is.character(options), length(options) >= 2L)
  structure(list(id = id, text = text, type = "choice", options = options),
            class = "panel_item")
}

#' @rdname panel_items
#' @export
item_open <- function(id, text) {
  stopifnot(is.character(id), nzchar(id), is.character(text), nzchar(text))
  structure(list(id = id, text = text, type = "open", options = NULL),
            class = "panel_item")
}

#' Assemble an instrument
#'
#' @param items A list of [panel_items] (`item_likert()`, `item_choice()`,
#'   `item_open()`); ids must be unique.
#' @param randomize Which orders to randomize per respondent:
#'   `"item_order"`, `"option_order"`, both (default), or `character(0)`
#'   for none. What each respondent saw is recorded in the responses.
#' @return An object of class `panel_instrument`.
#' @examples
#' panel_instrument(list(
#'   item_likert("trust", "How much do you trust the city council?"),
#'   item_open("reason", "What is the main reason for your answer?")))
#' @export
panel_instrument <- function(items, randomize = c("item_order", "option_order")) {
  if (inherits(items, "panel_item")) items <- list(items)
  stopifnot(is.list(items), length(items) >= 1L)
  for (it in items) {
    if (!inherits(it, "panel_item")) {
      abort("`items` must be built with item_likert()/item_choice()/item_open().")
    }
  }
  ids <- vapply(items, `[[`, "", "id")
  if (anyDuplicated(ids)) abort("Item ids must be unique.")
  bad <- setdiff(randomize, c("item_order", "option_order"))
  if (length(bad)) abort("`randomize` may contain 'item_order' and/or 'option_order'.")
  structure(list(items = items, randomize = randomize),
            class = "panel_instrument")
}

#' @export
print.panel_instrument <- function(x, ...) {
  cat(sprintf("<panel_instrument | %d item(s) | randomized: %s>\n",
              length(x$items),
              if (length(x$randomize)) paste(x$randomize, collapse = ", ")
              else "nothing"))
  for (it in x$items) cat(sprintf("  [%s] (%s) %s\n", it$id, it$type, it$text))
  invisible(x)
}

.draw_conjoint_task <- function(attributes, task, profiles) {
  draw_profile <- function() {
    # index-based draw: sample(lv, 1L) on a single numeric level would trigger
    # R's scalar-sampling rule and fabricate levels 1..lv.
    vapply(attributes,
           function(lv) as.character(lv[[sample.int(length(lv), 1L)]]),
           character(1))
  }

  rows <- list()
  seen <- character(0)
  for (pf in profiles) {
    prof <- draw_profile()
    tries <- 0L
    while (paste(prof, collapse = "\r") %in% seen && tries < 25L) {
      prof <- draw_profile()
      tries <- tries + 1L
    }
    seen <- c(seen, paste(prof, collapse = "\r"))
    rows[[length(rows) + 1L]] <- tibble::as_tibble(
      c(list(task = task, profile = pf), as.list(prof)))
  }
  do.call(rbind, rows)
}

#' Conjoint tasks
#'
#' Random profile pairs (or k-tuples) over the supplied attributes, the
#' design for a forced-choice conjoint. Profiles are sampled uniformly and
#' independently per attribute; set a seed beforehand for a reproducible
#' design (the function never sets one). At administration, fresh profiles are
#' drawn independently for every respondent from the same attribute levels.
#'
#' @param attributes Named list of level vectors.
#' @param n_tasks Tasks per respondent.
#' @param profiles_per_task Profiles shown per task (default 2).
#' @return A `conjoint_design` list with fields `profiles`, a tibble containing
#'   `task`, `profile`, and one column per attribute, and `attributes`, the
#'   named list of attribute levels. Render it into forced-choice items with
#'   [conjoint_instrument()] and estimate with [conjoint_amce()] after
#'   administration. Profiles within a task are distinct when the attribute
#'   space permits them. When it does not, duplicates remain and a warning is
#'   issued.
#' @examples
#' set.seed(110)
#' conjoint_design(
#'   list(price = c("$10", "$20"), speed = c("slow", "fast")),
#'   n_tasks = 4)
#' @export
conjoint_design <- function(attributes, n_tasks = 5L, profiles_per_task = 2L) {
  stopifnot(is.list(attributes), length(attributes) >= 2L)
  if (is.null(names(attributes)) || any(!nzchar(names(attributes)))) {
    abort("`attributes` must be a named list of level vectors.")
  }
  rows <- lapply(seq_len(n_tasks), function(tk) {
    .draw_conjoint_task(attributes, tk, seq_len(profiles_per_task))
  })
  cramped <- any(vapply(rows, function(task) {
    anyDuplicated(task[, names(attributes), drop = FALSE]) > 0L
  }, logical(1)))
  if (cramped) {
    cli::cli_warn(paste(
      "The attribute space is too small for distinct profiles in every",
      "task; some tasks contain duplicates."))
  }
  profiles <- tibble::as_tibble(do.call(rbind, rows))
  structure(list(profiles = profiles, attributes = attributes),
            class = "conjoint_design")
}

#' @export
print.conjoint_design <- function(x, ...) {
  profiles <- x$profiles
  cat(sprintf("<conjoint_design | %d task(s) x %d profile(s) | %d attribute(s)>\n",
              length(unique(profiles$task)), length(unique(profiles$profile)),
              length(x$attributes)))
  invisible(x)
}

#' Build a conjoint instrument
#'
#' Converts a [conjoint_design()] into one forced-choice item per task:
#' each respondent receives a fresh independent profile draw at administration
#' and is asked to pick one by label.
#'
#' @param design A [conjoint_design()] object.
#' @param question Question text shown above each task's profiles.
#' @return A `panel_instrument` whose items are task-level choice items
#'   (ids `task_1`, `task_2`, ...; options `"Profile 1"`, `"Profile 2"`,
#'   ...). Each item carries the attribute levels used for its respondent-level
#'   draws, and the instrument's `$conjoint` field carries the design metadata
#'   for [conjoint_amce()].
#' @details Only option order is randomized; item order stays fixed so the
#'   task ids remain interpretable. Attribute order inside each profile
#'   description follows the design's column order. The profiles recorded in
#'   the design are not reused across respondents.
#' @examples
#' set.seed(110)
#' panel <- panel_from_margins(list(group = c(A = .5, B = .5)), n = 4)
#' design <- conjoint_design(
#'   list(economy = c("weak", "strong"), taxes = c("lower", "higher")),
#'   n_tasks = 3)
#' instrument <- conjoint_instrument(design, "Which candidate do you prefer?")
#' instrument
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' \dontrun{
#' panel_administer(panel, instrument, cfg)
#' }
#' @export
conjoint_instrument <- function(design,
                                question = "Which profile do you prefer?") {
  if (!inherits(design, "conjoint_design")) {
    abort("`design` must be a conjoint_design built with conjoint_design().")
  }
  stopifnot(is.character(question), length(question) == 1L, nzchar(question))
  profiles <- design$profiles
  if (!is.data.frame(profiles) ||
      !all(c("task", "profile") %in% names(profiles))) {
    abort("`design` must have columns task and profile.")
  }
  attrs <- design$attributes
  if (is.null(attrs) || !is.list(attrs)) {
    abort("`design` is missing its attribute metadata; rebuild it with conjoint_design().")
  }
  attr_cols <- names(attrs)
  if (!length(attr_cols) || !all(attr_cols %in% names(profiles))) {
    abort("`design` has no attribute columns.")
  }

  tasks <- sort(unique(profiles$task))
  items <- lapply(tasks, function(k) {
    dk <- profiles[profiles$task == k, , drop = FALSE]
    dk <- dk[order(dk$profile), , drop = FALSE]
    profile_ids <- dk$profile
    item <- item_choice(paste0("task_", k), question,
                        paste("Profile", profile_ids))
    item$conjoint <- list(
      attributes = attrs[attr_cols], task = k, profiles = profile_ids)
    item
  })
  out <- panel_instrument(items, randomize = "option_order")
  out$conjoint <- design
  out
}
