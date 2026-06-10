# instrument.R --------------------------------------------------------------------
# Instruments: items with response options, plus factorial stimulus builders.
# Option order is data, not noise: administer() randomizes it per response
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
#' @export
instrument <- function(items, randomize = c("item_order", "option_order")) {
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

#' Factorial vignettes
#'
#' Expands `factors` into the full factorial and renders the vignette text
#' per cell with literal `{factor}` substitution. Each rendered vignette is
#' typically paired with one or two items via [instrument()].
#'
#' @param template Vignette text containing `{factor}` placeholders.
#' @param factors Named list of level vectors.
#' @return A tibble: `vignette_id`, one column per factor, `text`.
#' @examples
#' vignette_design(
#'   "A {age} applicant with {experience} experience applies for the job.",
#'   list(age = c("younger", "older"), experience = c("5 years", "20 years"))
#' )
#' @export
vignette_design <- function(template, factors) {
  stopifnot(is.character(template), length(template) == 1L,
            is.list(factors), length(factors) >= 1L)
  grid <- expand.grid(factors, stringsAsFactors = FALSE)
  grid$text <- vapply(seq_len(nrow(grid)), function(i) {
    .fill(template, as.list(grid[i, , drop = FALSE]))
  }, character(1))
  out <- tibble::as_tibble(grid)
  tibble::add_column(out, vignette_id = seq_len(nrow(out)), .before = 1)
}

#' Conjoint tasks
#'
#' Random profile pairs (or k-tuples) over the supplied attributes, the
#' design for a forced-choice conjoint. Profiles are sampled uniformly and
#' independently per attribute; set a seed beforehand for a reproducible
#' design (the function never sets one).
#'
#' @param attributes Named list of level vectors.
#' @param n_tasks Tasks per respondent.
#' @param profiles_per_task Profiles shown per task (default 2).
#' @return A tibble: `task`, `profile`, one column per attribute, ready for
#'   [administer()] after pairing with an [item_choice()] asking which
#'   profile the respondent prefers. Profiles within a task are guaranteed
#'   distinct (a forced choice between identical profiles measures
#'   nothing); when the attribute space is too small to allow distinct
#'   profiles, duplicates remain and a warning says so. Estimation
#'   (`amce()`) arrives in 0.2.
#' @export
conjoint_design <- function(attributes, n_tasks = 5L, profiles_per_task = 2L) {
  stopifnot(is.list(attributes), length(attributes) >= 2L)
  if (is.null(names(attributes)) || any(!nzchar(names(attributes)))) {
    abort("`attributes` must be a named list of level vectors.")
  }
  draw_profile <- function() {
    vapply(attributes, function(lv) as.character(sample(lv, 1L)), character(1))
  }
  rows <- list()
  cramped <- FALSE
  for (tk in seq_len(n_tasks)) {
    seen <- character(0)
    for (pf in seq_len(profiles_per_task)) {
      prof <- draw_profile()
      tries <- 0L
      while (paste(prof, collapse = "\r") %in% seen && tries < 25L) {
        prof <- draw_profile()
        tries <- tries + 1L
      }
      if (paste(prof, collapse = "\r") %in% seen) cramped <- TRUE
      seen <- c(seen, paste(prof, collapse = "\r"))
      rows[[length(rows) + 1L]] <- tibble::as_tibble(
        c(list(task = tk, profile = pf), as.list(prof)))
    }
  }
  if (cramped) {
    cli::cli_warn(paste(
      "The attribute space is too small for distinct profiles in every",
      "task; some tasks contain duplicates."))
  }
  do.call(rbind, rows)
}
