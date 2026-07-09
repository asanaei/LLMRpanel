# instrument.R --------------------------------------------------------------------
# Instruments: items with response options, plus factorial stimulus builders.
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

#' Factorial vignettes
#'
#' Expands `factors` into the full factorial and renders the vignette text
#' per cell with literal `{factor}` substitution. Each rendered vignette is
#' typically paired with one or two items via [panel_instrument()].
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
#' @return A tibble: `task`, `profile`, one column per attribute, carrying
#'   the original attribute list in `attr(x, "attributes")`. Render it into
#'   forced-choice items with [conjoint_instrument()] and estimate with
#'   [amce()] after administration. Profiles within a task are guaranteed
#'   distinct (a forced choice between identical profiles measures
#'   nothing); when the attribute space is too small to allow distinct
#'   profiles, duplicates remain and a warning says so.
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
  draw_profile <- function() {
    # index-based draw: sample(lv, 1L) on a single numeric level would trigger
    # R's scalar-sampling rule and fabricate levels 1..lv.
    vapply(attributes,
           function(lv) as.character(lv[[sample.int(length(lv), 1L)]]),
           character(1))
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
  out <- do.call(rbind, rows)
  attr(out, "attributes") <- attributes
  out
}

#' Build a conjoint instrument
#'
#' Converts a [conjoint_design()] into one forced-choice item per task:
#' the profiles are rendered as labeled descriptions, and the respondent
#' is asked to pick one by label.
#'
#' @param design A [conjoint_design()] tibble (columns `task`, `profile`,
#'   one column per attribute, and the attribute list in
#'   `attr(design, "attributes")`).
#' @param question Question text shown above each task's profiles.
#' @return A `panel_instrument` whose items are task-level choice items
#'   (ids `task_1`, `task_2`, ...; options `"Profile 1"`, `"Profile 2"`,
#'   ...) and whose `$conjoint` field carries the design for [amce()].
#' @details Only option order is randomized; item order stays fixed so the
#'   task ids remain interpretable. Attribute order inside each profile
#'   description follows the design's column order.
#' @examples
#' set.seed(110)
#' panel <- panel_from_margins(list(group = c(A = .5, B = .5)), n = 4)
#' design <- conjoint_design(
#'   list(economy = c("weak", "strong"), taxes = c("lower", "higher")),
#'   n_tasks = 3)
#' instr <- conjoint_instrument(design, "Which candidate do you prefer?")
#' instr
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' \dontrun{
#' panel_administer(panel, instr, cfg)
#' }
#' @export
conjoint_instrument <- function(design,
                                question = "Which profile do you prefer?") {
  stopifnot(is.data.frame(design), is.character(question),
            length(question) == 1L, nzchar(question))
  if (!all(c("task", "profile") %in% names(design))) {
    abort("`design` must have columns task and profile.")
  }
  attrs <- attr(design, "attributes")
  if (is.null(attrs) || !is.list(attrs)) {
    abort("`design` must carry attr(design, 'attributes'); rebuild it with conjoint_design().")
  }
  attr_cols <- setdiff(names(design), c("task", "profile"))
  if (!length(attr_cols)) abort("`design` has no attribute columns.")

  tasks <- sort(unique(design$task))
  items <- lapply(tasks, function(k) {
    dk <- design[design$task == k, , drop = FALSE]
    dk <- dk[order(dk$profile), , drop = FALSE]
    profiles <- dk$profile
    lines <- vapply(seq_len(nrow(dk)), function(i) {
      vals <- vapply(attr_cols, function(a) as.character(dk[[a]][i]),
                     character(1))
      sprintf("Profile %s: %s.", profiles[i],
              paste(sprintf("%s: %s", attr_cols, vals), collapse = "; "))
    }, character(1))
    item_choice(paste0("task_", k),
                paste0(question, "\n\n", paste(lines, collapse = "\n")),
                paste("Profile", profiles))
  })
  out <- panel_instrument(items, randomize = "option_order")
  out$conjoint <- design
  out
}
