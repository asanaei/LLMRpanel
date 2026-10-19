# instrument.R --------------------------------------------------------------------
# Instruments: items with response options and conjoint task builders.
# panel_administer() records displayed option order. When requested by the
# instrument, it permutes choice options and randomly reverses Likert scales.

#' Survey items
#'
#' Three item types cover most quantitative instruments: a Likert item (an
#' agree-disagree battery row), a forced choice, and an open item (free
#' text, returned verbatim). Likert responses also get a numeric `score`
#' (position on the scale as given, 1-based).
#'
#' Replies are matched to the labels after trimming surrounding spaces and,
#' failing an exact match, without regard to case. Labels must therefore
#' differ by more than case or surrounding spaces; labels that do not, and
#' missing or empty labels, are refused when the item is built.
#'
#' @param id Item identifier: one nonempty string, unique within an
#'   instrument.
#' @param text The question text: one nonempty string.
#' @param scale For [item_likert()]: two or more response labels from low to
#'   high.
#' @param options For [item_choice()]: two or more choice labels.
#' @return An object of class `panel_item`.
#' @examples
#' item_likert("wk4", "A four-day work week would benefit society.")
#' item_choice("vote", "Which proposal do you prefer?", c("A", "B"))
#' item_open("why", "What would improve the city council's work?")
#' @name panel_items
NULL

# A short printed form of an offending value, for error messages.
.show_value <- function(x) {
  shown <- paste(deparse(x, width.cutoff = 60L), collapse = " ")
  if (nchar(shown) > 60L) shown <- paste0(substr(shown, 1L, 57L), "...")
  shown
}

# A design count such as n_tasks: one whole number of at least 1.
.check_count <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 1 ||
      x != round(x)) {
    abort(sprintf("`%s` must be one whole number of at least 1, not %s.",
                  arg, .show_value(x)))
  }
  invisible(x)
}

# An item id or question text: one nonempty, nonmissing string.
.check_item_text <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    abort(sprintf("`%s` must be one nonempty character string, not %s.",
                  arg, .show_value(x)))
  }
  invisible(x)
}

# Response labels: at least two, none missing or empty, and none that a reply
# could not tell apart, since replies are matched after trimming and, failing
# an exact match, without regard to case.
.check_labels <- function(x, arg) {
  if (!is.character(x) || length(x) < 2L) {
    abort(sprintf("`%s` must be a character vector of at least two labels, not %s.",
                  arg, .show_value(x)))
  }
  bad <- is.na(x) | !nzchar(trimws(x))
  if (any(bad)) {
    abort(sprintf("`%s` has a missing or empty label at position %s.",
                  arg, paste(which(bad), collapse = ", ")))
  }
  key <- tolower(trimws(x))
  clash <- duplicated(key) | duplicated(key, fromLast = TRUE)
  if (any(clash)) {
    abort(sprintf(paste(
      "`%s` has labels that replies cannot tell apart: %s.",
      "Labels must differ by more than case or surrounding spaces."),
      arg, paste(sprintf("\"%s\"", x[clash]), collapse = ", ")))
  }
  invisible(x)
}

#' @rdname panel_items
#' @export
item_likert <- function(id, text,
                        scale = c("strongly disagree", "disagree", "neutral",
                                  "agree", "strongly agree")) {
  .check_item_text(id, "id")
  .check_item_text(text, "text")
  .check_labels(scale, "scale")
  structure(list(id = id, text = text, type = "likert", options = scale),
            class = "panel_item")
}

#' @rdname panel_items
#' @export
item_choice <- function(id, text, options) {
  .check_item_text(id, "id")
  .check_item_text(text, "text")
  .check_labels(options, "options")
  structure(list(id = id, text = text, type = "choice", options = options),
            class = "panel_item")
}

#' @rdname panel_items
#' @export
item_open <- function(id, text) {
  .check_item_text(id, "id")
  .check_item_text(text, "text")
  structure(list(id = id, text = text, type = "open", options = NULL),
            class = "panel_item")
}

#' Assemble an instrument
#'
#' Combines survey items and records the option-order randomization used
#' during administration.
#'
#' @param items A list of [panel_items], or a single item. Item identifiers
#'   must be unique.
#' @param randomize `"option_order"` by default. Choice options are permuted
#'   for each response. A Likert scale retains its supplied order or is
#'   reversed, each with probability one-half. Use `character(0)` to retain
#'   the supplied option order.
#' @return A `panel_instrument` object containing `items` and `randomize`.
#' @details
#' Each persona-item pair is administered as an independent request.
#' The model does not receive earlier items or responses. `"item_order"`
#' is therefore refused.
#'
#' Administration records the displayed options in `option_order`.
#' `item_position` is the item's fixed, 1-based position in the instrument.
#' Likert scores use the supplied scale order, including when the displayed
#' scale is reversed.
#' @examples
#' panel_instrument(list(
#'   item_likert("trust", "The city council acts in the public interest."),
#'   item_open("reason", "What would improve the city council's work?")
#' ))
#'
#' panel_instrument(
#'   item_choice("fund", "Choose an investment.", c("transit", "roads")),
#'   randomize = character(0)
#' )
#' @export
panel_instrument <- function(items, randomize = "option_order") {
  if (inherits(items, "panel_item")) items <- list(items)
  stopifnot(is.list(items), length(items) >= 1L)
  for (it in items) {
    if (!inherits(it, "panel_item")) {
      abort("`items` must be built with item_likert()/item_choice()/item_open().")
    }
  }
  ids <- vapply(items, `[[`, "", "id")
  if (anyDuplicated(ids)) {
    abort(sprintf("Item ids must be unique; repeated: %s.",
                  paste(sprintf("\"%s\"", unique(ids[duplicated(ids)])),
                        collapse = ", ")))
  }
  if ("item_order" %in% randomize) {
    abort(paste(
      "`item_order` is not implemented: items are administered as",
      "independent requests, so there is no questionnaire order to randomize."))
  }
  bad <- setdiff(randomize, "option_order")
  if (length(bad)) abort("`randomize` may contain only 'option_order'.")
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
#' Within a task, a profile that repeats one already drawn is redrawn, up to
#' 25 times. When the attribute space holds fewer distinct profiles than
#' `profiles_per_task`, every task repeats a profile. Otherwise a repeat
#' remains only when all 25 redraws repeat a profile, which can happen when the
#' space is barely larger than the task. Either case draws a warning that says
#' which occurred. The profiles drawn for each respondent at administration
#' follow the same rule without a warning.
#'
#' @param attributes Named list of level vectors, with at least two
#'   attributes. Attribute names must be unique and nonempty and must not be
#'   `task`, `profile`, `persona_id`, or `chosen`, which name the design's own
#'   columns. Each attribute needs at least one level, and its levels must be
#'   unique, nonempty, and not missing.
#' @param n_tasks Tasks per respondent: one whole number of at least 1.
#' @param profiles_per_task Profiles shown per task (default 2): one whole
#'   number of at least 1. [conjoint_instrument()] needs at least two.
#' @return A `conjoint_design` list with fields `profiles`, a tibble containing
#'   `task`, `profile`, and one column per attribute, and `attributes`, the
#'   named list of attribute levels. Render it into forced-choice items with
#'   [conjoint_instrument()] and estimate with [conjoint_amce()] after
#'   administration.
#' @examples
#' set.seed(110)
#' conjoint_design(
#'   list(price = c("$10", "$20"), speed = c("slow", "fast")),
#'   n_tasks = 4)
#' @export
conjoint_design <- function(attributes, n_tasks = 5L, profiles_per_task = 2L) {
  stopifnot(is.list(attributes), length(attributes) >= 2L)
  nms <- names(attributes)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(trimws(nms)))) {
    abort("`attributes` must be a named list of level vectors.")
  }
  if (anyDuplicated(nms)) {
    abort(sprintf("Attribute names must be unique; repeated: %s.",
                  paste(sprintf("\"%s\"", unique(nms[duplicated(nms)])),
                        collapse = ", ")))
  }
  reserved <- intersect(nms, c("task", "profile", "persona_id", "chosen"))
  if (length(reserved)) {
    abort(sprintf(paste(
      "Attribute name %s is reserved for the design's own columns;",
      "rename the attribute."),
      paste(sprintf("\"%s\"", reserved), collapse = ", ")))
  }
  for (a in nms) {
    lv <- attributes[[a]]
    if (!is.atomic(lv) || !length(lv)) {
      abort(sprintf("Attribute \"%s\" must be a vector of at least one level.", a))
    }
    lv <- as.character(lv)
    if (anyNA(lv) || any(!nzchar(trimws(lv)))) {
      abort(sprintf("Attribute \"%s\" has a missing or empty level at position %s.",
                    a, paste(which(is.na(lv) | !nzchar(trimws(lv))), collapse = ", ")))
    }
    if (anyDuplicated(lv)) {
      abort(sprintf("Attribute \"%s\" repeats the level(s) %s.", a,
                    paste(sprintf("\"%s\"", unique(lv[duplicated(lv)])),
                          collapse = ", ")))
    }
  }
  .check_count(n_tasks, "n_tasks")
  .check_count(profiles_per_task, "profiles_per_task")
  rows <- lapply(seq_len(n_tasks), function(tk) {
    .draw_conjoint_task(attributes, tk, seq_len(profiles_per_task))
  })
  cramped <- any(vapply(rows, function(task) {
    anyDuplicated(task[, nms, drop = FALSE]) > 0L
  }, logical(1)))
  if (cramped) {
    space <- prod(lengths(attributes))
    if (space < profiles_per_task) {
      cli::cli_warn(paste(
        "The attribute space is too small for distinct profiles: it holds",
        "{space} distinct profile{?s} and each task shows {profiles_per_task},",
        "so every task contains duplicates."))
    } else {
      cli::cli_warn(paste(
        "Some tasks contain duplicate profiles because 25 redraws found no",
        "distinct one, although the attribute space holds {space} distinct",
        "profiles. Draw the design again, or show fewer profiles per task."))
    }
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
#' Converts a conjoint design into one forced-choice item per task.
#' Administration draws profiles for each persona and asks the model to
#' choose a profile by its label.
#'
#' @param design A [conjoint_design()] object.
#' @param question Question text shown above each task's profiles.
#' @return A `panel_instrument` with task identifiers such as `task_1` and
#'   profile options such as `"Profile 1"`. Each item retains the attribute
#'   levels used for profile draws. The instrument's `conjoint` field
#'   retains the design metadata used by [conjoint_amce()].
#' @details
#' Each persona-task pair is a separate request. Its `item_position` is
#' instrument metadata and does not describe an order of exposure.
#' Profile choice options are permuted for each request.
#'
#' Profile descriptions follow profile-number order. Attributes within a
#' description follow the design's attribute order. The initial profile
#' table in `design` is not reused across personas.
#' @examples
#' set.seed(110)
#' panel <- panel_from_margins(list(group = c(A = 1)), n = 4)
#' design <- conjoint_design(
#'   list(economy = c("weak", "strong"), taxes = c("lower", "higher")),
#'   n_tasks = 3
#' )
#' instrument <- conjoint_instrument(
#'   design, "Which candidate do you prefer?"
#' )
#' cfg <- LLMR::llm_config("groq", "example-model")
#' runner <- function(experiments, ...) {
#'   experiments$response_text <- "Profile 1"
#'   experiments
#' }
#' panel_administer(panel, instrument, cfg, .runner = runner)
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
