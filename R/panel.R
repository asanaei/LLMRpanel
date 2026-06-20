# panel.R -----------------------------------------------------------------------
# Persona panels from population margins. The package ships no demographic
# data and no "default Americans": you supply the margins (from ACS, ANES,
# CES, a census table), and what you supplied is what the report cites.

#' Draw a persona panel from population margins
#'
#' Samples `n` personas with attributes drawn independently from the
#' supplied margins, and renders each persona's text from a template.
#' Independence across attributes is a deliberate simplification: the
#' attributes are sampled marginally, not jointly. For instrument
#' pretesting this rarely matters; for anything resembling estimation it
#' does, and [panel_calibrate()] will tell you. When you hold microdata and want
#' the joint distribution preserved, use [panel_from_data()].
#'
#' For a reproducible panel, set a seed before calling (the function never
#' sets one itself).
#'
#' @param margins A named list; each element a named probability vector,
#'   e.g. `list(age = c("18-34" = .3, "35-64" = .45, "65+" = .25))`.
#'   Probabilities are renormalized if they do not sum to 1.
#' @param n Panel size.
#' @param persona_template Text with `{attribute}` placeholders rendered
#'   per persona. `NULL` builds a plain "attribute: value" persona.
#' @return A `silicon_panel`: a tibble with `persona_id`, one column per
#'   attribute, and `persona` (the rendered text).
#' @examples
#' set.seed(110)
#' panel <- panel_from_margins(
#'   list(cohort = c(young = .3, middle = .45, older = .25),
#'        party  = c(left = .45, right = .45, independent = .10)),
#'   n = 50,
#'   persona_template = "A {cohort} voter who leans {party}."
#' )
#' panel
#' @export
panel_from_margins <- function(margins, n, persona_template = NULL) {
  stopifnot(is.list(margins), length(margins) >= 1L, n >= 1L)
  if (is.null(names(margins)) || any(!nzchar(names(margins)))) {
    abort("`margins` must be a named list of named probability vectors.")
  }
  cols <- lapply(margins, function(m) {
    if (is.null(names(m)) || any(!nzchar(names(m)))) {
      abort("Every margin must be a *named* probability vector.")
    }
    sample(names(m), n, replace = TRUE, prob = m / sum(m))
  })
  out <- tibble::as_tibble(cols)
  out <- tibble::add_column(out, persona_id = seq_len(n), .before = 1)
  out$persona <- vapply(seq_len(n), function(i) {
    vals <- lapply(cols, `[[`, i)
    if (is.null(persona_template)) {
      paste(sprintf("%s: %s", names(vals), unlist(vals)), collapse = "; ")
    } else {
      .fill(persona_template, vals)
    }
  }, character(1))
  structure(out, class = c("silicon_panel", class(out)),
            margins = margins)
}

#' Draw a persona panel from microdata rows
#'
#' Samples rows from a data frame with replacement, which preserves the
#' joint distribution of the selected attributes, and renders each sampled
#' row as a persona. This is the joint-distribution counterpart of
#' [panel_from_margins()], which samples attributes independently. The
#' margins the report cites are computed from the source data, one
#' `prop.table(table())` per selected column.
#'
#' For a reproducible panel, set a seed before calling (the function never
#' sets one itself).
#'
#' @param data A data frame, one row per source case.
#' @param n Panel size.
#' @param persona_template Text with `{attribute}` placeholders rendered
#'   per persona. `NULL` builds a plain "attribute: value" persona.
#' @param columns Attribute columns to keep. Defaults to every column
#'   except the `weights` column when one is given.
#' @param weights Optional name of a single column of nonnegative sampling
#'   weights (rows are drawn with probability proportional to it).
#' @return A `silicon_panel`: a tibble with `persona_id`, the selected
#'   attribute columns, and `persona`.
#' @examples
#' set.seed(110)
#' src <- data.frame(
#'   education = c("college", "college", "no college", "no college"),
#'   income    = c("high", "high", "low", "low"),
#'   weight    = c(2, 2, 1, 1))
#' panel_from_data(src, n = 10, columns = c("education", "income"),
#'                 weights = "weight",
#'                 persona_template = "A {education} respondent earning {income}.")
#' @export
panel_from_data <- function(data, n, persona_template = NULL,
                            columns = NULL, weights = NULL) {
  stopifnot(is.data.frame(data), n >= 1L)
  if (!nrow(data)) abort("`data` must contain at least one row.")
  if (!is.null(weights)) {
    stopifnot(is.character(weights), length(weights) == 1L)
    if (!(weights %in% names(data))) abort("`weights` must name a column in `data`.")
    prob <- data[[weights]]
    if (!is.numeric(prob) || anyNA(prob) || any(prob < 0) || sum(prob) <= 0) {
      abort("`weights` must name a nonnegative numeric column with positive total weight.")
    }
  } else {
    prob <- NULL
  }
  if (is.null(columns)) {
    columns <- setdiff(names(data), weights %||% character(0))
  } else {
    stopifnot(is.character(columns))
    if (length(setdiff(columns, names(data)))) {
      abort("`columns` contains names not found in `data`.")
    }
  }
  if (!length(columns)) abort("`columns` must select at least one attribute column.")

  idx <- sample(seq_len(nrow(data)), n, replace = TRUE, prob = prob)
  attrs <- data[idx, columns, drop = FALSE]
  out <- tibble::as_tibble(attrs)
  out <- tibble::add_column(out, persona_id = seq_len(n), .before = 1)
  out$persona <- vapply(seq_len(n), function(i) {
    vals <- as.list(attrs[i, , drop = FALSE])
    if (is.null(persona_template)) {
      paste(sprintf("%s: %s", names(vals), vapply(vals, as.character, character(1))),
            collapse = "; ")
    } else {
      .fill(persona_template, vals)
    }
  }, character(1))
  # The cited margins are the source distribution the panel was drawn from.
  # With sampling weights, that distribution is the WEIGHTED one; an unweighted
  # prop.table(table(col)) would misreport the population the panel represents.
  margins <- lapply(data[columns], function(col) {
    if (is.null(prob)) {
      prop.table(table(col))
    } else {
      w <- tapply(prob, col, sum)
      w[is.na(w)] <- 0
      prop.table(w)
    }
  })
  structure(out, class = c("silicon_panel", class(out)), margins = margins)
}

#' Draw a panel from a persona data frame
#'
#' Turns rows of a persona data frame (one respondent per row, demographics plus
#' survey or attitude answers) into a `silicon_panel` whose personas can be
#' administered survey items. It is built for frames following the LLMR persona
#' contract, such as `LLMR::anes_2024_personas`: the demographics and the answers
#' are read with [LLMR::llm_persona_split()] (so answers are keyed by their
#' question wording when the frame carries a dictionary), and each persona is
#' rendered as a person to answer in character.
#'
#' Unlike [panel_from_margins()] and [panel_from_data()], the answers travel with
#' each respondent (they are not resampled across people), so a persona's stated
#' views stay internally consistent. The cited `margins` are the demographic
#' distribution of the chosen rows.
#'
#' For a reproducible draw, set a seed before calling (the function never sets
#' one itself).
#'
#' @param data A persona data frame. Defaults to `LLMR::anes_2024_personas`.
#' @param rows Optional row selector: an integer or logical vector, or a predicate
#'   `function(df)` returning a logical vector. Applied before sampling.
#' @param n Optional panel size. With `NULL`, every selected row is used; with a
#'   number, rows are sampled (without replacement when `n` does not exceed the
#'   pool, otherwise with replacement).
#' @return A `silicon_panel`: a tibble with `persona_id`, the demographic columns,
#'   and `persona`.
#' @seealso [LLMR::anes_2024_personas], [panel_administer()].
#' @examples
#' \donttest{
#' if (requireNamespace("LLMR", quietly = TRUE)) {
#'   set.seed(110)
#'   panel <- panel_from_personas(LLMR::anes_2024_personas, n = 8)
#' }
#' }
#' @export
panel_from_personas <- function(data = NULL, rows = NULL, n = NULL) {
  if (is.null(data)) {
    if (!requireNamespace("LLMR", quietly = TRUE))
      abort("Install LLMR (for anes_2024_personas) or pass `data`.")
    data <- LLMR::anes_2024_personas
  }
  stopifnot(is.data.frame(data))
  if (!nrow(data)) abort("`data` must contain at least one row.")

  N <- nrow(data)
  idx <- seq_len(N)
  if (!is.null(rows)) {
    idx <- if (is.function(rows)) which(rows(data))
           else if (is.logical(rows)) which(rows)
           else as.integer(rows)
    idx <- idx[idx >= 1L & idx <= N]
    if (!length(idx)) abort("`rows` selected no respondents.")
  }
  if (!is.null(n)) {
    stopifnot(n >= 1L)
    idx <- sample(idx, n, replace = n > length(idx))
  }

  demo_cols <- if (requireNamespace("LLMR", quietly = TRUE))
    LLMR::llm_persona_demographic_fields(data) else
    intersect(c("age", "sex", "gender", "education", "race", "income"), names(data))
  demo_cols <- intersect(demo_cols, names(data))

  # Render each chosen row as a person who will answer in character. The shared
  # split keys answers by question wording and drops missing/score fields; the
  # framing here is panel-specific (a respondent, not a discussant).
  render_one <- function(row_i) {
    if (requireNamespace("LLMR", quietly = TRUE)) {
      parts <- LLMR::llm_persona_split(data, row_i)
      demo <- parts$demographics; ans <- parts$responses
    } else {
      demo <- vapply(demo_cols, function(c) as.character(data[[c]][row_i]), "")
      ans <- character(0)
    }
    bits <- c(
      if (length(demo))
        sprintf("You are this person: %s.",
                paste(sprintf("%s %s", names(demo), demo), collapse = "; ")),
      if (length(ans))
        sprintf("On a questionnaire you gave these answers: %s.",
                paste(sprintf("%s -- %s", names(ans), ans), collapse = "; ")))
    paste(bits, collapse = " ")
  }

  chosen <- data[idx, , drop = FALSE]
  attrs <- tibble::as_tibble(lapply(chosen[, demo_cols, drop = FALSE], as.character))
  out <- tibble::add_column(attrs, persona_id = seq_along(idx), .before = 1)
  out$persona <- vapply(idx, render_one, character(1))

  margins <- lapply(chosen[, demo_cols, drop = FALSE],
                    function(col) prop.table(table(col[!is.na(col)])))
  structure(out, class = c("silicon_panel", class(out)), margins = margins)
}

#' @export
print.silicon_panel <- function(x, ...) {
  cat(sprintf("<silicon_panel | %d persona(s) | attributes: %s>\n",
              nrow(x), paste(names(attr(x, "margins")), collapse = ", ")))
  cat(sprintf("  e.g. %s\n", x$persona[1]))
  invisible(x)
}

#' @exportS3Method tibble::as_tibble
as_tibble.silicon_panel <- function(x, ...) {
  attr(x, "margins") <- NULL
  class(x) <- setdiff(class(x), "silicon_panel")
  tibble::as_tibble(x, ...)
}
