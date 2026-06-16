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
