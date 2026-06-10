# panel.R -----------------------------------------------------------------------
# Persona panels from population margins. The package ships no demographic
# data and no "default Americans": you supply the margins (from ACS, ANES,
# CES, a census table), and what you supplied is what the report cites.

#' Draw a persona panel from population margins
#'
#' Samples `n` personas with attributes drawn independently from the
#' supplied margins, and renders each persona's text from a template.
#' Independence across attributes is a deliberate 0.1 simplification --
#' attributes are sampled marginally, not jointly; joint draws from
#' microdata (and raking to known cross-tabs) arrive in 0.2. For instrument
#' pretesting this rarely matters; for anything resembling estimation it
#' does, and [calibrate()] will tell you.
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
#'   list(age   = c("18-34" = .3, "35-64" = .45, "65+" = .25),
#'        party = c(left = .45, right = .45, independent = .10)),
#'   n = 50,
#'   persona_template = "A {age}-year-old voter who leans {party}."
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

#' @export
print.silicon_panel <- function(x, ...) {
  cat(sprintf("<silicon_panel | %d persona(s) | attributes: %s>\n",
              nrow(x), paste(names(attr(x, "margins")), collapse = ", ")))
  cat(sprintf("  e.g. %s\n", x$persona[1]))
  invisible(x)
}

