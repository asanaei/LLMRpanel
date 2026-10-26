# panel.R -----------------------------------------------------------------------
# Persona panels from population margins, microdata rows, and persona data
# frames. The package ships no population margins and no "default Americans":
# you supply the margins (from ACS, ANES, CES, a census table) or the rows,
# and what you supplied is what the report cites. Its one dataset,
# anes_2024_personas, is 100 diversity-selected respondents with no weights.

# Restrict a persona_frame to a subset of columns, preserving the contract
# attributes (dictionary / demographic_fields / answer_fields) for the kept
# columns.
.restrict_persona_frame <- function(frame, columns) {
  keep <- intersect(columns, names(frame))
  out <- frame[, keep, drop = FALSE]
  dict <- attr(frame, "dictionary")
  if (!is.null(dict)) attr(out, "dictionary") <- dict[dict$handle %in% keep, , drop = FALSE]
  df <- attr(frame, "demographic_fields")
  if (!is.null(df)) attr(out, "demographic_fields") <- intersect(df, keep)
  af <- attr(frame, "answer_fields")
  if (!is.null(af)) attr(out, "answer_fields") <- intersect(af, keep)
  class(out) <- unique(c("persona_frame", setdiff(class(frame), "persona_frame")))
  out
}

# Base subsetting (`x[, j]`, `x[i, j]`, `subset()`) drops the attributes that
# describe anes_2024_personas, after which its demographics would be rendered
# as questionnaire answers under their raw handles. When `data` has no
# dictionary and every column is a column of the package's copy, the
# dictionary and demographic fields are taken back from that copy, with a
# message.
.panel_restore_dictionary <- function(data) {
  if (!is.null(attr(data, "dictionary"))) return(data)
  ref <- LLMRpanel::anes_2024_personas
  cols <- names(data)
  if (!length(cols) || !all(cols %in% names(ref))) return(data)
  dict <- attr(ref, "dictionary")
  attr(data, "dictionary") <- dict[dict$handle %in% cols, , drop = FALSE]
  attr(data, "demographic_fields") <- intersect(attr(ref, "demographic_fields"), cols)
  message(paste("`data` has lost the dictionary of anes_2024_personas (base subsetting",
                "drops it); its question wording and demographic fields were taken from",
                "the package's copy."))
  data
}

# The panel constructors create `persona_id` and `persona`; an input column of
# either name would be silently overwritten downstream. Abort early instead.
.check_reserved_names <- function(nms, what) {
  bad <- intersect(c("persona_id", "persona"), nms)
  if (length(bad)) {
    abort(sprintf(
      "%s must not use the reserved name(s) %s; the panel creates its own `persona_id` and `persona` columns. Rename the input.",
      what, paste(sprintf("'%s'", bad), collapse = ", ")))
  }
  invisible(nms)
}

#' Draw a persona panel from population margins
#'
#' Samples `n` personas with attributes drawn independently from the
#' supplied margins, and renders each persona's text from a template.
#' Attributes are sampled independently, so the result does not preserve their
#' joint distribution. Use [panel_from_data()] to sample complete microdata
#' rows.
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
  .check_reserved_names(names(margins), "`margins`")
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
            margins = margins, source = "margins")
}

#' Draw a persona panel from microdata rows
#'
#' Samples rows from a data frame with replacement, which preserves the
#' joint distribution of the selected attributes, and renders each sampled
#' row as a persona. This is the joint-distribution counterpart of
#' [panel_from_margins()], which samples attributes independently. The
#' margins the report cites are computed from the source data, one
#' `prop.table(table())` per selected column over its values as the personas
#' state them, so a missing-data label is not a category.
#'
#' Without a template, a plain data frame renders as "attribute: value" pairs,
#' and a `persona_frame` (see [as_persona_frame()]) renders as background and
#' stated answers, as [panel_from_personas()] does. Values are written as
#' text by the rules under "Rendering of values" in [panel_from_personas()].
#'
#' For a reproducible panel, set a seed before calling (the function never
#' sets one itself).
#'
#' @param data A data frame, one row per source case.
#' @param n Panel size.
#' @param persona_template Text with `{attribute}` placeholders rendered
#'   per persona. `NULL` builds a plain "attribute: value" persona. A
#'   placeholder whose value is left out as missing is filled with nothing,
#'   and a placeholder may not name an identifier or the `weights` column.
#' @param columns Attribute columns to keep. Defaults to every column
#'   except the `weights` column when one is given. Identifier columns are
#'   kept in the panel but never rendered into the persona text.
#' @param weights Optional name of a single column of nonnegative sampling
#'   weights (rows are drawn with probability proportional to it). The column
#'   is never rendered into the persona text.
#' @inheritParams panel_from_personas
#' @return A `silicon_panel`: a tibble with `persona_id`, the selected
#'   attribute columns, and `persona`. Its `source_rows` attribute gives, for
#'   each persona in `persona_id` order, the row of `data` it was drawn from.
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
                            columns = NULL, weights = NULL, na_strings = NULL) {
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
  .check_reserved_names(columns, "`columns`")
  na <- .panel_check_na_strings(na_strings)
  weight_col <- weights %||% character(0)
  never <- .panel_excluded(data, c(weight_col, "ideology_score"))
  if (!is.null(persona_template)) {
    blocked <- intersect(.panel_template_keys(persona_template), never)
    if (length(blocked)) {
      abort(sprintf(paste(
        "`persona_template` uses %s, which holds case identifiers, sampling",
        "weights, or `ideology_score`; such columns are never rendered into",
        "persona text."),
        paste0("{", blocked, "}", collapse = ", ")))
    }
  }

  # Rich rendering (question wording, demographics and answers apart) is
  # opt-in: it applies only to a `persona_frame` without a template, so a plain
  # data frame renders as "attribute: value" pairs. The persona text is drawn
  # from the source row (which carries the contract), restricted to the
  # selected `columns`.
  use_rich <- is.null(persona_template) && inherits(data, "persona_frame")
  if (use_rich) {
    src <- .restrict_persona_frame(data, columns)
    fields <- .panel_persona_fields(src, exclude = weight_col)
    shown <- fields$field
  } else {
    shown <- setdiff(columns, never)
  }
  if (is.null(persona_template) && !length(shown)) {
    abort(paste(
      "`columns` selects only columns that are never rendered into persona text",
      "(identifiers, the weight column, or `ideology_score`); select at least",
      "one attribute column."))
  }

  idx <- sample(seq_len(nrow(data)), n, replace = TRUE, prob = prob)
  attrs <- as.data.frame(data)[idx, columns, drop = FALSE]
  out <- tibble::as_tibble(attrs)
  out <- tibble::add_column(out, persona_id = seq_len(n), .before = 1)

  if (use_rich) {
    .panel_wording_conditions(fields)
    prepared <- .panel_prepare(src, fields$field, idx, na)
    rendered <- lapply(idx, function(r) {
      .render_persona_text(src, r, na_strings = na, exclude = weight_col,
                           fields = fields, prepared = prepared)
    })
  } else if (is.null(persona_template)) {
    prepared <- .panel_prepare(data, shown, idx, na)
    rendered <- lapply(idx, function(r) .panel_flat_text(r, prepared))
  } else {
    unknown <- setdiff(.panel_template_keys(persona_template), columns)
    if (length(unknown)) {
      warning(sprintf(paste("`persona_template` names %s, not among `columns`; the",
                            "placeholder is left in the text as written."),
                      paste0("{", unknown, "}", collapse = ", ")), call. = FALSE)
    }
    keys <- intersect(.panel_template_keys(persona_template), columns)
    prepared <- .panel_prepare(data, keys, idx, na)
    rendered <- lapply(idx, function(r) {
      .panel_template_text(r, persona_template, prepared)
    })
  }
  out$persona <- .panel_texts(rendered)
  .panel_rendering_conditions(rendered, template = !is.null(persona_template))
  # The cited margins are the source distribution the panel was drawn from,
  # over the values as the personas would state them (missing-data labels are
  # not categories). With sampling weights, that distribution is the WEIGHTED
  # one; an unweighted prop.table(table(col)) would misreport the population
  # the panel represents.
  margins <- lapply(stats::setNames(columns, columns), function(cl) {
    col <- .panel_decode_column(data[[cl]], na)
    if (is.null(prob)) {
      prop.table(table(col))
    } else {
      w <- tapply(prob, col, sum)
      w[is.na(w)] <- 0
      prop.table(w)
    }
  })
  structure(out, class = c("silicon_panel", class(out)), margins = margins,
            source = "microdata", source_rows = as.integer(idx))
}

#' Draw a panel from a persona data frame
#'
#' Turns rows of a persona data frame (one respondent per row, demographics plus
#' survey or attitude answers) into a `silicon_panel` whose personas can be
#' administered survey items. It is built for frames following the LLMR persona
#' contract, such as the bundled [anes_2024_personas]: the demographic fields
#' are those that [LLMR::llm_persona_demographic_fields()] names, every other
#' column is a stated answer, answers are shown under their question wording
#' when the frame carries a dictionary or `questions` gives one, and each
#' persona is rendered as a person to answer in character ("You are this
#' person: ... On a questionnaire you gave these answers: ...").
#'
#' Unlike [panel_from_margins()] and [panel_from_data()], this constructor keeps
#' each selected respondent's answers together. The `margins` attribute contains
#' the demographic distribution of the selected rows, and the `source_rows`
#' attribute gives, for each persona in `persona_id` order, the row of `data`
#' it was drawn from. A persona can therefore be traced to its source record
#' with `attr(panel, "source_rows")[panel$persona_id]`, which still holds after
#' the panel's rows are subset.
#'
#' For a reproducible draw, set a seed before calling (the function never sets
#' one itself).
#'
#' @section Rendering of values:
#' Each value of a rendered field is written as text by these rules.
#'
#' * A labeled column, such as `haven::read_dta()` and `haven::read_sav()`
#'   return, is decoded from its `labels` attribute; haven itself is not
#'   needed. A labeled code is shown as its label without the code prefix
#'   ("1. Male" becomes "Male"). A code without a label on a scale labeled only
#'   at its ends is shown with the scale, as in "5 on a scale from 1 (Fewer
#'   services) to 7 (More services)"; on a scale centered on zero the ends are
#'   the labeled codes -m and m, even when its midpoint is labeled too. A
#'   negative code without a label is left out as missing, with a warning. Any other code without a label is shown
#'   as the code, with a warning, unless the column labels at most one
#'   nonnegative code or only top and bottom codes ("80. Age 80 or older"), in
#'   which case the column holds numbers and the value is shown as one.
#'   Values that an SPSS file declares missing (`na_values`, `na_range`) are
#'   left out.
#' * A factor's levels are its labels. Levels as `haven::as_factor()` writes
#'   them ("1. Fewer services", "\[1\] 1. Fewer services", and bare numbers
#'   for unlabeled codes) are decoded by the rules above; a level without a
#'   code prefix ("Refused" from a file whose labels carry none) counts as a
#'   label whose code is unknown. A character column that mixes coded labels
#'   with bare numbers, as `as.character(haven::as_factor())` writes it, is
#'   decoded the same way. Code prefixes are removed from factor levels and
#'   from character values.
#' * `NA`, empty values, and missing-data labels are left out. A value or a
#'   value label is one when it matches `na_strings` and either carries a
#'   missing-data code (negative, or 90 and above: "-9 Refused", "998. Don't
#'   know") or says nothing but the matched wording once codes, interviewer
#'   markers, and asides in parentheses are set aside ("Refused to answer",
#'   "Don't know (VOL)"),
#'   as FocusGroup decides, so a free-text answer such as "I refused to answer
#'   the door" is kept. A value with a
#'   negative code prefix that does not match `na_strings` is shown, with a
#'   warning, since many releases use negative codes for missing data; the
#'   labels of negative codes on a scale centered on zero ("-3. Far left"
#'   beside "3. Far right") draw none.
#' * Numbers are written in full: 100000, not 1e+05.
#' * A label exactly 80 characters long draws a warning. Stata files cut value
#'   labels at 80 characters, and the ANES 2024 release has labels cut there.
#' * Identifier columns (named, ignoring case, `id`, `caseid`, `case_id`,
#'   `respondent_id`, `respid`, `resp_id`, `persona_id`, `uid`, `record_id`,
#'   or `row_id`, or with a variable label that calls them a case or
#'   respondent ID, as the ANES label of V240001 does), the column named by
#'   `weights`, a column named `persona`, and `ideology_score` are never
#'   rendered.
#'
#' The values left out as missing are reported in one message per call,
#' naming each field and the number of personas whose text omits it. A stated
#' answer shown under its column name, for want of a question wording, draws a
#' warning.
#'
#' @param data A persona data frame, such as [anes_2024_personas]. Those 100
#'   rows are diversity-selected United States respondents and carry no survey
#'   weights, so they stand for no population.
#' @param n Optional panel size. With `NULL`, every selected row is used; with a
#'   number, rows are sampled (without replacement when `n` does not exceed the
#'   pool, otherwise with replacement).
#' @param rows Optional row selector: a numeric vector of whole-number row
#'   positions, a logical vector with one entry per row, or a predicate
#'   `function(df)` returning such a logical vector. Applied before sampling.
#'   Character and factor selectors are refused, since a factor's codes are
#'   not row positions.
#' @param weights Optional survey weights for the draw: a column name in `data`,
#'   or a numeric vector aligned to the selected rows. Used only when `n` is
#'   given. `NULL` (default) draws uniformly. A weight column is never
#'   rendered into the persona text.
#' @param questions Optional named character vector of question texts keyed
#'   by column name, as in
#'   `c(services = "Should the government provide fewer or more services?")`.
#'   It takes precedence over the question in the frame's dictionary. Names
#'   that are not columns of `data` draw a warning.
#' @param na_strings Regular expressions, matched ignoring case against each
#'   value and value label, that mark it as missing. `NULL` (the default) uses
#'   the patterns of FocusGroup's `fg_default_na_strings()`, which match the
#'   missing-data labels of the ANES and similar releases: "Inapplicable",
#'   "Refused", "Don't know", "Not asked", "DK/RF", "No post-election
#'   interview", "Insufficient partial, interview deleted", "RESTRICTED",
#'   "Error", "Technical error", "Not reported", and the like. `character(0)`
#'   treats no label as missing. Answers that use such words ("2. Refused to
#'   take sides", "I keep missing the bus") are kept.
#' @return A `silicon_panel`: a tibble with `persona_id`, the demographic
#'   columns as the persona text gives them (`NA` where a value is left out),
#'   and `persona`, with the attributes `margins`, `source`, and `source_rows`.
#' @seealso [anes_2024_personas], [as_persona_frame()], [panel_administer()].
#' @examples
#' set.seed(110)
#' panel <- panel_from_personas(anes_2024_personas, n = 8)
#' panel
#'
#' # A labeled column, as haven::read_dta() returns it, is decoded from its
#' # labels: the unlabeled 5 is placed on its scale and -9 is left out.
#' survey <- data.frame(age = c("18-29", "65 or older", "30-49"))
#' survey$services <- structure(c(1, 5, -9), labels = c(
#'   "1. Government should provide many fewer services" = 1,
#'   "7. Government should provide many more services" = 7,
#'   "-9. Refused" = -9))
#' labeled_panel <- panel_from_personas(
#'   survey, questions = c(services = "Government services and spending"))
#' labeled_panel$persona
#' @export
panel_from_personas <- function(data, n = NULL, rows = NULL,
                                weights = NULL, questions = NULL,
                                na_strings = NULL) {
  if (missing(data) || is.null(data)) {
    abort(paste(
      "`data` is required. For the bundled example personas pass",
      "anes_2024_personas explicitly (100 diversity-selected United",
      "States respondents, no survey weights, not representative)."))
  }
  stopifnot(is.data.frame(data))
  if (!nrow(data)) abort("`data` must contain at least one row.")
  data <- .panel_restore_dictionary(data)
  na <- .panel_check_na_strings(na_strings)
  questions <- .panel_check_questions(questions, names(data))

  N <- nrow(data)
  idx <- seq_len(N)
  if (!is.null(rows)) {
    if (is.function(rows)) {
      flag <- rows(data)
      if (!is.logical(flag) || length(flag) != N) {
        abort("A `rows` predicate must return one logical per row of `data`.")
      }
      if (anyNA(flag)) {
        warning(sprintf(paste("The `rows` predicate returned NA for %d row(s); they",
                              "are left out."), sum(is.na(flag))), call. = FALSE)
      }
      idx <- which(flag)
    } else if (is.logical(rows)) {
      if (length(rows) != N) {
        abort("A logical `rows` selector must have one entry per row of `data`.")
      }
      if (anyNA(rows)) {
        warning(sprintf(paste("The logical `rows` selector is NA for %d row(s); they",
                              "are left out."), sum(is.na(rows))), call. = FALSE)
      }
      idx <- which(rows)
    } else {
      if (!is.numeric(rows) || anyNA(rows) || any(rows != floor(rows)) ||
          any(rows < 1) || any(rows > N)) {
        abort("`rows` indices must be whole numbers between 1 and nrow(data), with no NA.")
      }
      idx <- as.integer(rows)
    }
    if (!length(idx)) abort("`rows` selected no respondents.")
  }

  # resolve weights to a probability vector over the SELECTED rows.
  prob <- NULL
  if (!is.null(weights) && is.null(n)) {
    cli::cli_warn(paste(
      "`weights` are used only when `n` draws a sample; with `n = NULL`",
      "every selected row enters the panel and the weights are ignored."))
  }
  weight_col <- character(0)
  if (!is.null(weights)) {
    w <- if (is.character(weights) && length(weights) == 1L) {
      if (!(weights %in% names(data))) abort("`weights` must name a column in `data`.")
      weight_col <- weights
      col <- data[[weights]]
      if (!is.numeric(col)) {
        abort("`weights` must name a numeric column (a factor would be read as its codes).")
      }
      as.numeric(col)[idx]
    } else {
      if (!is.numeric(weights)) abort("`weights` must be numeric.")
      as.numeric(weights)
    }
    if (length(w) != length(idx)) abort("`weights` length must match the selected rows.")
    if (anyNA(w) || any(w < 0)) {
      abort("`weights` must be nonnegative with no NA; repair them rather than have them silently zeroed.")
    }
    if (sum(w) <= 0) abort("`weights` must have positive total weight.")
    prob <- w / sum(w)
  }

  if (!is.null(n)) {
    if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1 || n != floor(n)) {
      abort("`n` must be one whole number of at least 1.")
    }
    n <- as.integer(n)
    pool <- length(idx)
    if (n > pool) {
      warning(sprintf(paste0(
        "Drawing %d personas from %d distinct respondent(s) (~%.0f-fold ",
        "duplication). Diversity is capped by the source; supply a larger frame ",
        "or use panel_from_margins() for more distinct respondents."),
        n, pool, n / pool), call. = FALSE)
    }
    # Draw positions, then index: sample() on a single number x would draw
    # from 1:x instead of returning x. For two or more rows the draws and the
    # random-number stream are identical to sample(idx, ...).
    idx <- idx[sample.int(pool, n, replace = n > pool, prob = prob)]
  }

  .check_reserved_names(intersect(LLMR::llm_persona_demographic_fields(data), names(data)),
                        "The demographic columns of `data`")
  fields <- .panel_persona_fields(data, exclude = weight_col, questions = questions)
  demo_cols <- fields$field[fields$kind == "demographic"]
  if (!length(demo_cols)) {
    cli::cli_inform(paste(
      "No demographic columns were recognized in `data`; personas render from",
      "their stated answers alone, and the panel cites no demographic margins."))
  }
  .panel_wording_conditions(fields)

  # The panel's demographic columns hold the values as the persona text gives
  # them, so the cited margins count the categories the personas state.
  attrs <- tibble::as_tibble(lapply(stats::setNames(demo_cols, demo_cols), function(f) {
    as.character(.panel_decode_column(data[[f]], na))[idx]
  }), .rows = length(idx))
  out <- tibble::add_column(attrs, persona_id = seq_along(idx), .before = 1)
  prepared <- .panel_prepare(data, fields$field, idx, na)
  rendered <- lapply(idx, function(r) {
    .render_persona_text(data, r, na_strings = na, exclude = weight_col,
                         questions = questions, fields = fields, prepared = prepared)
  })
  out$persona <- .panel_texts(rendered)
  .panel_rendering_conditions(rendered)

  margins <- lapply(attrs[demo_cols], function(col) prop.table(table(col[!is.na(col)])))
  structure(out, class = c("silicon_panel", class(out)), margins = margins,
            source = "personas", source_rows = as.integer(idx))
}

#' @export
print.silicon_panel <- function(x, ...) {
  cat(sprintf("<silicon_panel | %d persona(s) | attributes: %s>\n",
              nrow(x), paste(names(attr(x, "margins")), collapse = ", ")))
  preview <- as.character(x$persona[1])
  if (nchar(preview) > 160L) preview <- paste0(substr(preview, 1L, 157L), "...")
  cat(sprintf("  e.g. %s\n", preview))
  invisible(x)
}

#' @exportS3Method tibble::as_tibble
as_tibble.silicon_panel <- function(x, ...) {
  attr(x, "margins") <- NULL
  attr(x, "source") <- NULL
  attr(x, "source_rows") <- NULL
  class(x) <- setdiff(class(x), "silicon_panel")
  tibble::as_tibble(x, ...)
}
