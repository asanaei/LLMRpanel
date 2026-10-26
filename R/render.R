# render.R ----------------------------------------------------------------------
# Decoding of persona fields into the text a persona states. Every value of a
# rendered field is either shown or left out for a recorded reason; the
# constructors gather those records and report them once per call (a message
# for values left out as missing, a warning for values shown in a form that
# needs checking). The rules follow FocusGroup's survey renderer
# (R/survey_rendering.R in FocusGroup), which LLMRpanel cannot import.

# The labels that ANES and other releases give their missing-data codes, as
# case-insensitive regular expressions. A copy of FocusGroup's
# fg_default_na_strings(); tests/testthat/test-rendering.R checks that the two
# stay identical.
.panel_na_strings <- c(
  "inapplicable", "missing", "refused", "don't know", "don.t know", "not asked",
  "legitimate skip", "no answer", "not applicable", "not available",
  "\\bdk/rf\\b", "^\\s*(-?[0-9]+\\.)?\\s*restricted\\b",
  "^\\s*(-?[0-9]+\\.)?\\s*error\\s*$", "technical error",
  "no post[- ](election )?interview", "interview deleted",
  "^\\s*-[0-9]+\\..*\\bbreak ?off\\b", "^\\s*-[0-9]+\\..*\\binsuffici?ent\\b",
  "separate file", "released separately", "will be (released|available)",
  "not reported")

# Column names that hold case identifiers, matched ignoring case. They are
# never rendered into persona text, nor is a column whose variable label calls
# it a case or respondent ID (the ANES case ID V240001 is labeled "2024 Time
# Series Case ID").
.panel_identifier_names <- c("id", "caseid", "case_id", "respondent_id",
                             "respid", "resp_id", "persona_id", "uid",
                             "record_id", "row_id", "responseid", "response_id",
                             "participant_id", "participantid", "subject_id",
                             "subjectid", "user_id", "userid", "prolific_pid",
                             "workerid", "worker_id", "caseno", "qkey")

.panel_identifier_label <- "\\b(case|respondent) ?id\\b"

# Columns of `data` never rendered: identifiers, the panel's own `persona`
# column, and any further names given (the sampling-weight column,
# `ideology_score`).
.panel_excluded <- function(data, extra = character()) {
  nms <- names(data)
  labelled_id <- vapply(data, function(col) {
    lab <- attr(col, "label", exact = TRUE)
    is.character(lab) && length(lab) == 1L && !is.na(lab) &&
      grepl(.panel_identifier_label, lab, ignore.case = TRUE)
  }, logical(1), USE.NAMES = FALSE)
  nms[tolower(nms) %in% .panel_identifier_names | nms == "persona" |
        nms %in% extra | labelled_id]
}

# `na_strings = NULL` stands for the default vocabulary; `character(0)` for
# none. Patterns must be valid regular expressions.
.panel_check_na_strings <- function(na_strings) {
  if (is.null(na_strings)) return(.panel_na_strings)
  if (!is.character(na_strings) || anyNA(na_strings) || any(!nzchar(na_strings))) {
    abort("`na_strings` must be NULL or a character vector of nonempty regular expressions.")
  }
  valid <- vapply(na_strings, function(p) {
    tryCatch({
      suppressWarnings(grepl(p, "", ignore.case = TRUE))
      TRUE
    }, error = function(e) FALSE)
  }, logical(1))
  if (!all(valid)) {
    abort(sprintf("`na_strings` holds pattern(s) that are not valid regular expressions: %s.",
                  paste0("\"", na_strings[!valid], "\"", collapse = ", ")))
  }
  unname(na_strings)
}

# `questions`: question texts named by column. Names not among the columns are
# reported, since a misspelled name would otherwise leave its column unworded.
.panel_check_questions <- function(questions, columns) {
  if (is.null(questions)) return(NULL)
  nms <- names(questions)
  ok <- is.character(questions) && length(questions) > 0L && !is.null(nms) &&
    !anyNA(nms) && all(nzchar(nms)) && !anyDuplicated(nms) &&
    !anyNA(questions) && all(nzchar(trimws(questions)))
  if (!ok) {
    abort(paste("`questions` must be a character vector of question texts named by",
                "column, with no missing, empty, or repeated entries."))
  }
  unknown <- setdiff(nms, columns)
  if (length(unknown)) {
    warning(sprintf("`questions` names column(s) not in `data`, which are ignored: %s.",
                    paste(unknown, collapse = ", ")), call. = FALSE)
  }
  questions
}

.panel_matches <- function(x, na_strings) {
  if (!length(na_strings) || !length(x)) return(rep(FALSE, length(x)))
  out <- grepl(paste(na_strings, collapse = "|"), x, ignore.case = TRUE)
  out & !is.na(x)
}

# Whether a value or a value label marks an answer as missing. It must
# match `na_strings`, and either carry a missing-data code (negative, or 90
# and above: "-9. Refused", "97. Refused", "998. Don't know (where to rate)")
# or say nothing but the matched wording once its code, interviewer markers
# ("{VOL}", "(VOL)"), and asides in parentheses are set aside ("Refused to
# answer", "I don't know", "Don't know/Refused (VOL.)"). So "2. Refused to take
# sides", "Missing the old system", and "I keep missing the bus" are answers.
# `code` gives a value label's code when it is known; otherwise a code is read
# from a prefix such as "-9. ", "-9 ", "[-9] ", "(-9) ", or "99: ".
.panel_missing_filler <- c("i", "to", "answer", "answers", "the", "a", "an", "of",
                           "this", "that", "question", "questions", "item", "r",
                           "respondent", "or", "and", "skipped", "skip", "by", "for",
                           "na", "n/a", "in", "on", "survey", "interview", "wave",
                           "data", "here", "it", "me", "vol", "volunteered", "dk",
                           "rf", "also", "other")

.panel_code_prefix <- "^\\s*[[(]?(-?[0-9]+)[])]?\\s*[.:)-]?\\s+"

.panel_missing_text <- function(text, na_strings, code = NULL) {
  text <- as.character(text)
  n <- length(text)
  if (!length(na_strings) || !n) return(rep(FALSE, n))
  matched <- .panel_matches(text, na_strings)
  if (!any(matched)) return(matched)
  prefixed <- grepl(.panel_code_prefix, text)
  parsed <- rep(NA_real_, n)
  parsed[prefixed] <- suppressWarnings(as.numeric(sub(paste0(.panel_code_prefix, ".*$"),
                                                      "\\1", text[prefixed])))
  if (is.null(code)) code <- parsed else {
    code <- suppressWarnings(as.numeric(code))
    code[is.na(code)] <- parsed[is.na(code)]
  }
  core <- sub(.panel_code_prefix, "", text)
  core <- gsub("\\{(VOL|SPECIFY)\\}|\\((VOL\\.?|volunteered)\\)|\\[VOL\\]", " ", core,
               ignore.case = TRUE)
  core <- gsub("\\([^)]*\\)|\\[[^]]*\\]", " ", core)
  rest <- gsub(paste(na_strings, collapse = "|"), " ", core, ignore.case = TRUE)
  words <- strsplit(tolower(rest), "[^a-z0-9/']+")
  bare <- vapply(words, function(w) {
    w <- setdiff(w[nzchar(w)], "/")
    !length(setdiff(w, .panel_missing_filler))
  }, logical(1))
  coded <- !is.na(code) & (code < 0 | code >= 90)
  matched & (coded | bare)
}

# Numbers written out in full, to 15 significant digits: 100000, not 1e+05;
# whole numbers without a decimal point. sprintf() is vectorized; format()
# writes the values that sprintf() would give an exponent (1e+15, 1e-07).
.panel_number_text <- function(x) {
  x <- as.vector(x)
  out <- as.character(x)
  shown <- !is.na(x) & is.finite(x)
  if (!any(shown)) return(out)
  text <- sprintf("%.15g", x[shown])
  exponent <- grepl("e", text, fixed = TRUE)
  text[exponent] <- vapply(x[shown][exponent], function(v) {
    format(v, scientific = FALSE, digits = 15, trim = TRUE)
  }, character(1))
  out[shown] <- text
  out
}

# Code prefixes that survey value labels carry: "1. Male", "-9. Refused", and
# haven's "[1] Male". A decimal such as "1.5" has no whitespace after the
# period and is not a prefix.
.panel_dot_prefix <- "^\\s*-?[0-9]+\\.\\s+"
.panel_bracket_prefix <- "^\\s*\\[-?[0-9]+\\]\\s+"

.panel_has_prefix <- function(x) {
  grepl(.panel_dot_prefix, x) | grepl(.panel_bracket_prefix, x)
}

.panel_negative_prefix <- function(x) {
  grepl("^\\s*-[0-9]+\\.\\s+", x) | grepl("^\\s*\\[-[0-9]+\\]\\s+", x)
}

.panel_label_body <- function(x) {
  x <- sub(.panel_bracket_prefix, "", as.character(x))
  trimws(sub(.panel_dot_prefix, "", x))
}

# A top or bottom code states a bound on a number ("80. Age 80 or older",
# "Six or more", "Under $5,000"). Such labels mark a variable that holds
# numbers, not the endpoints of an attitude scale.
.panel_bound_label <- function(x) {
  x <- tolower(.panel_label_body(x))
  comparative <- grepl(paste0(
    "\\b(or|and) (more|older|over|above|higher|greater|less|fewer|younger|",
    "under|below)\\b|^(under|less than|fewer than|more than|over|at least|",
    "at most|up to|below|above)\\b|\\+$"), x)
  number <- grepl(paste0(
    "[0-9]|\\b(one|two|three|four|five|six|seven|eight|nine|ten|eleven|",
    "twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|",
    "twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety|hundred|thousand|",
    "million)\\b"), x)
  comparative & number
}

# The positions in `codes` of the two labeled endpoints of the scale on which
# the unlabeled number `x` lies, or NULL. `codes` are the substantive labeled
# codes and `bound` flags their labels as top or bottom codes. On a scale
# centered on zero the endpoints are a pair -m, m; otherwise they
# are the nearest labeled codes on either side of `x`; no other substantive
# label may lie below the lower endpoint, except negative codes below a scale
# that starts at zero or above; labels above the upper endpoint must lie more
# than the width of the scale beyond it ("99. Haven't thought much about
# this" after a 1 to 7 scale), so a labeled midpoint is not taken for the end
# of a shorter scale. A scale with a negative lower endpoint must be centered
# on zero.
.panel_scale_endpoints <- function(x, codes, bound) {
  m <- .panel_center(codes)
  if (!is.na(m) && abs(x) < m) {
    ends <- match(c(-m, m), codes)
    if (!any(bound[ends])) return(ends)
  }
  below <- codes[codes < x]
  above <- codes[codes > x]
  if (!length(below) || !length(above)) return(NULL)
  lo <- max(below)
  hi <- min(above)
  if (lo < 0 && lo != -hi) return(NULL)
  under <- codes[codes < lo]
  if (length(under) && (lo < 0 || any(under >= 0))) return(NULL)
  beyond <- codes[codes > hi]
  if (any(beyond <= hi + (hi - lo))) return(NULL)
  ends <- match(c(lo, hi), codes)
  if (any(bound[ends])) return(NULL)
  ends
}

# The half-width m of a scale centered on zero, such as one labeled
# "-3. Extremely liberal", "0. Moderate", and "3. Extremely conservative", or
# NA. Every negative code must be mirrored by a positive one, the ends -m and
# m must be labeled with m of at least 2, and any code above m must lie more
# than the width of the scale beyond it. A stray negative label beside a
# short positive scale ("-1. Inap" beside "1. Yes" and "2. No") is no center.
.panel_center <- function(codes) {
  negative <- codes[codes < 0]
  if (!length(negative)) return(NA_real_)
  m <- -min(negative)
  if (m < 2 || !(m %in% codes) || !all(-negative %in% codes)) return(NA_real_)
  if (any(codes > m & codes <= 3 * m)) return(NA_real_)
  m
}

# Decoded values are kept as a list of three parallel character vectors:
# `text`, the text shown (NA when the value is left out); `issue`, NA for a
# value shown as the source gives it, "missing" for a value left out as
# missing, "negative" for an unlabeled negative code left out as missing,
# "negative_label" for the label of a negative code shown as an answer,
# "code" for an unlabeled code shown as the code, and "long" for a label
# exactly 80 characters long; and `detail`, what a report names.
.panel_decoded <- function(n) {
  list(text = rep(NA_character_, n), issue = rep(NA_character_, n),
       detail = rep(NA_character_, n))
}

.panel_take <- function(d, at) {
  lapply(d, function(v) v[at])
}

.panel_put <- function(d, at, part) {
  for (name in names(d)) d[[name]][at] <- part[[name]]
  d
}

# Codes `x` (no missing values) decoded against value labels `labels` named
# by code `codes`. `uncoded` holds substantive labels whose codes are not
# known (factor levels without a code prefix); they count as labeled values
# when deciding whether an unlabeled code is a number or a missing label.
.panel_decode_codes <- function(x, codes, labels, na_strings, numeric_codes = TRUE,
                                uncoded = character()) {
  out <- .panel_decoded(length(x))
  if (!length(x)) return(out)
  code <- if (numeric_codes) .panel_number_text(x) else as.character(x)
  body <- .panel_label_body(labels)
  missing <- .panel_missing_text(labels, na_strings,
                                 code = if (numeric_codes) codes else NULL)
  hit <- match(x, codes)

  labeled <- which(!is.na(hit))
  h <- hit[labeled]
  gone <- missing[h]
  out$issue[labeled[gone]] <- "missing"
  out$detail[labeled[gone]] <- labels[h[gone]]
  empty <- !gone & (is.na(body[h]) | !nzchar(body[h]))
  out$issue[labeled[empty]] <- "missing"
  out$detail[labeled[empty]] <- "an empty value label"
  shown <- !gone & !empty
  out$text[labeled[shown]] <- body[h[shown]]
  # the negative codes of a centered scale ("-3. Far left" beside "3. Far
  # right") are answers, not missing-data codes
  negative <- if (numeric_codes && is.na(.panel_center(codes[!missing]))) {
    shown & x[labeled] < 0
  } else {
    rep(FALSE, length(shown))
  }
  out$issue[labeled[negative]] <- "negative_label"
  out$detail[labeled[negative]] <- labels[h[negative]]
  long <- shown & !negative & nchar(labels[h]) == 80L
  out$issue[labeled[long]] <- "long"
  out$detail[labeled[long]] <- labels[h[long]]

  unlabeled <- which(is.na(hit))
  if (!length(unlabeled)) return(out)
  sub_codes <- codes[!missing]
  sub_labels <- labels[!missing]
  sub_body <- body[!missing]
  if (!numeric_codes) {
    out$text[unlabeled] <- code[unlabeled]
    if (length(sub_codes)) {
      out$issue[unlabeled] <- "code"
      out$detail[unlabeled] <- code[unlabeled]
    }
    return(out)
  }
  bound <- .panel_bound_label(sub_labels)
  held <- sub_codes >= 0
  numbers <- sum(held) + length(uncoded) <= 1L ||
    all(c(bound[held], .panel_bound_label(uncoded)))
  for (j in unlabeled) {
    ends <- .panel_scale_endpoints(x[[j]], sub_codes, bound)
    if (!is.null(ends)) {
      out$text[[j]] <- sprintf("%s on a scale from %s (%s) to %s (%s)", code[[j]],
                               .panel_number_text(sub_codes[ends[1]]), sub_body[ends[1]],
                               .panel_number_text(sub_codes[ends[2]]), sub_body[ends[2]])
      long <- sub_labels[ends][nchar(sub_labels[ends]) == 80L]
      if (length(long)) {
        out$issue[[j]] <- "long"
        out$detail[[j]] <- long[[1]]
      }
    } else if (x[[j]] < 0) {
      out$issue[[j]] <- "negative"
      out$detail[[j]] <- code[[j]]
    } else {
      out$text[[j]] <- code[[j]]
      if (!numbers) {
        out$issue[[j]] <- "code"
        out$detail[[j]] <- code[[j]]
      }
    }
  }
  out
}

# Text values: character values or the levels of a factor. `label` is TRUE
# for factor levels, which are value labels whether or not they carry a code;
# a character value counts as a label when it carries a code prefix.
.panel_decode_text <- function(v, na_strings, label = FALSE) {
  v <- as.character(v)
  out <- .panel_decoded(length(v))
  if (!length(v)) return(out)
  none <- is.na(v) | !nzchar(trimws(v)) | trimws(v) %in% "NA"
  out$issue[none] <- "missing"
  out$detail[none] <- "no value"
  body <- .panel_label_body(v)
  gone <- !none & .panel_missing_text(v, na_strings)
  out$issue[gone] <- "missing"
  out$detail[gone] <- v[gone]
  empty <- !none & !gone & !nzchar(body)
  out$issue[empty] <- "missing"
  out$detail[empty] <- "an empty value label"
  shown <- !none & !gone & !empty
  out$text[shown] <- body[shown]
  negative <- shown & .panel_negative_prefix(v)
  out$issue[negative] <- "negative_label"
  out$detail[negative] <- v[negative]
  stored <- sub(.panel_bracket_prefix, "", v)
  long <- shown & !negative & (label | .panel_has_prefix(v)) & nchar(stored) == 80L
  out$issue[long] <- "long"
  out$detail[long] <- v[long]
  out
}

# Factor levels (or the distinct values of a character column) that encode a
# labeled variable, as haven::as_factor() writes them: labeled codes carry a
# prefix ("1. Fewer services" or "[1] 1. Fewer services"), unlabeled codes
# are bare numbers ("5"), and labels without a code prefix ("Refused" in a
# file whose labels have none) are labels whose code is unknown. Returns the
# kind ("coded", "bare", or "text"), code, and label of every level, or NULL
# when the levels are plain labels or plain numbers.
.panel_coded_levels <- function(lv) {
  lv <- as.character(lv)
  bracket <- grepl("^\\s*\\[-?[0-9]+\\]\\s+\\S", lv)
  dot <- grepl("^\\s*-?[0-9]+\\.\\s+\\S", lv)
  bare <- grepl("^\\s*-?[0-9]+(\\.[0-9]+)?\\s*$", lv)
  coded <- bracket | dot
  text <- !coded & !bare
  if (!any(bare) && !any(coded)) return(NULL)
  if (!any(coded) && !any(text)) return(NULL)
  if (!any(bare) && any(text)) return(NULL)
  codes <- rep(NA_real_, length(lv))
  codes[bracket] <- as.numeric(sub("^\\s*\\[(-?[0-9]+)\\].*$", "\\1", lv[bracket]))
  only_dot <- dot & !bracket
  codes[only_dot] <- as.numeric(sub("^\\s*(-?[0-9]+)\\..*$", "\\1", lv[only_dot]))
  codes[bare] <- as.numeric(lv[bare])
  if (anyDuplicated(codes[!text])) return(NULL)
  labels <- ifelse(bracket, trimws(sub(.panel_bracket_prefix, "", lv)), lv)
  list(kind = ifelse(coded, "coded", ifelse(bare, "bare", "text")),
       codes = codes, labels = labels)
}

# Every level of a structure from .panel_coded_levels(), decoded: coded
# levels as labeled codes, text levels as labels, bare numbers as unlabeled
# codes against the coded labels.
.panel_decode_levels <- function(coded, na_strings) {
  out <- .panel_decoded(length(coded$kind))
  text <- which(coded$kind == "text")
  if (length(text)) {
    out <- .panel_put(out, text, .panel_decode_text(coded$labels[text], na_strings, label = TRUE))
  }
  uncoded <- coded$labels[text][!is.na(out$text[text])]
  number <- which(coded$kind != "text")
  if (length(number)) {
    labeled <- coded$kind == "coded"
    out <- .panel_put(out, number, .panel_decode_codes(
      coded$codes[number], coded$codes[labeled], coded$labels[labeled], na_strings,
      uncoded = uncoded))
  }
  out
}

.panel_is_labelled <- function(col) {
  if (is.factor(col) || !is.atomic(col)) return(FALSE)
  labels <- attr(col, "labels", exact = TRUE)
  !is.null(labels) && length(labels) > 0L && !is.null(names(labels))
}

# A labeled column (haven's haven_labelled class, or any vector with a
# `labels` attribute) decoded at `rows` from its codes and labels, without
# calling haven.
.panel_decode_labelled <- function(col, rows, na_strings) {
  values <- as.vector(unclass(col))[rows]
  labels <- attr(col, "labels", exact = TRUE)
  labels <- labels[!is.na(labels)]
  numeric_codes <- is.numeric(labels) && is.numeric(values)
  codes <- if (numeric_codes) as.numeric(unname(labels)) else as.character(unname(labels))
  # numbers labeled with character codes are matched as text written in full
  x <- if (numeric_codes) {
    values
  } else if (is.numeric(values)) {
    .panel_number_text(values)
  } else {
    as.character(values)
  }
  out <- .panel_decoded(length(x))
  none <- is.na(x)
  if (is.character(x)) none <- none | !nzchar(trimws(x))
  out$issue[none] <- "missing"
  out$detail[none] <- "no value"
  na_values <- attr(col, "na_values", exact = TRUE)
  na_range <- attr(col, "na_range", exact = TRUE)
  declared <- rep(FALSE, length(x))
  if (length(na_values)) declared <- values %in% na_values
  if (is.numeric(values) && length(na_range) == 2L) {
    declared <- declared | (values >= na_range[[1]] & values <= na_range[[2]])
  }
  declared <- !none & declared
  out$issue[declared] <- "missing"
  out$detail[declared] <- "a value declared missing"
  live <- which(!none & !declared)
  if (length(live)) {
    u <- unique(x[live])
    part <- .panel_decode_codes(u, codes, names(labels), na_strings, numeric_codes)
    out <- .panel_put(out, live, .panel_take(part, match(x[live], u)))
  }
  out
}

# Column `col` decoded at `rows` (every row when NULL). Each distinct value is
# decoded once.
.panel_decode <- function(col, rows = NULL, na_strings = .panel_na_strings) {
  if (is.null(rows)) rows <- seq_along(col)
  out <- .panel_decode_any(col, rows, na_strings)
  # a value without text is always recorded, never dropped unseen
  unseen <- is.na(out$text) & is.na(out$issue)
  out$issue[unseen] <- "missing"
  out$detail[unseen] <- "no value"
  out
}

.panel_decode_any <- function(col, rows, na_strings) {
  if (.panel_is_labelled(col)) return(.panel_decode_labelled(col, rows, na_strings))
  if (is.factor(col)) {
    lv <- levels(col)
    coded <- .panel_coded_levels(lv)
    by_level <- if (is.null(coded)) {
      .panel_decode_text(lv, na_strings, label = TRUE)
    } else {
      .panel_decode_levels(coded, na_strings)
    }
    k <- as.integer(col)[rows]
    out <- .panel_take(by_level, k)
    none <- is.na(k)
    out$issue[none] <- "missing"
    out$detail[none] <- "no value"
    return(out)
  }
  if (inherits(col, "haven_labelled") || (is.numeric(col) && !is.object(col))) {
    x <- as.vector(unclass(col))[rows]
    if (is.numeric(x)) {
      out <- .panel_decoded(length(x))
      none <- is.na(x)
      out$text[!none] <- .panel_number_text(x[!none])
      out$issue[none] <- "missing"
      out$detail[none] <- "no value"
      return(out)
    }
    v <- as.character(x)
    whole <- as.character(as.vector(unclass(col)))
  } else {
    v <- as.character(col[rows])
    whole <- as.character(col)
  }
  # A character column that mixes coded labels with bare numbers, as
  # as.character(haven::as_factor()) writes it, is decoded like that factor.
  u <- unique(whole[!is.na(whole)])
  coded <- .panel_coded_levels(u)
  if (!is.null(coded) && any(coded$kind == "coded") && any(coded$kind == "bare")) {
    return(.panel_take(.panel_decode_levels(coded, na_strings), match(v, u)))
  }
  u <- unique(v)
  .panel_take(.panel_decode_text(u, na_strings), match(v, u))
}

# One value of a column, decoded: a list of `text`, `issue`, and `detail`.
.panel_value <- function(col, i, na_strings = .panel_na_strings) {
  .panel_take(.panel_decode(col, i, na_strings), 1L)
}

# A whole column decoded, as a factor of the texts the personas state (NA
# where a value is left out). The levels follow the column's own order: a
# factor's levels, or the sorted values.
.panel_decode_column <- function(col, na_strings) {
  text <- .panel_decode(col, NULL, na_strings)$text
  if (is.factor(col)) {
    lv <- levels(col)
    lv_text <- .panel_decode(factor(lv, levels = lv), NULL, na_strings)$text
    return(factor(text, levels = unique(lv_text[!is.na(lv_text)])))
  }
  key <- if (is.atomic(col)) as.vector(unclass(col)) else seq_along(col)
  first <- which(!duplicated(key) & !is.na(key))
  first <- first[order(key[first])]
  factor(text, levels = unique(text[first][!is.na(text[first])]))
}

# The fields a persona frame renders, in order: its demographic fields, then
# its stated answers, each with the text it is shown under. Identifiers,
# `persona`, `ideology_score`, and the names in `exclude` are never rendered.
# An answer is "unworded" when neither `questions` nor the frame's dictionary
# gives a question other than the column name.
.panel_persona_fields <- function(frame, exclude = character(), questions = NULL) {
  nms <- names(frame)
  out <- .panel_excluded(frame, c(exclude, "ideology_score"))
  demo <- intersect(LLMR::llm_persona_demographic_fields(frame), nms)
  af <- attr(frame, "answer_fields")
  ans <- if (is.null(af)) setdiff(nms, demo) else setdiff(intersect(af, nms), demo)
  demo <- setdiff(demo, out)
  ans <- setdiff(ans, out)
  fields <- c(demo, ans)
  dict <- attr(frame, "dictionary")
  question <- if (is.data.frame(dict) && all(c("handle", "question") %in% names(dict))) {
    as.character(dict$question[match(fields, dict$handle)])
  } else {
    rep(NA_character_, length(fields))
  }
  given <- fields %in% names(questions)
  question[given] <- unname(questions[fields[given]])
  blank <- is.na(question) | !nzchar(trimws(question))
  worded <- !blank & question != fields
  question[blank] <- fields[blank]
  data.frame(field = fields,
             kind = rep(c("demographic", "answer"), c(length(demo), length(ans))),
             question = question, worded = worded, stringsAsFactors = FALSE)
}

# The decoded values of `columns` at `rows`, decoded once for all the personas
# of a call.
.panel_prepare <- function(data, columns, rows, na_strings) {
  rows <- unique(rows)
  decoded <- lapply(columns, function(f) .panel_decode(data[[f]], rows, na_strings))
  list(columns = columns, rows = rows, decoded = decoded)
}

# The values of one row from a preparation: texts, issues, and details by
# column.
.panel_row <- function(prepared, row_i) {
  k <- match(row_i, prepared$rows)
  pick <- function(name) {
    vapply(prepared$decoded, function(d) d[[name]][[k]], character(1))
  }
  list(text = pick("text"), issue = pick("issue"), detail = pick("detail"))
}

# Record of the values of one persona that were left out or need checking.
.panel_record <- function(row_i, columns, values) {
  keep <- !is.na(values$issue)
  if (!any(keep)) return(NULL)
  data.frame(row = row_i, field = columns[keep], issue = values$issue[keep],
             detail = values$detail[keep], stringsAsFactors = FALSE)
}

# Text of a persona as background ("You are this person: ...") and stated
# answers ("On a questionnaire you gave these answers: ..."), with the record
# of left-out and flagged values in the attribute "rendering". The
# constructors pass `prepared` (from .panel_prepare()) so that each column is
# decoded once per call rather than once per persona.
.render_persona_text <- function(frame, row_i, na_strings = .panel_na_strings,
                                 exclude = character(), questions = NULL,
                                 fields = NULL, prepared = NULL) {
  if (is.null(fields)) fields <- .panel_persona_fields(frame, exclude, questions)
  if (is.null(prepared)) prepared <- .panel_prepare(frame, fields$field, row_i, na_strings)
  values <- .panel_row(prepared, row_i)
  text <- values$text
  demo <- fields$kind == "demographic" & !is.na(text)
  ans <- fields$kind == "answer" & !is.na(text)
  bits <- c(
    if (any(demo))
      sprintf("You are this person: %s.",
              paste(fields$question[demo], text[demo], collapse = "; ")),
    if (any(ans))
      sprintf("On a questionnaire you gave these answers: %s.",
              paste(sprintf("%s -- %s", fields$question[ans], text[ans]), collapse = "; ")))
  out <- paste(bits, collapse = " ")
  attr(out, "rendering") <- .panel_record(row_i, fields$field, values)
  out
}

# The flat "column: value" text that panel_from_data() renders for a data frame
# without a template.
.panel_flat_text <- function(row_i, prepared) {
  values <- .panel_row(prepared, row_i)
  shown <- !is.na(values$text)
  out <- paste(sprintf("%s: %s", prepared$columns[shown], values$text[shown]),
               collapse = "; ")
  attr(out, "rendering") <- .panel_record(row_i, prepared$columns, values)
  out
}

# Placeholders a template uses.
.panel_template_keys <- function(template) {
  tags <- regmatches(template, gregexpr("\\{[^{}]+\\}", template))[[1]]
  unique(substring(tags, 2L, nchar(tags) - 1L))
}

# A template filled with the decoded values of one row; `prepared` holds the
# columns the template uses. A value left out is filled with nothing and
# recorded.
.panel_template_text <- function(row_i, template, prepared) {
  values <- .panel_row(prepared, row_i)
  filled <- values$text
  filled[is.na(filled)] <- ""
  out <- .fill(template, as.list(stats::setNames(filled, prepared$columns)))
  attr(out, "rendering") <- .panel_record(row_i, prepared$columns, values)
  out
}

# The text of each rendered persona without its record.
.panel_texts <- function(rendered) {
  vapply(rendered, function(x) as.character(x)[[1]], character(1), USE.NAMES = FALSE)
}

.panel_list_items <- function(x, n = 8L) {
  x <- unique(x)
  shown <- paste(utils::head(x, n), collapse = "; ")
  if (length(x) > n) shown <- sprintf("%s; and %d more", shown, length(x) - n)
  shown
}

# Report the records of a set of rendered personas: one warning for each kind
# of value shown in a form that needs checking, one message for the values
# left out as missing. With `template`, missing values are a warning, since a
# template is filled with nothing where a value is missing.
.panel_rendering_conditions <- function(rendered, template = FALSE) {
  records <- lapply(rendered, attr, which = "rendering", exact = TRUE)
  rec <- do.call(rbind, Filter(Negate(is.null), records))
  texts <- .panel_texts(rendered)
  empty <- sum(!nzchar(trimws(texts)))
  if (empty) {
    warning(sprintf(paste(
      "%d persona(s) have no text: every rendered field was missing or is",
      "never rendered (an identifier or the weight column)."), empty), call. = FALSE)
  }
  if (is.null(rec) || !nrow(rec)) return(invisible(NULL))
  describe <- function(issue) {
    r <- rec[rec$issue %in% issue, , drop = FALSE]
    if (!nrow(r)) return(character())
    by_field <- split(r$detail, factor(r$field, levels = unique(r$field)))
    vapply(names(by_field), function(f) {
      sprintf("%s (%s)", f, paste(unique(by_field[[f]]), collapse = ", "))
    }, character(1), USE.NAMES = FALSE)
  }
  code <- describe("code")
  if (length(code)) {
    warning("Shown as bare codes because their column has no value label for them: ",
            .panel_list_items(code), ". Label or decode these values.", call. = FALSE)
  }
  long <- describe("long")
  if (length(long)) {
    warning(paste("Labels exactly 80 characters long, the length at which Stata files",
                  "cut value labels, may have been cut short: "),
            .panel_list_items(long), ". Supply the full labels.", call. = FALSE)
  }
  negative_label <- describe("negative_label")
  if (length(negative_label)) {
    warning(paste(
      "Labels of negative codes are shown as answers, though the ANES and other",
      "releases use negative codes for missing data: "),
      .panel_list_items(negative_label),
      ". Add their wording to `na_strings` to leave them out.", call. = FALSE)
  }
  negative <- describe("negative")
  if (length(negative)) {
    warning("Negative codes without a value label were taken as missing and left out: ",
            .panel_list_items(negative), ". Label them if they are answers.", call. = FALSE)
  }
  missing <- rec[rec$issue %in% "missing", , drop = FALSE]
  if (nrow(missing)) {
    counts <- table(factor(missing$field, levels = unique(missing$field)))
    listed <- .panel_list_items(sprintf("%s (%d)", names(counts), as.integer(counts)))
    if (template) {
      warning(sprintf(paste(
        "%d template value(s) are missing (no value or a missing-data label),",
        "and the template shows nothing in their place: %s. Leave out these rows",
        "or render without a template."), nrow(missing), listed), call. = FALSE)
    } else {
      message(sprintf(paste(
        "%d value(s) were left out of the persona text because they are missing",
        "(no value or a missing-data label): %s."), nrow(missing), listed))
    }
  }
  invisible(rec)
}

# Warn when column names stand in for the questions of stated answers.
.panel_wording_conditions <- function(fields) {
  unworded <- fields$field[fields$kind == "answer" & !fields$worded]
  if (!length(unworded)) return(invisible(NULL))
  warning(sprintf(paste(
    "No question wording for %d answer field(s); their column names stand in for",
    "the questions: %s. Supply the wording with `questions` in",
    "panel_from_personas() or as_persona_frame()."),
    length(unworded), .panel_list_items(unworded)), call. = FALSE)
  invisible(unworded)
}
