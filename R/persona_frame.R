# persona_frame.R -----------------------------------------------------------
# A thin, classed wrapper that attaches the LLMR persona contract to a data
# frame so panel_from_data() will render rich persona text (question wording,
# demographics and answers separated) instead of a flat "key: value" string.
# Rich rendering is opt-in by construction: only a `persona_frame` triggers it,
# never a plain data frame that happens to carry attributes, so existing
# panel_from_data() output is unchanged.

#' Attach the persona contract to a data frame
#'
#' Attaches persona metadata to a decoded data frame. When a `persona_frame` is
#' passed to [panel_from_data()] without a template, demographic fields and
#' stated answers are rendered separately. A question map supplies the wording
#' used for answer fields. Plain data frames use the flat key-value rendering.
#'
#' @param data A decoded data frame, one respondent per row. Values should
#'   already be human-readable labels (decode a labelled survey file with, for
#'   example, `haven::as_factor()` first).
#' @param questions Optional named character vector mapping column names to the
#'   human question wording, e.g.
#'   `c(pid = "Party identification", ab = "Abortion position")`. Columns absent
#'   from this map keep their column name. Without it the column names stand in
#'   for the questions, which is formatting, not a faithful translation.
#' @param demographics Optional character vector of columns to treat as
#'   demographic background (the rest become stated answers). Defaults to the
#'   common demographic names found in `data`.
#' @param answers Optional character vector restricting which columns may appear
#'   as stated answers. Defaults to every column that is not a demographic, an
#'   `id`-named, or a `weight`-named column, so analysis-only columns do not leak
#'   into the prompt.
#' @return `data` with the persona contract attached and class `persona_frame`.
#' @seealso [panel_from_data()], [panel_from_personas()],
#'   [LLMR::llm_persona_split()].
#' @examples
#' df <- data.frame(
#'   age = c("35-44", "65+"),
#'   pid = c("Strong Democrat", "Strong Republican"),
#'   ab  = c("Always legal", "Never legal"))
#' pf <- as_persona_frame(
#'   df,
#'   questions = c(pid = "Party identification", ab = "Abortion position"),
#'   demographics = "age")
#' @export
as_persona_frame <- function(data, questions = NULL, demographics = NULL,
                             answers = NULL) {
  stopifnot(is.data.frame(data))
  if (!nrow(data)) abort("`data` must contain at least one row.")
  nms <- names(data)

  # demographics: explicit, else the common names present in the frame.
  if (is.null(demographics)) {
    common <- c("age", "sex", "gender", "education", "race", "race/ethnicity",
                "marital status", "household income", "income", "religion",
                "census region", "region", "community type", "employment status",
                "home ownership", "children in household", "union household",
                "military service", "attention to politics")
    demographics <- intersect(common, nms)
  }
  demographics <- intersect(demographics, nms)

  # answers: explicit, else every non-demographic column that is not obviously an
  # id or a weight (so analysis-only columns stay out of the persona text).
  # Three spellings are caught: separated (case_id, sampling.weight), compound id
  # suffixes on the usual survey stems (caseid, respid, userid), and compound
  # weight suffixes (caseweight, pweights). Words that merely contain the
  # letters, such as "identity" or "weightlifting", are kept.
  if (is.null(answers)) {
    drop <- grepl(paste0(
      "(^|[._ ])(id|ids|weight|weights|wt)([._ ]|$)",
      "|(case|resp|respondent|user|person|subject|panel|row)ids?$",
      "|[a-z0-9]weights?$"),
      nms, ignore.case = TRUE)
    answers <- setdiff(nms[!drop], demographics)
  }
  answers <- intersect(answers, nms)

  # dictionary: handle -> question wording (column name when unmapped) -> domain.
  field_cols <- union(demographics, answers)
  dict <- data.frame(
    handle   = field_cols,
    question = vapply(field_cols, function(h) {
      q <- if (!is.null(questions) && h %in% names(questions)) questions[[h]] else NULL
      if (is.null(q) || !nzchar(q)) h else q
    }, ""),
    domain   = ifelse(field_cols %in% demographics, "demographic", "answer"),
    stringsAsFactors = FALSE)

  attr(data, "dictionary") <- dict
  attr(data, "demographic_fields") <- demographics
  attr(data, "answer_fields") <- answers
  class(data) <- unique(c("persona_frame", class(data)))

  if (requireNamespace("LLMR", quietly = TRUE)) {
    LLMR::llm_validate_persona_frame(data)
  }
  data
}

#' @export
print.persona_frame <- function(x, ...) {
  d <- attr(x, "dictionary")
  n_demo <- sum(d$domain == "demographic")
  n_ans  <- sum(d$domain == "answer")
  cat(sprintf("<persona_frame | %d respondent(s) | %d demographic field(s), %d answer field(s)>\n",
              nrow(x), n_demo, n_ans))
  has_q <- any(d$question != d$handle)
  cat(if (has_q) "  question wording attached\n"
      else "  no question wording (column names stand in)\n")
  invisible(x)
}
