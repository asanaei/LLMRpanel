# Rendering of persona values: missing-data labels, labeled survey columns,
# numbers, identifier and weight columns, question wording, and the reports
# of what was left out. All data here are synthetic; checks on the bundled
# dataset use aggregates only.

# A labeled vector as haven::labelled() builds it, without haven.
lab <- function(x, labels, ...) {
  structure(x, labels = labels, ...,
            class = c("haven_labelled", "vctrs_vctr", if (is.character(x)) "character" else "double"))
}

# A persona frame with one demographic field and the given answer columns.
frame_of <- function(..., age = NULL) {
  answers <- list(...)
  n <- length(answers[[1]])
  df <- data.frame(age = age %||% rep("40-49", n), stringsAsFactors = FALSE)
  for (nm in names(answers)) df[[nm]] <- answers[[nm]]
  df
}

render <- function(df, ...) {
  suppressMessages(panel_from_personas(df, questions = c(q = "The question"), ...))$persona
}

# The same vocabulary as FocusGroup's fg_default_na_strings() and the vector in
# test-anes-personas.R.
shared_na_vocabulary <- c(
  "inapplicable", "missing", "refused", "don't know", "don.t know", "not asked",
  "legitimate skip", "no answer", "not applicable", "not available", "\\bdk/rf\\b",
  "^\\s*(-?[0-9]+\\.)?\\s*restricted\\b", "^\\s*(-?[0-9]+\\.)?\\s*error\\s*$",
  "technical error", "no post[- ](election )?interview", "interview deleted",
  "^\\s*-[0-9]+\\..*\\bbreak ?off\\b", "^\\s*-[0-9]+\\..*\\binsuffici?ent\\b",
  "separate file", "released separately", "will be (released|available)",
  "not reported")

anes_missing_labels <- c(
  "-9. Refused", "-3. RESTRICTED", "-6. No post interview",
  "-7. Insufficient partial, interview deleted", "-8. Don't know",
  "-1. Inapplicable", "-2. DK/RF in the respondent's own words",
  "-4. Error", "-5. Interview breakoff (sufficient partial IW)")

test_that("the missing-data vocabulary is FocusGroup's", {
  expect_identical(LLMRpanel:::.panel_na_strings, shared_na_vocabulary)
  # FocusGroup is not a dependency; compare with it when it is installed
  fg <- "FocusGroup"
  if (requireNamespace(fg, quietly = TRUE) &&
      "fg_default_na_strings" %in% getNamespaceExports(fg)) {
    expect_identical(getExportedValue(fg, "fg_default_na_strings")(), shared_na_vocabulary)
  }
  # na_strings = NULL is the vocabulary
  x <- factor(c("-9. Refused", "1. Agree"))
  expect_identical(render(frame_of(q = x)), render(frame_of(q = x), na_strings = shared_na_vocabulary))
})

test_that("missing-data labels after haven::as_factor() are left out", {
  levels <- c(anes_missing_labels, "1. Strongly agree", "2. Agree")
  x <- factor(c(anes_missing_labels, "1. Strongly agree", "2. Agree"), levels = levels)
  df <- frame_of(q = x)
  for (form in list(x, as.character(x))) {
    df$q <- form
    p <- suppressMessages(panel_from_personas(df, questions = c(q = "The question")))
    text <- p$persona
    for (label in anes_missing_labels) {
      expect_false(any(grepl(label, text, fixed = TRUE)), info = label)
      expect_false(any(grepl(sub("^-[0-9]+\\. ", "", label), text, fixed = TRUE)), info = label)
    }
    expect_identical(sum(grepl("The question --", text, fixed = TRUE)), 2L)
    expect_true(any(grepl("The question -- Strongly agree.", text, fixed = TRUE)))
    expect_false(any(grepl("1. Strongly agree", text, fixed = TRUE)))
  }
  # the same labels on a labeled column
  codes <- c(-9, -3, -6, -7, -8, -1, -2, -4, -5)
  y <- lab(c(codes, 1), stats::setNames(c(codes, 1, 7), c(anes_missing_labels, "1. Low", "7. High")))
  text <- render(frame_of(q = y))
  expect_false(any(grepl("Refused|RESTRICTED|interview|Don't know|Inapplicable|DK/RF|Error", text)))
  expect_identical(sum(grepl("The question -- Low.", text, fixed = TRUE)), 1L)
})

test_that("answers that only mention missing-data words are kept", {
  kept <- c("There is an error", "Terrorism", "Knowledgeable", "Applicable to me",
            "No post office nearby", "Partial agreement")
  text <- render(frame_of(q = kept))
  for (k in kept) expect_true(any(grepl(paste0("The question -- ", k, "."), text, fixed = TRUE)), info = k)
  # hard negatives for the anchored patterns: these are missing-data labels
  gone <- c("Error", "2. Error", "RESTRICTED", "Technical error")
  expect_false(any(grepl("The question", render(frame_of(q = gone)), fixed = TRUE)))
})

test_that("na_strings can be narrowed, extended, and is validated", {
  x <- c("-9. Refused", "Prefer not to say", "Agree")
  expect_warning(
    none <- render(frame_of(q = x), na_strings = character(0)),
    "Labels of negative codes are shown as answers")
  expect_true(any(grepl("The question -- Refused.", none, fixed = TRUE)))
  more <- render(frame_of(q = x), na_strings = c(shared_na_vocabulary, "prefer not to say"))
  expect_false(any(grepl("Prefer not|Refused", more)))
  expect_true(any(grepl("The question -- Agree.", more, fixed = TRUE)))
  expect_error(panel_from_personas(frame_of(q = x), na_strings = "("), "not valid regular expressions")
  expect_error(panel_from_personas(frame_of(q = x), na_strings = NA_character_), "nonempty regular")
  expect_error(panel_from_data(frame_of(q = x), n = 2, na_strings = 1), "nonempty regular")
})

test_that("labeled columns are decoded from their labels", {
  spend <- lab(c(1, 5, 7, 3, 99),
               c("1. Spend much less" = 1, "7. Spend much more" = 7,
                 "-9. Refused" = -9, "99. Haven't thought much about this" = 99))
  text <- render(frame_of(q = spend))
  expect_identical(text, paste(
    "You are this person: age 40-49. On a questionnaire you gave these answers:",
    c("The question -- Spend much less.",
      "The question -- 5 on a scale from 1 (Spend much less) to 7 (Spend much more).",
      "The question -- Spend much more.",
      "The question -- 3 on a scale from 1 (Spend much less) to 7 (Spend much more).",
      "The question -- Haven't thought much about this.")))
  # the audit's reproduction: a bare code with no scale
  expect_false(any(grepl("-- 5.", text, fixed = TRUE)))
  # a labeled code shows its label without the code prefix
  sex <- lab(c(1, 2), c("1. Male" = 1, "2. Female" = 2))
  expect_identical(sub(".*-- ", "", render(frame_of(q = sex))), c("Male.", "Female."))
  # an attribute-only labeled vector (no haven class) is decoded the same way
  plain <- structure(c(1, 5), labels = c("1. Spend much less" = 1, "7. Spend much more" = 7))
  expect_identical(render(frame_of(q = plain)), text[1:2])
  # character codes
  party <- lab(c("D", "R", "I"), c(Democrat = "D", Republican = "R"))
  expect_warning(got <- render(frame_of(q = party)), "bare codes .*q \\(I\\)")
  expect_identical(sub(".*-- ", "", got), c("Democrat.", "Republican.", "I."))
})

test_that("haven::as_factor() levels keep their scale", {
  lv <- c("-9. Refused", "-8. Don't know", "1. Fewer services", "3", "5",
          "7. More services", "99. Haven't thought much about this")
  x <- factor(c("5", "1. Fewer services", "-8. Don't know", "3"), levels = lv)
  expect_identical(sub(".*-- ", "", render(frame_of(q = x)))[c(1, 2, 4)], c(
    "5 on a scale from 1 (Fewer services) to 7 (More services).",
    "Fewer services.",
    "3 on a scale from 1 (Fewer services) to 7 (More services)."))
  both <- factor(c("[1] 1. Fewer services", "5", "[7] 7. More services", "[-9] -9. Refused"))
  got <- render(frame_of(q = both))
  expect_identical(sub(".*-- ", "", got[1:3]), c(
    "Fewer services.", "5 on a scale from 1 (Fewer services) to 7 (More services).",
    "More services."))
  expect_false(grepl("--", got[4], fixed = TRUE))
  # a factor whose levels are not all coded is read as plain labels
  mixed <- factor(c("1. Yes", "Other"))
  expect_identical(sub(".*-- ", "", render(frame_of(q = mixed))), c("Yes.", "Other."))
})

test_that("unlabeled codes are never silently dropped or silently shown", {
  # an unlabeled negative code is missing, with a warning naming the column
  x <- lab(c(1, -5, 2), c("1. Yes" = 1, "2. No" = 2, "-9. Refused" = -9))
  expect_warning(got <- panel_from_personas(frame_of(q = x), questions = c(q = "Q")),
                 "Negative codes without a value label .*q \\(-5\\)")
  expect_false(grepl("-5", got$persona[2], fixed = TRUE))
  expect_false(grepl("Q --", got$persona[2], fixed = TRUE))
  # any other unlabeled code is shown as the code, with a warning
  y <- lab(c(1, 3, 4), c("1. Yes" = 1, "2. No" = 2, "4. Maybe" = 4))
  expect_warning(text <- render(frame_of(q = y)), "bare codes .*q \\(3\\)")
  expect_identical(sub(".*-- ", "", text), c("Yes.", "3.", "Maybe."))
  # a number labeled only at its top code and its missing codes is a number,
  # shown without a warning
  d <- data.frame(age = c("40-49", "80 or older", "40-49"))
  d$years <- lab(c(45, 80, -9), c("80. Age 80 or older" = 80, "-9. Refused" = -9))
  expect_no_warning(got <- suppressMessages(panel_from_personas(
    d, questions = c(years = "Age in years"))))
  expect_identical(sub(".*-- ", "", got$persona[1:2]), c("45.", "Age 80 or older."))
  # a centered scale keeps its negative unlabeled codes
  lr <- lab(c(-1, 2), c("-3. Far left" = -3, "3. Far right" = 3))
  expect_identical(sub(".*-- ", "", render(frame_of(q = lr))), c(
    "-1 on a scale from -3 (Far left) to 3 (Far right).",
    "2 on a scale from -3 (Far left) to 3 (Far right)."))
  # values an SPSS file declares missing are left out
  sp <- structure(c(1, 8), labels = c(Yes = 1, No = 2, "Not sure" = 8), na_values = 8,
                  class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double"))
  got <- render(frame_of(q = sp))
  expect_true(grepl("-- Yes.", got[1], fixed = TRUE))
  expect_false(grepl("Not sure", got[2], fixed = TRUE))
})

test_that("numbers are written in full", {
  df <- data.frame(age = c("30-39", "50-59", "60-69"), income = c(1e5, 52000.5, 3),
                   stringsAsFactors = FALSE)
  df$q <- c(1.5, 7L, 1e-7)
  text <- render(df)
  expect_true(grepl("income 100000;|income 100000\\.", text[1]))
  expect_false(any(grepl("e+05", text, fixed = TRUE)))
  expect_false(any(grepl("e-07", text, fixed = TRUE)))
  expect_true(grepl("income 52000.5", text[2], fixed = TRUE))
  expect_true(grepl("income 3.", text[3], fixed = TRUE))
  expect_false(grepl("3.0", text[3], fixed = TRUE))
  expect_true(grepl("-- 1.5.", text[1], fixed = TRUE))
  expect_true(grepl("-- 7.", text[2], fixed = TRUE))
  expect_true(grepl("-- 0.0000001.", text[3], fixed = TRUE))
  expect_identical(attr(suppressMessages(panel_from_personas(df, questions = c(q = "Q"))), "margins")$income,
                   prop.table(table(c("100000", "52000.5", "3"))))
  flat <- suppressMessages(panel_from_data(df, n = 3))$persona
  expect_false(any(grepl("e+05", flat, fixed = TRUE)))
  set.seed(110)
  tpl <- panel_from_data(df, n = 6, persona_template = "Earns {income}.")$persona
  expect_true(all(tpl %in% c("Earns 100000.", "Earns 52000.5.", "Earns 3.")))
})

test_that("identifier, weight, and persona columns are never rendered", {
  df <- data.frame(
    id = c(101, 102), caseid = c("C1", "C2"), CaseID = c("K1", "K2"),
    Case_ID = c("Z1", "Z2"), respondent_id = c("R1", "R2"), RespID = c("Q1", "Q2"),
    resp_id = c("S1", "S2"), uid = c("U1", "U2"), record_id = c("D1", "D2"),
    row_id = c("W1", "W2"), persona_id = c("P1", "P2"), persona = c("old text", "old text"),
    wt = c(1.2, 0.8), age = c("30-39", "40-49"), identity = c("Strong", "Weak"),
    idea = c("Yes", "No"), stringsAsFactors = FALSE)
  hidden <- c("101", "C1", "K1", "Z1", "R1", "Q1", "S1", "U1", "D1", "W1", "P1",
              "old text", "1.2", "102", "R2")
  hidden_names <- c("id --", "caseid", "CaseID", "Case_ID", "respondent_id", "RespID",
                    "resp_id", "uid", "record_id", "row_id", "persona_id", "persona --", "wt")
  set.seed(110)
  p <- suppressWarnings(panel_from_personas(df, n = 2, weights = "wt"))
  for (h in c(hidden, hidden_names)) expect_false(any(grepl(h, p$persona, fixed = TRUE)), info = h)
  # hard negatives: names that only contain "id" are rendered
  expect_true(all(grepl("identity -- ", p$persona, fixed = TRUE)))
  expect_true(all(grepl("idea -- ", p$persona, fixed = TRUE)))
  # without weights =, wt is an ordinary answer
  q <- suppressWarnings(panel_from_personas(df, rows = 1))
  expect_true(grepl("wt -- 1.2", q$persona, fixed = TRUE))

  flat_df <- df[setdiff(names(df), c("persona", "persona_id"))]
  set.seed(110)
  f <- suppressMessages(panel_from_data(flat_df, n = 4, weights = "wt"))
  for (h in c(hidden[hidden != "P1" & hidden != "old text"], "id:", "caseid:", "wt:")) {
    expect_false(any(grepl(h, f$persona, fixed = TRUE)), info = h)
  }
  expect_true(all(grepl("identity: ", f$persona, fixed = TRUE)))
  # identifiers stay in the panel as columns
  expect_true(all(c("id", "respondent_id") %in% names(f)))
  # even when selected explicitly
  f2 <- panel_from_data(flat_df, n = 2, columns = c("id", "wt", "age"), weights = "wt")
  expect_false(any(grepl("id:|wt:", f2$persona)))
  expect_true(all(grepl("^age: ", f2$persona)))
  expect_error(panel_from_data(flat_df, n = 2, columns = c("id", "respondent_id")),
               "never rendered")
  # templates may not name them
  expect_error(panel_from_data(flat_df, n = 2, persona_template = "Case {id}."), "never rendered")
  expect_error(panel_from_data(flat_df, n = 2, weights = "wt", persona_template = "W {wt}."),
               "never rendered")
  expect_identical(panel_from_data(flat_df, n = 1, persona_template = "I am {identity}.")$persona %in%
                     c("I am Strong.", "I am Weak."), TRUE)
  # the rich path of panel_from_data
  pf <- as_persona_frame(flat_df, demographics = "age",
                         answers = c("id", "respondent_id", "wt", "identity"))
  set.seed(110)
  r <- suppressWarnings(panel_from_data(pf, n = 3, weights = "wt"))
  for (h in c("101", "R1", "1.2", "respondent_id", "wt --", "id --")) {
    expect_false(any(grepl(h, r$persona, fixed = TRUE)), info = h)
  }
  expect_true(all(grepl("identity -- ", r$persona, fixed = TRUE)))
})

test_that("values left out as missing are reported, not dropped silently", {
  df <- data.frame(age = c("30-39", NA), stringsAsFactors = FALSE)
  df$q <- c(NA, "Agree")
  expect_message(p <- panel_from_personas(df, questions = c(q = "Q")),
                 "2 value\\(s\\) were left out of the persona text")
  expect_message(panel_from_personas(df, questions = c(q = "Q")), "age \\(1\\)")
  expect_message(panel_from_personas(df, questions = c(q = "Q")), "q \\(1\\)")
  expect_false(any(grepl("NA", p$persona, fixed = TRUE)))
  expect_identical(p$age, c("30-39", NA))
  # hard negative: nothing missing, nothing reported
  whole <- data.frame(age = "50-59", q = "Agree")
  expect_no_message(panel_from_personas(whole, questions = c(q = "Q")))
  # the flat path
  expect_message(f <- panel_from_data(df, n = 4), "left out of the persona text")
  expect_false(any(grepl("NA", f$persona, fixed = TRUE)))
  # a template shows nothing in place of a missing value, with a warning
  set.seed(110)
  expect_warning(t <- panel_from_data(df, n = 4, persona_template = "Says {q}."),
                 "template shows nothing in their place: q")
  expect_true(all(t$persona %in% c("Says .", "Says Agree.")))
  # a persona with no text at all draws a warning
  empty <- data.frame(age = NA_character_, q = NA_character_)
  expect_warning(suppressMessages(panel_from_personas(empty, questions = c(q = "Q"))),
                 "have no text")
})

test_that("questions word the answers and unworded answers warn", {
  df <- data.frame(age = c("30-39", "50-59"), pid = c("Democrat", "Republican"),
                   ab = c("Always legal", "Never legal"), stringsAsFactors = FALSE)
  expect_warning(p <- panel_from_personas(df), "column names stand in for the questions: pid; ab")
  expect_true(grepl("pid -- Democrat", p$persona[1], fixed = TRUE))
  expect_no_warning(p <- panel_from_personas(
    df, questions = c(pid = "Party identification", ab = "Abortion position")))
  expect_true(grepl("Party identification -- Democrat; Abortion position -- Always legal",
                    p$persona[1], fixed = TRUE))
  # questions override a frame's dictionary
  pf <- as_persona_frame(df, questions = c(pid = "Party", ab = "Abortion"), demographics = "age")
  p <- panel_from_personas(pf, questions = c(pid = "Party identification"))
  expect_true(grepl("Party identification -- Democrat; Abortion -- Always legal",
                    p$persona[1], fixed = TRUE))
  expect_warning(panel_from_personas(df, questions = c(pid = "P", ab = "A", pdi = "typo")),
                 "pdi")
  expect_error(panel_from_personas(df, questions = c("P", "A")), "named by")
  expect_error(panel_from_personas(df, questions = c(pid = "")), "named by")
  # demographics shown under their names do not warn
  expect_no_warning(panel_from_personas(df[, "age", drop = FALSE]))
  # a frame that lost its dictionary to column subsetting warns
  d <- df
  attr(d, "dictionary") <- data.frame(handle = c("age", "pid", "ab"),
                                      question = c("age", "Party identification", "Abortion"))
  attr(d, "demographic_fields") <- "age"
  expect_no_warning(panel_from_personas(d))
  expect_warning(panel_from_personas(d[, c("age", "pid")]), "stand in for the questions: pid")
})

test_that("labels of exactly 80 characters draw a warning", {
  cut <- paste0("1. ", strrep("a", 77))
  expect_identical(nchar(cut), 80L)
  short <- paste0("1. ", strrep("b", 76))
  long <- paste0("2. ", strrep("c", 78))
  expect_warning(render(frame_of(q = factor(cut))), "80 characters long.*q")
  expect_warning(render(frame_of(q = lab(1, stats::setNames(c(1, 2), c(cut, "2. No"))))),
                 "80 characters long")
  expect_warning(render(frame_of(q = lab(4, stats::setNames(c(1, 7), c(cut, "7. High"))))),
                 "80 characters long")
  expect_warning(render(frame_of(q = cut)), "80 characters long")
  # hard negatives: 79 and 81 characters, and a complete plain text of 80
  expect_no_warning(render(frame_of(q = factor(c(short, long)))))
  complete <- "Make all unauthorized immigrants felons and send them back to their home country"
  expect_identical(nchar(complete), 80L)
  expect_no_warning(render(frame_of(q = complete)))
})

test_that("decoded demographics are the panel's columns and margins", {
  df <- data.frame(age = c("30-39", "40-49", "50-59"), stringsAsFactors = FALSE)
  df$sex <- factor(c("1. Male", "2. Female", "-9. Refused"))
  p <- suppressMessages(panel_from_personas(df))
  expect_identical(p$sex, c("Male", "Female", NA))
  expect_identical(names(attr(p, "margins")$sex), c("Female", "Male"))
  # weighted microdata margins over a labeled column
  src <- data.frame(w = c(1, 3))
  src$party <- lab(c(1, 2), c("1. Left" = 1, "2. Right" = 2))
  set.seed(110)
  m <- attr(panel_from_data(src, n = 4, weights = "w"), "margins")$party
  expect_equal(m[["Left"]], 0.25)
  expect_equal(m[["Right"]], 0.75)
})

test_that("the bundled personas render cleanly", {
  x <- LLMRpanel::anes_2024_personas
  rendered_na <- sum(is.na(x[setdiff(names(x), "ideology_score")]))
  expect_no_warning(expect_message(p <- panel_from_personas(x),
                                   sprintf("^%d value\\(s\\) were left out", rendered_na)))
  text <- p$persona
  expect_length(text, 100L)
  expect_false(any(grepl("[{}]", text)))
  expect_false(any(grepl("\\bNA\\b", text)))
  expect_false(any(grepl("(-- |: |; |^)-?[0-9]+\\.\\s", text)))
  expect_false(any(grepl(paste(shared_na_vocabulary, collapse = "|"), text, ignore.case = TRUE)))
  expect_false(any(grepl("[0-9]e[+-][0-9]", text)))
  expect_false(any(grepl("ideology", text, fixed = TRUE)))
  # the checks above reject what they exist to reject
  expect_true(grepl("[{}]", "Q -- Another candidate {SPECIFY}."))
  expect_true(grepl("\\bNA\\b", "You are this person: age NA."))
  expect_true(all(grepl("(-- |: |; |^)-?[0-9]+\\.\\s", c("Q -- 1. Male.", "sex: -9. Refused", "1. Male"))))
  expect_false(grepl("(-- |: |; |^)-?[0-9]+\\.\\s", "Q -- 5 on a scale from 1 (Low) to 7 (High)."))
  expect_true(grepl(paste(shared_na_vocabulary, collapse = "|"), "Q -- Don't know.", ignore.case = TRUE))
  expect_true(grepl("[0-9]e[+-][0-9]", "income 1e+05"))
  expect_true(all(grepl("^You are this person: ", text)))
  expect_identical(sum(grepl("^You are this person: age ", text)), sum(!is.na(x$demo_age)))
  dict <- attr(x, "dictionary")
  expect_true(all(grepl(dict$question[dict$handle == "att_iss_abortion_position"], text, fixed = TRUE) |
                    is.na(x$att_iss_abortion_position)))
  # a persona frame renders the same text through panel_from_data()
  sub <- LLMRpanel:::.restrict_persona_frame(x, c("demo_age", "att_iss_abortion_position"))
  set.seed(110)
  b <- suppressMessages(panel_from_data(sub, n = 5))
  a <- suppressMessages(panel_from_personas(sub, rows = attr(b, "source_rows")))
  expect_identical(b$persona, a$persona)
})

test_that("haven::as_factor() levels without a code prefix keep the codes' rules", {
  # a file whose labels mostly carry codes, with one that does not ("Other")
  lv <- c("-9. Refused", "-5", "1. Fewer", "5", "7. More", "Other")
  x <- factor(c("1. Fewer", "5", "7. More", "-5", "Other"), levels = lv)
  expect_warning(got <- render(frame_of(q = x)), "Negative codes without a value label .*q \\(-5\\)")
  expect_identical(sub(".*-- ", "", got[c(1:3, 5)]), c(
    "Fewer.", "5 on a scale from 1 (Fewer) to 7 (More).", "More.", "Other."))
  expect_false(grepl("--", got[4], fixed = TRUE))
  # a file whose labels carry no codes: an unlabeled code is flagged, a
  # negative one is left out
  gss <- factor(c("Fewer", "5", "More", "-5", "Refused"),
                levels = c("Refused", "-5", "Fewer", "5", "More"))
  got <- collect_warnings(render(frame_of(q = gss)))
  expect_true(any(grepl("bare codes .*q \\(5\\)", got$warnings)))
  expect_true(any(grepl("Negative codes without a value label .*q \\(-5\\)", got$warnings)))
  expect_identical(sub(".*-- ", "", got$value[1:3]), c("Fewer.", "5.", "More."))
  expect_false(any(grepl("-5|Refused", got$value[4:5])))
  # hard negatives: numbers beside one text label, or beside top codes, are
  # numbers and draw no warning
  expect_no_warning(render(frame_of(q = factor(c("0", "2", "None")))))
  expect_no_warning(render(frame_of(q = factor(c("1", "2", "3 or more")))))
})

test_that("a character column written by as.character(haven::as_factor()) is decoded", {
  x <- c("1. Fewer", "5", "7. More", "-9. Refused", "-5")
  expect_warning(got <- render(frame_of(q = x)), "Negative codes without a value label .*q \\(-5\\)")
  expect_identical(sub(".*-- ", "", got[1:3]), c(
    "Fewer.", "5 on a scale from 1 (Fewer) to 7 (More).", "More."))
  expect_false(any(grepl("--", got[4:5], fixed = TRUE)))
  # hard negative: a character column without coded values is read as it is
  expect_no_warning(plain <- render(frame_of(q = c("2", "3", "None"))))
  expect_identical(sub(".*-- ", "", plain), c("2.", "3.", "None."))
})

test_that("a scale centered on zero keeps both halves", {
  lr <- lab(c(-2, -1, 1, 2, -3), c("-3. Extremely liberal" = -3, "0. Moderate" = 0,
                                   "3. Extremely conservative" = 3))
  expect_no_warning(got <- render(frame_of(q = lr)))
  expect_identical(sub(".*-- ", "", got), c(
    sprintf("%d on a scale from -3 (Extremely liberal) to 3 (Extremely conservative).", c(-2L, -1L, 1L, 2L)),
    "Extremely liberal."))
  # hard negatives: a stray negative label beside a short positive scale is
  # no center and still warns
  inap <- lab(c(-1, 1), c("-1. Inap" = -1, "1. Yes" = 1, "2. No" = 2))
  expect_warning(render(frame_of(q = inap)), "Labels of negative codes are shown")
  three <- lab(-1, c("-1. Disagree" = -1, "0. Neither" = 0, "1. Agree" = 1))
  expect_warning(render(frame_of(q = three)), "Labels of negative codes are shown")
  expect_identical(LLMRpanel:::.panel_center(c(-3, 0, 3, 99)), 3)
  expect_true(is.na(LLMRpanel:::.panel_center(c(-3, 0, 3, 5))))
})

test_that("labels without names, bracket prefixes, and character codes are handled", {
  # a label whose name is NA leaves its values out, with a report
  l <- c(1, 2)
  names(l) <- c("1. Yes", NA)
  expect_message(got <- panel_from_personas(frame_of(q = lab(c(1, 2, 2), l)),
                                            questions = c(q = "Q")),
                 "q \\(2\\)")
  expect_identical(sum(grepl("Q --", got$persona, fixed = TRUE)), 1L)
  # the 80-character check measures the label, not haven's "[k] " prefix
  label80 <- paste0("1. ", strrep("a", 77))
  expect_warning(render(frame_of(q = paste("[1]", label80))), "80 characters long")
  expect_no_warning(render(frame_of(q = paste("[1]", substr(label80, 1, 76)))))
  # numbers labeled with character codes are matched and written in full
  sex <- structure(c(1, 2, 1e5), labels = c(Male = "1", Female = "2"))
  expect_warning(got <- render(frame_of(q = sex)), "bare codes .*q \\(100000\\)")
  expect_identical(sub(".*-- ", "", got), c("Male.", "Female.", "100000."))
  expect_identical(LLMRpanel:::.panel_number_text(c(1e5, 1e15, 1e-7, 0.1 + 0.2, 52000.5, -3, 2^53)),
                   c("100000", "1000000000000000", "0.0000001", "0.3", "52000.5", "-3",
                     "9007199254740992"))
})

test_that("ideology_score and labeled case IDs are never rendered", {
  df <- data.frame(age = c("30-39", "40-49"), ideology_score = c(1.23, -0.5),
                   party = c("Left", "Right"), stringsAsFactors = FALSE)
  df$V240001 <- structure(c(200015, 200022), label = "2024 Time Series Case ID")
  df$strength <- structure(c("Strong", "Weak"), label = "Identity strength")
  set.seed(110)
  flat <- suppressMessages(panel_from_data(df, n = 4))$persona
  expect_false(any(grepl("ideology|1.23|V240001|200015|200022", flat)))
  expect_true(all(grepl("strength: ", flat, fixed = TRUE)))
  expect_error(panel_from_data(df, n = 2, persona_template = "{ideology_score}"), "never rendered")
  expect_error(panel_from_data(df, n = 2, persona_template = "{V240001}"), "never rendered")
  rich <- suppressWarnings(panel_from_personas(df))$persona
  expect_false(any(grepl("ideology|V240001|200015", rich)))
  expect_true(all(grepl("strength -- ", rich, fixed = TRUE)))
})

test_that("plain text is missing only when it says nothing but missing-data wording", {
  na <- .panel_check_na_strings(NULL)
  missing <- c("Refused", "Don't know", "I don't know", "Refused to answer",
               "Not applicable (skipped)", "No answer", "Legitimate skip", "DK/RF",
               "Inapplicable", "Not asked")
  answers <- c("I refused to answer the door", "I keep missing the bus",
               "My landlord refused to fix the heat", "Missing person case",
               "There is an error", "Don't know enough to say", "Evening hours")
  expect_true(all(.panel_decode_text(missing, na)$issue %in% "missing"))
  expect_false(any(.panel_decode_text(answers, na)$issue %in% "missing"))
  # Value labels still count on any match.
  expect_true(all(.panel_decode_text(c("-9. Refused to answer the question", "97. Missing"),
                                     na)$issue %in% "missing"))
  frame <- data.frame(age = c(30, 40), comment = c("I keep missing the bus", "Refused"))
  set.seed(110)
  out <- suppressMessages(panel_from_data(frame, n = 20, columns = c("age", "comment")))
  expect_true(any(grepl("I keep missing the bus", out$persona, fixed = TRUE)))
  expect_true(any(!grepl("comment", out$persona, fixed = TRUE)))
  expect_false(any(grepl("Refused", out$persona, fixed = TRUE)))
})

test_that("a ResponseId column is an identifier and never rendered", {
  frame <- data.frame(ResponseId = c("R_1abc", "R_2def"), age = c(30, 40))
  set.seed(110)
  out <- suppressMessages(panel_from_data(frame, n = 2, columns = c("ResponseId", "age")))
  expect_false(any(grepl("R_1abc|R_2def|ResponseId", out$persona)))
})

test_that("a template placeholder that names no selected column draws a warning", {
  frame <- data.frame(age = c(30, 40), city = c("Akron", "Boise"))
  set.seed(110)
  expect_warning(panel_from_data(frame, n = 2, persona_template = "Age {age}; {agee}.",
                                 columns = c("age", "city")),
                 "names \\{agee\\}, not among `columns`")
  expect_no_warning(panel_from_data(frame, n = 2, persona_template = "Age {age}.",
                                    columns = c("age", "city")))
})

test_that("NA in a rows selector is reported, not dropped in silence", {
  x <- anes_2024_personas
  flag <- rep(TRUE, nrow(x)); flag[1:3] <- NA
  expect_warning(suppressMessages(panel_from_personas(x, rows = flag)), "NA for 3 row")
  expect_warning(suppressMessages(panel_from_personas(x, rows = function(d) flag)),
                 "returned NA for 3 row")
})

test_that("one rule decides missing values and value labels", {
  na <- .panel_check_na_strings(NULL)
  missing <- c("-9. Refused", "-9 Refused", "[-8] Don't know", "(-8) Don't know",
               "{VOL} Don't know", "Don't know (VOL)", "Don't know/Refused (VOL.)",
               "Refused (VOL)", "Refused / don't know", "Not asked in survey",
               "97. Refused", "998. Don't know (where to rate)",
               "999. Don't recognize (don't know who this is)", "9. Missing",
               "-7. Insufficient partial, interview deleted",
               "-2. DK/RF in V241288 or V241289", "-5. Sufficient partial, breakoff",
               "Inapplicable, legitimate skip", "Refused to answer", "I don't know",
               "No answer", "Missing; Not available", "Error", "2. Error", "[-9] Refused",
               "Other missing")
  kept <- c("5. Missing the old system", "2. Refused to take sides",
            "Missing the old system", "I keep missing the bus",
            "My landlord refused to fix the heat", "Missing person case",
            "There is an error", "Terrorism", "-3. Strongly disagree",
            "70 on a 0-100 feeling thermometer", "5 on a scale from 1 (A) to 7 (B)",
            "Not sure", "I refused to answer the door")
  expect_true(all(.panel_missing_text(missing, na)))
  expect_false(any(.panel_missing_text(kept, na)))
  expect_true(all(.panel_decode_text(missing, na)$issue %in% "missing"))
  expect_false(any(.panel_decode_text(kept, na)$issue %in% "missing"))
  # As value labels, with their codes known.
  expect_true(all(.panel_missing_text(c("Refused", "Don't know"), na, code = c(-9, 998))))
  expect_false(.panel_missing_text("Refused to take sides", na, code = 2))
})

test_that("other identifier names are never rendered", {
  ids <- c("participant_id", "subject_id", "user_id", "PROLIFIC_PID", "workerId",
           "caseno", "QKEY")
  frame <- as.data.frame(setNames(lapply(ids, function(i) c("X1", "X2")), ids))
  frame$age <- c(30, 40)
  set.seed(110)
  out <- suppressMessages(panel_from_data(frame, n = 10, columns = c(ids, "age")))
  expect_false(any(grepl("X1|X2", out$persona)))
  expect_true(all(grepl("age", out$persona)))
  # "pid" is party identification in survey data, not an identifier.
  out <- suppressMessages(panel_from_data(data.frame(pid = c("Democrat", "Republican")),
                                          n = 4, columns = "pid"))
  expect_true(all(grepl("Democrat|Republican", out$persona)))
})

test_that("a subset of the bundled personas gets its dictionary back", {
  x <- anes_2024_personas[1:20, c(1:5, 20:25)]
  expect_null(attr(x, "dictionary"))
  expect_message(p <- suppressWarnings(panel_from_personas(x)), "lost the dictionary")
  expect_true(all(grepl("You are this person", p$persona, fixed = TRUE)))
  expect_false(any(grepl("demo_", p$persona, fixed = TRUE)))
  # A frame that is not a subset of the bundled data is left as it is.
  y <- data.frame(other = c("a", "b"))
  expect_no_message(suppressWarnings(.panel_restore_dictionary(y)))
})
