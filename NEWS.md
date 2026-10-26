# LLMRpanel 0.6.3

## The bundled ANES personas

* `anes_2024_personas` now ships with LLMRpanel, and the examples, the Studio,
  and the tests use it in place of `LLMR::anes_2024_personas`. FocusGroup
  ships an identical copy. The data were rebuilt from the May 19, 2026 Full
  Release of the ANES 2024 Time Series Study with corrected coding, so the 100
  respondents and their `ideology_score` values differ from those in LLMR's
  copy. The old build had turned the middle answers of six seven-point scales
  into missing values, which also tilted the selection of respondents and the
  ideology scores toward people who chose the ends of those scales. It kept
  four immigration-policy labels that the Stata file cuts at 80 characters,
  interviewer notes such as "{VOL}", two items from the spouse/partner survey
  that gave 14 personas their partner's answers, and six fields that were
  empty for everyone because their items were asked only on paper or only of
  the GSS re-interview sample, which this release does not include. Answers on
  the six scales now read "5 on a scale from 1 (...) to 7 (...)" and feeling
  thermometers "70 on a 0-100 feeling thermometer". `?anes_2024_personas`
  documents the coding and the selection.

* Changed handles: `demo_community_type` is replaced by
  `demo_rural_urban_identity`, and the three confidence items
  `att_val_confidence_in_the_supreme_co`, `att_val_confidence_in_the_scientific`,
  and `att_val_confidence_in_the_press` by `att_val_trust_in_the_judiciary`,
  `att_val_trust_in_scientists`, and `att_val_trust_in_traditional_media`.
  `att_val_general_happiness` and `att_val_importance_of_remaining_a_de` are
  gone. `demo_home_ownership` now comes from the item asked of everyone, so its
  values include "Rented for cash"; the MAGA thermometer and satisfaction with
  democracy come from post-election items asked of every respondent. The frame
  has 123 columns, 17 of them demographic.

## Rendering of persona values

* Missing-data labels are left out of the persona text. `panel_from_personas()`
  and `panel_from_data()` gain `na_strings`, regular expressions that mark a
  value or value label as missing; `NULL`, the default, uses the patterns of
  FocusGroup's `fg_default_na_strings()`. Before, a frame decoded with
  `haven::as_factor()`, as the help of `as_persona_frame()` advised, gave
  personas answers such as "-9. Refused", "-3. RESTRICTED", and "-8. Don't
  know". A few patterns are anchored: "Error" must be the whole label and
  "RESTRICTED" its first word, so "Terrorism" and "There is an error" are
  kept. A value or value label that matches is left out when it carries a
  missing-data code (negative, or 90 and above: "-9 Refused", "[-8] Don't
  know", "998. Don't know (where to rate)") or says nothing but such wording
  once codes, interviewer markers ("{VOL}", "(VOL)"), and asides in
  parentheses are set aside ("Refused to answer", "Don't know/Refused
  (VOL.)"). Answers that use such words ("2. Refused to take sides", "I
  refused to answer the door") are kept. FocusGroup decides by the same rule.
  Values left out are reported.

* `panel_from_personas()` restores the dictionary and demographic fields of
  `anes_2024_personas` from the package's copy, with a message, when base
  subsetting (`x[, j]`, `subset()`) has dropped them; before, the
  demographics of such a subset were rendered as questionnaire answers under
  their raw handles.

* Identifier columns also include ResponseId, participant_id, subject_id,
  user_id, PROLIFIC_PID, workerId, caseno, and QKEY.

* Labeled survey columns, as `haven::read_dta()` and `haven::read_sav()` return
  them, are decoded from their value labels, without haven. A labeled code is
  shown as its label without the code prefix, and an unlabeled code on a scale
  labeled only at its ends as "5 on a scale from 1 (...) to 7 (...)", where a
  scale centered on zero runs from -m to m even when its midpoint is labeled.
  Before, such a column was rendered as bare codes ("spend -- 5"), or stopped
  with an error when haven was not loaded. An unlabeled negative code is left
  out as missing, and any other unlabeled code is shown as the code, each with
  a warning naming the column. Factor levels as `haven::as_factor()` writes
  them, and character columns as `as.character(haven::as_factor())` writes
  them, are decoded by the same rules, and code prefixes ("1. ") are removed
  from factor levels and character values.

* Numbers are written in full: "income 100000", not "income 1e+05".

* Identifier columns (`id`, `caseid`, `case_id`, `respondent_id`, `respid`,
  `resp_id`, `persona_id`, `uid`, `record_id`, and `row_id`, ignoring case,
  and columns whose variable label calls them a case or respondent ID, as the
  ANES label of V240001 does), the column named by `weights`, a column named
  `persona`, and `ideology_score` are never rendered. Before,
  `panel_from_personas()` and `panel_from_data()` without a template wrote
  them into the persona text ("id -- 101; respondent_id -- R1", "wt -- 1.2"),
  and `panel_from_data()` wrote the `ideology_score` of the bundled personas.
  They remain columns of the panel where they were before. A
  `persona_template` that names one of them is an error.

* Values left out of the persona text are reported in a message that names
  each field and the number of personas without it. Before, they were dropped
  silently, and `panel_from_data()` without a template wrote "column: NA". A
  template placeholder without a value is now filled with nothing, with a
  warning; before, it showed "NA".

* `panel_from_personas()` gains `questions`, question texts named by column,
  which take precedence over the frame's dictionary. When column names stand
  in for the questions of stated answers, as when column subsetting has
  dropped a frame's dictionary, a warning lists the columns.

* A value label exactly 80 characters long draws a warning: Stata files cut
  value labels at that length.

* The demographic columns of a panel from `panel_from_personas()` hold the
  values as the personas state them, and the margins both constructors cite
  count those values, so a missing-data label is no longer a category.

* In `run_panel_studio()`, the persona preview, now titled "Selected persona as
  sent", shows the text sent with each request, rendered from the selected
  fields only; before, it showed every field while only the selected ones were
  sent. `ideology_score`, which is never rendered, is no longer offered as a
  field.

# LLMRpanel 0.6.2

* The help and vignette describe each persona-item pair as an independent
  request with option-order randomization; claims about item-order
  randomization and sample-size planning are removed.

* `run_panel_studio()` refuses malformed population margins and benchmark
  shares, naming the entry, instead of dropping it.

* `panel_from_personas()` draws the selected respondent when `rows` selects a
  single row; it used to sample from rows 1 to that number instead. Draws from
  two or more rows are unchanged.

* `panel_from_personas()` stops with "`rows` indices must be whole numbers
  between 1 and nrow(data), with no NA." when `rows` is fractional, character,
  or a factor. Before, `rows = c(1.9, 3.7)` selected rows 1 and 3,
  `rows = "3"` selected row 3, and a factor selected the rows numbered by its
  level codes. A fractional `n` stops with "`n` must be one whole number of at
  least 1."; before, `n = 2.5` drew two personas.

* `panel_from_personas()` and `panel_from_data()` record the source rows they
  drew in a new `source_rows` attribute, giving for each persona in
  `persona_id` order the row of `data` it came from. `as_tibble()` drops it
  along with `margins` and `source`.

* `conjoint_amce()` records each attribute's reference level in a new
  `reference` attribute and prints the reference levels below the header. The
  reference is still the first declared level present in the estimation data.
  When that is not the first declared level, for example because every
  execution showing it failed, the function warns "Some declared conjoint
  levels are absent from the estimation data." and names the level that took
  its place; before, the baseline changed without a message. The same warning
  names any other declared level that is absent and so has no row.

* `conjoint_amce()` stores the number of personas whose responses entered the
  estimate, the clusters behind its standard errors, in a new `n_clusters`
  attribute and prints it beside `n_respondents`. `n_respondents` still counts
  every persona administered, including those whose responses all failed or
  did not parse, so the two differ after such failures.

* `item_likert()`, `item_choice()`, and `item_open()` stop with "`id` must be
  one nonempty character string, not ..." when the id is missing, empty, or
  longer than one string, and with the same message for `text`. Before, an
  `NA` id or question was accepted and sent as the text "NA", and a longer
  vector was accepted and failed later or built a malformed request.

* `item_choice()` and `item_likert()` stop with "`options` has a missing or
  empty label at position ..." (or "`scale` has ...") when a label is `NA` or
  empty, and with "`options` has labels that replies cannot tell apart: ..."
  when two labels are equal after ignoring case and surrounding spaces, which
  is how replies are matched to labels. Before, such labels were accepted: a
  repeated Likert label made its score ambiguous, and a reply matching two
  labels was assigned to the first. A label set that is not character or has
  fewer than two labels stops with "must be a character vector of at least two
  labels" instead of a `stopifnot()` message.

* `panel_instrument()` names the repeated ids in its error "Item ids must be
  unique; repeated: ...".

* `conjoint_design()` stops with "`n_tasks` must be one whole number of at
  least 1, not ..." and with the same message for `profiles_per_task`.
  Before, `n_tasks = 2.5` built two tasks, `profiles_per_task = 2.5` showed two
  profiles per task, and a count of zero built an empty design.

* `conjoint_design()` stops with "Attribute names must be unique; repeated:
  ..." and, for attributes named `task`, `profile`, `persona_id`, or `chosen`,
  with "Attribute name ... is reserved for the design's own columns". Before,
  both failed later with a tibble error about duplicated column names. It also
  stops when an attribute has no levels ("must be a vector of at least one
  level"), a missing or empty level ("has a missing or empty level at position
  ..."), or a repeated level ("repeats the level(s) ..."). Before, a repeated
  level was drawn more often than the others and made `conjoint_amce()` stop on
  a singular design matrix, and an `NA` level was shown as "NA".

* The duplicate-profile warning of `conjoint_design()` names its cause. "The
  attribute space is too small for distinct profiles: it holds ..." means a
  task shows more profiles than the attributes can form; "Some tasks contain
  duplicate profiles because 25 redraws found no distinct one, ..." means the
  redraws ran out in a space that could have supplied them. Before, both cases
  were reported as a space too small.

* `panel_bias_audit()` fills `order_test_note` whenever `order_effect_p` is
  `NA`, with the reason no p-value is reported: an open item, an option order
  that did not vary, fewer than four parsed responses, identical parsed
  responses, one option shown first in every parsed response, or sparse cells.
  Before, only sparse cells had a note, and the other cases were `NA` in both
  columns. When `stats::chisq.test()` itself fails, the note records its
  message and the function warns "The first-option test for item ... did not
  run: ..."; before, the failure was dropped without a trace. `LLMR::report()`
  prints the note beside "order p = n/a".

# LLMRpanel 0.6.1

Corrections replacing 0.6.0, which was never published.

* `item_order` randomization is gone, and `panel_instrument()` refuses it
  with an explanation: every persona-item pair is an independent request,
  so no respondent ever sees a questionnaire order, and shuffling a
  recorded position number claimed an exposure that was never administered.
  `item_position` now documents the item's fixed instrument position.
* Option-order randomization respects scale structure. Choice options are
  permuted. A Likert scale has two readable presentations, so it is shown
  reversed for a random half of responses.
* The submitted grid is the experimental record. A runner contributes
  responses and provider diagnostics; the assignments it returns are
  ignored in favor of the ones submitted. Retained provenance grows to
  request hashes (the default runner now asks LLMR for them), model
  versions, and durations.
* `panel_benchmark()` now rejects a human reference whose probabilities
  fall outside [0, 1], whose shares miss one, or whose labels were never
  offered. Conjoint responses are refused, since profiles differ by
  respondent. An item whose every response failed to parse gets `NA`
  shares; zero shares would claim the categories were available and
  unchosen.
* `panel_bias_audit()` reports `NA` with a note when chi-square expected
  counts are sparse, instead of an unreliable p-value.
* An administration whose replies come back empty with `finish_reason`
  `"length"` now warns and names `max_tokens` as the cause. Reasoning models
  can spend a small budget entirely on hidden reasoning and emit no visible
  text.
* `panel_from_personas()` requires `data`; the ANES example must be named.
  A weight that is NA or negative now stops the call, where it used to be
  quietly set to zero, and weight columns must be numeric. Weights supplied
  without `n` draw a warning, since only a sample uses them.
* `panel_power()` is removed. Its arithmetic treated model-persona
  dispersion as if it were human outcome variance, which is not a sound
  basis for planning a human study; a design-sensitivity function with an
  explicit dispersion source may return later.
* Placeholder filling is single-pass, so a substituted value containing
  braces is left alone.
* The panel print shows a truncated persona preview.
* The Studio's persona-field default is the demographic fields. Sending all
  125 columns put prior attitude items into the prompt, where they leak into
  the answers;
  the vignette now executes offline end to end through a deterministic
  runner, with one live-gated chunk; cross-model comparisons in it reuse
  the seed so both models face the same assignments.

# LLMRpanel 0.6.0

Initial CRAN release.

- `panel_from_margins()`, `panel_from_data()`, and
  `panel_from_personas()` create panels from supplied margins, microdata
  rows, or persona data frames. `as_persona_frame()` attaches question and
  field metadata to a data frame. Reports identify which panel source was used.
- `panel_administer()` administers every item to every persona. It can
  randomize item and option order and records `item_position` and
  `option_order`. Its classed result stores response rows in `$data` and the
  panel, instrument, benchmark, and token usage in separate fields. Response
  rows retain `response_text`, `response_id`, `success`, `model`, and
  `provider` as columns.
- `panel_batch_submit()` submits an administration to a provider's
  asynchronous batch API. `panel_batch_status()` and `panel_batch_fetch()`
  inspect and retrieve the job. Synchronous and batch submission share the
  `max_calls` and `confirm` gate.
- `panel_benchmark()` compares response shares with supplied human
  benchmarks and records benchmark coverage. `plot()` displays the
  compared shares.
- `panel_bias_audit()` reports parse failures and first-option sensitivity.
- `conjoint_instrument()` creates conjoint choice tasks with profiles randomized
  independently for each respondent. `conjoint_design()` returns a classed list
  with profile and attribute fields, and `conjoint_amce()` estimates from the
  recorded respondent-level profiles with standard errors clustered by
  respondent in a classed result that retains run counts as columns.
- `run_panel_studio()` provides the panel workflow in an optional Shiny
  application and can download responses, reports, and benchmark tables. The
  studio now preserves readable long-form output, presents response shares and
  diagnostics before technical details, carries administered responses into
  benchmarking and analysis, supports repeated administrations and conjoint
  instruments, exposes power and AMCE calculations, permits full ANES persona
  inspection and field selection, honors shared generation settings, and shows
  recorded call timing when it is available. Numbered configuration sections
  can be collapsed while the administration action and run plan remain in
  view. Persona templates and item wording have editable working defaults.
  Panel construction, administration, benchmark comparison, power, and AMCE
  actions are disabled when their known inputs are incomplete, with the unmet
  requirement shown beside the control.
  ANES construction separates sampled panels from panels made from selected
  respondents. Sampled panels disclose replacement sampling above the
  100-respondent source size and no longer impose a 500-person GUI ceiling;
  selected panels require at least one respondent and ignore panel size.
  A blank shared maximum-output-token field now uses and displays a
  512-token package default, while an explicit sidebar value takes precedence.
  Response text and response labels receive most of the width in display
  tables, internal identifiers remain available through a column control, and
  shares, benchmark deviations, and other double columns use concise
  display-only rounding.
