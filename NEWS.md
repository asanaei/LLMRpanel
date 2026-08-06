# LLMRpanel 0.6.1

Corrections from an external methodological review of 0.6.0 (never on CRAN).

* `item_order` randomization is gone, and `panel_instrument()` refuses it
  with an explanation: every persona-item pair is an independent request,
  so no respondent ever sees a questionnaire order, and shuffling a
  recorded position number claimed an exposure that was never administered.
  `item_position` now documents the item's fixed instrument position.
* Option-order randomization respects scale structure: choice options are
  permuted, while a Likert scale is shown reversed for a random half of
  responses (an ordered scale has two readable orders, not k factorial).
* The submitted grid is the experimental record: a runner may contribute
  responses and provider diagnostics, never rewrite assignments. Retained
  provenance grows to request hashes (the default runner now asks LLMR for
  them), model versions, and durations.
* `panel_benchmark()` validates its human reference: probabilities in
  [0, 1], one row per item-response pair, shares summing to one, labels
  drawn from the offered options, and no conjoint responses (profiles
  differ by respondent). An item whose every response failed to parse gets
  `NA` shares, not a zero distribution.
* `panel_bias_audit()` reports `NA` with a note when chi-square expected
  counts are sparse, instead of an unreliable p-value.
* `panel_from_personas()` requires `data` (the bundled ANES example is an
  explicit choice, not a silent default), validates `rows` selectors,
  refuses NA or negative weights rather than zeroing them, requires numeric
  weight columns, and warns when weights are supplied without `n`.
* `panel_power()` is removed. Its arithmetic treated model-persona
  dispersion as if it were human outcome variance, which is not a sound
  basis for planning a human study; a design-sensitivity function with an
  explicit dispersion source may return later.
* Placeholder filling is single-pass, so substituted values containing
  braces are never re-substituted; the panel print shows a truncated
  persona preview.
* The Studio's persona-field default is the demographic fields rather than
  all 125 columns (prior attitude items in the prompt are target leakage);
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
