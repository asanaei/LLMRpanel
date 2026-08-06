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
