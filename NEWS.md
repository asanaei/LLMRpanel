# LLMRpanel 0.1.0

First public cut of calibrated silicon sampling. Working today:

- `panel_from_margins()`: persona panels from user-supplied population
  margins (no data shipped, no "default" populations), with template-
  rendered persona text.
- `item_likert()` / `item_choice()` / `item_open()` and `instrument()`
  with per-respondent item- and option-order randomization, recorded per
  response; `vignette_design()` and `conjoint_design()` for factorial
  stimuli -- conjoint profiles within a task are guaranteed distinct, with
  a warning when the attribute space is too small to allow it.
- `administer()`: persona-conditioned answering through LLMR's parallel
  engine; replies matched to offered options, Likert positions scored,
  failures kept as `NA`.
- `calibrate()`: **coverage-aware** comparison to a human benchmark --
  restricted to items the benchmark covers (no fake deviations from
  unbenchmarked items), per-item nonresponse recorded, benchmark shares
  checked to sum to 1, and a three-state banner: UNCALIBRATED ->
  PARTIALLY CALIBRATED (k/m items) -> calibrated, with the measured
  deviation. Deviations are reported, never adjusted away.
- `bias_audit()`: option-order effects (chi-squared per item) and
  non-response; `panel_report()` leads with calibration status, coverage
  included.
- Runnable examples throughout; the design vignette runs fully offline
  (a simulated respondent through the `.runner` seam), with a gated live
  section.

Exported as design contracts, arriving in 0.2: `silicon_power()` and
`amce()`. The logprob administration path, persona-paraphrase arms, and
joint microdata draws are documented future work rather than exported
stubs.
