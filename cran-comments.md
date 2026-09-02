## Resubmission (references, examples, seed)

The three points raised in review are addressed; the version is unchanged
because 0.6.1 was never published.

- The Description field cites the methods: Argyle et al. (2023)
  <doi:10.1017/pan.2023.2> for samples of language-model personas, Bisbee et
  al. (2024) <doi:10.1017/pan.2024.5> for benchmarking them against human
  data, and Hainmueller et al. (2014) <doi:10.1093/pan/mpt024> for the average
  marginal component effect.
- No example uses `\dontrun{}`. The administration, conjoint, and benchmark
  examples now execute through the package's `.runner` seam, which answers
  without a provider, so the AMCE example recovers a known effect offline in
  under a tenth of a second. Live and batch calls sit in `\donttest{}` behind a
  check for `GROQ_API_KEY`, and the two batch-retrieval examples read the job
  file a submission would have written, so all of them are no-ops without a
  key. The check makes no network call.
- `R/gui.R` sets no specific seed. The Studio has an optional "Random seed"
  field: left blank, nothing is seeded and the session's random stream is
  untouched; given a value, it governs the panel draw, the conjoint profile
  draws, and the option order each persona sees. Repeated runs of one
  administration still share a single randomization, so what differs between
  runs is the model's answer; when no seed is supplied that shared value is
  drawn once from the session's own stream.

## Earlier resubmission

Resubmission of LLMRpanel, bumped to 0.6.1 (0.6.0 was never published).
The July pretest declined 0.6.0 with a WARNING because three objects its GUI
uses were not exported by the LLMR.shiny then on CRAN; LLMR.shiny 0.1.2,
which exports them, is on CRAN now, and the Suggests entry states the
requirement (LLMR.shiny >= 0.1.2).

Version 0.6.1 also changes package behavior:

- Item-order randomization is gone. Each persona-item pair is an independent
  request, so no questionnaire order was ever shown to the model.
- A Likert scale is shown reversed for half of responses instead of
  arbitrarily permuted.
- Human reference distributions are validated before any comparison, and an
  item whose every response failed to parse gets NA shares.
- A runner supplies responses and diagnostics; it cannot rewrite the
  experimental assignments.
- The vignette executes offline from beginning to end.

The pretest NOTE flagged "Benchmarked" in the Description as possibly
misspelled; it is ordinary English, the past participle of "benchmark".

## The package

The package Imports LLMR (>= 0.8.9), which is on CRAN. It Suggests
LLMR.shiny (>= 0.1.2); every use of LLMR.shiny (and of the other GUI
dependencies shiny, bslib, DT) is guarded with requireNamespace(), and the
package's core has no Shiny dependency. The test suite and the vignette run
fully offline through the package's `.runner` seam. No test or vignette makes
a network call, and the examples' live blocks are skipped unless the user has
set an API key.

## Test environments

- local macOS 26.5 (arm64), R 4.4.3
- R CMD build; R CMD check --as-cran on the tarball

## R CMD check results

0 errors | 0 warnings | 3 notes

- "checking CRAN incoming feasibility ... NOTE: New submission".
- "checking for future file timestamps ... NOTE: unable to verify current
  time". Environmental; no network during the check.
- "checking HTML version of manual ... NOTE": emitted by an older system
  `tidy` that does not recognize the HTML5 elements R generates; it does not
  reproduce on CRAN.
