## Resubmission

Resubmission of LLMRpanel, bumped to 0.6.1 (0.6.0 was never published).
The July pretest declined 0.6.0 with a WARNING because three objects its GUI
uses were not exported by the LLMR.shiny then on CRAN; LLMR.shiny 0.1.2,
which exports them, is on CRAN now, and the Suggests entry states the
requirement (LLMR.shiny >= 0.1.2).

Version 0.6.1 also incorporates corrections from an external methodological
review: the inert item-order randomization is gone (each persona-item pair
is an independent request, so no questionnaire order is ever shown), Likert
scales are reversed rather than arbitrarily permuted, human reference
distributions are validated before comparison, an all-parse-failure item
gets NA rather than zero shares, runners can no longer rewrite experimental
assignments, and the vignette now executes offline end to end.

The pretest NOTE flagged "Benchmarked" in the Description as possibly
misspelled; it is ordinary English, the past participle of "benchmark".

## The package

The package Imports LLMR (>= 0.8.9), which is on CRAN. It Suggests
LLMR.shiny (>= 0.1.2); every use of LLMR.shiny (and of the other GUI
dependencies shiny, bslib, DT) is guarded with requireNamespace(), and the
package's core has no Shiny dependency. The test suite and the vignette run
fully offline through the package's `.runner` seam; no example, test, or
vignette makes a network call.

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
