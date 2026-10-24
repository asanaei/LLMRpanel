## The update

This updates LLMRpanel 0.6.1, published on 2026-09-12. Version 0.6.2 was
prepared but never submitted; its changes are part of this release and are
listed under 0.6.2 in NEWS.

The main change of 0.6.3 corrects how survey answers become persona text.
Missing-data labels from survey files, such as "-9. Refused" and "-8. Don't
know", were written into personas as answers; they are now left out, with a
message naming the fields, through a new `na_strings` argument. Labeled
survey columns as `haven::read_dta()` returns them are decoded from their
value labels without haven, and an unlabeled code on a scale labeled only at
its ends is shown with that scale. Numbers are written in full rather than as
"1e+05". Identifier columns and the sampling-weight column are never written
into persona text. `panel_from_personas()` gains a `questions` argument, and
answers shown under column names rather than question wording draw a
warning. In the Studio, the persona preview now shows the text that is sent.

The package now ships the dataset `anes_2024_personas`, 100 respondents to the
ANES 2024 Time Series Study, which it previously used from LLMR. It was
rebuilt from the May 19, 2026 release of the study with corrected coding;
FocusGroup ships the identical file. The data file is small:
`data/anes_2024_personas.rda` is 24,980 bytes (about 300 KB in memory), and
the built tarball is about 320 KB.

Exported names are unchanged; the new arguments come after the existing
ones.

## Test environments

- local macOS 26.7.1 (aarch64-apple-darwin20), R 4.4.3: R CMD check
  --as-cran --no-manual on the built tarball, with LLMR 0.9.0, the version
  on CRAN, and LLMR.shiny 0.1.3. CRAN has LLMR.shiny 0.1.2; this release
  changes no call to LLMR.shiny. The check ran with user startup files
  disabled, so no API key was set.

## R CMD check results

0 errors | 0 warnings | 1 note

- "checking for future file timestamps ... NOTE: unable to verify current
  time": the check machine could not reach a time server.

CRAN's spell check may flag the author names Bisbee and Hainmueller and the
terms of art estimand and benchmarked in DESCRIPTION, and AMCE and Likert in
the help pages. All are spelled correctly.

The examples run offline. The live and batch examples are in `\donttest{}`
and run only when `GROQ_API_KEY` is set; the check above ran without it, so
no example contacted a provider.

## Reverse dependencies

None: `tools::package_dependencies("LLMRpanel", reverse = TRUE)` against
CRAN, counting Depends, Imports, LinkingTo, Suggests, and Enhances, finds no
package.
