## The update

This updates LLMRpanel 0.6.1, published on 2026-09-12. When `rows` selected a
single row k, `panel_from_personas()` sampled from rows 1 to k; it now draws
the selected row, refuses fractional, character, and factor selectors and a
fractional `n`, and records the source row of each persona in a `source_rows`
attribute. The Studio refuses population margins and benchmark shares it
cannot parse instead of dropping them. The item constructors refuse missing,
empty, or indistinguishable labels and malformed ids, and `conjoint_design()`
refuses fractional or zero counts and unusable attribute names or levels.
`conjoint_amce()` records and prints its reference levels and the number of
estimation clusters, and warns when a declared level is absent from the
estimation data. `panel_bias_audit()` gives a reason whenever it reports no
order-test p-value. The help, the vignette, and the shipped usage guide no
longer describe the item-order randomization and sample-size planning removed
in 0.6.1, and the key-gated `panel_batch_submit()` example submits three
requests instead of 200. Exported names and documented result columns are
unchanged; NEWS lists each new error.

## Test environments

- local macOS 26.7.1 (aarch64-apple-darwin20), R 4.4.3: R CMD check
  --as-cran --no-manual on the built tarball, with
  _R_CHECK_FORCE_SUGGESTS_=false, against LLMR 0.8.11 from CRAN with
  LLMR.shiny 0.1.2 from CRAN and, separately, with LLMR.shiny 0.1.3, which is
  being submitted at the same time.

## R CMD check results

0 errors | 0 warnings | 1 note

- "checking for future file timestamps ... NOTE: unable to verify current
  time": the check machine could not reach a time server.

The examples run offline. The live and batch examples are in `\donttest{}`
and run only when `GROQ_API_KEY` is set; the check above ran without it, so
no example contacted a provider.

## Reverse dependencies

None.
