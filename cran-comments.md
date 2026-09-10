## R CMD check results

0 errors | 0 warnings | 1 note

The note is `Days since last update`. This release follows 0.1.0 quickly on purpose: it clears the clang-ASAN and gcc-ASAN entries on the additional issues page, which are due by 2026-09-12.

## The ASAN reports

The overlapping `memcpy` is in ragg's copy of AGG, at `agg_font_cache_manager.h:176`. No frame in either stack belongs to this package. It fires when AGG's 32-slot font cache evicts an entry, and that cache is a session-wide static, so rebuilding all the vignettes in one process eventually reaches it. I have reported it upstream with a reproducer.

This release stays under the threshold: the vignettes now use the platform's default `png()` device, with ragg left in only three small chunks. The R-hub clang-asan and gcc-asan jobs both pass.

## valgrind and rchk on R-hub

Both jobs are red, and both report only third-party findings.

valgrind's own verdict is `Status: OK`. The examples leak nothing; the tests show a few dozen records, none naming this package. They come from systemfonts, librsvg, fontconfig and R itself. The job fails only because R-hub treats a non-zero ERROR SUMMARY as failure, and every leak record counts as an error. Two leaks that were ours are fixed here: the layout engine's static macro tables, and a raw owning pointer in the line splitter.

rchk reports two lines inside `Rcpp/protection/Shield.h`, which every package using Rcpp produces. The `_gridmicrotex_*` entries carry no findings.

## Changes

The typeface fallback is now a message rather than a warning, raised only when `render_mode = "typeface"` was asked for, and at most once per device. `unloadNamespace()` now releases the macro tables and the shared object.

The diff is larger than that suggests: the portable C++ moved out of `src/` into the vendored engine directory, leaving only the R binding. No exported function or default changed.
