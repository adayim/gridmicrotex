## R CMD check results

0 errors | 0 warnings | 1 note

The note is `Days since last update`. This release comes soon after 0.1.0 on
purpose: it clears the `clang-ASAN` and `gcc-ASAN` entries on the additional
issues page, which are due by 2026-09-12.

## About those ASAN reports

The overlapping `memcpy` they report is in ragg's vendored copy of AGG, at
`agg/include/agg_font_cache_manager.h:176`; no frame in either stack belongs
to this package. It runs only when AGG's 32-slot font cache evicts an entry,
and that cache is a session-wide `static`, so `R CMD check` rebuilding all
vignettes in one process accumulates enough distinct font sizes to reach it.
I have reported it upstream with a reproducer.

This release simply stops reaching that threshold: the vignettes now use far
fewer distinct font sizes. I verified it with an ASAN-instrumented build of
ragg and `R CMD check` covering examples, tests and vignettes, which reports
nothing. UBSAN over the same range is also clean, as is valgrind (its only
findings sit inside `librsvg`).

## Changes

The only user-visible change is that the typeface fallback is now a message
rather than a warning, raised only when `render_mode = "typeface"` was
actually asked for, and at most once per device.

## Also seen on win-builder

The R-devel Windows builder reports one further note:

    * checking compiled code ... NOTE
    Error in ccE(lines, flags = new_flags, include = include) :
      'cc' is not on the path

That comes from the check code, not from this package: `tools:::ccE()` runs
the literal command `cc`, which no Rtools toolchain provides (`R CMD config
CC` reports `gcc`), and its caller reads R's own headers to enumerate the R
API. The same check passes on the Debian builder, where `cc` exists.
