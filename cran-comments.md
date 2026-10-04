## R CMD check results

0 errors | 0 warnings | 2 notes

`R CMD check --as-cran` on Ubuntu (R release), and the same with `NOT_CRAN=true` so that every test runs, give the same two notes:

* `CRAN incoming feasibility`: the number of updates in the past 6 months. This package has had several releases in a short time.
* `compilation flags used`: `-mno-omit-leaf-frame-pointer`, which is in Ubuntu's own R build and not in this package's flags.

The package also passes the tests and `R CMD check` on GitHub Actions (Ubuntu with oldrel, release and devel; Windows), and `--use-valgrind`, LTO, and the R-hub clang-asan, gcc-asan, clang-ubsan and nold containers show nothing from this package.

## valgrind and rchk on R-hub

These two jobs are red, and both report only third-party findings.

valgrind's own verdict is `Status: OK`. Its records are leaks in systemfonts, librsvg, fontconfig and R itself, and a few reads of uninitialised values inside librsvg; no frame of any of them belongs to this package, and there are no invalid reads or writes. The job fails only because R-hub treats a non-zero ERROR SUMMARY as failure, and every leak record counts as an error.

rchk reports five lines inside `Rcpp/protection/Armor.h` and `Shield.h`. A small test package with no code of ours gets the same five once one of its functions calls `Rcpp::DataFrame::create` (without that call it gets only the `Shield.h` line), so they come from Rcpp itself (1.1.2) and not from this package. The `_gridmicrotex_*` entries carry no findings.

## Changes

A new parser for LaTeX, a document mode and a markdown layout built on it, `load_font()`, and bug fixes; see NEWS.md.

The diff is large because the portable C++ lives in the vendored engine directory, with only the R binding in `src/`.
