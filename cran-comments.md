## R CMD check results

0 errors | 0 warnings | 1 note

The note appears on the win-builder R-devel Windows builder only:

    * checking compiled code ... NOTE
    Error in ccE(lines, flags = new_flags, include = include) :
      'cc' is not on the path

It comes from the check code rather than from this package. `tools:::ccE()` runs the literal command `cc`, which no Rtools toolchain provides (`R CMD config CC` reports `gcc`). Its caller, `tools:::getOneFunAPI()`, is readin R's own headers under `R.home("include")` to enumerate the R API, so no source file in this package is involved; the error reproduces with the
package neither installed nor loaded. The same check passes on the Debian builder, where `cc` is present.

11 bug fix and some improvements
