## R CMD check results

0 errors | 0 warnings | 1 note

The note appears on the win-builder R-devel Windows builder only:

    * checking compiled code ... NOTE
    Error in ccE(lines, flags = new_flags, include = include) :
      'cc' is not on the path

It comes from the check code rather than from this package. `tools:::ccE()`
runs the literal command `cc`, which no Rtools toolchain provides (`R CMD
config CC` reports `gcc`), and its caller reads R's own headers under
`R.home("include")` to enumerate the R API. No file in this package is
involved, and the same check passes on the Debian builder, where `cc` exists.

The clang23 compilation error is fixed, together with the two warning classes
in that log. The package now builds with clang 23.1.0 under the same flags,
with no errors and no warnings.

12 bug fix and some improvements
