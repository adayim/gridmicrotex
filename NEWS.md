# gridmicrotex 0.0.3

- Self-contained `load_font()` example so CRAN's donttest additional checks no longer fail on the unreliable CTAN font download.
- New commands.

# gridmicrotex 0.0.2

- Support the `\def` command
- New function `grobMark`.
- Bug fix `ggplot2` intergration.
- Bug fix coloring body.
- `ggplot2` integration respects `latex_options`.
- Defer `systemfonts` registration of the bundled Lete and STIX fonts to first render. This avoids the `XType: Using static font registry.` notice that older macOS SDKs emit on Core Text font registration, which had caused spurious WARN/NOTEs on `r-oldrel-macos-arm64`.

# gridmicrotex 0.0.1

Initial release.

