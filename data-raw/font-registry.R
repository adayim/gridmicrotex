# Generate R/sysdata.rda. Re-run when the downloadable font assets change.
#   Rscript data-raw/font-registry.R

font_dir     <- "inst/fonts"
release_tag  <- "fonts-v1"
release_base <- sprintf(
  "https://github.com/adayim/gridmicrotex/releases/download/%s",
  release_tag
)

make_entry <- function(display, key, aliases, otf, clm) {
  otf_path <- file.path(font_dir, otf)
  clm_path <- file.path(font_dir, clm)
  stopifnot(file.exists(otf_path), file.exists(clm_path))
  list(
    key          = key,
    display_name = display,
    aliases      = aliases,
    otf = list(
      file   = otf,
      url    = sprintf("%s/%s", release_base, otf),
      sha256 = unname(tools::sha256sum(otf_path)),
      size   = file.info(otf_path)$size
    ),
    clm = list(
      file   = clm,
      url    = sprintf("%s/%s", release_base, clm),
      sha256 = unname(tools::sha256sum(clm_path)),
      size   = file.info(clm_path)$size
    )
  )
}

.font_registry <- list(
  lm = make_entry(
    "Latin Modern Math", "lm", c("lm", "latinmodern"),
    "latinmodern-math.otf", "latinmodern-math.clm2"
  ),
  stix = make_entry(
    "STIX Two Math", "stix", c("stix", "stix2"),
    "STIXTwoMath-Regular.otf", "STIXTwoMath-Regular.clm2"
  ),
  dejavu = make_entry(
    "TeX Gyre DejaVu Math", "dejavu", c("dejavu", "texgyre"),
    "texgyredejavu-math.otf", "texgyredejavu-math.clm2"
  )
)

save(.font_registry, file = "R/sysdata.rda", compress = "xz")
message("Wrote R/sysdata.rda with ", length(.font_registry), " entries.")
