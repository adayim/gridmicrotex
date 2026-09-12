# Turning base-graphics math interception on and off.
#
# The C side registers a graphics system so it is told about every device
# the engine creates or destroys; this file only flips the switch and
# guarantees the switch and the devices never disagree.

# Arm or disarm every device. Safe to call repeatedly.
.gm_base_set <- function(on) {
  gm_base_set_enabled(isTRUE(on), .gm_base_layout)
  invisible(on)
}

#' Is base-graphics math interception active?
#'
#' Diagnostic helper: the number of open devices currently intercepted.
#' Should be 0 whenever \code{latex_options(device_math = TRUE)} has not
#' been set, and is used by the tests to prove that every device is
#' released again.
#'
#' @return Integer count of armed devices.
#' @noRd
.gm_base_armed <- function() gm_base_armed_count()
