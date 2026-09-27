# Turning base-graphics math interception on and off.
#
# The C side registers a graphics system so it is told about every device
# the engine creates or destroys; this file only flips the switch and
# guarantees the switch and the devices never disagree.

# Arm or disarm every device. Safe to call repeatedly.
.gm_base_set <- function(on) {
  gm_base_set_enabled(isTRUE(on), .gm_base_layout)
  if (!isTRUE(on) && gm_base_release_pending()) .gm_base_watch_release()
  invisible(on)
}

# Name of the task callback below; .onUnload() removes it.
.gm_release_task <- "gridmicrotex-release"

# A device another package still wraps keeps the graphics system
# registered after switching off (release_system() in
# src/device_hook.cpp). Its entry is dropped when that device closes, but
# from inside the engine's own teardown, where unregistering is not safe.
# So the job is finished after the top-level call that closed it; until
# then every recordPlot() snapshot names gridmicrotex, and replaying one
# calls library(gridmicrotex).
.gm_base_watch_release <- function() {
  if (!.gm_release_task %in% getTaskCallbackNames()) {
    addTaskCallback(function(...) gm_base_release_pending(),
                    name = .gm_release_task)
  }
  invisible()
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
