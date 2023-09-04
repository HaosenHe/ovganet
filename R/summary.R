#' Summary method for ovganet
#'
#' @param obj An obj of class 'ovganet'
#' @param ... Additional arguments to be passed to the summary function of 'summary.oem'
#' @return A summary obj for 'ovganet'
#' @export
summary.ovganet <- function(obj, ...) {
  if (!inherits(obj, "ovganet")) stop("obj must be of class 'ovganet'")

  # Extracting the core parts of the 'ovganet' obj that are compatible with 'oem' summary
  # Assuming 'oem' has a summary method named 'summary.oem'.
  # If it doesn't, you'll need to adapt this.
  oem_summary <- oem:::summary.oem(obj, ...)

  # Additional post-processing if required

  return(oem_summary)
}

#' Summary method for cv.ovganet
#'
#' @param obj An obj of class 'cv.ovganet'
#' @param ... Additional arguments to be passed to the summary function of 'summary.cv.oem'
#' @return A summary obj for 'cv.ovganet'
#' @export
summary.cv.ovganet <- function(obj, ...) {
  if (!inherits(obj, "cv.ovganet")) stop("obj must be of class 'cv.ovganet'")

  # Extracting the core parts of the 'cv.ovganet' obj that are compatible with 'oem' summary
  # Assuming 'oem' has a summary method named 'summary.cv.oem'.
  # If it doesn't, you'll need to adapt this.
  oem_cv_summary <- oem:::summary.cv.oem(obj, ...)

  # Additional post-processing if required

  return(oem_cv_summary)
}
