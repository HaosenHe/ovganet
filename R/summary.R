#' Summary method for ovganet
#'
#' @param object An object of class 'ovganet'
#' @param ... Additional arguments passed to the summary function of 'summary.oem'
#' @return A summary object for 'ovganet'
#' @export
summary.ovganet <- function(object, ...) {
  if (!inherits(object, "ovganet")) stop("object must be of class 'ovganet'")

  # Extracting the core parts of the 'ovganet' object that are compatible with 'oem' summary
  # Assuming 'oem' has a summary method named 'summary.oem'.
  # If it doesn't, you'll need to adapt this.
  oem_summary <- oem:::summary.oem(object, ...)

  # Additional post-processing if required

  return(oem_summary)
}

#' Summary method for cv.ovganet
#'
#' @param object An object of class 'cv.ovganet'
#' @param ... Additional arguments passed to the summary function of 'summary.cv.oem'
#' @return A summary object for 'cv.ovganet'
#' @export
summary.cv.ovganet <- function(object, ...) {
  if (!inherits(object, "cv.ovganet")) stop("object must be of class 'cv.ovganet'")

  # Extracting the core parts of the 'cv.ovganet' object that are compatible with 'oem' summary
  # Assuming 'oem' has a summary method named 'summary.cv.oem'.
  # If it doesn't, you'll need to adapt this.
  oem_cv_summary <- oem:::summary.cv.oem(object, ...)

  # Additional post-processing if required

  return(oem_cv_summary)
}
