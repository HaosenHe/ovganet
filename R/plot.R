#' Plot method for ovganet plotting (coef. path)
#'
#' @param x An object of class 'ovganet'
#' @param ... Additional arguments passed to the plot function of 'plot.oem'
#' @export
plot.ovganet <- function(x, ...) {
  if (!inherits(x, "ovganet")) stop("x must be of class 'ovganet'")
  # Using oem's plot function
  new_obj <- x
  new_obj$beta <-list("grp.lasso" = x$beta.latent)
  oem:::plot.oem(new_obj, ...)
}

#' Plot method for cv.ovganet
#'
#' @param x An object of class 'cv.ovganet'
#' @param ... Additional arguments passed to the plot function of 'plot.cv.oem'
#' @export
plot.cv.ovganet <- function(x, ...) {
  if (!inherits(x, "cv.ovganet")) stop("obj must be of class 'cv.ovganet'")
  # Extracting the core parts of the 'ovganet' object that are compatible with 'oem' plot
  # Using oem's plot function
  oem:::plot.cv.oem(x, ...)
}

