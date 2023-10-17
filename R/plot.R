#' Plot method for ovganet plotting (coef. path)
#'
#' @param obj An object of class 'ovganet'
#' @param ... Additional arguments passed to the plot function of 'plot.oem'
#' @export
plot.ovganet <- function(obj, ...) {
  if (!inherits(obj, "ovganet")) stop("x must be of class 'ovganet'")
  # Using oem's plot function
  new_obj <- obj
  new_obj$beta <-list("grp.lasso" = obj$beta.latent)
  oem:::plot.oem(new_obj, ...)
}

#' Plot method for cv.ovganet
#'
#' @param obj An object of class 'cv.ovganet'
#' @param ... Additional arguments passed to the plot function of 'plot.cv.oem'
#' @export
plot.cv.ovganet <- function(obj, ...) {
  if (!inherits(obj, "cv.ovganet")) stop("obj must be of class 'cv.ovganet'")
  # Extracting the core parts of the 'ovganet' object that are compatible with 'oem' plot
  # Using oem's plot function
  oem:::plot.cv.oem(obj, ...)

}

