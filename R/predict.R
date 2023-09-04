#' Predict method for ovganet
#'
#' @param obj An object of class 'ovganet'
#' @param newx New data to predict from. If not provided, will use the data from the original fit.
#' @param ... Additional arguments to be passed to the predict function of 'predict.oem'
#' @return Predicted values based on the 'ovganet' object
#' @export
predict.ovganet <- function(obj, newx, ...) {
  if (!inherits(obj, "ovganet")) stop("object must be of class 'ovganet'")
  group <- obj$group
  X.latent <- expandMat(newx, group) # latent matrix
  new_obj <- obj
  new_obj$beta <-list("grp.lasso" = obj$beta.latent)
  # Extracting the core parts of the 'ovganet' object that are compatible with 'oem' predict
  predictions <- oem:::predict.oem(new_obj, X.latent, ...)

  return(predictions)
}

#' Predict method for cv.ovganet
#'
#' @param obj An object of class 'cv.ovganet'
#' @param newx New data to predict from. If not provided, will use the data from the original fit.
#' @param ... Additional arguments to be passed to the predict function of 'predict.cv.oem'
#' @return Predicted values based on the 'cv.ovganet' object
#' @export
predict.cv.ovganet <- function(obj, newx, ...) {
  if (!inherits(obj, "cv.ovganet")) stop("object must be of class 'cv.ovganet'")

  group <- obj$group

  X.latent <- expandMat(newx, group) # latent matrix

  # Extracting the core parts of the 'cv.ovganet' object that are compatible with 'oem' predict
  cv_predictions <- oem:::predict.cv.oem(obj, X.latent, ...)

  return(cv_predictions)
}
