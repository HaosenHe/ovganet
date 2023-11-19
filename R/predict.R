#' Predict method for ovganet
#'
#' @param object An object of class 'ovganet'
#' @param newx New data to predict from. If not provided, will use the data from the original fit.
#' @param ... Additional arguments passed to the predict function of 'predict.oem'
#' @return Predicted values based on the 'ovganet' object
#' @export
predict.ovganet <- function(object, newx, ...) {
  if (!inherits(object, "ovganet")) stop("object must be of class 'ovganet'")
  group <- object$group
  X.latent <- expandMat(newx, group) # latent matrix
  new_object <- object
  new_object$beta <- list("grp.lasso" = object$beta.latent)
  # Extracting the core parts of the 'ovganet' object that are compatible with 'oem' predict
  predictions <- oem:::predict.oem(new_object, X.latent, ...)

  return(predictions)
}

#' Predict method for cv.ovganet
#'
#' @param object An object of class 'cv.ovganet'
#' @param newx New data to predict from. If not provided, will use the data from the original fit.
#' @param ... Additional arguments passed to the predict function of 'predict.cv.oem'
#' @return Predicted values based on the 'cv.ovganet' object
#' @export
predict.cv.ovganet <- function(object, newx, ...) {
  if (!inherits(object, "cv.ovganet")) stop("object must be of class 'cv.ovganet'")

  group <- object$group

  X.latent <- expandMat(newx, group) # latent matrix

  # Extracting the core parts of the 'cv.ovganet' object that are compatible with 'oem' predict
  cv_predictions <- oem:::predict.cv.oem(object, X.latent, ...)

  return(cv_predictions)
}
