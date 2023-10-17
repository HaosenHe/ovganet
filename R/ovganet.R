#' Overlapping group elastic net using package 'oem'
#'
#' @param X Design matrix (features). Note that "oem" is optimized for n » p settings.
#' @param y Response vector (outcomes).
#' @param group A list of vectors containing group information.
#' @param weights A vector of weights for each group.
#' @param alpha Mixing value for elastic.net.
#' @param family Use "gaussian" for least squares problems and "binomial" for binary response.
#' @param ... other parameters passed to "cv.ovganet" function.
#' @return An object with S3 class "ovganet".
#' @examples
#' library(doMC)
#' library(ovganet)
#' registerDoMC(5)
#' data(mtcars)
#' X <- as.matrix(mtcars[,-1])
#' y <- as.vector(mtcars$mpg)
#' group = list(c(1,2), c(2,3), c(3,4,5),c(4,5,6))
#' fit <- ovganet(X = X, y = y, group = group, family ='gaussian')
#' @import Matrix
#' @import oem
#' @export
ovganet <- function(X, y,
                    group,
                    weights = NULL,
                    alpha = 1,
                    family = c('gaussian', 'binomial'), ...) {

  family <- match.arg(family)

  # Define penalty
  penalty <- 'grp.lasso'
  if (alpha != 1) penalty <- 'grp.lasso.net'

  # Check matrix:
  if (is.matrix(X)) {
    tmp <- try(X <- as.matrix(X), silent=TRUE)
    if (class(tmp)[1] == "try-error")  {
      stop("X must be a matrix or able  coerced to a matrix")
    }
  }
  if (storage.mode(X)=="integer") X <- 1.0*X

  # Check weights
  if (is.null(weights)) weights <- sqrt(lengths(group))

  # Format latent groups
  incid.mat <- incidenceMat(X, group) # incidence matrix
  over.mat <- over.temp <- Matrix(incid.mat %*% t(incid.mat)) # overlapping matrix
  grp.vec <- rep(1:nrow(over.mat), times = diag(over.mat)) # group vector fed to oem
  X.latent <- expandMat(X, group) # latent matrix

  # Fit model
  fit <- oem(x = X.latent, y = y, groups = grp.vec, penalty = penalty, group.weights = weights, family = family, alpha = alpha, ...)
  fit$beta.latent <- fit$beta$grp.lasso
  if(alpha==1){
    fit$beta <- gamma2beta(gamma = fit$beta$grp.lasso, incid.mat, grp.vec, family = family)
  }
  else{
    fit$beta <- gamma2beta(gamma = fit$beta$grp.lasso.net, incid.mat, grp.vec, family = family)
  }
  fit$incidence.mat <- incid.mat
  fit$group <- group
  fit$grp.vec <- grp.vec
  fit$weights <- weights
  class(fit) <- "ovganet"

  fit
}

