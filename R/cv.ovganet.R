#' Cross-validated overlapping group elastic net using package 'oem'
#'
#' @param X Design matrix (features). Note that the "oem" package we use is optimized for n » p settings.
#' @param y Response vector (outcomes).
#' @param group A list of vectors containing group information.
#' @param weights A vector of weights for each group.
#' @param alpha Mixing value for elastic.net.
#' @param type.measure Measure to evaluate for cross-validation.
#'  The default is type.measure = "deviance." See package "oem" for more options.
#' @param family Use "gaussian" for least squares problems and "binomial" for binary response.
#' @param ... other parameters to be passed to "cv.ovganet" function.
#' @return An object with S3 class "cv.ovganet".
#' @examples
#' library(doMC)
#' library(ovganet)
#' registerDoMC(5)
#' cv_overlap_grp_lasso <- cv.ovganet(X = X, y = y, group = group, weights = group_weights, family ='binomial', parallel = TRUE)
#' @import Matrix
#' @import oem
#' @export
# -------------------------------------------------------------------------------
cv.ovganet <- function(X, y,
                    group,
                    weights = NULL,
                    alpha = 1,
                    type.measure = 'deviance',
                    family = c('gaussian', 'binomial'), ...) {

  family <- match.arg(family)

  # Define penalty
  penalty <- 'grp.lasso'
  if (alpha != 1) penalty <- 'grp.lasso.net'

  # Check matrix:
  if (is.matrix(X)) {
    tmp <- try(X <- as.matrix(X), silent=TRUE)
    if (class(tmp)[1] == "try-error")  {
      stop("X must be a matrix or able to be coerced to a matrix")
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
  cvfit <- cv.oem(x = X.latent, y = y, groups = grp.vec, penalty = penalty, group.weights = weights, type.measure = type.measure, family = family, alpha = alpha, ...)
  cvfit$beta.latent <- cvfit$oem.fit$beta$grp.lasso
  cvfit$beta <- gamma2beta(gamma = cvfit$beta.latent, incid.mat, grp.vec, family = family)
  cvfit$incidence.mat <- incid.mat
  cvfit$group <- group
  cvfit$grp.vec <- grp.vec
  cvfit$weights <- weights

  # optimal (latent) groups based on CV
  index_cv_min <- which(cvfit$lambda[[1]] == cvfit$lambda.min)
  index_cv_1se <- which(cvfit$lambda[[1]] == cvfit$lambda.1se)

  if (alpha==1){
    cvfit$coef.latent.min <- cvfit$oem.fit$beta$grp.lasso[,index_cv_min]
    cvfit$coef.latent.1se <- cvfit$oem.fit$beta$grp.lasso[,index_cv_1se]
  }
  else{
    cvfit$coef.min <- cvfit$oem.fit$beta$grp.lasso[,index_cv_min]
    cvfit$coef.1se <- cvfit$oem.fit$beta$grp.lasso[,index_cv_1se]
  }
  class(cvfit) <- "cv.ovganet"
  cvfit
}
