#' Expand a matrix of predictors to a matrix of latent variables.
#'
#' @param X Design matrix (features).
#' @param group A list of vectors containing group information.
#' @return A sparse matrix with all latent variables included
#' @import Matrix
#' @import oem
#' @export
expandMat <- function(X, group) { # Expand X to augmented matrix with latent vars (See Obozinski et. al, 2011)
  incidence.mat <- incidenceMat(X, group) # group membership incidence matrix
  over.mat <- Matrix(incidence.mat %*% t(incidence.mat), sparse = TRUE)
  grp.vec <- rep(1:nrow(over.mat), times = diag(over.mat)) # group index vector
  # expand X to X.latent
  X.latent <- NULL
  names <- NULL
  ## the following code will automatically remove variables not included in 'group'
  for(i in 1:nrow(incidence.mat)) {
    idx <- incidence.mat[i,]==1
    X.latent <- cbind(X.latent, X[, idx, drop=FALSE])
    names <- c(names, colnames(incidence.mat)[idx])
  }
  colnames(X.latent) <- paste('grp', grp.vec, '_', names, sep = "")
  X.latent
}

#' Convert latent variable coefficients (gammas) to variable coefficients (betas)
#'
#' @param X Design matrix (features).
#' @param incidence.mat Incidence matrix
#' @param grp.vec A vector indicating group membership
#' @param family "gaussian" for least squares problems and "binomial" for binary response.
#' @return A vector of lasso coefficients.
#' @import Matrix
#' @import oem
#' @export
gamma2beta<- function(gamma, incidence.mat, grp.vec, family) {
  p <- ncol(incidence.mat)
  J <- nrow(incidence.mat)
  beta <- matrix(0, ncol = ncol(gamma), nrow = p)
  intercept <- gamma[1, , drop = FALSE]
  gamma <- gamma[-1, , drop = FALSE]
  for (i in 1:J) {
    ind <- which(incidence.mat[i, ] == 1)
    beta[ind, ] <- beta[ind, ] + gamma[which(grp.vec == i), , drop = FALSE]
  }
  if (family != 'cox') {
    beta <- rbind(intercept, beta)
    rownames(beta) <- c("(Intercept)", colnames(incidence.mat))
  } else {
    rownames(beta) <- colnames(incidence.mat)
  }
  beta
}

#' Create an incidence matrix indicating group membership
#'
#' @param X Design matrix (features).
#' @param group A list of vectors containing group information.
#' @return A sparse matrix with all latent variables included
#' @import Matrix
#' @import oem
#' @export
incidenceMat <- function(X, group) {
  n <- nrow(X)
  p <- ncol(X)
  if (! is.list(group)) {
    stop("Argument 'group' must be a list of integer indices or character names of variables!")
  }
  J <- length(group)
  grp.mat <- Matrix(0, nrow = J, ncol = p, sparse = TRUE,
                    dimnames=list(as.character(rep(NA, J)),
                                  as.character(rep(NA, p))))
  if(is.null(colnames(X))) {
    colnames(X) <- paste("V", 1:ncol(X), sep="")
  }
  if (is.null(names(group))) {
    names(group) <- paste("grp", 1:J, sep="")
  }

  if (is.numeric(group[[1]])) {
    for (i in 1:J) {
      ind <- group[[i]]
      grp.mat[i, ind] <- 1
      colnames(grp.mat)[ind] <- colnames(X)[ind]
    }
  } else { ## character, names of variables
    for (i in 1:J) {
      grp.i <- as.character(group[[i]])
      ind <- colnames(X) %in% grp.i
      grp.mat[i, ] <- 1*ind
      colnames(grp.mat)[ind] <- colnames(X)[ind]
    }
  }
  rownames(grp.mat) <- as.character(names(group))
  # check grp.mat
  if (all(grp.mat == 0)) {
    stop("The names of variables in X don't match with names in group!")
  }
  grp.mat
}

