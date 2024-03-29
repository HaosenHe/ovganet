% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{gamma2beta}
\alias{gamma2beta}
\title{Convert latent variable coefficients (gammas) to variable coefficients (betas)}
\usage{
gamma2beta(gamma, incidence.mat, grp.vec, family)
}
\arguments{
\item{gamma}{A vector of latent variable coefficients.}

\item{incidence.mat}{Incidence matrix.}

\item{grp.vec}{A vector indicating group membership.}

\item{family}{"gaussian" for least squares problems and "binomial" for binary response.}
}
\value{
A vector of lasso coefficients.
}
\description{
Convert latent variable coefficients (gammas) to variable coefficients (betas)
}
