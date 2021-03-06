% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_algebra.R
\name{basis}
\alias{basis}
\title{Basis for linear space spanned by the columns of a matrix}
\usage{
basis(X, tol = 1e-09)
}
\arguments{
\item{X}{a matrix}

\item{tol}{the tolerance used to determine singularity. Default 1e-9}
}
\value{
a matrix of full column rank spanning the same space as \code{X}
}
\description{
Basis for linear space spanned by the columns of a matrix
}
