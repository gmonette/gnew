% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new.R
\name{as.data.frame_}
\alias{as.data.frame_}
\alias{as.data.frame_.default}
\alias{as.data.frame_.list}
\title{as.data.frame.list fails if an element of the list is a list.
This version works in that eventuality.}
\usage{
as.data.frame_(x, row.names = NULL, optional = FALSE, ...)

\method{as.data.frame_}{default}(x, row.names = NULL, optional = FALSE, ...)

\method{as.data.frame_}{list}(x, row.names = NULL, optional = FALSE, ...)
}
\arguments{
\item{x}{list of elements of equal length possibly including lists}

\item{row.names}{default NULL. See \link{as.data.frame}}

\item{optional}{logical. See \link{as.data.frame}}

\item{...}{See \link{as.data.frame}}
}
\description{
as.data.frame.list fails if an element of the list is a list.
This version works in that eventuality.
}
\section{Methods (by class)}{
\itemize{
\item \code{as.data.frame_(default)}: \code{"default"} method

\item \code{as.data.frame_(list)}: \code{"list"} method

}}
\examples{
alist <- list(x = 1:3, a = letters[1:3], lst = list(a=1, b= 'a', c= list(1:4)))
as.data.frame_(alist)
}
