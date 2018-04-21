# 
# New functions

#' Extract R, G or V matrix in a mixed or GLS model
#' 
#' These functions call \code{\link{getVarCov}} in the 
#' \pkg{nlme} package. They are intended to have names and functions
#' that are easy to remember.
#' 
#' @param fit model created with \code{\link{lme}} or 
#' \code{\link{gls}}
#' @param individuals for \code{getR} or \code{getV} with
#'  \code{\link{lme}} objects, select the clusters for which
#'  variances are returned. If not specified, the variance of the
#'  first cluster is returned.
#' @return For \code{\link{lme}} objects, \code{getG} returns
#' the between-cluster variance of random effects, \code{getV},
#' and \code{getR} returns a list with the within-cluster marginal
#' variance and the within-cluster conditional variance respectively
#' for the the clusters listed in \code{individuals}. If 
#' \code{individuals} is missing, the variance of the first
#' cluster is returned. For 
#' \code{\link{gls}} objects ....
#' 
#' @examples
#' library(spida2)
#' library(nlme)
#' data <- expand.grid( Xdev = c(-3,-2,-1,0,1,2,3), id = 1:5 )
#'  
#' set.seed(12345)
#' data <- within(data, {
#'   Xmean <- 2*id
#'   X <- Xdev + Xmean
#'   Y <- (-1 + .1*rnorm(max(id)))[id] * Xdev + 
#'     2 * Xmean + .3 * id * rnorm(length(id))
#' })
#' 
#' library(lattice)
#' gd()
#' xyplot(Y ~ X, data, groups = id)
#' fit0 <- lme(Y ~ X, data,
#'             random = ~ 1+ X |id)
#' fit <- lme(Y ~ X, data, 
#'            random = ~ 1 + X | id,
#'            weights = varConstPower(form = ~ fitted(.)),
#'            correlation = corAR1(form = ~ 1 | id),
#'            control = list(returnObject = TRUE))
#' 
#' summary(fit)
#' getVarCov(fit)
#' getVarCov(fit, individuals = '2')
#' getVarCov(fit, individuals = '2', type = 'conditional') %>% 
#'   .[[1]] %>% 
#'   diag
#' getVarCov(fit,  type = 'conditional')%>% 
#'   .[[1]] %>% 
#'   diag
#' 
#' 
#' ?VarCorr
#' getG <- function(x) UseMethod('getG')
#' getG.default <- function(x,...) "Unknown class"
#' getG.lme <- function(x,...) getVarCov(x,...)
#' 
#' getR <- function(x) UseMethod('getR')
#' getR.default <- function(x, ...) "Unknown class"
#' getR.lme <- function(x, ...) {
#'   getVarCov(x, type = 'conditional', ...)
#' }
#' 
#' getV <- function(x) UseMethod('getV')
#' getV.default <- function(x, ...) "Unknown class"
#' getV.lme <- function(x, ...) {
#'   getVarCov(x, type = 'marginal', ...)
#' }
#' 
#' getG(fit)
#' getR(fit)[[1]]
#' getV(fit)[[1]]
#' 
#' 
#' (Z <- cbind(1, 2+seq(-3,3)))
#' Z
#' (getG(fit))
#' 
#' Z %*% getG(fit) %*% t(Z)
#' 
#' getV(fit)[[1]]
#' getR(fit)[[1]]
#' sigma(fit)
#' getVarCov(fit, type = 'random.effects')
#' getVarCov(fit)
#' Z %*% getG(fit) %*% t(Z)
#' getV(fit)[[1]] - Z %*% getG(fit) %*% t(Z) - getR(fit)[[1]]
#' 
#' getG(fit0)
#' Z %*% getG(fit0) %*% t(Z)
#' Z %*% getG(fit0) %*% t(Z) %>% svd %>% .$d
#' getR(fit0)
#' sigma(fit0)
#' getV(fit0)
#' Z %*% getG(fit0) %*% t(Z) + getR(fit0)[[1]]
#' @export
getG <- function(fit,...) UseMethod('getG')
getG.default <- function(fit,...) "Unknown class"
getG.lme <- function(fit,...) getVarCov(fit,...)
getG.gls <- function(git,...) getVarCov(fit,...)
#' @rdname getG
#' @export
getR <- function(fit,...) UseMethod('getR')
getR.default <- function(fit,...) "Unknown class"
getR.lme <- function(fit, ...) {
  getVarCov(fit, type = 'conditional', ...)
}
#' @rdname getG
#' @export
getV <- function(fit,...) UseMethod('getV')
getV.default <- function(fit, ...) "Unknown class"
getV.lme <- function(fit, ...) {
  getVarCov(fit, type = 'marginal', ...)
}

if(FALSE) {
library(spida2)
library(nlme)
data <- expand.grid( Xdev = c(-3,-2,-1,0,1,2,3), id = 1:5 )

set.seed(12345)
data <- within(data, {
  Xmean <- 2*id
  X <- Xdev + Xmean
  Y <- (-1 + .1*rnorm(max(id)))[id] * Xdev + 
    2 * Xmean + .3 * id * rnorm(length(id))
})

library(lattice)
gd()
xyplot(Y ~ X, data, groups = id)
fit0 <- lme(Y ~ X, data,
            random = ~ 1+ X |id)
fit <- lme(Y ~ X, data, 
          random = ~ 1 + X | id,
          weights = varConstPower(form = ~ fitted(.)),
          correlation = corAR1(form = ~ 1 | id),
          control = list(returnObject = TRUE))

summary(fit)
getVarCov(fit)
getVarCov(fit, individuals = '2')
getVarCov(fit, individuals = '2', type = 'conditional') %>% 
  .[[1]] %>% 
  diag
getVarCov(fit,  type = 'conditional')%>% 
  .[[1]] %>% 
  diag


?VarCorr
getG <- function(x) UseMethod('getG')
getG.default <- function(x,...) "Unknown class"
getG.lme <- function(x,...) getVarCov(x,...)

getR <- function(x) UseMethod('getR')
getR.default <- function(x, ...) "Unknown class"
getR.lme <- function(x, ...) {
  getVarCov(x, type = 'conditional', ...)
}

getV <- function(x) UseMethod('getV')
getV.default <- function(x, ...) "Unknown class"
getV.lme <- function(x, ...) {
  getVarCov(x, type = 'marginal', ...)
}

getG(fit)
getR(fit)[[1]]
getV(fit)[[1]]


(Z <- cbind(1, 2+seq(-3,3)))
Z
(getG(fit))

Z %*% getG(fit) %*% t(Z)

getV(fit)[[1]]
getR(fit)[[1]]
sigma(fit)
getVarCov(fit, type = 'random.effects')
getVarCov(fit)
Z %*% getG(fit) %*% t(Z)
getV(fit)[[1]] - Z %*% getG(fit) %*% t(Z) - getR(fit)[[1]]

getG(fit0)
Z %*% getG(fit0) %*% t(Z)
Z %*% getG(fit0) %*% t(Z) %>% svd %>% .$d
getR(fit0)
sigma(fit0)
getV(fit0)
Z %*% getG(fit0) %*% t(Z) + getR(fit0)[[1]]
}
