## ----eval=FALSE,echo=FALSE-----------------------------------------------
#  rmarkdown::html_vignette
#  vignette: >
#    %\VignetteIndexEntry{pdInd: G matrix with patterns of zeros}
#    %\VignetteEngine{knitr::rmarkdown}
#    %\VignetteEncoding{UTF-8}

## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "  "
)
  library(gnew)
  library(spida2)
  library(nlme)

## ------------------------------------------------------------------------

(fac <- cbind( c(1,0,0,0,0), c(1,2,0,0,0), c(0,1,3,0,0), c(1,0,0,4,0), c(0,1,0, 0, 5) ))
fac %*% t(fac)


## ------------------------------------------------------------------------
(fac <- cbind( c(1,0,0,0,0), c(1,2,0,0,0), c(0,1,3,0,0), c(1,0,0,4,0), c(0,1,0, -1, 5) ))
fac %*% t(fac)

## ------------------------------------------------------------------------
methods(class='pdInd')

gnew:::pdInd

gnew:::pdConstruct.pdInd

gnew:::pdFactor.pdInd

gnew:::pdMatrix.pdInd

gnew:::solve.pdInd

## ------------------------------------------------------------------------
methods(class='pdMat')

nlme:::pdMat

nlme:::pdConstruct.pdMat

nlme:::pdMatrix.pdMat

nlme:::pdFactor.pdMat

nlme:::solve.pdMat

nlme:::VarCorr.pdMat

nlme:::as.matrix.pdMat

nlme:::`matrix<-.pdMat`

nlme:::coef.pdMat

nlme:::`coef<-.pdMat`


## ------------------------------------------------------------------------

nlme:::pdSymm

methods(class='pdSymm')

nlme:::pdConstruct.pdSymm

nlme:::pdMatrix.pdSymm

nlme:::pdFactor.pdSymm

nlme:::solve.pdSymm

nlme:::coef.pdSymm


## ------------------------------------------------------------------------

nlme:::lme.formula

nlme:::reStruct


