##  General coding matrix for factors
##   
##
##
#' General coding matrix for factors
#' 
#' This function creates functions (closures) that implement a flexible coding
#' matrix for a factor.
#'
#' The coding function can be designed with specified constraints and 
#' specified estimated model parameters.
#' 
#' The coding function can also be used to generate portions of hypothesis 
#' matrices to estimate interpretable functions of parameters.
#' 
#' 
#' @export
coding_function <- function(
  x, 
  levs = levels(as.factor(x)), 
  intercept = rbind(c(1,rep(0,length(levs)-1))),
  constraints = NULL,
  estimates = diag(length(levs))[-1,, drop = FALSE],
  tolerance = 1e-16,
  debug = FALSE
) {
  #basis <- gnew::basis
  
  Xmat <- diag(length(levs))
  rownames(Xmat) <- levs
  Cmat <- rbind(intercept, constraints)
  Cmat <- t(basis(t(Cmat)))
  Emat <- rbind(estimates, Xmat)
  A <- rbind(Cmat, Emat)
  A <- t(basis(t(A)))
  Emat <- A[-seq_len(nrow(Cmat)),,drop=FALSE]
  
  if(debug) svs <- svd(A,nu=0,nv=0)
  
  G <- solve( A, Xmat[, -seq_len(nrow(Cmat)), drop = FALSE])
  colnames(G) <- rownames(Emat)
  # 
  # create closure
  #
  ret <- function(x, D = NULL) {
    if(is.null(D)) return(Xmat[x,,drop=FALSE] %*% G) # model matrix
    else('not yet done')
  }
  class(ret) <- 'coding_matrix'
  ret
}

if(FALSE) {
  zd <- data.frame(x = 1:20, a = sample(letters[1:4], 20, replace = T))
  zd $y <- zd$x + rnorm(20)
  
  cm <- coding_function(zd$a)
  cm
  cm(zd$a)
  summary(lm(y~cm(a), zd ))
  summary(lm(y~cm(a), zd ))
}



