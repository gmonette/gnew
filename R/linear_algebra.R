#' Basis for linear space spanned by the columns of a matrix
#' 
#' @param X a matrix
#' @param tol the tolerance used to determine singularity. Default 1e-9
#'
#' @return a matrix of full column rank spanning the same space as \code{X}
#' @export
basis  <- function(X, tol = 1e-9) {
  # returns linear independent columns
  # with possible pivoting
  q <- qr(X, tol = tol)
  sel <- q$pivot[seq_len(q$rank)]
  ret <- X[, sel, drop = FALSE]
  colnames(ret) <- colnames(X)[sel]
  ret
}