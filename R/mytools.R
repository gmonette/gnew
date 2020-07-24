#' Custom install to make vignettes
#' 
#' @export
myinstall <- function(...) devtools::install(build_vignettes = TRUE,...)
#' Custom gitinstall to include vignettes
#' 
#' @export
mygitinstall <- function(x,...) devtools::install_github(x,build_opts = NULL, ...)
#'
#' Simple speed tests
#' 
#' @export
stest <- function() benchmark(
  exp = {for (i in 1:10^6) exp(i)},
  mult_diag = {for (i in 1:10^2) diag(1:100) %*% diag(1:100)},
  svd_singular = {for( i in 1:2) svd(matrix(1:100, 100,100))},
  svd_I = {for( i in 1:2) svd((diag(100)))})
