#' Custom install to make vignettes
#' 
#' @export
myinstall <- function(...) devtools::install(build_vignettes = TRUE,...)
#' Custom gitinstall to include vignettes
#' 
#' @export
mygitinstall <- function(x,...) devtools::install_github(x,build_opts = NULL, ...)