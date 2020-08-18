.onLoad <- function(libname, pkgname) {
  # install these from github, not CRAN:
  cat('\nRunning .onLoad\n')
  zzz <- 'was run'
  pkglist <- list(
    c(name='spida2',url='gmonette/spida2')
    )
  
  for(pkg in pkglist)
    if(!suppressWarnings(
      suppressPackageStartupMessages(
        require(pkg['name'], 
                quietly=TRUE,
                character.only=TRUE)))){
          devtools::install_github(pkg['url'])
      suppressPackageStartupMessages( 
        library(pkg['name'],character.only=TRUE))
  }
}