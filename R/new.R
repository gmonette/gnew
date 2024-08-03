# new functions
# gnew/R/new.R
#
#' List functions in all of two or more packages
#' 
#' Useful to check discrepancies among objects with the same name in different
#' packages. Use \link{ga} to 
#' 
#' @param sources a character vector or list with characters and/or numeric values
#'        identifying two or more sources in which to look for common functions as
#'        package names in quotes. "" refers to the Global environment
#' 
#' @return
#' The definitions of functions in two or more sources
#' 
#' @examples
#' compare_packages(c('spida2','gnew'))  
#' @export
compare_packages <- function(places) {
  library(spida2)
  places <- lapply(places, function(place) {
    if(!is.numeric(place) && !grepl(":", place)) place <- paste0('package:',place)
    else place
  })
  obnames <- lapply(places, function(place) {
    ls(envir = as.environment(place))
  })
  obs <- lapply(obnames, function(nn) {
    lapply(places, function(place) {
      get(nn, envir = place) 
    })
  })
  ret <- obnames[[1]]
  for(nns in obnames[-1]) {
    ret <- intersect(ret, nns)
  }
  cat('\n\n=============    COMMMON OBJECTS     ==============\n\n')
  print(ret)
  cat('\n')
  for(nn in ret) {
    cat('\n\n=============    ', nn, '     ==============\n\n')
    ln <- length(obs)
    dups <- rep.int(FALSE, ln)
    if (ln > 1L) 
      for (i in 2L:ln) for (j in 1L:(i - 1L)) if (identical(objs[[i]], 
                                                            objs[[j]], ignore.environment = TRUE)) {
        dups[i] <- TRUE
        break
      }
    for(ob in obs){
      print(nn)
      fn <- get(nn, envir = as.environment(ob))
      print(fn)
    } 
  }
}

#' 
#' Add descriptions to variables
#' 
#' Best to use dplyr left_join, etc to keep attributes
#' @export 
`desc<-` <- function(x, value) {
  attr(x,'desc') <- value
  return(x)
}
#' @export 
desc <- function(x, ...) UseMethod('desc')
#' @export 
desc.data.frame <- function(x,...) {
  mode <- sapply(x,mode)
  typeof <- sapply(x,typeof)
  mode <- ifelse(mode == typeof, mode, paste(mode,typeof,sep=':'))
  ret <- data.frame(
    # names = names(x), 
    mode = mode, 
    class = sapply(x, function(x) class(x)[1]),
    desc = unlist(sapply(x, desc))
  )
  rownames(ret) <- names(x)
  # disp(class(ret))
  class(ret) <- c("desc", class(ret))
  ret
}
#' @export 
desc.default <- function(x, ...) {
  ret <- attr(x, 'desc')
  if(is.null(ret)) '' else ret
}
#' @export 
print.desc <- function(x,...) {
  print.data.frame(x, ..., right = FALSE)
  invisible(x)
}
#' grep patterns and replace with name
#' 
#' @param matches  object in which to find regular expression matches
#' @param pattern  to match
#' @export
grepf <- function(matches, pats) {
  # 
  # replaces strings with a match with the grepped pattern
  # or with the name of the pattern
  # 
  # grepf(c(replace = 'matched'), 'this is matched') 
  # returns: 'replace'
  # 
  # 
  reps <- names(matches)
  if(is.null(reps)) reps <- matches
  # disp(reps)
  reps[reps==''] <- matches[reps=='']
  ret <- rep(NA, length(pats))
  for(i in seq_along(matches)){
    ret[grepl(matches[i],pats) & is.na(ret)] <- reps[i]
  }
  ret[is.na(ret)] <- pats[is.na(ret)]
  ret
}
if(FALSE) {
  local({
  grepf(c(replace = 'matched'), 'this is matched') 
  grepf(c('matched$'), c('this is matched','matched not') )
  grepf(c(matched = 'matched$'), c('this is matched','matched not') )
  
  a <- c('long short name','other name', 'short name','name not at end')  
  grepf(c('long', 'short'), a)
  grepf(c('long', 'short', name = 'name$'), a)
  grepf(c('long', 'short', 'Something more elaborate..'  = 'name$'), a)
  })
}
#' Difference of two dates in years
#' 
#' date2 - date1 in years
#' 
#' @param date1,date2 Date objects or objects that can be coerced to Dates
#' 
#' @returns difference: date2 - date 1 in years
#' 
#' @export
yearsdiff <- function(date1, date2, ...) {
  if(class(try(as.Date(date2))) == 'try-error') {
    disp(date2)
    disp(class(date2))
  }
  ret <- as.numeric(as.Date(date1) - as.Date(date2))/365.25
  attr(ret,'units') <- 'years'
  ret
}
#' as.data.frame.list fails if an element of the list is a list.
#' This version works in that eventuality.
#' 
#' @param x list of elements of equal length possibly including lists
#' @param row.names default NULL. See \link{as.data.frame}
#' @param optional logical. See \link{as.data.frame}
#' @param ... See \link{as.data.frame}
#' @examples
#' alist <- list(x = 1:3, a = letters[1:3], lst = list(a=1, b= 'a', c= list(1:4)))
#' as.data.frame_(alist)
#' @export
as.data.frame_ <- function(x, row.names = NULL, optional = FALSE, ...){
  if (is.null(x)) 
    return(as.data.frame(list()))
  UseMethod("as.data.frame_")
}

#' @describeIn as.data.frame_ \code{"default"} method
#' @export
as.data.frame_.default <- function(x, row.names = NULL, optional = FALSE, ... ){
  as.data.frame(x, row.names = row.names, optional = optional, ...)
}
#' @describeIn as.data.frame_ \code{"list"} method
#' @export
as.data.frame_.list <- function(x, row.names = NULL, optional = FALSE, ... ){
  xx <- Filter(x, function(x) !is.list(x))
  xl <- Filter(x, function(x) is.list(x))
  ret <- as.data.frame(xx, row.names = row.names, optional = optional, ...)
  for(i in seq_along(xl)) ret[[names(xl)[[i]]]] <- xl[[i]]
  ret
}
#' Remove lists from a data frame
#' 
#' Many methods for data frames fail is a variable is a list but lists can be very
#' useful for many purposes.
#' 
#' @param x a data frame
#' @param quiet do not warn if TRUE, default FALSE
#' 
#' @returns data set dropping variables that are lists
#' @export
droplists <- function(x,...) UseMethod("droplists")
#' @describeIn droplists
#' @export
droplists.data.frame <- function(x, quiet = FALSE, ...) {
  if(any(islist_ <- sapply(x, is.list))) {
    if(!quiet) warning(paste("Following variables dropped because they are lists:",
                             paste(names(x)[islist_], collapse = ', ')))
    x[!islist_]
    
  } else x
}



