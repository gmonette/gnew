#' Take a slice of an array selecting indices along a margin
#' 
#' Performs the same role as selecting with subscripts except that
#' this is easier to use when dimension of the array is not known.
#' Its application is limited, however, to selecting single slices
#' along a single dimension.
#' 
#' @param a an array
#' @param MARGIN a single dimension along which to select a slice (i.e. a section)
#' @param ind the level at which to select the slice. If ind is out of range,
#'        slice returns NAs with the correct structure
#' @examples
#' Titanic
#' slice(Titanic, 1, 1)  
#' slice(slice(Titanic, 1, 1), 1, 2)  
#' @export
slice <- function(a, MARGIN, ind) {
  ret <- array(a[slice.index(a, MARGIN) == ind], dim(a)[-MARGIN])
  dimnames(ret) <- dimnames(a)[-MARGIN]
  ret
}
slice_alt <- function(a, MARGIN, ind) {
  # returns an error if ind is out of bounds 
  structure(a[slice.index(a, MARGIN) == ind], 
            dim = dim(a)[-MARGIN],
            dimnames = dimnames(a)[-MARGIN])
}
#' Alternative printing for special purposes
#' 
#' @param x an object to be printed
#' @param ... additional arguments
#' 
#' @examples
#' pr(Titanic)
#' @export
pr <- function(x, ...) UseMethod('pr')
#' @describeIn pr default method
#' @export
pr.default <- function(x, ...) print(x, ...)
#' @describeIn pr array method
#' @export
pr.array <- function(x, ...) {
  if(length(dim(x)) < 3 ) print(x,...)
  else {
    x <- aperm(x, c(length(dim(x)),seq_len(length(dim(x))-1)))
    dn <- dimnames(x)[[1]]
    head <- names(dimnames(x))[1]
    for(i in seq_len(dim(x)[1])) {
      cat(paste0(head,": ", dn[i], '\n'))
      pr(slice(x,1,i),...)
    }
  }
}
#' @describeIn pr table method
#' @export
pr.table <- function(x, ...) pr.array(x, ...)
#' 
#' Turn an array into a list of smaller arrays sectioning along a margin
#' 
#' @param a array
#' @param MARGIN vector of dimensions along which to section
#' 
#' @returns list of subarrays with names consisting of headings for each subarray
#'
#' @examples 
#' a <- Titanic
#' library(spida2)
#' tab(a)
#' tab(aperm(a,c(2,4,1,3))
#' tab(aperm(a,c(2,4,1,3)), pct = c(1,3,4))
#' round(tab(aperm(a,c(2,4,1,3)), pct = c(1,3,4)),1)
#' list_array(round(tab(aperm(a,c(2,4,1,3)), pct = c(1,3,4)),1),3:4)
#' 
#' list_array(tab(a),3:4)
#' list_array(a,3:4)
#' list_array(tab(a, pct = c(1,3:4),3:4)
#' @export
list_array <- function(a, MARGIN) {
  inds <- slice.index(a, MARGIN)
  # disp(inds)
  dim_out <- dim(a)[-MARGIN]
  # disp(dim_out)
  dn_out <- dimnames(a)[-MARGIN]
  # disp(dn_out)
  # disp(unique(c(inds)))
  ret <- lapply(unique(c(inds)), function(i) {
    sub_arr <- a[inds == i]
    dim(sub_arr) <- dim_out
    dimnames(sub_arr) <- dn_out
    sub_arr
  })
  heads <- dimnames(a)[MARGIN]
  # disp(heads)
  labs <- do.call(expand.grid, heads)
  # disp(labs)
  # pad
  labs <- labs[rev(1:ncol(labs))]
  maxlen <- max(nchar(names(labs)))
  pad <- lapply(maxlen - nchar(names(labs)), function(iter) paste0(rep('_', iter), collapse = ''))
  names(labs) <- paste0(pad, names(labs) )
  labs <- lapply(seq_along(labs), function(i) paste0(names(labs)[i],': ', labs[[i]],'\n'))
  # disp(labs)
  labs <- do.call(paste0,labs)
  labs <- gsub('_',' ', labs)
  names(ret) <- labs
  class(ret) <- 'list_array'
  ret
}
#' @export
print.list_array <- function(x,...) {
  for(i in seq_along(x)) {
    cat('\n')
    cat(names(x)[i])
    cat('\n')
    print(x[[i]],...)
  }
  invisible(x)
}
#' Use kableExtra::kbl to print an array. 
#' 
#' Requires version 1.2.0 or later of kableExtra
#' from devtools::install_github("haozhu233/kableExtra")
#' 
#' See https://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf. 
#' The output can be postprocessed with other function in kableExtra.
#' 
#' @param a array
#' @param MARGIN a vector of dimension along which the array will be collapse
#' @examples
#' \dontrun{
#' kbl_array(Titanic, 3:4, longtable = T)
#' }
#' @export
kbl_array <- function(a, MARGIN, ...) {
  library(kableExtra)
  larr <- list_array(a, MARGIN)
  mat <- do.call(rbind, larr)
  narr <- sapply(larr, nrow)
  nend <- cumsum(narr)
  nr <- narr[1]
  nstart <- nend - nr + 1
  
  ret <- kableExtra::kbl(mat,...)
  for( i in seq_along(narr)){
    ret <- kableExtra::group_rows(ret, names(larr)[i], start_row = nstart[i], end_row = nend[i]) 
  }
  ret <- kableExtra::kable_styling(ret)
  ret
}

if(FALSE){
  library(gnew)
  library(kableExtra)
  a <- Titanic
  list_array(a, 4)  
  list_array(a, 4)  %>% pr
  list_array(a, 3:4)  
  list_array(a, 3:4)  %>% pr
  kbl_array(tab(a),2:3)
  kbl_array(tab(a, pct =1 ),2:3)
  
}