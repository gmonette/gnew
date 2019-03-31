# copied from Heather merge 2019_03_27
#' Print a nested data frame
#' 
#' @param dd a nested data frame
#' @param form a formula for grouping variables evaluated in dd
#' @param sortform an optional formal to sort rows within subgroups
#' @param sep separator between clusters
#' 
#' @export  
printup <- function(
  dd, form, sortform = NULL, 
  row.names = FALSE, right = FALSE,
  sep = "======================================", ...) {
  invars <- names(up(dd, form))
  vars <- setdiff(names(dd), invars)
  group_var <- model.frame(form, dd)[[1]]
  groups <- unique(group_var)
  # groups <- split(dd, dd[,invars], drop = TRUE)
  for(gg in groups){
    # if(!is.null(sortform)) gg <- sortdf(gg, sortform)
    subdd <- dd[gg == group_var,, drop = FALSE]
    cat('\n',sep,'\n',sep = '')
    print(subdd[1,invars, drop = FALSE], row.names = row.names, right = right, ...)
    cat('\n')
    print(subdd[,vars, drop = FALSE], row.names = row.names, right = right, ...)
  }
}
#'
#' Evaluate a string
#' 
#' @param s string toevaluate
#' @export
ev <- function(s) eval(parse(text = s))

#' Round numeric values in a a data frame
#' 
#' @param x data frame
#' @param ... other arguments to round if x has mode double
#' @export
rnd <- function(x,...) UseMethod('round')
#' @export
rnd.data.frame <- function(x,...) {
  as.data.frame(lapply(x, rnd), stringsAsFactors = FALSE)
}
#' @export
rnd.double <- function(x,...) round(x,...)
#' @export
rnd.default <- function(x,...) x


#' Assignment for a pipepline
#' 
#' @param value to assign
#' @param x name
#' @param pos position in search() list in which to assign (default = 1)
#' @param ... other arguments to 'assign'
#' 
#' @export
assn <- function(value, x, pos = 1, ...) assign(x = x, value = value, pos = pos, ...)
#' Distance between two strings using weighted overlap of words
#' 
#' @param a set of strings from first corpus
#' @param b set of strings from second corpus
#' @param weights to be given to 'intersection' and 'union' matching
#' distance respectively. If NULL (default), both distances are
#' returned.
#' 
#' @export
overlap_distance <- function(strings1, strings2, split = ' ') {
  # This function uses two sets of strings to create
  # a corpus of words each of whose frequencies is
  # inverted to create a weight.  
  # A function is produced that can be used to 
  # compute the 'distance' between two strings
  # in a way that depends on the presence of
  # words in both strings, with rare words given
  # a higher weight than common words
  corpus <- c(strings1, strings2)
  corpus <- lapply(corpus, strsplit, split = split)
  corpus_words <- unique(unlist(corpus))
  corpus_freq <- table(unlist(corpus))
  corpus_wt <- 1/corpus_freq
  corp0 <- 0 * corpus_wt
  
  retfcn <- function(a, b, weights = NULL) {
    # returns a vector 
    # - the first element is weight of the intersection over
    #   the minimum weight and the second is the weight of
    #   the intersection over the maximum weight.
    # - first will be equal to 1 if either string is a
    #   subset of the other, the latter will be 1 iff
    #   they are identical
    # - the components are weighted sums of words
    #   with weights inversely proportional to their
    #   frequency in the corpus
    force(weights)
    a <- unlist(strsplit(a, split = split))
    b <- unlist(strsplit(b, split = split))
    # disp(a)
    a <- unlist(lapply(a, intersect, corpus_words))
    b <- unlist(lapply(b, intersect, corpus_words))
    if(length(a) == 0 | length(b) == 0) {
      ret <- c(0,0)
    } else {
      comb <- c(a,b)
      # disp(comb)
      a <- table(a)
      b <- table(b)
      comb <- table(comb)
      corpa <- corpb <- corpcomb <- corp0
      corpa[names(a)] <- a
      corpb[names(b)] <- b
      # disp(length(corpa))
      # disp(length(corpb))
      corpint <- pmin(corpa,corpb)
      # disp(length(corpint))
      cintw <- sum(corpint * corpus_wt)
      caw <- sum(corpa * corpus_wt)
      cbw <- sum(corpb * corpus_wt)
      ret <- cintw / c(min(caw,cbw), max(caw,cbw))
    }
    if(!is.null(weights)) {
      1 - sum(weights*ret) } else {
        1 - ret
      }
  }
  retfcn
}