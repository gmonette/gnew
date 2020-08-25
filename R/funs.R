# from UTFA common.R
#' Order a factor using the frequency of levels
#' 
#' @param x a vector: numerical, character, factor, etc.
#' 
#' @return a factor with levels ordered by frequency (largest first)
#' @examples
#'   zf <- letters[c(1,1,2,2,2,3,2,4,5,6)]
#' zf
#' table(zf)
#' zo <- order_by_freq(zf)
#' zo
#' table(zo)
#' attributes(zo)
#' @export
order_by_freq <- function(x, order = is.ordered(x)){
  x <- as.factor(x)
  ns <- - spida2::capply(x, x, length)
  stats:::reorder.default(x, ns, order = order)
} 
#
# from Ali/funs.F
#' 
#' Turn character or factor to numeric
#' 
#' @param x character or factor representing a numeric value
#' 
#' @return a numeric value represented by 'x', or NA
#' @export
tonum <- function(x) as.numeric(as.character(x))
#' Character to factor
#' 
#' @param x object to be turned into a factor if 'is.character' is true
#' @param ... arguments to factor
#' 
#' @return factor(x)
#' 
#' @rdname tonum
#' @export
c2f <- function(x, ...) if(is.character(x)) factor(x,...) else x
#' Character to factor
#' 
#' @param x object to be turned into a character if 'is.factor' is true
#' 
#' @return factor(x)
#' 
#' @rdname tonum
#' @export
f2c <- function(x) if(is.factor(x)) as.character(x) else x
#' 
#' Combine small categories
#' 
#' @param x factor or character to be condensed
#' @param nlevels number of levels to keep
#' @param other name for 'other' category
#' @param verbose default FALSE
#' 
#' @aliases gr
#' @rdname tonum
#' @export
combine_small_categories <- function(x, nlevels = 3, other = "Other", verbose = FALSE, ...) {
  if(is.numeric(x)) return(x)
  tt <- table(x)
  if(length(tt) < nlevels + 1) return (x)
  tt <- rev(sort(tt[rev(order(names(tt)))]))
  from <- names(tt)
  to <- names(tt)
  to[(nlevels):(length(to))] <- other
  if(verbose) print(data.frame(from = from, to = to, n = c(tt)))
  spida2::tran(from, to, x)
}
#'
gr <- combine_small_categories

if(FALSE) {
  z <- rep(letters[1:4], c(5,5,3,1))
  gr(z,3)
}


Clip <- function(x, value = .7, n = NULL) {
  if(!is.null(n)) {
    if(length(n) == 1) n <- c(n,n)
    vals <- sort(x)
    value <- vals[c(1+n[1], length(vals) - n[2])]
  }
  if(length(value) == 1) value = c(-value,value)
  pmin(pmax(x, value[1]), value[2])
}
if(FALSE) {
  z <- c(1.1, 1.5, 2.4, -4.1, Inf, NA, -Inf)  
  Clip(z, 2)
  Clip(z, Inf)
  Clip(z, n = c(1,3))
  z <- 1:10
  Clip(z, n = c(0,0))
  Clip(z, n = c(1,0))
  Clip(z, n = c(1,4))
}

valid <- function(x) !is.na(x)
# test:
if(FALSE){
  zf <- factor(c('a','a','a','b','c','d','e','z','z'))
  combine_small_categories(zf)
  combine_small_categories(letters)
  combine_small_categories(factor(letters))
  tran('a','other',letters)
}


#
# case <- function(condition, ...) {
#   replace <- list(...)
#   levels <- names(replace)
#   which <- match(as.character(condition), levels)
#   what <- do.call(cbind, replace)
#   what[cbind(1:nrow(what), which)]
# }
# --- use much improved version in spida2
#
#' Patterns of missing values by variable 
#' 
#' @param data frame to analyze
#' 
#' @export
tablemissing <-
function (x, sortby = "variable", 
          bot = 6, mar = c(bot, 1, 1.5, 1), 
          adj = c(0, -0.1), xpd = TRUE, srt = -60, cex = 0.8, 
          main = 'Missing Value Patterns',
          ...) 
{
  x1 <- as.numeric(apply(x, 2, function(x) length(which(is.na(x)))))
  x1 <- c(x1, nrow(x))
  z1 <- ifelse(is.na(x), 0, 1)
  tab = table(apply(z1, 1, paste, collapse = ","))
  tab = tab[order(names(tab), decreasing = TRUE)]
  tab = data.frame(combination = names(tab), count = as.numeric(tab))
  tabp <- t(apply(tab, 1, function(x) {
    as.numeric(unlist(strsplit(x, ",", fixed = TRUE)))
  }))
  tabp <- as.data.frame(tabp)
  tabp <- rbind(tabp, x1)
  names(tabp) <- c(names(x), "Total")
  row.names(tabp) <- c(seq(1, nrow(tab)), "Total")
  if (sortby == "variable") {
    tabfinal <- tabp
  }
  if (sortby == "row") {
    tabfinal <- tabp[-nrow(tabp), ]
    tabfinal <- tabfinal[order(tabfinal$Total, decreasing = TRUE), 
                         ]
    tabfinal <- rbind(tabfinal, tabp[nrow(tabp), ])
  }
  if (sortby == "column") {
    tabfinal <- tabp[, -ncol(tabp)]
    vals <- unlist(tabfinal[nrow(tabfinal), ])
    tabfinal <- tabfinal[order(vals, decreasing = TRUE)]
    tabfinal <- cbind(tabfinal, Total = tabp$Total)
  }
  if (sortby == "both") {
    tabf <- tabp[-nrow(tabp), ]
    tabf <- tabf[order(tabf$Total, decreasing = TRUE), ]
    tabf <- rbind(tabf, tabp[nrow(tabp), ])
    tabfinal <- tabf[, -ncol(tabf)]
    vals <- unlist(tabfinal[nrow(tabfinal), ])
    tabfinal <- tabfinal[order(vals, decreasing = TRUE)]
    tabfinal <- cbind(tabfinal, Total = tabf$Total)
  }
  finaltable <<- tabfinal
  finaltable
  opar <- par(mar = mar)
  on.exit(par(opar))
  nop = nrow(finaltable) - 1
  nov = ncol(finaltable) - 1
  width = 100/(nov)
  height = 10
  x1 = 0
  x2 = width
  y1 = 30
  y2 = y1 + height
  pylim = y1 + 10 * nop
  plot(10, 20, type = "n", xlim = c(0, 120), ylim = c(0, pylim), 
       axes = FALSE, xlab = "", ylab = "", main = main, ...)
  for (i in nop:1) {
    for (j in 1:nov) {
      if (finaltable[i, j] == 0) {
        polygon(c(x1, x2, x2, x1), c(y1, y1, y2, y2), 
                col = "yellow", border = "yellow3")
      }
      else {
        polygon(c(x1, x2, x2, x1), c(y1, y1, y2, y2), 
                col = "blue", border = "skyblue")
      }
      x1 = x1 + width
      x2 = x2 + width
    }
    x1 = 0
    x2 = width
    y1 = y1 + height
    y2 = y2 + height
  }
  bx1 = width/4
  bx2 = 3 * bx1
  by1 = 5
  by3 = 25
  bsize = 20
  for (i in 1:nov) {
    m = finaltable[nop + 1, i]/finaltable[nop + 1, nov + 
                                            1] * bsize
    p = bsize - m
    by2 = by1 + p
    polygon(c(bx1, bx2, bx2, bx1), c(by1, by1, by2, by2), 
            col = "blue", border = NA)
    polygon(c(bx1, bx2, bx2, bx1), c(by2, by2, by3, by3), 
            col = "red", border = NA)
    text(bx1, 0, names(finaltable)[i], srt = srt, adj = adj, 
         xpd = xpd, cex = cex)
    bx1 = bx1 + width
    bx2 = bx1 + width/2
  }
  px1 = 105
  py1 = 30
  py2 = py1 + 7
  for (i in nop:2) {
    if (sum(finaltable[1, 1:nov]) == nov) {
      psize = finaltable[i, nov + 1]/(finaltable[nop + 
                                                   1, nov + 1] - finaltable[1, nov + 1]) * 20
    }
    else {
      psize = finaltable[i, nov + 1]/(finaltable[nop + 
                                                   1, nov + 1]) * 20
    }
    if (psize < 0.2) {
      psize = 0.2
    }
    px2 = px1 + psize
    polygon(c(px1, px2, px2, px1), c(py1, py1, py2, py2), 
            col = "blue", border = NA)
    py1 = py1 + 10
    py2 = py2 + 10
  }
  psize = finaltable[1, nov + 1]/finaltable[nop + 1, nov + 
                                              1] * 20
  px2 = px1 + psize
  if (sum(finaltable[1, 1:nov]) == nov) {
    polygon(c(px1, px2, px2, px1), c(py1, py1, py2, py2), 
            col = "blue", border = "red")
  }
  else {
    polygon(c(px1, px2, px2, px1), c(py1, py1, py2, py2), 
            col = "blue", border = NA)
  }
  finaltable
}

#################################################################################################
# getData.lm <- spida2:::getData.lm
#'
#' Drop one case or cluster 
#' 
#' Uses parallel::mclapply which is not effective on PCs
#' 
#' @param fit a fitted model
#' @param form a formula to 
#' @export
# dropone <-
# function (fit, form = NULL, FUN = if (inherits(fit, "lme")) fixef else coef, 
#     data = getData(fit), ...) 
# {
#     
# if (is.null(form)) {
#         by <- factor(rownames(data))
#         data$by <- by
#         dframe <- data
#     }
#     else {
#         by <- model.frame(form, data)
#         by <- do.call(paste, c(by, sep = "/"))
#         data$by <- factor(by)
#         dframe <- up(data, form)
#     }
#     values <- dframe$by
#     names(values) <- values
#     ret <- parallel::mclapply(values, function(v) {
#         ret <- try(update(fit, data = data[by != v, , drop = FALSE], 
#             ...))
#         if (class(ret) == "try-error") 
#             NA
#         else FUN(ret)
#     })
#     ret <- do.call(rbind, ret)
#     max_dfbetas <- apply(scale(ret), 1, function(x) max(abs(x), na.rm = T))
#     colnames(ret) <- paste0("b_", colnames(ret))
#     ret <- cbind(ret, dframe)
#     ret$max_dfbetas <- max_dfbetas
#     ret
# }
dropone <- function (fit, form = NULL, FUN = if (inherits(fit, "lme")) fixef else coef, 
    data = getData(fit), ...) 
{
    getData.lm <- spida2:::getData.lm
      if (is.null(form)) {
        by <- factor(rownames(data))
        data$by <- by
        dframe <- data
    }
    else {
        by <- model.frame(form, data)
        by <- do.call(paste, c(by, sep = "/"))
        data$by <- factor(by)
        dframe <- up(data, form)
    }
    values <- dframe$by
    names(values) <- values
    ret <- parallel::lapply(values, function(v) {
        ret <- try(update(fit, data = data[by != v, , drop = FALSE]))
        if (class(ret) == "try-error") 
            NA
        else FUN(ret)
    })
    ret <- do.call(rbind, ret)
    max_dfbetas <- apply(scale(ret), 1, function(x) max(abs(x), na.rm = T))
    colnames(ret) <- paste0("b_", colnames(ret))
    ret <- cbind(ret, dframe)
    ret$max_dfbetas <- max_dfbetas
    ret
}

#' @export
pr_head <- function(x, depth = 3) {
  # print a markdown header
  # should be placed in results='asis' chunk
  head <- paste(rep('#',depth),collapse='')
  cat('\n\n', head, ' ', x, '\n\n', sep = '')
}

#' @export
md_head <- pr_head

#' @export
md_pr <- function(x,...){
  cat('\n```\n')
  print(x,...)
  cat('\n```\n')
  invisible(x)  
}

#' @export
simple_models <- function(fstring) {
  # From a formula entered as a string, form all the models with a
  # single term
  fstring <- gsub('\\n| |\\t', '', fstring)
  (fstring <- strsplit(fstring, '~'))
  dep <- fstring[[1]][1]
  # disp(dep)
  # disp(fstring[[1]][2])
  preds <- strsplit(fstring[[1]][2],'\\+')
  paste(dep, '~', preds[[1]])
}

#                                                                                                            
# "logfrc_diff ~ sb_clean + sb_cont +\n     sb_cover + \n     sb_sunshade + sb_waterused +\n     turb + wattemp_ma + ph_ma + etime_diff" %>%
#   simple_models

#' @export
fmla <- function(y, ...) {
  preds <- paste(c(...), collapse = ' + ')
  paste(c(y, preds), collapse = ' ~ ')
}

#' @export
ev <- function(...) {
  eval(parse(text = paste(c(...), collapse = '')))
}



L <- function(ww) {
  ww[[1]]$L
}
#' @export
wald_diffs <- function(fit,L, ...) {
  L[[1]] <- rowdiffs(L(wald(fit, L)))
  wald(fit, L)
}

#
#  xmerge -- highly rudimentary
#

xmerge <- function(a, b, by, FUN = pmax) {
  require(spida2)
  # THIS VERSION TREATS 0 AS SPECIAL AND ONLY WORKS IF ALL VARIABLES
  # ARE NUMERICAL ... SO NEEDS A LOT OR WORK
  # do a merge using all = TRUE except that .x and .y version of variables
  # are combined to 'fill in NA'. If there's a conflict in non-missing values
  # then the value in data set a is used and a warning generated in 
  # variable with suffix .xy
  # BUGS:
  # NAs are turned to 0 so 2 NAs become FUN(0,0)
  dm <- merge(a, b, by = by, suffixes = c('____a','____b'), all = T)
  nams <- grepv('____',names(dm))
  for(n in nams) {
    dm[[n]] <- na20(dm[[n]])
  }
  roots <- unique(sub('____.*$','', nams))
  for(n in roots) {
    dm[[n]] <- 
      FUN(
        dm[[paste0(n,'____a')]],
        dm[[paste0(n,'____b')]])
  }
  dm <- dm[, - grep('____',names(dm))]
  dm
}
  
# test xmerge
if(FALSE) {
  psum <- function(x,y) x + y
  z1 <- data.frame(id = c(1,2,4,5,6),
                   name = letters[1:5],
                   v1 = c(1,2,NA,NA,NA), 
                   v2 = c(1,2,3,NA,NA),
                   v3 = c(3,4,NA,3,4))
  z2 <- data.frame(id = c(0,2,4,5,6), 
                   v1 = c(1,2,NA,NA,2), 
                   v2 = c(NA,2,3,NA,4),
                   v4 = c(3,4,NA,3,4))
  xmerge(z1,z2, by = 'id')
}