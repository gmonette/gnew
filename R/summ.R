#' @export
summ <- function(x,...) UseMethod('summ')
#' @export
summ.lme <- function(x,...){
  # print lme summary without correlations
  ret <- summary(x,...)
  ret$vif <- car::vif(x)
  class(ret) <- c('summ.lme', class(ret))
  ret
}
#' @export
summ.lm <- function(x,...){
  # print lme summary without correlations
  ret <- summary(x,...)
  ret$vif <- car::vif(x)
  class(ret) <- c('summ.lm', class(ret))
  ret
}
#' @export
summ.default <- function(x,...) summary(x,...)
#' @export
print.summ.lme <- 
function (x, verbose = FALSE, ...) 
{
    dd <- x$dims
    verbose <- verbose || attr(x, "verbose")
    if (inherits(x, "nlme")) {
        cat("Nonlinear mixed-effects model fit by ")
        cat(if (x$method == "REML") 
            "REML\n"
        else "maximum likelihood\n")
        cat("  Model:", deparse(x$call$model), "\n")
    }
    else {
        cat("Linear mixed-effects model fit by ")
        cat(if (x$method == "REML") 
            "REML\n"
        else "maximum likelihood\n")
    }
    cat(" Data:", deparse(x$call$data), "\n")
    if (!is.null(x$call$subset)) {
        cat("  Subset:", deparse(asOneSidedFormula(x$call$subset)[[2L]]), 
            "\n")
    }
    print(data.frame(AIC = x$AIC, BIC = x$BIC, logLik = c(x$logLik), 
        row.names = " "), ...)
    if (verbose) {
        cat("Convergence at iteration:", x$numIter, "\n")
    }
    cat("\n")
    print(summary(x$modelStruct), sigma = x$sigma, reEstimates = x$coef$random, 
        verbose = verbose, ...)
    cat("Fixed effects: ")
    fixF <- x$call$fixed
    if (inherits(fixF, "formula") || is.call(fixF)) {
        cat(deparse(x$call$fixed), "\n")
    }
    else {
        cat(deparse(lapply(fixF, function(el) as.name(deparse(el)))), 
            "\n")
    }
    xtTab <- as.data.frame(x$tTable)
    wchPval <- match("p-value", names(xtTab))
    for (i in names(xtTab)[-wchPval]) {
        xtTab[, i] <- format(zapsmall(xtTab[, i]))
    }
    xtTab[, wchPval] <- format(round(xtTab[, wchPval], 4))
    if (any(wchLv <- (as.double(levels(xtTab[, wchPval])) == 
        0))) {
        levels(xtTab[, wchPval])[wchLv] <- "<.0001"
    }
    row.names(xtTab) <- dimnames(x$tTable)[[1L]]
    print(xtTab, ...)
    if (nrow(x$tTable) > 1) {
        vif <- x$vif
        cat('\nLinear Model Variance Inflation Factors:\n')
        print(x$vif)
    }
    cat("\nStandardized Within-Group Residuals:\n")
    print(x$residuals, ...)
    cat("\nNumber of Observations:", x$dims[["N"]])
    cat("\nNumber of Groups: ")
    Ngrps <- dd$ngrps[1:dd$Q]
    if ((lNgrps <- length(Ngrps)) == 1) {
        cat(Ngrps, "\n")
    }
    else {
        sNgrps <- 1:lNgrps
        aux <- rep(names(Ngrps), sNgrps)
        aux <- split(aux, array(rep(sNgrps, lNgrps), c(lNgrps, 
            lNgrps))[!lower.tri(diag(lNgrps))])
        names(Ngrps) <- unlist(lapply(aux, paste, collapse = " %in% "))
        cat("\n")
        print(rev(Ngrps), ...)
    }
    invisible(x)
}

#' Adaptation of print.summary.lm
#' 
#' Prints VIF from car instead of correlation matrix
#' @export
print.summ.lm <- 
function (x, digits = max(3L, getOption("digits") - 3L), 
    symbolic.cor = x$symbolic.cor, signif.stars = getOption("show.signif.stars"), 
    ...) 
{
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", 
        collapse = "\n"), "\n\n", sep = "")
    resid <- x$residuals
    df <- x$df
    rdf <- df[2L]
    cat(if (!is.null(x$weights) && diff(range(x$weights))) 
        "Weighted ", "Residuals:\n", sep = "")
    if (rdf > 5L) {
        nam <- c("Min", "1Q", "Median", "3Q", 
            "Max")
        rq <- if (length(dim(resid)) == 2L) 
            structure(apply(t(resid), 1L, quantile), dimnames = list(nam, 
                dimnames(resid)[[2L]]))
        else {
            zz <- zapsmall(quantile(resid), digits + 1L)
            structure(zz, names = nam)
        }
        print(rq, digits = digits, ...)
    }
    else if (rdf > 0L) {
        print(resid, digits = digits, ...)
    }
    else {
        cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
        cat("\n")
    }
    if (length(x$aliased) == 0L) {
        cat("\nNo Coefficients\n")
    }
    else {
        if (nsingular <- df[3L] - df[1L]) 
            cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", 
                sep = "")
        else cat("\nCoefficients:\n")
        coefs <- x$coefficients
        if (any(aliased <- x$aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, 
                colnames(coefs)))
            coefs[!aliased, ] <- x$coefficients
        }
        printCoefmat(coefs, digits = digits, signif.stars = signif.stars, 
            na.print = "NA", ...)
    }
    cat("\nResidual standard error:", format(signif(x$sigma, 
        digits)), "on", rdf, "degrees of freedom")
    cat("\n")
    if (nzchar(mess <- naprint(x$na.action))) 
        cat("  (", mess, ")\n", sep = "")
    if (!is.null(x$fstatistic)) {
        cat("Multiple R-squared: ", formatC(x$r.squared, 
            digits = digits))
        cat(",\tAdjusted R-squared: ", formatC(x$adj.r.squared, 
            digits = digits), "\nF-statistic:", formatC(x$fstatistic[1L], 
            digits = digits), "on", x$fstatistic[2L], "and", 
            x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L], 
                x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE), 
                digits = digits))
        cat("\n")
    }
    vif <- x$vif
    if(!is.null(vif)) {
      cat("\nVariance Inflation Factors:\n")
      print(vif)
    }
    # correl <- x$correlation
    # if (!is.null(correl)) {
    #     p <- NCOL(correl)
    #     if (p > 1L) {
    #         cat("\nCorrelation of Coefficients:\n")
    #         if (is.logical(symbolic.cor) && symbolic.cor) {
    #             print(symnum(correl, abbr.colnames = NULL))
    #         }
    #         else {
    #             correl <- format(round(correl, 2), nsmall = 2, 
    #               digits = digits)
    #             correl[!lower.tri(correl)] <- ""
    #             print(correl[-1, -p, drop = FALSE], quote = FALSE)
    #         }
    #     }
    # }
    # cat("\n")
    invisible(x)
}
