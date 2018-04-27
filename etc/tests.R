
if(FALSE) {
cov <- matrix(FALSE, 4,4)
cov[row(cov) == 1 & col(cov) > 1] <- TRUE
cov

cov[2,3] <- TRUE
cov
debug(pdConstruct.pdInd)

hs$sexh <- (hs$Sex=='Male') - 1/2
fit6b <- lme(mathach ~ ses + sexh + Minority, hs, 
             method = 'REML',
             random = 
               list( 
                 school = pdInd( ~ 1 + ses + sexh + Minority, cov = cov)),
             control = list(msVerbose=T,returnObject=T,msMaxIter=1000,
                            msMaxEval = 2000,
                            sing.tol=1e-20))
fit6c <- lme(mathach ~ ses + sexh + Minority, hs, 
             method = 'REML',
             random = 
               list( 
                 school = pdInd( ~ 1 + ses + sexh + Minority)),
             control = list(msVerbose=T,returnObject=T,msMaxIter=1000,
                            msMaxEval = 2000,
                            sing.tol=1e-20))
anova(fit6b, fit6c)
summary(fit6b)
fit6b$apVar  %>% unclass
%>% svd %>% .$d
fit6c$apVar %>% svd %>% .$d



getG(fit6b)
corr <- function(mat) (sqrt(diag(1/diag(mat)))) %*% mat %*% (sqrt(diag(1/diag(mat))))
getG(fit6b) %>% svd %>% .$d
getG(fit6b) %>% as.matrix %>% corr %>% svd %>% .$d
names(fit6b)
fit6b$modelStruct$reStruct %>% .$school %>% unclass
methods(class='reStruct')
fit6$modelStruct



fit <- lme(mathach ~ 1 + ses + Sex, hs, random = ~ 1 + ses + Sex | school,
           control = list(msMaxIter = 1000))
fit$modelStruct$reStruct %>% str
fit$modelStruct$reStruct %>% .$school %>% str
fit$modelStruct$reStruct %>% .$school %>% as.matrix
fit$modelStruct$reStruct %>% .$school %>% pdFactor
fit$modelStruct$reStruct %>% .$school %>% pdMatrix
fit$modelStruct$reStruct %>% .$school %>% pdMatrix(factor = T)

# Let's try pdInd with a single covariate which should give the same thing as the default

fit <- lme(mathach ~ 1 + ses + Sex, hs, 
           random = ~ 1 + ses  | school,
           control = list(msMaxIter = 1000))
fit$modelStruct$reStruct %>% str
fit$modelStruct$reStruct %>% .$school %>% str
fit$modelStruct$reStruct %>% .$school %>% as.matrix
fit$modelStruct$reStruct %>% .$school %>% pdFactor
fit$modelStruct$reStruct %>% .$school %>% pdMatrix
fit$modelStruct$reStruct %>% .$school %>% pdMatrix(factor = T)

fit2 <- lme(mathach ~ 1 + ses + Sex, hs, 
            random = list(school =  pdInd(~ 1 + ses)),
            control = list(msMaxIter = 1000))
summary(fit)
summary(fit2)
fit2$modelStruct$reStruct %>% str
fit2$modelStruct$reStruct %>% .$school -> z
fit2$modelStruct$reStruct %>% .$school %>% as.matrix
fit$modelStruct$reStruct %>% .$school %>% as.matrix

fit2$modelStruct$reStruct %>% .$school %>% pdFactor
fit2$modelStruct$reStruct %>% .$school %>% pdMatrix
fit2$modelStruct$reStruct %>% .$school %>% pdMatrix(factor = T)
fit$modelStruct$reStruct %>% .$school %>% pdMatrix(factor = T)

z %>% str
pdFactor(z)
coef(z)
as.vector(z)
nlme:::coef.pdMat(z)

fit$modelStruct$reStruct %>% .$school  %>% pdMatrix
fit2$modelStruct$reStruct %>% .$school  %>% pdMatrix

fit$modelStruct$reStruct %>% .$school  %>% coef
fit2$modelStruct$reStruct %>% .$school  %>% coef

fit$modelStruct$reStruct %>% .$school  %>% unclass
fit2$modelStruct$reStruct %>% .$school  %>% unclass
#

cov <- matrix(FALSE, 3, 3)
cov[row(cov)<col(cov)] <- TRUE
cov

fit3 <- lme(mathach ~ 1 + ses + Sex, hs, 
            random = list(school =  pdInd(~ 1 + ses + Sex, cov = cov)),
            control = list(msMaxIter = 1000))
summary(fit3)
fit3 %>% .$modelStruct %>% .$reStruct %>% .$school -> z
z %>% unclass


fit6c <- lme(mathach ~ ses + sexh + Minority, hs, 
             method = 'REML',
             random = 
               list( 
                 school = pdInd( ~ 1 + ses + sexh + Minority)),
             control = list(msVerbose=T,returnObject=T,msMaxIter=1000,
                            msMaxEval = 2000,
                            sing.tol=1e-20))

random <- list( id = pdBlocked(
  list(pdDiag(form = ~control-1),
       pdSymm(form = ~patient+days_post.injury-1))))

}