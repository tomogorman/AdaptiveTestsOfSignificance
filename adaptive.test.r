#
#                                 Program:  adaptive.test.r
#                                 Revision Date: January 23, 2018
#
#  This adaptive.test function computes a p-value for any single
#  coefficient in a linear model having fixed effects.
#  The observations are adaptively weighted by default. 
#
#  References: 
#
#
#  The adaptive test is performed by calling the function
#
#       adaptive.test<- function(test.df, model, equalwts=0, r = 2000,
#         righttail = 1, permwts=0, s1=7832, s2=25933, s3=19857, details=1)
#
#  The function arguments are:
#
#    1)  test.df is an R data frame that includes all of the variables that
#        will be used in the analysis.
#    2)  model is a character string that specifies the full model. 
#        This model uses the same syntax as the lm() function.
#        The p-value will be for a test of the null hypothesis that
#        the last variable equals zero.
#    3)  if equalwts = 1 the observations are given equal weights,
#        if equalwts = 0 the observations are given adaptive weights,
#        which is the default.
#    4)  r is the number of permutations used in the test. Note that
#        r = 2000 is the default.
#    5)  if righttail = 1 then right tail p-value will be computed, 
#        otherwise, the left tail will be computed.
#    6)  if permwts = 1 the weights will be permuted,
#        if permwts = 0 the weights wil be recomputed for each
#        permutation if the reduced model includes more terms than
#        the intercept, but if the reduced model is limited to only
#        one intercept term the weights will be permuted.
#    7)  s1 is one of three random number seeds. It can be any integer
#        in the range of 1 to 30268.
#    8)  s2 is another random number seed in the range of 1 to 30306.
#    9)  s3 is the last random number seed in the range of 1 to 30322.
#   10)  if details = 1 (default) the details will be printed.
#        if details = 0 the p-value will be returned by the function, but
#        no output will be printed.
#        if details = 2 the weights given to the unpermuted data 
#        in addition to the details = 1 output.
#
#  Notes:
#
#    1) The first argument must be the data set name, the second argument
#       must be the model, the remaining arguments must be specified by
#       their complete names, as shown in the examples below.
#    2) The first two arguments are required. If you want to compute the 
#       p-value using adaptive weighting, and you want to recompute the weights,   
#       and the default random number seeds are acceptable,
#       then you only need to enter the first two arguments.
#    3) The data frame cannot contain missing values for any variables
#       used in the complete model.
#    4) This function calls the adonetailp, adaptiveweights, rootcdf,
#       cdfhat, and shufflewh functions.
#    5) This function is written in base R.  No packages are required. 
#
#  Examples:
#
#    If blood pressure data is used to create a data frame (bp.df) that has
#    blood pressure (bp), age (age), and a treatment indicator (group),
#    then we could compute a p-value  for the group effect by
#    using this code:
#
#      source("adaptive.test.r")
#      bplimits <- adaptive.test(test.df=bp.df, model=c("bp~group") )
#
#    We could expand this example if we needed to include age as a
#    covariate.
#
#      source("adaptive.test.r")
#      bplimits <- adaptive.test(test.df=bp.df, model=c("bp ~ age + group"),
#                    s1 = 3682, s2 = 27812, s3 = 12973 )
#
#  Note that the group variable was specified as the last variable in the
#  model because we wanted the test for the group effect.
#
#  These R functions were carefully checked and I believe
#  that the functions are correct.  However, the author is not
#  responsible for any errors that may still exist in the code.
#
#  Please report any issues concerning this code to T. W. O'Gorman via 
#  email at twogorman@gmail.com
#

adaptive.test <- function(test.df, model, equalwts=0, r=2000,
                   righttail=1,
                   permwts=0, s1=7832, s2=25933, s3=19857, details=1) {

complete <- model
vars <- strsplit(complete,"~")[[1]]
depvar<- vars[[1]]
vars <- strsplit(complete,"+",fixed=TRUE)[[1]]
nvars <- length(vars)

if(nvars ==1) {
  vars <- strsplit(complete,"~",fixed=TRUE)[[1]]
  testvar <- vars[[2]]
  redvec  <- c(depvar,"1")
  reduced <- paste(redvec,collapse="~")
               }

if(nvars >1) {
  testvar    <- vars[[nvars]]
  nreduced <- nvars - 1
  redvec   <- vars[1:nreduced]
  reduced  <- paste(redvec,collapse="+")
              }


depvar  <- gsub(" ","",depvar)
reduced <- gsub(" ","",reduced)
testvar   <- gsub(" ","",testvar)
n <- length(test.df[,depvar])
limits <- double(2)
                  
# The value of r specifies the number of permutations.

r <- 2000

if(details >= 1) {
  cat("\n")
  cat("Function arguments for the adaptive.test function:","\n\n")
  cat("  Data frame:",deparse(substitute(test.df)),"\n")
  cat("  Dependent variable: ",depvar, "\n")
  cat("  Complete model: ",complete,"\n")
  cat("  Reduced model : ",reduced,"\n")
  cat("  Number of permutations = ",r,"\n")
  cat("  Test for : ",testvar, "\n")
  cat("  Random seeds = ",s1,s2, s3, "\n")
  if(righttail == 1) cat("  Right tail p-value ","\n")
  if(righttail != 1) cat("  Left tail p-value ","\n")
  if(equalwts == 1) cat("  Equal weights used ","\n")
  if(equalwts == 0) cat("  Adaptive weights used ","\n")
  if(permwts == 1) cat("  Weights are permuted ","\n\n")
  if(permwts == 0) cat("  Weights are not permuted ","\n\n")
                  }


if( (equalwts != 0) & (equalwts != 1) ) stop("equalwts must be 0 or 1.")
                    
if( (s1 < 1) | (s1 > 30268) ) stop("s1 must be in the range of 1 to 30268.")
if( (s2 < 1) | (s2 > 30306) ) stop("s2 must be in the range of 1 to 30306.")
if( (s3 < 1) | (s3 > 30322) ) stop("s3 must be in the range of 1 to 30322.")
if( (details < 0) | (details > 2) ) stop("details must be 0, 1, or 2.")

#     The next three lines reset the random number seeds.

s1 <- (171*s1) %% 30269
s2 <- (172*s2) %% 30307
s3 <- (170*s3) %% 30323

if(righttail != 1){
  pvalueslist <- adonetailp(test.df,depvar,complete,reduced,testvar,r,
                      s1,s2,s3,n,righttail,equalwts,permwts,details)
  pvalue <- pvalueslist[[1]]
  s1    <- pvalueslist[[2]]
  s2    <- pvalueslist[[3]]
  s3    <- pvalueslist[[4]]
  if(details == 1) cat("  Left tail p-value = ", pvalue, "\n\n")
                   }

if(righttail == 1){
  pvalueslist <- adonetailp(test.df,depvar,complete,reduced,testvar,r,
                      s1,s2,s3,n,righttail,equalwts,permwts,details)
  pvalue <- pvalueslist[[1]]
  s1     <- pvalueslist[[2]]
  s2     <- pvalueslist[[3]]
  s3     <- pvalueslist[[4]]
  if(details == 1) cat("  Right tail p-value = ", pvalue, "\n\n")
                   }                                      

  return(pvalue)
                                                                      }

#
#  The adonetailp function produces a one-tailed p-value
#  based on t test statistics from an adaptively weighted model.
#  If the variable righttail=1 then the right tail p-value will
#  be computed, otherwise the left tail p-value will be computed.
#
#  If the reduced model contains only an intercept term, the weights
#  will be permuted, rather than computed from the permuted data.
#

adonetailp <- function(dfadonetail, depvar, complete, reduced, indvar, r,
                    s1, s2, s3, n, righttail, equalwts, permwts, details) {
localwt             <- double(n)
dfadonetail$w2      <- double(n)
dfadonetail$weights <- double(n)

redu <- lm(as.formula(reduced), data = dfadonetail)
yhat <- predict(redu)
yresidual <- residuals(redu)

if(equalwts == 1) {
  dfadonetail$w2 <- rep(1,n)
  } else {
  dfadonetail    <- adaptiveweights(dfadonetail,reduced)
  if(details == 2){
    cat("\n\n")
    cat(" The adaptive weights for the unpermuted data.",
        "\n\n")
    dfadonetail$weights <- sqrt(dfadonetail$w2)
    print(dfadonetail)
    cat("\n\n")
                   }
         }
 

compu <- lm(as.formula(complete), data = dfadonetail, weights = dfadonetail$w2)

tunperm <- summary(compu)$coefficients[indvar,3]

e <- 0

dfadonetail$w2perm  <- double(n)

simple <- paste(depvar,c("~1"),sep="")

#  Simple models have the intercept as the only predictor variable in
#  the reduced model.  In these models the adaptive weights do not need
#  to be recomputed, they can be permuted because the recomputed weights
#  will equal the permuted weights.
#  Also, if permwts = 1 then the weights will be permuted.

if( (reduced == simple) | (permwts == 1) ) {
  countnum <- double(n)
  ynew     <- double(n)
  yresshuf <- double(n)
  dfadonetail$w2perm <- double(n)
  countnum <- c(1:n)
  for (k in 1:r) {
    permlist <- shufflewh(countnum,s1,s2,s3,n)
    s1 <- permlist[[2]]
    s2 <- permlist[[3]]
    s3 <- permlist[[4]]

    permnums <- permlist[[1]]

    yresshuf <- yresidual[permnums]
    dfadonetail$w2perm <- dfadonetail$w2[permnums]

    dfadonetail[,depvar] <- yhat + yresshuf

    compw <-lm(as.formula(complete), data = dfadonetail,
               weights = dfadonetail$w2perm)
    tperm <- summary(compw)$coefficients[indvar,3]

    if( (righttail != 1) & (tperm <= tunperm) ) e  <-  e + 1
    if( (righttail == 1) & (tperm >= tunperm) ) e  <-  e + 1
                   }
                                             }

if( (reduced != simple) & (permwts != 1) ) {
  for (k in 1:r) {
    sresidlist <- shufflewh(yresidual,s1,s2,s3,n)
    s1 <- sresidlist[[2]]
    s2 <- sresidlist[[3]]
    s3 <- sresidlist[[4]]
    dfadonetail[,depvar] <- yhat+sresidlist[[1]]
    if(equalwts == 1) {
      dfadonetail$w2 <- rep(1,n)
      } else {
      dfadonetail    <- adaptiveweights(dfadonetail,reduced)
              }
    compw  <- lm(as.formula(complete), data = dfadonetail,
               weights = dfadonetail$w2)

    tperm  <- summary(compw)$coefficients[indvar,3]
    if( (righttail != 1) & (tperm <= tunperm) ) e  <-  e + 1
    if( (righttail == 1) & (tperm >= tunperm) ) e  <-  e + 1
    
                 }
                                              }
p <- (e+1)/(r+1)
plist <- list(p,s1,s2,s3)
return(plist)
}


#  The adaptiveweights function produces weights for observations
#  based on residuals from the reduced model.
#  Reference: O'Gorman, T. W., Adaptive Tests of Significance using
#  Permutations of Residuals with R and SAS. 2012, Wiley.

adaptiveweights <- function(dfweights,reduced) {
red <- lm(as.formula(reduced), data=dfweights)
resid <- residuals(red)

#                               compute traditional quantiles
probs <- c(0.10, 0.25, 0.40, 0.60, 0.75, 0.90)
q <- quantile(resid,probs,type=6)
q10 <- q[1]; q25 <- q[2]; q40 <- q[3]
q60 <- q[4]; q75 <- q[5]; q90 <- q[6]

iqr <- q75 - q25
sigmat <- iqr/1.349

#                               compute bandwidth (h)

n <- length(resid)
h <- 1.587*sigmat*n^(-0.33333333333)

minr  <- min(resid)
maxr  <- max(resid)
lower <- minr - iqr/10
upper <- maxr + iqr/10
tol   <- 0.000000001*iqr
cdf25 <- rootcdf(resid,h,0.25,q10,q40,lower,maxr,tol)
cdf50 <- rootcdf(resid,h,0.50,q40,q60,minr,maxr,tol)
cdf75 <- rootcdf(resid,h,0.75,q60,q90,minr,upper,tol)
sigma <- (cdf75-cdf25)/1.349

#                               compute adaptive weights

s <- (resid-cdf50)/sigma
w  <- double(n)
dfweights$w2 <- double(n)

residdh <- resid/h


for (i in 1:n) {
  phi  <- pnorm(residdh[i] - residdh)
  fhat <- sum(phi)/n
  z    <- qnorm(fhat)
  if( abs(s[i]) >= 0.0001 ) w[i] <- z/s[i] else w[i] <- 1 
  }

dfweights$w2 <- w*w
                
return(dfweights)
}

#
#  This root finding function is used to compute the pth percentile,
#  based on the smoothed cumulative distribution function.
#  If the interval [xlow, xhigh] contains the percentile then
#  it proceeds with finding the root; otherwise it uses the much
#  wider interval [lower,upper]. The function uses bisection for the first
#  few iterations, then uses the false position method for the
#  remaining iterations.  Convergence is achieved when the cdf
#  is within a small tolerance around the desired percentile.
#

rootcdf <- function(x,h,p,xlow,xhigh,lower,upper,tolerance) {
  nbisections <- 3
  flow  <-  cdfhat(x,h,xlow)
  fhigh  <-  cdfhat(x,h,xhigh)  
  if ( (flow >p) | (fhigh < p) ) {
    nbisections <- 8
    xlow  <- lower
    xhigh <- upper
    flow  <-  cdfhat(x,h,xlow)
    fhigh  <-  cdfhat(x,h,xhigh)  
                                   }
  for (i in seq(1:60) ) {
    if( i < nbisections ) {
      xmiddle <- (xlow+xhigh)/2
      } else {
      xmiddle <- ((fhigh-p)*xlow-(flow-p)*xhigh)/(fhigh-flow)
      }
    fmiddle <- cdfhat(x,h,xmiddle)
    if( fmiddle < p  ) {xlow   <-  xmiddle;  flow  <-  fmiddle}
    if( fmiddle > p  ) {xhigh  <-  xmiddle; fhigh  <-  fmiddle}
    if(  (abs(fmiddle - p)) <= tolerance ) break
    if( i==60 ){print (" stop in rootcdf with over 60 iterations");q()}
    }
  return(xmiddle)
  }

#
#   This function computes the smooth estimate of the cumulative
#   distribution function, using the smoothing parameter h, at xpoint.
#
cdfhat <- function(xvector,h,xpoint){
  phi <- pnorm((xpoint-xvector)/h)
  cdf <- sum(phi)/length(phi)
  return(cdf)
  }

#   The shufflewh function computes n-1 uniform random numbers
#   using the Wichmann-Hill method and then performs the Durstenfeld
#   shuffle on the rows of y. The vector y and the random number
#   seeds are returned in a list.

shufflewh <- function(y, s1, s2, s3, n) {

k <- c(1:n)
yold <- double(n)
yold <- y
for (i in n:2) {
  s1 <- (171*s1) %% 30269
  s2 <- (172*s2) %% 30307
  s3 <- (170*s3) %% 30323
  u  <- (s1/30269.0 + s2/30307.0 + s3/30323.0) %% 1.000000
  itrade      <- floor(i*u + 1) 
  ktemp       <- k[itrade]
  k[itrade]   <- k[i]
  k[i]        <- ktemp
  }
for (i in 1:n)  {
  iplace <- k[i]
  y[iplace] <- yold[i]
  }
yandseeds <- list(y, s1, s2, s3)
return(yandseeds)
}


