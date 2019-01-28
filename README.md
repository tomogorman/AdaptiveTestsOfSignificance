
  README file for:

  Program:  adaptive.test.r
  Revision date: January 21, 2019


  The adaptive.ci function computes a test of significance
  for any single coefficient in a linear model having fixed effects.
  The observations are adaptively weighted by default.

  This program is written in the base R language so no other packages
  are needed.

  Author: T. W. O'Gorman            email:  twogorman@gmail.com

  Language: R  version 3.5.2

  Operating System:  Windows 7 Professional

  Packages needed: None
  


  References: O'Gorman, T. W. (2019) An efficient method of computing
              adaptive tests of significance and confidence intervals

  The p-value is found by calling the function

       adaptive.test<- function(test.df, model, equalwts=0, r = 2000,
         righttail = 1, permwts=0, s1=7832, s2=25933, s3=19857, details=1)

  The function arguments are:

    1)  test.df is an R data frame that includes all of the variables that
        will be used in the analysis.
    2)  model is a character string that specifies the full model. 
        This model uses the same syntax as the lm() function.
        The p-value will be for a test of the null hypothesis that
        the last variable equals zero.
    3)  if equalwts = 1 the observations are given equal weights,
        if equalwts = 0 the observations are given adaptive weights,
        which is the default.
    4)  r is the number of permutations used in the test. Note that
        r = 2000 is the default.
    5)  if righttail = 1 then right tail p-value will be computed, 
        otherwise, the left tail will be computed.
    6)  if permwts = 1 the weights will be permuted,
        if permwts = 0 the weights wil be recomputed for each
        permutation if the reduced model includes more terms than
        the intercept, but if the reduced model is limited to only
        one intercept term the weights will be permuted.
    7)  s1 is one of three random number seeds. It can be any integer
        in the range of 1 to 30268.
    8)  s2 is another random number seed in the range of 1 to 30306.
    9)  s3 is the last random number seed in the range of 1 to 30322.
   10)  if details = 1 (default) the details will be printed.
        if details = 0 the p-value will be returned by the function, but
        no output will be printed.
        if details = 2 the weights given to the unpermuted data 
        in addition to the details = 1 output.

  Notes:

    1) The first argument must be the data set name, the second argument
       must be the model, the remaining arguments must be specified by
       their complete names, as shown in the examples below.
    2) The first two arguments are required. If you want to compute the 
       p-value using adaptive weighting, and you want to recompute the weights,   
       and the default random number seeds are acceptable,
       then you only need to enter the first two arguments.
    3) The data frame cannot contain missing values for any variables
       used in the complete model.
    4) This function calls the adonetailp, adaptiveweights, rootcdf,
       cdfhat, and shufflewh functions.
    5) This function is written in base R.  No packages are required. 

  Examples:

    If blood pressure data is used to create a data frame (bp.df) that has
    blood pressure (bp), age (age), and a treatment indicator (group),
    then we could compute a p-value  for the group effect by
    using this code:

      source("adaptive.test.r")
      bplimits <- adaptive.test(test.df=bp.df, model=c("bp~group") )

    We could expand this example if we needed to include age as a
    covariate.

      source("adaptive.test.r")
      bplimits <- adaptive.test(test.df=bp.df, model=c("bp ~ age + group"),
                    s1 = 3682, s2 = 27812, s3 = 12973 )

  Note that the group variable was specified as the last variable in the
  model because we wanted the test for the group effect.

  These R functions were carefully checked and I believe
  that the functions are correct.  However, the author is not
  responsible for any errors that may still exist in the code.

  Please report any issues concerning this code to T. W. O'Gorman via 
  email at twogorman@gmail.com
