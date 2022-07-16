# This function aims to take the cleaned/organized data and return relevant regression models
# relies on lm() and glm() from stats
library(stats)
#' Create models and calculate indices of inequality
#'
#' This function aims to take the cleaned/organized data from the dfclean() function and returns relevant regression models.
#' It also returns estimates of the Slope Index of Inequality and Relative Index of Inequality for the comparison of both
#' the least and greatest ridit values in the dataframe as well as the theoretical min and max of 0 and 1.
#'
#' @param df the input dataframe organized correctly by previously using the dfclean() function
#' @param yvar the rate variable name, in quotations, used as the response variable in some of the model options
#' @param method the regression method to be used with options "lm", "poisson", "nb", and "logistic"
#' @param offsetvar the count variable, in quotations, of the total number of outcomes in each group used as weights/offsets in the models
#' @param countvar the count variable, in quotations, of the number of occurrences in each group used as counts in the logistic regression
#' @param conf_level confidence level for the confidence intervals
#'
#' @return A list consisting of the following:
#' \item{regression}{The summary output of the selected regression model.}
#' \item{SII_01}{The Slope Index of Inequality comparing ridit=0 and ridit=1.}
#' \item{RII_01}{The Relative Index of Inequality comparing ridit=0 and ridit=1.}
#' \item{SII_minmax}{The Slope Index of Inequality comparing the min ridit value from df and the max ridit value from df.}
#' \item{RII_minmax}{The Relative Index of Inequality comparing the min ridit value from df and the max ridit value from df.}
#' @export
#'
#' @examples
#' df<-dfclean(ineqvar = "IDSx", dfexample, order = "l2h", offsetvar = "NV")
#' riditreg(df, yvar = "TMN", method = "lm", offsetvar = "NV")
#' riditreg(df, yvar = "MN", method = "poisson", offsetvar = "NV")
#' riditreg(df, yvar = "MN", method = "nb", offsetvar = "NV")
#' riditreg(df, countvar = "MN", method = "logistic", offsetvar = "NV")
riditreg<-function(df, yvar, method, offsetvar, countvar, conf_level = 0.95){
  # df is dataframe
  # yvar is the dependent variable being modeled (must be in quotations)
  # method is the chosen regression method with options:
  #   "lm" for simple linear regression
  #   "poisson" for poisson regression
  #   "nb" for negative binomial regression
  #   "logistic" for logistic regression
  # offsetvar is the is the variable used in the offset for poisson and nb
  # conf_level is the the desired confidence level for the confidence intervals on the indices
  if (method == "lm"){
    reg<-lm(df[[yvar]]~ridit, df, weights = df[[offsetvar]])
    SII_01<-reg$coeff[[1]] - (reg$coeff[[1]] + reg$coeff[[2]])
    RII_01<-reg$coeff[[1]] / (reg$coeff[[1]] + reg$coeff[[2]])
    SII_minmax<-(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1]) - (reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])
    RII_minmax<-(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1]) / (reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])
    SII_01LL<-reg$coeff[[1]] - (reg$coeff[[1]] + reg$coeff[[2]])
    RII_01LL<-reg$coeff[[1]] / (reg$coeff[[1]] + reg$coeff[[2]])
    SII_minmaxLL<-(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1]) - (reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])
    RII_minmaxLL<-(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1]) / (reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])
    SII_01UL<-reg$coeff[[1]] - (reg$coeff[[1]] + reg$coeff[[2]])
    RII_01UL<-reg$coeff[[1]] / (reg$coeff[[1]] + reg$coeff[[2]])
    SII_minmaxUL<-(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1]) - (reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])
    RII_minmaxUL<-(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1]) / (reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])
    output<-list(summary(reg), SII_01, RII_01, SII_minmax, RII_minmax)
    names(output)<-c("regression", "SII_01", "RII_01", "SII_minmax", "RII_minmax")
    return(output)
  } else if (method == "poisson"){
    reg<-glm(df[[yvar]]~ridit, data = df, family = poisson(link = "log"), offset = log(df[[offsetvar]]))
    SII_01<-exp(reg$coeff[[1]]) - exp(reg$coeff[[1]] + reg$coeff[[2]])
    RII_01<-exp(reg$coeff[[1]]) / exp(reg$coeff[[1]] + reg$coeff[[2]])
    SII_minmax<-exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1]) - exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])
    RII_minmax<-exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1]) / exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])
    output<-list(summary(reg), SII_01, RII_01, SII_minmax, RII_minmax)
    names(output)<-c("regression", "SII_01", "RII_01", "SII_minmax", "RII_minmax")
    return(output)
  } else if (method == "nb"){
    reg<-MASS::glm.nb(df[[yvar]]~ridit+offset(log(df[[offsetvar]])), data = df)
    SII_01<-exp(reg$coeff[[1]]) - exp(reg$coeff[[1]] + reg$coeff[[2]])
    RII_01<-exp(reg$coeff[[1]]) / exp(reg$coeff[[1]] + reg$coeff[[2]])
    SII_minmax<-exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1]) - exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])
    RII_minmax<-exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1]) / exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])
    output<-list(summary(reg), SII_01, RII_01, SII_minmax, RII_minmax)
    names(output)<-c("regression", "SII_01", "RII_01", "SII_minmax", "RII_minmax")
    return(output)
  } else if (method == "logistic"){
    reg<-glm(df[[countvar]]/df[[offsetvar]]~ridit, data = df, family = "binomial", weights = df[[offsetvar]])
    SII_01<-(exp(reg$coeff[[1]])/(1 + exp(reg$coeff[[1]]))) - (exp(reg$coeff[[1]] + reg$coeff[[2]])/(1+exp(reg$coeff[[1]] + reg$coeff[[2]])))
    RII_01<-(exp(reg$coeff[[1]])/(1 + exp(reg$coeff[[1]]))) / (exp(reg$coeff[[1]] + reg$coeff[[2]])/(1+exp(reg$coeff[[1]] + reg$coeff[[2]])))
    SII_minmax<-(exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1])/(1+exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1]))) - (exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])/(1+exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])))
    RII_minmax<-(exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1])/(1+exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[1]))) / (exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])/(1+exp(reg$coeff[[1]] + reg$coeff[[2]] * df$ridit[length(df$ridit)])))
    output<-list(summary(reg), SII_01, RII_01, SII_minmax, RII_minmax)
    names(output)<-c("regression", "SII_01", "RII_01", "SII_minmax", "RII_minmax")
    return(output)
  }
}
