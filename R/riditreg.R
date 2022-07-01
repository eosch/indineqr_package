# This function aims to take the cleaned/organized data and return relevant regression models
# relies on lm() and glm() from stats
library(MASS)
#' Title
#'
#' @param df
#' @param yvar
#' @param method
#' @param offsetvar
#' @param countvar
#' @param conf_level
#'
#' @return
#' @export
#'
#' @examples
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
    reg<-lm(df[[yvar]]~ridit, df)
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
    reg<-glm.nb(df[[yvar]]~ridit+offset(log(df[[offsetvar]])))
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
