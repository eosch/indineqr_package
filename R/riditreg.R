# This function aims to take the cleaned/organized data and return relevant regression models
# relies on lm() and glm() from stats
library(MASS)
riditreg<-function(df, yvar, method, offsetvar, countvar){
  # df is dataframe
  # yvar is the dependent variable being modeled (must be in quotations)
  # method is the chosen regression method with options:
  #   "lm" for simple linear regression
  #   "poisson" for poisson regression
  #   "nb" for negative binomial regression
  #   "logistic" for logistic regression
  # offsetvar is the is the variable used in the offset for poisson and nb
  if (method == "lm"){
    reg<-lm(df[[yvar]]~ridit, df)
    summary(reg)
  } else if (method == "poisson"){
    reg<-glm(df[[yvar]]~ridit, data = df, family = poisson(link = "log"), offset = log(df[[offsetvar]]))
    summary(reg)
  } else if (method == "nb"){
    reg<-glm.nb(df[[yvar]]~ridit+offset(log(df[[offsetvar]])))
    summary(reg)
  } else if (method == "logistic"){
    reg<-glm(df[[countvar]]/df[[offsetvar]]~ridit, data = df, family = "binomial", weights = df[[offsetvar]])
    summary(reg)
  }
}
