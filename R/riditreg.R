# This function aims to take the cleaned/organized data and return relevent regression models
# relies on lm() and glm() from stats and if negative binomial is an option may have to rely on glm.nb() from MASS
riditreg<-function(df, yvar, method, offsetvar){
  # df is dataframe
  # yvar is the dependent variable being modeled (must be in quotations)
  # method is the chosen regression method with options "lm" for simple linear regression and "poisson" for poisson regression
  if (method == "lm"){
    reg<-lm(df[[yvar]]~ridit, df)
    summary(reg)
  } else if (method == "poisson"){
    reg<-glm(df[[yvar]]~ridit, data = df, family = poisson(link = "log"), offset = log(df[[offsetvar]]))
    summary(reg)
  }
}
