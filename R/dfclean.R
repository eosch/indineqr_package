library(tidyverse)
# function aims to rearrange the data in df by the variable measuring inequality (ineqvar) in a specified order
dfclean<-function(ineqvar, df, order){
  # ineqvar must be input with quotations to work
  # df is relevant dataframe
  # order is either "l2h" for low to high or "h2l" for high to low
  if (order == "l2h") {
    df|>
      arrange(df[[ineqvar]])
  } else if (order == "h2l"){
    df|>
      arrange(desc(df[[ineqvar]]))
  }
}
