library(tidyverse)
dfclean<-function(ineqvar, df, order){
  if (order="l2h") {
    df |>
      arrange(ineqvar)->df
  } else if (order="h2l"){
    df |>
      desc(ineqvar)->df
  }
}
