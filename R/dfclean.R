library(tidyverse)
#' Clean and organize data to prepare for calculating indices of inequality
#'
#' This function aims to rearrange the data in a dataframe by the variable measuring inequality (ineqvar) in a specified order.
#' Additionally, it creates new variables within the dataframe for frequency, cumulative frequency, and ridit.
#'
#' @param ineqvar variable name in quotations of the variable within df measuring inequality
#' @param df the relevant input dataframe
#' @param order either "l2h" for low to high or "h2l" for high to low based on how the ineqvar needs to be arranged
#' @param offsetvar the count variable from df in quotations used for creating the frequencies and the ridit
#'
#' @return dataframe with the new variables and data arranged in the specified order
#' @export dfclean
#'
#' @examples
dfclean<-function(ineqvar, df, order, offsetvar){
  # rearranging by inequality
  if (order == "l2h") {
    df|>
      arrange(df[[ineqvar]])->df
  } else if (order == "h2l"){
    df|>
      arrange(desc(df[[ineqvar]]))->df
  }

  # creating frequency vector
  freq<-vector(mode = "numeric", length = length(df[[offsetvar]]))
  for (i in 1:length(df[[offsetvar]])){
    freq[i]=(df[[offsetvar]][i])/(sum(df[[offsetvar]]))
  }

  # creating cumulative frequency vector
  cumfreq<-vector(mode = "numeric", length = length(df[[offsetvar]]))
  for (j in 1:length(df[[offsetvar]])){
    if (j == 1){
      cumfreq[j]=freq[j]
    } else {
      cumfreq[j]=cumfreq[j-1] + freq[j]
    }
  }

  # creating ridit vector
  ridit<-vector(mode = "numeric", length = length(df[[offsetvar]]))
  for (k in 1:length(df[[offsetvar]])){
    if (k == 1){
      ridit[k]=cumfreq[k]/2
    } else {
      ridit[k]=(cumfreq[k-1] + cumfreq[k])/2
    }
  }

  # Adding new vectors as columns to the dataframe
  df|>
    mutate(freq=freq,
           cumfreq=cumfreq,
           ridit=ridit)->df
  return(df)
}
