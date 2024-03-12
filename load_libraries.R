# File used to load the necessary tools and libraries
library(tidyverse)
library(lubridate)
library(RSQLite)

pin_diffs <- function(pin1,pin2){
  if(is.na(pin1) || is.na(pin2)){
    return(NA)
  }
  else if(nchar(pin1) != nchar(pin2)){
    return(NA)
  } else {
    return( 4 - sum(strsplit(pin1,"")[[1]] == strsplit(pin2,"")[[1]]) )
  }
}
