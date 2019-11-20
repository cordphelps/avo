
suppressPackageStartupMessages(library(dplyr))

library(ggplot2)

library(dplyr)
library(rlang) # see sym()

library(tidyverse)
library(knitr)
library(kableExtra)

savePDF <- function(image, filenameNoExt) {
   
  filename <- paste("./output/", filenameNoExt, ".pdf", sep="")
  
  # 6 inches = 15.24 cm
  ggsave(filename, plot = image,
         width = 15.24, height = 10.16, units = "cm")
  
}

fmt_dcimals <- function(decimals=0){
  # https://stackoverflow.com/questions/10035786/ggplot2-y-axis-label-decimal-precision
    function(x) format(x,nsmall = decimals,scientific = FALSE)
}




