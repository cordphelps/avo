
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

nutrientStats <- function(data, n) {

  if (n=="nitrateNitrogen.lbsAF") {

      data <- mutate(data, delta=nitrogenRangeHigh - nitrogenRangeLow)
      data <- mutate(data, ave=(nitrogenRangeHigh + nitrogenRangeLow)/2)

  } else if (n=="phosphorousP2O5.lbsAF") {

      data <- mutate(data, delta=1)
      data <- mutate(data, ave=1)    

  } else {

      data <- mutate(data, delta=-1)
      data <- mutate(data, ave=-1)

  }



  return(data)
}


plotNutrient <- function(df, nutrient, saveFile, savePath) {

  df <- nutrientStats(data=df, n=nutrient)

  nutrient <- ggplot(data = df, aes(x=year)) + 
  #nutrient <- ggplot(data = df, aes_string(x=dfGroup)) + 
  
  #geom_boxplot(aes(y = nitrateNitrogen.lbsAF, group=year), shape=21) +
  #geom_boxplot(aes_string(y = nutrient, group=dfGroup), shape=21) +
  geom_boxplot(aes_string(y = nutrient, group="year"), shape=21) +


  stat_summary(data = df, aes(x=year, y=delta), fun.y=mean, colour="red", geom="line") +

  stat_summary(data = df, aes(x=year, y=ave), fun.y=mean, colour="blue", geom="line") +
  #stat_summary(data = df, aes_string(x=dfGroup, y=ave), fun.y=mean, colour="blue", geom="line") +
  
  scale_y_continuous(trans = 'log10') +

  theme_bw() +
  theme(axis.title.y.right = element_text(angle = 90)) 

  if (saveFile) {

    fileName <- paste("ggsave.", nutrient, ".pdf", sep="")
  
    if (file.exists(fileName)) { file.remove(fileName) }
      
    ggsave(fileName, 
            plot = nitrogen, 
            device = NULL, path = savePath,
            scale = 1, width = 6, height = NA, dpi = 300, limitsize = TRUE,
            units = c("in", "cm", "mm"))
  
  }

  return(nutrient)
}

