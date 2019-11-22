
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

  returnList <= list()

  if (n=="nitrateNitrogen.lbsAF") {

      data <- mutate(data, delta=nitrogenRangeHigh - nitrogenRangeLow)
      data <- mutate(data, ave=(nitrogenRangeHigh + nitrogenRangeLow)/2)

      returnList[[2]] <- "nitrogenRangeLow"
      returnList[[3]] <- "nitrogenRangeHigh"

  } else if (n=="phosphorousP2O5.lbsAF") {

      data <- mutate(data, delta=P2O5RangeHigh - P2O5RangeLow)
      data <- mutate(data, ave=(P2O5RangeHigh + P2O5RangeLow)/2) 

      returnList[[2]] <- "P2O5RangeLow"
      returnList[[3]] <- "P2O5RangeHigh"

  } else if (n=="potassiumK2OExch.lbsAF") {

      data <- mutate(data, delta=K2OExchHigh - K2OExchLow)
      data <- mutate(data, ave=(K2OExchHigh + K2OExchLow)/2)

      returnList[[2]] <- "K2OExchLow"
      returnList[[3]] <- "K2OExchHigh"

  } else if (n=="potassiumK2OSol.lbsAF") {

      data <- mutate(data, delta=K2OSolHigh - K2OSolLow)
      data <- mutate(data, ave=(K2OSolHigh + K2OSolLow)/2)

      returnList[[2]] <- "K2OSolLow"
      returnList[[3]] <- "K2OSolHigh"

  } else if (n=="calciumExch.lbsAF") {

      data <- mutate(data, delta=0)
      data <- mutate(data, ave=0)  

  } else if (n=="calciumSol.lbsAF") {

      data <- mutate(data, delta=0)
      data <- mutate(data, ave=0)

  } else if (n=="magnesiumExch.lbsAF") {

      data <- mutate(data, delta=0)
      data <- mutate(data, ave=0)  

  } else if (n=="magnesiumSol.lbsAF") {

      data <- mutate(data, delta=0)
      data <- mutate(data, ave=0)

  } else if (n=="sodiumExch.lbsAF") {

      data <- mutate(data, delta=0)
      data <- mutate(data, ave=0)

  } else if (n=="sodiumSol.lbsAF") {

      data <- mutate(data, delta=0)
      data <- mutate(data, ave=0)

  } else if (n=="sulphate.lbsAF") {

      data <- mutate(data, delta=0)
      data <- mutate(data, ave=0)

  } else {

      data <- mutate(data, delta=-1)
      data <- mutate(data, ave=-1)

  }

  returnList[[1]] <- data

  return(returnList)
}


plotNutrient <- function(df, nutrient, saveFile, savePath) {

  returnList <- nutrientStats(data=df, n=nutrient)

  nutrient <- ggplot(data = returnList[[1]], aes(x=year)) + 
  #nutrient <- ggplot(data = df, aes_string(x=dfGroup)) + 
  
  #geom_boxplot(aes(y = nitrateNitrogen.lbsAF, group=year), shape=21) +
  #geom_boxplot(aes_string(y = nutrient, group=dfGroup), shape=21) +
  geom_boxplot(aes_string(y = nutrient, group="year"), fill="green", alpha=0.5, shape=21) +

  geom_ribbon(aes_string(ymin=returnList[[2]], ymax=returnList[[3]], x="year"), fill="blue", alpha=.4) +

  stat_summary(data = returnList[[1]], aes(x=year, y=delta), fun.y=mean, colour="red", geom="line") +

  stat_summary(data = returnList[[1]], aes(x=year, y=ave), fun.y=mean, colour="blue", geom="line") +
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

