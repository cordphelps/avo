
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


plotNitrogen <- function(df, saveFile, savePath) {

  nitrogen <- ggplot(data = df, aes(x=year)) + 
  
  geom_boxplot(aes(y = nitrateNitrogen.lbsAF, group=year, color=year), shape=21) +
  #geom_dotplot(aes(y = nitrateNitrogen.lbsAF, group=year, color=year), binaxis='y', stackdir='center', dotsize=1) +
  #geom_dotplot(aes(y = nitrateNitrogen.lbsAF), binaxis='y', stackdir='center', dotsize=1)
  
  scale_y_continuous(trans = 'log10') +

  theme_bw() +
  theme(axis.title.y.right = element_text(angle = 90)) 

  if (saveFile) {

    fileName <- paste("ggsave.nitrogen.pdf", sep="")
  
    if (file.exists(fileName)) { file.remove(fileName) }
      
    ggsave(fileName, 
            plot = nitrogen, 
            device = NULL, path = savePath,
            scale = 1, width = 6, height = NA, dpi = 300, limitsize = TRUE,
            units = c("in", "cm", "mm"))
  
  }

  return(nitrogen)
}

