

``` r
source("./code/bug-library.R")


source.url <- c("https://raw.githubusercontent.com/cordphelps/avo/master/data/soilData.csv")
soil-data.df <- read.csv(source.url, header=TRUE, row.names=NULL)

# setwd("./code/avo/")

```

### weekly composition of species and individuals?

``` r

ggsave.path <- "./code/output/"

returnList <- scanBugPercentages(bugs.df)

returnList <- createFamilyPercentages(returnList)

gg <- plotBugPercentages(returnList, spidersOnly=FALSE)

print(gg)

```

