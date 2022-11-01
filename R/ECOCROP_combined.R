# Combine ECOCROP for soil and climate

library(raster)
library(dplyr)
library(magrittr)

wd <- getwd()
setwd('..')
dir <- getwd()
setwd(wd)

ECOCROP_suit <- read.table(paste0(dir, '/Excel/ECOCROP_input.csv')
                           , sep = ';'
                           , header = TRUE
)

for(i in 3:ncol(ECOCROP_suit))
{
  ECOCROP_suit[, i] <- as.numeric(ECOCROP_suit[, i])
}
rm(i)

ECOCROP_suit$name2 <- gsub("[[:space:]]", "", ECOCROP_suit$ECOCROP_NAME)

library(stringr)
library(withr)
with_options(
  c(scipen = 999), 
  ECOCROP_suit$code2 <- str_pad(ECOCROP_suit$CODE, 4, pad = "0")
)

ECOCROP_suit <- ECOCROP_suit[complete.cases(ECOCROP_suit), ]

f1 <- function(a, b) {
  out <- round(a*b, digits = 3)
  return(out)
}

for(i in 1:nrow(ECOCROP_suit))
{
  r1 <- paste0(dir
               , '/ECOCROP_soil/ECOCROP_'
               , ECOCROP_suit$code2[i], '_'
               , ECOCROP_suit$name2[i]
               , '.tif') %>% raster()
  
  r2 <- paste0(dir
               , '/ECOCROP_clim/ECOCROP_'
               , ECOCROP_suit$code2[i], '_'
               , ECOCROP_suit$name2[i]
               , '.tif') %>% raster()
  
  rs <- stack(r1, r2)
  
  beginCluster(8)
  
  clusterR(rs
           , overlay
           , args = list(fun = f1)
           , filename = paste0(dir
                               , '/ECOCROP_combined/ECOCROP_'
                               , ECOCROP_suit$code2[i], '_'
                               , ECOCROP_suit$name2[i]
                               , '.tif')
           , overwrite = TRUE
           , datatype  = 'FLT4S'
  )
  
  endCluster()
}

# END