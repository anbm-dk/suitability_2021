# Test suitability mapping based on ECOCROP

wd <- getwd()
setwd('..')
dir <- getwd()
setwd(wd)

library(raster)
library(BiodiversityR)

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


# ECOCROP soil

tex <- raster(paste0(dir, '/ECOCROP_input/FAO_tex.tif'))
drain <- raster(paste0(dir, '/ECOCROP_input/ECOCROP_drainage.tif'))

for(i in 1:nrow(ECOCROP_suit))
{
  rcl_tex   <- cbind(c(1:4), t(ECOCROP_suit[i, 11:14]))
  rcl_drain <- cbind(c(1:2), t(ECOCROP_suit[i, 15:16]))

  suit_tex   <- reclassify(tex,   rcl = rcl_tex)
  suit_drain <- reclassify(drain, rcl = rcl_drain)
  suit_pH <- raster(paste0(dir
                           , '/ECOCROP_pH/ECOCROP_'
                           , ECOCROP_suit$code2[i], '_'
                           , ECOCROP_suit$name2[i]
                           , '_pH.tif'))

  s <- stack(suit_tex, suit_drain, suit_pH)
  
  f <- function(d, e, f) 
  {
    out <- round(d*e*f, digits = 3)
    return(out)
  }

  beginCluster(8)
  
  clusterR(s
           , overlay
           , args = list(fun = f)
           , filename = paste0(dir
                               , '/ECOCROP_soil/ECOCROP_'
                               , ECOCROP_suit$code2[i], '_'
                               , ECOCROP_suit$name2[i]
                               , '.tif')
           , overwrite = TRUE
           , datatype  = 'FLT4S'
           )
  
  endCluster()

  print(i)
}
rm(i, rcl_drain, rcl_tex, suit_tex, suit_drain, suit_pH)


# END