# ECOCROP pH suitability

wd <- getwd()
setwd('..')
dir <- getwd()
setwd(wd)

library(raster)

ECOCROP_values <- read.table(paste0(dir, '/Excel/ECOCROP_input.csv')
                             , sep = ';'
                             , header = TRUE
                             )

ECOCROP_values$name2 <- gsub("[[:space:]]", "", ECOCROP_values$ECOCROP_NAME)

library(stringr)
library(withr)
with_options(
  c(scipen = 999), 
  ECOCROP_values$code2 <- str_pad(ECOCROP_values$CODE, 4, pad = "0")
)

# pH_1 <- raster(paste0(dir, "/CROP_COVS/pH_000_005.tif"))
# pH_2 <- raster(paste0(dir, "/CROP_COVS/pH_005_015.tif"))
# pH_3 <- raster(paste0(dir, "/CROP_COVS/pH_015_030.tif"))
# 
# pH <- (pH_1*5 + pH_2*10 + pH_3*15)/30
# 
# rm(pH_1, pH_2, pH_3)
# 
# writeRaster(pH
#             , filename = paste0(dir, '/rasters/pH.tif')
#             , format = 'GTiff')

pH <- raster(paste0(dir, '/rasters/pH.tif'))

i <- 1

fun <- function(x)
{
  PHMIN <- ECOCROP_values$PHMIN[i]
  PHOPMN <- ECOCROP_values$PHOPMN[i]
  PHOPMX <- ECOCROP_values$PHOPMX[i]
  PHMAX <- ECOCROP_values$PHMAX[i]
  out <- x
  out[x <= PHOPMX & x >= PHOPMN] <- 1
  out[x <= PHMIN] <- 0
  out[x >= PHMAX] <- 0
  ind1 <- x < PHOPMN & x > PHMIN
  ind2 <- x < PHMAX & x > PHOPMX
  out[ind1] <- (x[ind1] - PHMIN)/(PHOPMN - PHMIN)
  out[ind2] <- 1 - (x[ind2] - PHOPMX)/(PHMAX - PHOPMX)
  out <- round(out, digits = 3)
  return(out)
}

# res <- fun(seq(3, 10, 0.1))
# plot(seq(3, 10, 0.1), res)

for(i in 1:nrow(ECOCROP_values))
{
  if (!is.na(ECOCROP_values$PHMIN[i])) {
    beginCluster(8)
    
    clusterR(pH
             , fun = fun
             , export = c('i', 'ECOCROP_values')
             , filename = paste0(dir
                                 , '/ECOCROP_pH/ECOCROP_'
                                 , ECOCROP_values$code2[i], '_'
                                 , ECOCROP_values$name2[i]
                                 , '_pH.tif')
             , overwrite = TRUE
             , datatype  = 'FLT4S'
    )
    
    endCluster()
  }
}
rm(i)

# END