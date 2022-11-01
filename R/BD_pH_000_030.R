# Aggregate bulk density and pH for the depth interval 0 - 30 cm.

library(raster)

loadandstack <- function(dir = NULL)
{
  rlist<-list.files(dir, pattern = "tif$", full.names = TRUE)
  for(r in rlist)
  {
    name <- unlist(strsplit(r, "[.]"))[length(unlist(strsplit(r, "[.]"))) - 1]
    assign(name, raster(r))
  }
  output <- stack(rlist)
  return(output)
}

wd <- getwd()
setwd('..')
dir <- getwd()
setwd(wd)

BD_000_005 <- raster(paste0(dir, '/COVARIATES/BD_000_005.tif'))
BD_005_015 <- raster(paste0(dir, '/COVARIATES/BD_005_015.tif'))
BD_015_030 <- raster(paste0(dir, '/COVARIATES/BD_015_030.tif'))

pH_000_005 <- raster(paste0(dir, '/COVARIATES/pH_000_005.tif'))
pH_005_015 <- raster(paste0(dir, '/COVARIATES/pH_005_015.tif'))
pH_015_030 <- raster(paste0(dir, '/COVARIATES/pH_015_030.tif'))

BD_000_030 <- (BD_000_005*5 + BD_005_015*10 + BD_015_030*15)/30

pH_000_030 <- (pH_000_005*5 + pH_005_015*10 + pH_015_030*15)/30

BD_000_030 <- round(BD_000_030, digits = 2)

pH_000_030 <- round(pH_000_030, digits = 1)

writeRaster(BD_000_030
            , filename  = paste0(dir, '/COVARIATES/BD_000_030.tif')
            , datatype  = 'FLT4S'
            , overwrite = TRUE
            )

writeRaster(pH_000_030
            , filename  = paste0(dir, '/COVARIATES/pH_000_030.tif')
            , datatype  = 'FLT4S'
            , overwrite = TRUE
)

# END