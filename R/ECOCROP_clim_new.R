# New ECOCROP rasters, climate

library(raster)
library(spatialEco)
library(dismo)


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

folders <- c('/wc2.1_30s_prec/', '/wc2.1_30s_tavg/', '/wc2.1_30s_tmin/')
newfolders <- c('/wc2.1_DK_prec/', '/wc2.1_DK_tavg/', '/wc2.1_DK_tmin/')
newnames <- c('prec', 'tavg', 'tmin')

# Load raster for Denmark

dk <- raster(paste0(dir, '/RASTERS/fields.tif'))


# Extent of Danish rasters

e <- raster::extent(dk)


# Transform extent to polygon

e <- as(e, 'SpatialPolygons')

sp::proj4string(e) <- proj4string(dk)

lat_to_m <- 111131.745
inc_x <- 99
matsize <- 41

elevation <- raster(paste0(dir, '/COVARIATES/decimals_2/elevation.tif'))

# for(k in 1:length(folders))
# {
#   # Load worldClim 2 data to rasterstack
#   
#   wc2 <- loadandstack(paste0(dir, folders[k]))
#   
#   # Reproject extent polygon to the projection of the worldclim 2 data
#   
#   e.geo <- sp::spTransform(e, crs(wc2))
#   
#   # Expand the extent 1 degree in all directions, to make sure nothing is left
#   # out.
#   
#   e.geo <- extend(extent(e.geo), 1, 1, 1, 1)
#   
#   # Crop the worldclim 2 data using the new extent
#   
#   wc2_crop <- raster::crop(wc2, e.geo)
#   
#   
#   # Gaussian smoothing of the cropped worldclim 2 data, taking into account the
#   # rectangular shape of the cells.
#   
#   ym <- yres(wc2_crop)*lat_to_m
#   xm <- cellStats(area(wc2_crop), stat = 'mean')*1000/(ym/1000)
#   
#   inc_y <- round(inc_x*ym/xm, digits = 0)
#   
#   gf_big <- gaussian.kernel(sigma = 1*inc_y, n = matsize*inc_y)
#   
#   sum_gf <- matrix(numeric(), ncol = matsize, nrow = matsize)
#   
#   startrow <- (matsize*inc_y - matsize*inc_x)/2
#   
#   for(i in 1:matsize){
#     for(j in 1:matsize)
#       sum_gf[i, j] <- sum(gf_big[((i - 1)*inc_x + 1 + startrow):(i*inc_x + startrow)
#                                  , ((j - 1)*inc_y + 1):(j*inc_y)])
#   }
#   
#   sum_gf <- t(sum_gf)
#   
#   weightr <- focal(wc2_crop[[1]]*0 + 1, w = sum_gf, na.rm = TRUE, pad = TRUE
#                    , NAonly = TRUE)
#   
#   wc2_gauss <- list()
#   
#   for(i in 1:nlayers(wc2_crop))
#   {
#     wc2_gauss[[i]] <- focal(wc2_crop[[i]], w = sum_gf, na.rm = TRUE, pad = TRUE
#                             , NAonly = TRUE)/weightr
#   }
#   
#   wc2_gauss <- stack(wc2_gauss)
#   
#   # Reproject the cropped worldclim 2 data to the projection, extent and
#   # resolution of the Danish data.
#   
#   wc2_proj <- projectRaster(from = wc2_gauss, to = dk)
#   
#   
#   # Mask reprojected rasters
#   
#   
#   
#   dir.create(paste0(dir, newfolders[k]))
#   
#   wc2_proj <- mask(wc2_proj, elevation,
#                    filename = paste0(dir, newfolders[k], newnames[k], '.tif')
#                    )
# }

ECOCROP_suit <- read.table(paste0(dir, '/Excel/ECOCROP_input.csv')
                           , sep = ';'
                           , header = TRUE
)


ECOCROP_suit$name2 <- gsub("[[:space:]]", "", ECOCROP_suit$ECOCROP_NAME)

library(stringr)
library(withr)
with_options(
  c(scipen = 999), 
  ECOCROP_suit$code2 <- str_pad(ECOCROP_suit$CODE, 4, pad = "0")
)

allcrops <- getCrop()

ECOCROP_suit <- ECOCROP_suit[ECOCROP_suit$ECOCROP_NAME %in% allcrops$NAME, ]

prec <- brick(paste0(dir, '/wc2.1_DK_prec/prec.tif'))
tavg <- brick(paste0(dir, '/wc2.1_DK_tavg/tavg.tif'))
tmin <- brick(paste0(dir, '/wc2.1_DK_tmin/tmin.tif'))

all <- stack(tmin, tavg, prec)

f1 <- function(x) {
  if(is.na(sum(x))) {
    out <- NA
  } else {
    tmin <- x[1:12]
    tavg <- x[13:24]
    prec <- x[25:26]
    
    out <- round(ecocrop(crop
                         , tmin
                         , tavg
                         , prec
                         , rainfed = FALSE)@maxsuit
                 , digits = 3)
  }
  
  return(out)
}

i <- 1

for(i in 1:nrow(ECOCROP_suit))
{
  crop <- getCrop(ECOCROP_suit$ECOCROP_NAME[i])
  
  beginCluster(12)
  
  suit <- clusterR(all
                   , overlay
                   , args = list(fun = f1)
                   , export = 'crop'
                   , filename = paste0(dir
                                       , '/ECOCROP_clim/ECOCROP_'
                                       , ECOCROP_suit$code2[i], '_'
                                       , ECOCROP_suit$name2[i]
                                       , '.tif')
                   , overwrite = TRUE
                   , datatype  = 'FLT4S'
  )
  
  endCluster()
  
  plot(suit, main = ECOCROP_suit$ECOCROP_NAME[i])
}

# END