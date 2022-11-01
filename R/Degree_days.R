# Degree days above 5 degrees Celcius

# Load world climatic data and reproject them for Denmark.

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


# Load worldClim 2 data to rasterstack

wc2 <- loadandstack(paste0(dir, '/wc2.0_30s_tavg/'))


# Load raster for Denmark

dk <- raster(paste0(dir, '/RASTERS/fields.tif'))


# Extent of Danish rasters

e <- raster::extent(dk)


# Transform extent to polygon

e <- as(e, 'SpatialPolygons')

sp::proj4string(e) <- proj4string(dk)


# Reproject extent polygon to the projection of the worldclim 2 data

e.geo <- sp::spTransform(e, crs(wc2))


# Expand the extent 1 degree in all directions, to make sure nothing is left
# out.

e.geo <- extend(extent(e.geo), 1, 1, 1, 1)


# Crop the worldclim 2 data using the new extent

wc2_crop <- raster::crop(wc2, e.geo)


# Gaussian smoothing of the cropped worldclim 2 data, taking into account the
# rectangular shape of the cells.

lat_to_m <- 111131.745
ym <- yres(wc2_crop)*lat_to_m
xm <- cellStats(area(wc2_crop), stat = 'mean')*1000/(ym/1000)

library(spatialEco)

inc_x <- 99
inc_y <- round(inc_x*ym/xm, digits = 0)

matsize <- 41

gf_big <- gaussian.kernel(sigma = 1*inc_y, n = matsize*inc_y)

sum_gf <- matrix(numeric(), ncol = matsize, nrow = matsize)

startrow <- (matsize*inc_y - matsize*inc_x)/2

for(i in 1:matsize){
  for(j in 1:matsize)
    sum_gf[i, j] <- sum(gf_big[((i - 1)*inc_x + 1 + startrow):(i*inc_x + startrow)
                               , ((j - 1)*inc_y + 1):(j*inc_y)])
}

sum_gf <- t(sum_gf)

weightr <- focal(wc2_crop[[1]]*0 + 1, w = sum_gf, na.rm = TRUE, pad = TRUE
                 , NAonly = TRUE)

wc2_gauss <- list()

for(i in 1:nlayers(wc2_crop))
{
  wc2_gauss[[i]] <- focal(wc2_crop[[i]], w = sum_gf, na.rm = TRUE, pad = TRUE
                          , NAonly = TRUE)/weightr
}

wc2_gauss <- stack(wc2_gauss)


# Reproject the cropped worldclim 2 data to the projection, extent and
# resolution of the Danish data.

wc2_proj <- projectRaster(from = wc2_gauss, to = dk)


# Mask reprojected rasters

elevation <- raster(paste0(dir, '/COVARIATES/elevation.tif'))

wc2_proj <- mask(wc2_proj, elevation)


# Calculate degree days

days_month <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

ddays <- (wc2_proj - 5)*days_month

for(i in 1:nlayers(ddays))
{
  ddays[[i]][ddays[[i]] < 0] <- 0
}
rm(i)
  
ddays_sum <- sum(ddays)

writeRaster(ddays_sum
            , filename = paste0(dir, '/COVARIATES/ddays.tif')
            , datatype = 'INT2U'
)

# END