# Correlation between ECOCROP and ML suitability

library(raster)

wd <- getwd()
setwd('..')
dir <- getwd()
setwd(wd)

eco <- raster(paste0(dir, '/ECOCROP_combined/ECOCROP_0001_Potato.tif'))
ml  <- raster(paste0(dir, '/results/first_try_potatoes.tif'))

raster_stack <- stack(eco, ml)

jnk <- layerStats(raster_stack, 'pearson', na.rm = TRUE)
corr_matrix <- jnk$'pearson correlation coefficient'

corr_matrix

# END