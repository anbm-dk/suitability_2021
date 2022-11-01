# ECOCROP drainage

# Dra	TEXT
# I(1): Poorly (saturated>50% of year)
#         DC4 and DC5 without artficial drainage
# W(2):	Well (dry spells)
#         DC1, DC2 and DC3
# E(3):	Excessive (dry/moderately dry)
#         None

library(raster)

dc <- raster('C:/Users/au542768/GEODATA/covariates/dc.tif')

drained <- raster('C:/Users/au542768/Dropbox/AU/Papers_in_progress/'
                  ,'Crop_suitability/CROP_COVS/drainage_probability.tif')

reclasser <- cbind(c(1, 2, 3, 4, 5), c(2, 2, 2, 1, 1))

ECOCROP_drain <- reclassify(dc, reclasser)

ECOCROP_drain[drained > 0.5] <- 2

plot(ECOCROP_drain)

writeRaster(ECOCROP_drain
            , filename  = paste0('C:/Users/au542768/Dropbox/AU/'
                                 ,'Papers_in_progress/Crop_suitability/'
                                 ,'ECOCROP_input/ECOCROP_drainage.tif')
            , datatype  = 'INT2U'
            , overwrite = TRUE
            )

# END