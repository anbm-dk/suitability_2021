# Clean IMK rasters

library(raster)

loadandstack <- function(dir = NULL)
{
  rlist<-list.files(dir, pattern="tif$", full.names = TRUE)
  for(r in rlist)
  {
    name <- unlist(strsplit(r, "[.]"))[length(unlist(strsplit(r, "[.]"))) - 1]
    assign(name, raster(r))
  }
  output <- stack(rlist)
  return(output)
}

# 1: Aggregate forested areas

IMK <- loadandstack('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/IMK_raster')

codes <- read.csv('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/Agriculture_codes.csv'
                  , header = TRUE
                  , sep = ';'
                  )

codes_forest <- codes$AfgKode[codes$Agriculture == -1]

forest <- list()

for (i in 1:nlayers(IMK))
{
  forest[[i]] <- IMK[[i]] %in% codes_forest
}
rm(i)

forest <- stack(forest)

forest_total <- sum(forest) > 0

forest_total[forest_total == 0] <- NA

writeRaster(forest_total
            , filename = 'C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/RASTERS/forest.tif'
            , datatype = 'LOG1S'
            )

IMK_2 <- mask(IMK, forest_total, inverse = TRUE)


# 2: Remove non-agricultural areas

codes_nonag <- codes$AfgKode[codes$Agriculture == 0]

for(i in 1:nlayers(IMK_2))
{
  IMK_2[[i]][IMK_2[[i]] %in% codes_nonag] <- NA
}
rm(i)

for (i in 1:nlayers(IMK_2)) {
  writeRaster(IMK_2[[i]]
              , filename = paste0('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/IMK_cleaned/fields_'
                                  , (2010 + i), '.tif')
              , datatype = 'INT2U'
              , overwrite = TRUE
  )
}
rm(i)

fields <- sum(is.na(IMK_2) == FALSE)

fields[fields == 0] <- NA

writeRaster(fields
            , filename = 'C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Crop_sums/fields.tif'
            , datatype = 'INT2U'
            , overwrite = TRUE
)

fields_logic <- fields*0 + 1

fields_logic <- fields_logic == 1

fields_logic[fields_logic == 0] <- NA

writeRaster(fields_logic
            , filename = 'C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/RASTERS/studyarea.tif'
            , datatype = 'LOG1S'
            , overwrite = TRUE
)

# END