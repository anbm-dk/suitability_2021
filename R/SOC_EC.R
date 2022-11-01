# Format socioeconomic variables

# Start up
wd <- getwd()
setwd('..')
dir <- getwd()
setwd(wd)

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


# Crop rasters (done)

# socec <- loadandstack(paste0(dir, '/SOCEC/'))
#
# names <- names(socec)
# 
# dk <- raster(paste0(dir, '/RASTERS/studyarea.tif'))
# 
# socec2 <- crop(socec, dk, filename = paste0(dir, '/temp/tmp.tif'))
# 
# for(i in 1:nlayers(socec2))
# {
#   writeRaster(socec2[[i]], filename = paste0(dir, '/SOCEC2/', names[i], '.tif')
#               , progress = 'text')
#   print(names[i])
# }


# Format and round rasters

socec2 <- loadandstack(paste0(dir, '/SOCEC2/'))

names(socec2)

dir.create(paste0(dir, '/SOCEC3/'))

writeRaster(socec2[[3]]
            , filename  = paste0(dir, '/SOCEC3/', names(socec2)[3], '.tif')
            , datatype  = 'INT2U'
            , overwrite = TRUE
            , progress  = 'text'
)

# END