# Round off digits for covariates

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

loadlist <- function(dir = NULL)
{
  rlist<-list.files(dir, pattern="tif$", full.names = TRUE)
  for(r in rlist)
  {
    name <- unlist(strsplit(r, "[.]"))[length(unlist(strsplit(r, "[.]"))) - 1]
    assign(name, raster(r))
  }
  output <- list(rlist)
  return(output)
}

rs <- unlist(loadlist('C:/Users/au542768/Desktop/CROP_EXTRA/'))

asp <- raster('C:/Users/au542768/GEODATA/covariates/asp_cos.tif')


# Expand all rasters:

for (i in 1:length(rs))
{
  r <- raster(rs[i])
  
  extend(r, asp
         , filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', names(r), '.tif')
         , datatype = dataType(r)
         , overwrite = TRUE
         , format = 'GTiff' 
         )
}
rm(i, r)


# Resample "DMI_solar"

r <- raster('C:/Users/au542768/Desktop/CROP_EXTRA/DMI_solar.tif')

resample(r, asp
         , filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', names(r), '.tif')
         , datatype = dataType(r)
         , overwrite = TRUE
         , format = 'GTiff' 
         )


# Resample winter wheat yeilds

r <- raster('C:/Users/au542768/Desktop/CROP_EXTRA/WWheat_yield.tif')

resample(r, asp
         , filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', names(r), '.tif')
         , datatype = dataType(r)
         , overwrite = TRUE
         , format = 'GTiff' 
)

# Load rasters

rs <- loadandstack('C:/Users/au542768/Desktop/CROP_EXTRA/')


# Bulk density

names <- c("BD_000_005", "BD_005_015", "BD_015_030", "BD_030_060", "BD_060_100")

for (i in 1:length(names))
{
  r <- raster(paste0('C:/Users/au542768/Desktop/CROP_EXTRA/', names[i], '.tif'))
  
  beginCluster(2)
  
  clusterR(r, calc, args = list(fun = function(x) {
    out <- round(x, digits = 2)
    return(out)
  }
  )
  , filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', names[i], '.tif')
  , datatype = 'FLT4S'
  , overwrite = TRUE
  , format = 'GTiff'
  )
  
  endCluster()
}
rm(i, r)


# Frost risk

name <- 'DMI_frostrisk'

r <- raster(paste0('C:/Users/au542768/Desktop/CROP_EXTRA/', name, '.tif'))

beginCluster(2)

clusterR(r, calc, args = list(fun = function(x) {
  out <- round(x, digits = 2)
  return(out)
}
)
, filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', name, '.tif')
, datatype = 'FLT4S'
, overwrite = TRUE
, format = 'GTiff'
)

endCluster()


# Growing days

name <- 'DMI_growdays'

r <- raster(paste0('C:/Users/au542768/Desktop/CROP_EXTRA/', name, '.tif'))

beginCluster(2)

clusterR(r, calc, args = list(fun = function(x) {
  out <- round(x, digits = 1)
  return(out)
}
)
, filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', name, '.tif')
, datatype = 'FLT4S'
, overwrite = TRUE
, format = 'GTiff'
)

endCluster()


# Precipitation

name <- 'DMI_precipitation'

r <- raster(paste0('C:/Users/au542768/Desktop/CROP_EXTRA/', name, '.tif'))

beginCluster(2)

clusterR(r, calc, args = list(fun = function(x) {
  out <- round(x, digits = 1)
  return(out)
}
)
, filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', name, '.tif')
, datatype = 'FLT4S'
, overwrite = TRUE
, format = 'GTiff'
)

endCluster()


# Solar radiation

name <- 'DMI_solar'

r <- raster(paste0('C:/Users/au542768/Desktop/CROP_EXTRA/', name, '.tif'))

beginCluster(2)

clusterR(r, calc, args = list(fun = function(x) {
  out <- round(x, digits = 0)
  return(out)
}
)
, filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', name, '.tif')
, datatype = 'INT2U'
, overwrite = TRUE
, format = 'GTiff'
)

endCluster()


# Plant-available water

names <- c("PAW_000_030", "PAW_030_060", "PAW_060_100", "PAW_100_200")

for (i in 1:length(names))
{
  r <- raster(paste0('C:/Users/au542768/Desktop/CROP_EXTRA/', names[i], '.tif'))
  
  beginCluster(2)
  
  clusterR(r, calc, args = list(fun = function(x) {
    out <- round(x, digits = 1)
    return(out)
  }
  )
  , filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', names[i], '.tif')
  , datatype = 'FLT4S'
  , overwrite = TRUE
  , format = 'GTiff'
  )
  
  endCluster()
}
rm(i, r)


# pH

names <- c("pH_000_005", "pH_005_015", "pH_015_030", "pH_030_060", "pH_060_100", "pH_100_200")

for (i in 1:length(names))
{
  r <- raster(paste0('C:/Users/au542768/Desktop/CROP_EXTRA/', names[i], '.tif'))
  
  beginCluster(2)
  
  clusterR(r, calc, args = list(fun = function(x) {
    out <- round(x, digits = 1)
    return(out)
  }
  )
  , filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', names[i], '.tif')
  , datatype = 'FLT4S'
  , overwrite = TRUE
  , format = 'GTiff'
  )
  
  endCluster()
}
rm(i, r)


# Yannik's terrons

names <- c("Terron_YER_group", "Terron_YER_nat", "Terron_YER_reg")

for (i in 1:length(names))
{
  r <- raster(paste0('C:/Users/au542768/Desktop/CROP_EXTRA/', names[i], '.tif'))
  
  beginCluster(2)
  
  clusterR(r, calc, args = list(fun = function(x) {
    out <- round(x, digits = 0)
    return(out)
  }
  )
  , filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', names[i], '.tif')
  , datatype = 'INT2U'
  , overwrite = TRUE
  , format = 'GTiff'
  )
  
  endCluster()
}
rm(i, r)


# Yi's terron distances

names <- c("Terron_YP_1_dist", "Terron_YP_2_dist", "Terron_YP_3_dist", "Terron_YP_4_dist", "Terron_YP_5_dist", "Terron_YP_6_dist", "Terron_YP_7_dist", "Terron_YP_8_dist", "Terron_YP_9_dist")

for (i in 1:length(names))
{
  r <- raster(paste0('C:/Users/au542768/Desktop/CROP_EXTRA/', names[i], '.tif'))
  
  beginCluster(2)
  
  clusterR(r, calc, args = list(fun = function(x) {
    out <- round(x, digits = 3)
    return(out)
  }
  )
  , filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', names[i], '.tif')
  , datatype = 'FLT4S'
  , overwrite = TRUE
  , format = 'GTiff'
  )
  
  endCluster()
}
rm(i, r)


# Yi's terrons

name <- 'Terron_YP_class'

r <- raster(paste0('C:/Users/au542768/Desktop/CROP_EXTRA/', name, '.tif'))

beginCluster(2)

clusterR(r, calc, args = list(fun = function(x) {
  out <- round(x, digits = 0)
  return(out)
}
)
, filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', name, '.tif')
, datatype = 'INT2U'
, overwrite = TRUE
, format = 'GTiff'
)

endCluster()


# Winter wheat yield

name <- 'WWheat_yield'

r <- raster(paste0('C:/Users/au542768/Desktop/CROP_EXTRA/', name, '.tif'))

beginCluster(2)

clusterR(r, calc, args = list(fun = function(x) {
  out <- round(x, digits = 1)
  return(out)
}
)
, filename = paste0('C:/Users/au542768/Desktop/CROP_EXTRA_2/', name, '.tif')
, datatype = 'FLT4S'
, overwrite = TRUE
, format = 'GTiff'
)

endCluster()


# Artificial drainage probability

r <- raster('C:/Users/au542768/Desktop/drain/prob_v11.tif')

beginCluster(2)

clusterR(r, calc, args = list(fun = function(x) {
  out <- round(x, digits = 3)
  return(out)
}
)
, filename = 'C:/Users/au542768/Desktop/drain/drainage_probability.tif'
, datatype = 'FLT4S'
, overwrite = TRUE
, format = 'GTiff'
)

endCluster()

# END
