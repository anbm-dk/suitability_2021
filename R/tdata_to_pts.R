# Training data to points

library(raster)
library(caret)
library(ranger)
library(dplyr)
library(tidyr)
library(rgdal)

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

dir.create(paste0(dir, '/xy_pts/'))


# Load target crops

crops <- read.csv(paste0(dir, '/Excel/Target_crops_2.csv')
                  , header = TRUE
                  , sep = ';'
                  , stringsAsFactors = FALSE
)
crops$UK2 <- gsub("[[:space:]]", "", crops$NameUK)


# Load covariates

years    <- 2011:2019
decimals <- 0:3

cov_permanent <- loadandstack(paste0(dir, '/COVARIATES/permanent/'))

cov_cell <- list()

for(i in 1:length(decimals))
{
  cov_cell[[i]] <- loadandstack(paste0(dir, '/COVARIATES/decimals_'
                                       , decimals[i]))
}

cov_cell <- stack(cov_cell)

cov_field <- list()

for(i in 1:length(years))
{
  cov_field[[i]] <- loadandstack(paste0(dir, '/COVARIATES/Y_', years[[i]]))
}


# Load cells

cropnumbers <- c(152, 407, 418, 424)

cell_xy_crops <- function(cropnumber) {
  cells <- readRDS(file = paste0(dir, '/tdata/tdata_', cropnumber, '.rds'))
  
  cells <- cells[[3]]$cellindex[cells[[2]]$target == 'X1']
  
  xy <- xyFromCell(cov_cell, cells)
  
  xypts <- SpatialPointsDataFrame(xy, proj4string = crs(cov_cell), data = as.data.frame(xy))
  
  writeOGR(xypts
           , dsn = paste0(dir, "/xy_pts/xy_", crops$UK2[crops$Newcode == cropnumber], ".shp")
           , layer = "xy_carrots"
           , driver = "ESRI Shapefile")
}

for(i in 1:length(cropnumbers)) {
  cell_xy_crops(cropnumber = cropnumbers[i])
}


# cellstats for cell covariates

rmean <- cellStats(cov_cell, 'mean')
rmin <- cellStats(cov_cell, 'min')
rmax <- cellStats(cov_cell, 'max')

rmat <- cbind(rmean, rmin, rmax)

colnames(rmat) <- c('mean', 'min', 'max')
rownames(rmat) <- names(cov_cell)

write.table(rmat, file = paste0(dir, '/Excel/cov_stat.csv'), sep = ';')


# cellstats for permanent covariates

rmean <- cellStats(cov_permanent, 'mean')
rmin <- cellStats(cov_permanent, 'min')
rmax <- cellStats(cov_permanent, 'max')

rmat <- cbind(rmean, rmin, rmax)

colnames(rmat) <- c('mean', 'min', 'max')
rownames(rmat) <- names(cov_permanent)

write.table(rmat, file = paste0(dir, '/Excel/cov_stat_perm.csv'), sep = ';')

# END