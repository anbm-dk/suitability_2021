# Generate training data

# Packages and functions

library(raster)
library(dplyr)
library(tabularaster)
library(tidyr)

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


# Start up
wd <- getwd()
setwd('..')
dir <- getwd()
setwd(wd)

years <- 2011:2019


# Load IMK

IMK3 <- loadandstack(paste0(dir, '/IMK3_newcodes/'))


# Load target crops

crops <- read.csv(paste0(dir, '/Excel/Target_crops_2.csv')
                  , header = TRUE
                  , sep = ';'
                  , stringsAsFactors = FALSE
)
crops$UK2 <- gsub("[[:space:]]", "", crops$NameUK)


# Extract raster cells

dir.create(paste0(dir, '/crop_cells/'))

cellcount <- matrix(numeric(), nrow = nrow(crops), ncol = length(years))

for(i in 1:nrow(crops))
{
  crop <- crops$Newcode[i]
  crop_cells <- list()
  
  beginCluster(16) # change this
  
  tmp <- clusterR(IMK3, calc, args = list(
    fun = function(x) {
      out <- x == crop
      out[out == 0] <- NA
      return(out)
    })
  , export    = 'crop'
  , filename  = paste0(dir, '/temp/tmp.tif')
  , overwrite = TRUE
  , datatype  = 'INT2U'
  )
  
  endCluster()
  for(j in 1:length(years))
  {
    crop_cells[[j]] <- drop_na(tabularaster::as_tibble(tmp[[j]]))
  }
  
  saveRDS(crop_cells, file = paste0(dir, '/crop_cells/cells_', crop, '.tif'))
}


# Randomly sampled raster cells for each year

randcells <- list()

for(i in 1:length(years))
{
  randcells[[i]] <- tibble::as_tibble(sampleRandom(IMK3[[i]]
                                                   , 24000
                                                   , cells = TRUE
                                                   ))[, 2:1]
  
  colnames(randcells[[i]]) <- c('cellvalue', 'cellindex')
  
  print(i)
}

saveRDS(randcells, file = paste0(dir, '/crop_cells/cells_rand.rds'))

rand2 <- readRDS(paste0(dir, '/crop_cells/cells_rand.rds'))

# More code (not run)

# xy <- xyFromCell(IMK3[[1]], crop_cells[[1]]$cellindex)
# 
# plot(IMK3[[1]])
# 
# points(xy, add = TRUE)
# 
# vals <- IMK3[crop_cells[[1]]$cellindex]

# END