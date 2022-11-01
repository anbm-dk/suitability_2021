# Spatiotemporal training/test data splits for use in prediction models

library(dplyr)
library(tidyr)
library(colorRamps)

wd <- getwd()
setwd('..')
dir <- getwd()
setwd(wd)

dir.create(paste0(dir, '/tsplits/'), showWarnings = FALSE)

source(paste0(wd, '/spt_holdout.R'))

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


# Load target crops

crops <- read.csv(paste0(dir, '/Excel/Target_crops_2.csv')
                  , header = TRUE
                  , sep = ';'
                  , stringsAsFactors = FALSE
)
crops$UK2 <- gsub("[[:space:]]", "", crops$NameUK)


# Load covariates (for indices)

cov_permanent <- loadandstack(paste0(dir, '/COVARIATES/permanent/'))


# Training/test indices for each crop

for(i in 1:length(crops$Newcode))
{
  crop <- crops$Newcode[i]
  
  d <- readRDS(file = paste0(dir, '/tdata/tdata_', crop, '.rds'))
  
  d_xy <- xyFromCell(cov_permanent[[1]], d$index_year$cellindex)
  
  indices <- spt_holdout(xy = d_xy
                         , n = 100
                         , mindist = 1000
                         , repeats = 4
                         , time = d$index_year$year
                         , ttype = 'each'
                         )
  
  saveRDS(indices, file = paste0(dir, '/tsplits/tsplits_', crop, '.rds'))
}

# Test code and plot (not run)

# # 1: Random time slices
# 
# tcolors <- primary.colors()[as.numeric(as.factor(d$index_year$year))]
#
# set.seed(401351)
# 
# try <- spt_holdout(xy = d_xy, n = 100, mindist = 0.1, repeats = 6, time = d$index_year$year
#                    , ttype = 'rand')
# 
# par(mfrow = c(2, 3), mai = c(0.6, 0.5, 0.1, 0.1))
# 
# for(i in 1:6)
# {
#   plot(d_xy[try$train[[i]], ], asp = 1
#        , xlim = c(min(d_xy[, 1]), max(d_xy[, 1]))
#        , ylim = c(min(d_xy[, 2]), max(d_xy[, 2]))
#        , col = tcolors[try$train[[i]]], pch = 21)
# 
#   points(d_xy[try$holdout[[i]], ]
#          , col = tcolors[try$holdout[[i]]], bg = 'white', pch = 16)
# }
# 
# 
# # 1: Systematic time slices
# 
# try2 <- spt_holdout(xy = d_xy, n = 100, mindist = 0.1, repeats = 1, time = d$index_year$year
#                    , ttype = 'each')
# 
# par(mfrow = c(3, 3), mai = c(0.6, 0.5, 0.1, 0.1))
# 
# for(i in 1:9)
# {
#   plot(d_xy[try2$train[[i]], ], asp = 1
#        , xlim = c(min(d_xy[, 1]), max(d_xy[, 1]))
#        , ylim = c(min(d_xy[, 2]), max(d_xy[, 2]))
#        , col = 'grey', pch = 21)
#   
#   points(d_xy[try2$holdout[[i]], ]
#          , col = tcolors[try2$holdout[[i]]], bg = 'white', pch = 16)
# }

# END