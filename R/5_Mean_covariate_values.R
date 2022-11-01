# Average covariate values within fields

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

wd <- getwd()
setwd('..')
dir <- getwd()
setwd(wd)


# Mean values and standard deviation for each covariate within each field

years <- 2011:2019
decimals <- 0:3

IMK_ID <- loadandstack(paste0(dir, '/IMK_ID/'))

dtype <- c('INT2S', 'FLT4S', 'FLT4S', 'FLT4S')

# missing piece

load(file = 'ijk.Rdata')
load(file = 'stuff_j.Rdata')

# change this:
i <- 5
j <- 4
k <- 24
# /change this


from_k <- k
from_j <- j + 1
from_i <- i + 1

rs <- loadandstack(paste0(dir, '/COVARIATES/decimals_', j - 1, '/'))

zstat1 <- zonal(rs, IMK_ID[[i]], digits = j - 1)
zstat2 <- zonal(rs, IMK_ID[[i]], digits = j - 1, fun = 'sd')
zstat1[is.nan(zstat1)] <- NA
zstat2[is.nan(zstat2)] <- NA
# This sets missing standard deviations to 0
# Not done for the first three years:
zstat2[is.na(zstat2)] <- 0
reclasser <- cbind(zstat1, zstat2[, -1])

lnames <- c(paste0(names(rs), '_', 'mean')
            , paste0(names(rs), '_', 'sd'))
folder <- paste0(dir, '/COVARIATES/Y_', years[i], '/')

reclasser <- round(reclasser, digits = j - 1)

save(list = c('reclasser', 'lnames', 'folder'), file = 'stuff_j.Rdata')

if(from_k < (length(lnames) + 1))
{
  for(k in from_k:length(lnames))
  {
    reclasser_sub <- reclasser[, c(1, k + 1)]
    
    beginCluster(12)
    
    clusterR(IMK_ID[[i]], reclassify, args = list(rcl = reclasser_sub)
             , export = 'reclasser_sub'
             , filename = paste0(folder, lnames[k], '.tif')
             , datatype = dtype[j]
             , overwrite = TRUE
    )
    
    endCluster()
    
    save(list = c('i', 'j', 'k'), file = 'ijk.Rdata')
    
    print(c(years[i], lnames[k]))
  }
}


if(from_j < 5)
{
  for(j in from_j:length(decimals))
  {
    rs <- loadandstack(paste0(dir, '/COVARIATES/decimals_', j - 1, '/'))
    
    zstat1 <- zonal(rs, IMK_ID[[i]], digits = j - 1)
    zstat2 <- zonal(rs, IMK_ID[[i]], digits = j - 1, fun = 'sd')
    zstat1[is.nan(zstat1)] <- NA
    zstat2[is.nan(zstat2)] <- NA
    # This sets missing standard deviations to 0
    # Not done for the first three years:
    zstat2[is.na(zstat2)] <- 0
    reclasser <- cbind(zstat1, zstat2[, -1])
    
    lnames <- c(paste0(names(rs), '_', 'mean')
                , paste0(names(rs), '_', 'sd'))
    folder <- paste0(dir, '/COVARIATES/Y_', years[i], '/')
    
    reclasser <- round(reclasser, digits = j - 1)
    
    save(list = c('reclasser', 'lnames', 'folder'), file = 'stuff_j.Rdata')
    
    for(k in 1:length(lnames))
    {
      reclasser_sub <- reclasser[, c(1, k + 1)]
      
      beginCluster(12)
      
      clusterR(IMK_ID[[i]], reclassify, args = list(rcl = reclasser_sub)
               , export = 'reclasser_sub'
               , filename = paste0(folder, lnames[k], '.tif')
               , datatype = dtype[j]
               , overwrite = TRUE
      )
      
      endCluster()
      
      save(list = c('i', 'j', 'k'), file = 'ijk.Rdata')
      
      print(c(years[i], lnames[k]))
    }
  }
}


# /missing piece

for(i in from_i:length(years))
{
  dir.create(paste0(dir, '/COVARIATES/Y_', years[i], '/')
             , showWarnings = FALSE)
  
  for(j in 1:length(decimals))
  {
    rs <- loadandstack(paste0(dir, '/COVARIATES/decimals_', j - 1, '/'))
    
    zstat1 <- zonal(rs, IMK_ID[[i]], digits = j - 1)
    zstat2 <- zonal(rs, IMK_ID[[i]], digits = j - 1, fun = 'sd')
    zstat1[is.nan(zstat1)] <- NA
    zstat2[is.nan(zstat2)] <- NA
    # This sets missing standard deviations to 0
    # Not done for the first three years:
    zstat2[is.na(zstat2)] <- 0
    reclasser <- cbind(zstat1, zstat2[, -1])
    
    lnames <- c(paste0(names(rs), '_', 'mean')
               , paste0(names(rs), '_', 'sd'))
    folder <- paste0(dir, '/COVARIATES/Y_', years[i], '/')
    
    reclasser <- round(reclasser, digits = j - 1)
    
    save(list = c('reclasser', 'lnames', 'folder'), file = 'stuff_j.Rdata')
    
    for(k in 1:length(lnames))
    {
      reclasser_sub <- reclasser[, c(1, k + 1)]
      
      beginCluster(12)
      
      clusterR(IMK_ID[[i]], reclassify, args = list(rcl = reclasser_sub)
                    , export = 'reclasser_sub'
                    , filename = paste0(folder, lnames[k], '.tif')
                    , datatype = dtype[j]
                    , overwrite = TRUE
      )
      
      endCluster()
      
      print(c(years[i], lnames[k]))
      
      save(list = c('i', 'j', 'k'), file = 'ijk.Rdata')
    }
  }
}
# rm(i, j, rs, zstat1, zstat2, reclasser, lnames, folder, k, reclasser_sub,
#    reclasser_1, reclasser_2)


# END