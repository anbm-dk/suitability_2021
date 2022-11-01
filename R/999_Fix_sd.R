# Fix standard deviations for the first three years
# For fields with only one cell, SD was set to NA for these years
# For the other years, I set SD to 0
# Now, I'll also set SD 0 zero for single cell fields in the other years

library(raster)
library(tabularaster)

decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

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

loadandstack_sd <- function(dir = NULL)
{
  rlist<-list.files(dir, pattern = "_sd.tif$", full.names = TRUE)
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

years <- 2011:2013
decimals <- 0:3

IMK_ID <- loadandstack(paste0(dir, '/IMK_ID/'))

dir.create(paste0(dir, '/COVARIATES/sd_new/'), showWarnings = FALSE)

n_decimals <- c(3, 3, 2, 2, 2, 1, 1, 1, 2, 0, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
                , 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3
                , 3, 3, 3, 1, 2)

for(i in 1:length(years))
{
  year <- years[i]
  
  dir.create(paste0(dir, '/COVARIATES/sd_new/Y_', year, '/')
             , showWarnings = FALSE)
  
  sd <-  loadandstack_sd(paste0(dir, '/COVARIATES/Y_', year))
  
  names <- names(sd)
  dtypes <- dataType(sd)
  
  f <- freq(IMK_ID, merge = TRUE, useNA = 'no')
  
  f_year <- f$value[f[, i + 1] == 1]
  f_year <- f_year[!is.na(f_year)]
  
  singlefields <- calc(IMK_ID[[i]], fun = function(x)
  {
    out <- x %in% f_year
    out[out == 0] <- NA
    return(out)
  }
  , filename  = paste0(dir, '/temp/temp.tif')
  , overwrite = TRUE
  )
  
  singlecells <- drop_na(tabularaster::as_tibble(singlefields))
  
  for(j in 1:nlayers(sd))
  {
    sd_new <- sd[[j]]
    
    sd_new[singlecells$cellindex] <- 0
    
    beginCluster(12)
    
    clusterR(sd_new, calc, args = list(fun = function(x)
      {
        out <- round(x, digits = n_decimals[j])
        return(out)
      })
      , export = c('n_decimals', 'j')
      , filename  = paste0(dir, '/COVARIATES/sd_new/Y_', year
                           , '/', names[j], '.tif')
      , overwrite = TRUE
      , datatype  = dtypes[j]
      , progress  = 'text'
    )
    
    endCluster()

    removeTmpFiles(h = 0)
  }
}


# END