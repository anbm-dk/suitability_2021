# Make training datasets and 

# Get ready

library(raster)
library(caret)
library(ranger)
library(dplyr)
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

wd <- getwd()
setwd('..')
dir <- getwd()
setwd(wd)

dir.create(paste0(dir, '/tdata/'), showWarnings = FALSE)

# Load target crops

crops <- read.csv(paste0(dir, '/Excel/Target_crops_2.csv')
                  , header = TRUE
                  , sep = ';'
                  , stringsAsFactors = FALSE
)
crops$UK2 <- gsub("[[:space:]]", "", crops$NameUK)


# Define constants

years    <- 2011:2019
maxn_yr1 <- 2000  # Points sampled per yerar at first
maxn_yr2 <- 1500  # After removing other crops and NAs


# Table for cells per year per crop

cells_yr_crop <- matrix(numeric()
                        , nrow = nrow(crops)
                        , ncol = length(years)
                        , dimnames = list(crops$Newcode, years)
)


# Load covariates

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


# Load randomly sampled points

rand <- readRDS(paste0(dir, '/crop_cells/cells_rand.rds'))

rand <- lapply(1:length(years), function(x)
{
  out <- dplyr::sample_n(rand[[x]], maxn_yr1)
  return(out)
})


# Extracts for randomly sampled points

rand_all <- dplyr::bind_rows(rand)

extr_rand_all_perm <- as.data.frame(cov_permanent[rand_all$cellindex])
extr_rand_all_cell <- as.data.frame(cov_cell[rand_all$cellindex])

extr_rand_perm  <- list()
extr_rand_cell  <- list()
extr_rand_field <- list()
extr_rand_all   <- list()

for(i in 1:length(years))
{
  extr_rand_perm[[i]]  <- extr_rand_all_perm[match(rand[[i]]$cellindex
                                                   , rand_all$cellindex), ]
  extr_rand_cell[[i]]  <- extr_rand_all_cell[match(rand[[i]]$cellindex
                                                   , rand_all$cellindex), ]
  extr_rand_field[[i]] <- as.data.frame(cov_field[[i]][rand[[i]]$cellindex])
  extr_rand_all[[i]]   <- cbind(extr_rand_perm[[i]]
                               , extr_rand_cell[[i]]
                               , extr_rand_field[[i]])
  extr_rand_cell[[i]]  <- cbind(extr_rand_perm[[i]]
                               , extr_rand_cell[[i]])
  extr_rand_cell[[i]]  <- tibble::as_tibble(extr_rand_cell[[i]])
  extr_rand_field[[i]] <- cbind(extr_rand_perm[[i]]
                               , extr_rand_field[[i]])
  extr_rand_field[[i]] <- tibble::as_tibble(extr_rand_field[[i]])
}


# Remove NAs for randomly sampled points

true_rand <- lapply(extr_rand_all, function(x1)
{
  out1 <- apply(x1, 1, FUN = function(x2)
  {
    out2 <- sum(is.na(x2)) == 0
    return(out2)
  })
  return(out1)
})

for(i in 1:length(years))
{
  rand[[i]] <- rand[[i]][true_rand[[i]], ]
  extr_rand_cell[[i]]  <- extr_rand_cell[[i]][true_rand[[i]], ]
  extr_rand_field[[i]] <- extr_rand_field[[i]][true_rand[[i]], ]
}


# Extracts for each crop

for(j in 33:nrow(crops))  # Change this value
{
  crop <- crops$Newcode[j]
  
  pres <- readRDS(paste0(dir, '/crop_cells/cells_', crop, '.rds'))
  
  n_yr1 <- lapply(pres, function(x) {
    return(min(maxn_yr1, nrow(x)))
  })
  
  pres <- lapply(1:length(years), function(x)
  {
    out <- dplyr::sample_n(pres[[x]], n_yr1[[x]])
    return(out)
  }
  )
  
  abs_ind <- lapply(1:length(years), function(x)
  {
    out <- rand[[x]]$cellvalue != crop
    return(out)
  })
  
  abs <- list()
  extr_abs_cell  <- list()
  extr_abs_field <- list()
  
  for(i in 1:length(years))
  {
    abs[[i]] <- rand[[i]][abs_ind[[i]], ]
    extr_abs_cell[[i]]  <- extr_rand_cell[[i]][abs_ind[[i]], ]
    extr_abs_field[[i]] <- extr_rand_field[[i]][abs_ind[[i]], ]
  }
  
  
  # Extract cell values for presence points

  pres_all <- dplyr::bind_rows(pres)
  
  extr_pres_all_perm <- as.data.frame(cov_permanent[pres_all$cellindex])
  extr_pres_all_cell <- as.data.frame(cov_cell[pres_all$cellindex])
  
  extr_pres_perm  <- list()
  extr_pres_cell  <- list()
  extr_pres_field <- list()
  extr_pres_all   <- list()

  
  for(i in 1:length(years))
  {
    extr_pres_perm[[i]]  <- extr_pres_all_perm[match(pres[[i]]$cellindex
                                                     , pres_all$cellindex), ]
    extr_pres_cell[[i]]  <- extr_pres_all_cell[match(pres[[i]]$cellindex
                                                     , pres_all$cellindex), ]
    extr_pres_field[[i]] <- as.data.frame(cov_field[[i]][pres[[i]]$cellindex])
    extr_pres_all[[i]]   <- cbind(extr_pres_perm[[i]]
                                  , extr_pres_cell[[i]]
                                  , extr_pres_field[[i]])
    extr_pres_cell[[i]]  <- cbind(extr_pres_perm[[i]]
                                  , extr_pres_cell[[i]])
    extr_pres_cell[[i]]  <- tibble::as_tibble(extr_pres_cell[[i]])
    extr_pres_field[[i]] <- cbind(extr_pres_perm[[i]]
                                  , extr_pres_field[[i]])
    extr_pres_field[[i]] <- tibble::as_tibble(extr_pres_field[[i]])
  }
  
  
  # Remove NAs for presence points
  
  true_pres <- lapply(extr_pres_all, function(x1)
  {
    out1 <- apply(x1, 1, FUN = function(x2)
    {
      out2 <- sum(is.na(x2)) == 0
      return(out2)
    })
    return(out1)
  })
  
  for(i in 1:length(years))
  {
    pres[[i]] <- pres[[i]][true_pres[[i]], ]
    extr_pres_cell[[i]]  <- extr_pres_cell[[i]][true_pres[[i]], ]
    extr_pres_field[[i]] <- extr_pres_field[[i]][true_pres[[i]], ]
  }
  
  
  # Downsample pres and abs

  n_yr2 <- lapply(1:length(years), function(x) {
    return(min(maxn_yr2, nrow(pres[[x]]), nrow(abs[[x]])))
  })
  
  cells_yr_crop[j, ] <- unlist(n_yr2)
  
  set.seed(1)
  
  for(i in 1:length(years))
  {
    sel_pres <- sample(nrow(pres[[i]]), n_yr2[[i]])
    sel_abs  <- sample(nrow(abs[[i]]), n_yr2[[i]])
    
    pres[[i]] <- pres[[i]][sel_pres, ]
    extr_pres_cell[[i]]  <- extr_pres_cell[[i]][sel_pres, ]
    extr_pres_field[[i]] <- extr_pres_field[[i]][sel_pres, ]
    
    abs[[i]] <- abs[[i]][sel_abs, ]
    extr_abs_cell[[i]]  <- extr_abs_cell[[i]][sel_abs, ]
    extr_abs_field[[i]] <- extr_abs_field[[i]][sel_abs, ]
  }
  rm(sel_pres, sel_abs)
  
  
  # Collate training data
  
  for(i in 1:length(years))
  {
    if(nrow(pres[[i]]) > 0)
    {
      pres[[i]]$year <- years[i]
    }
    if(nrow(abs[[i]]) > 0)
    {
      abs[[i]]$year  <- years[i]
    }
  }
  pres <- dplyr::bind_rows(pres)
  abs  <- dplyr::bind_rows(abs)
  
  extr_pres_cell  <- dplyr::bind_rows(extr_pres_cell)
  extr_pres_field <- dplyr::bind_rows(extr_pres_field)
  
  extr_abs_cell  <- dplyr::bind_rows(extr_abs_cell)
  extr_abs_field <- dplyr::bind_rows(extr_abs_field)
  
  extr_pres_cell$target  <- 1
  extr_pres_field$target <- 1
  
  extr_abs_cell$target  <- 2
  extr_abs_field$target <- 2
  
  data_cell  <- rbind(extr_pres_cell,  extr_abs_cell)
  data_field <- rbind(extr_pres_field, extr_abs_field)
  
  data_cell$target  <- as.factor(data_cell$target)
  data_field$target <- as.factor(data_field$target)
  
  levels(data_cell$target)  <- make.names(levels(data_cell$target))
  levels(data_field$target) <- make.names(levels(data_field$target))
  
  index_year <- dplyr::bind_rows(pres, abs)[, -1]
  
  alldata <- list(cell = data_cell
                  , field = data_field
                  , index_year = index_year)
  
  saveRDS(alldata, file = paste0(dir, '/tdata/tdata_', crop, '.rds'))
}

write.csv2(cells_yr_crop, file = paste0(dir, '/cells_yr_crop.csv'))

# END