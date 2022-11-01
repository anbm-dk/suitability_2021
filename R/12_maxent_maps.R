# Maxent maps and covariate importance

library(rJava)
options(java.parameters = "-Xmx50g")
library(dismo)
library(ENMeval)
library(dplyr)
library(tidyr)
library(snow)
library(foreach)
library(doSNOW)
library(raster)
library(pROC)


# Load and stack function

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


# Load target crops

crops <- read.csv(paste0(dir, '/Excel/Target_crops_2.csv')
                  , header = TRUE
                  , sep = ';'
                  , stringsAsFactors = FALSE
)
crops$UK2 <- gsub("[[:space:]]", "", crops$NameUK)


# Define constants

years <- 2011:2019


# Load covariates

decimals <- 0:3

cov_permanent <- loadandstack(paste0(dir, '/COVARIATES/permanent/'))

cov_cell <- list()

for(i in 1:length(decimals))
{
  cov_cell[[i]] <- loadandstack(paste0(dir, '/COVARIATES/decimals_'
                                       , decimals[i]))
}
rm(i)

cov_cell <- stack(cov_cell)
cov_cell <- stack(cov_permanent, cov_cell)

cov_field <- list()

for(i in 1:length(years))
{
  cov_field[[i]] <- loadandstack(paste0(dir, '/COVARIATES/Y_', years[[i]]))
  cov_field[[i]] <- stack(cov_permanent, cov_field[[i]])
}
rm(cov_permanent, decimals, i)


# Function to train models and write rasters

me_map <- function(dat, pres, covs, tag)
{
  vars <- colnames(dat)[colnames(dat) %in% names(covs)]
  dat <- dat[, colnames(dat) %in% vars]
  
  model <- maxent(x = dat
                  , p = pres
  )
  
  vi <- var.importance(model)
  
  vars <- as.character(vi$variable[vi$percent.contribution > 0])
  dat <- dat[, colnames(dat) %in% vars]
  
  model <- maxent(x = dat
                  , p = pres
                  , path = paste0(dir, '/models_me_v1/model_', crop, '_', tag)
  )
  
  covs <- subset(covs, vars)
  
  beginCluster(12)
  
  preds <- clusterR(covs
                    , raster::predict
                    , args = list(model = model)
                    , filename = paste0(dir, '/results_me_', tag, '_v1/pred_'
                                        , crop, '_', cropname
                                        , '_', tag, '.tif'))
  
  endCluster()
}


# Create models and run predictions

dir.create(paste0(dir, '/models_me_v1/'))
dir.create(paste0(dir, '/results_me_cell_v1/'))
dir.create(paste0(dir, '/results_me_field_v1/'))

for(i in 1:nrow(crops))
{
  crop <- crops$Newcode[i]
  cropname <- crops$UK2[i]
  
  d <- readRDS(file = paste0(dir, '/tdata/tdata_', crop, '.rds'))
  
  pres_i <- as.numeric(d$field$target)
  pres_i[pres_i == 2] <- 0
  
  # Field-based models
  me_map(dat    = d$field[, -ncol(d$field)]
         , pres = pres_i
         , covs = cov_field[[9]]
         , tag  = 'field'
  )
  
  # Cell-based models
  me_map(dat    = d$cell[, -ncol(d$cell)]
         , pres = pres_i
         , covs = cov_cell
         , tag  = 'cell'
  )
}

# END