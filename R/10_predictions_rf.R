# Model predictions

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


# Load target crops

crops <- read.csv(paste0(dir, '/Excel/Target_crops_2.csv')
                  , header = TRUE
                  , sep = ';'
                  , stringsAsFactors = FALSE
)
crops$UK2 <- gsub("[[:space:]]", "", crops$NameUK)


# Define constants

years    <- 2011:2019


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
names(cov_cell)[names(cov_cell) == 'layer'] <- 'dirinsola'

cov_field <- list()

for(i in 1:length(years))
{
  cov_field[[i]] <- loadandstack(paste0(dir, '/COVARIATES/Y_', years[[i]]))
  cov_field[[i]] <- stack(cov_permanent, cov_field[[i]])
}
rm(cov_permanent, decimals, i)


# Run predictions for field level models

for(i in 1:length(crops$Newcode))  # Change this
{
  crop <- crops$Newcode[i]
  cropname <- crops$UK2[i]

  rf_field <- readRDS(paste0(dir, '/models/model_', crop, '_field.rds'))

  beginCluster(12)

  clusterR(cov_field[[9]],
           predict,
           args = list(model = rf_field,
                       na.rm = TRUE,
                       type  = 'prob'
           ),
           filename = paste0(dir, '/results/pred_', crop, '_', cropname
                             , '_field.tif'),
           overwrite = TRUE
  )

  endCluster()
}


# Run predictions for cell level models

dir.create(paste0(dir, '/results_rf_cell/'), showWarnings = FALSE)

for(i in 1:length(crops$Newcode))
{
  crop <- crops$Newcode[i]
  cropname <- crops$UK2[i]
  
  rf_cell <- readRDS(paste0(dir, '/models_rf/model_', crop, '_cell.rds'))
  
  beginCluster(12)
  
  clusterR(cov_cell,
           predict,
           args = list(model = rf_cell,
                       na.rm = TRUE,
                       type  = 'prob'
           ),
           filename = paste0(dir, '/results_rf_cell/pred_', crop, '_', cropname
                             , '_cell.tif'),
           overwrite = TRUE
  )
  
  endCluster()
}


# Plot predictions

library(viridis)

s <- loadandstack(paste0(dir, '/results_rf_cell/'))

pdf(file = paste0(dir, '/results_rf_cell_small.pdf')
    , width = 16
    , height = 12
    )

for(i in 1:nlayers(s))
{
  qs <- unique(round(unname(quantile(s[[i]], probs = seq(0, 1, 0.1))), digits = 2))

  plot(s[[i]]
       , main = crops$NameUK[i]
       , maxpixels = 10^6
       , breaks = qs
       , col = cividis(length(qs) - 1)
       , family = 'serif'
       )
}

dev.off()
dev.off()

# END