# Try MaxEnt

library(rJava)
options(java.parameters = "-Xmx50g")
# library(dismo)
library(dplyr)
library(tidyr)
library(snow)
library(foreach)
library(doSNOW)
library(raster)
library(pROC)
library(ParallelLogger)


# File name for log file

logFileName <- "log.txt"


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


# Functions to cross-validate maxent

max.run.field <- function(j) {
  options(java.parameters = "-Xmx50g")
  require(dismo)
  
  # d_j <- d$field[ind$train[[j]], -ncol(d$field)]
  # h_j <- d$field[ind$holdout[[j]], -ncol(d$field)]
  d_j <- d$field[ind$train[[j]], ]
  h_j <- d$field[ind$holdout[[j]], ]
  p_j <- pres[ind$train[[j]]]
  
  model <- maxent(x = d_j
                  , p = p_j
                  )
  
  pred <- dismo::predict(object = model,
                         x = h_j,
                         progress = ""
  )
  return(pred)
}

max.run.cell <- function(j) {
  options(java.parameters = "-Xmx50g")
  require(dismo)
  
  # d_j <- d$cell[ind$train[[j]], -ncol(d$cell)]
  # h_j <- d$cell[ind$holdout[[j]], -ncol(d$cell)]
  d_j <- d$cell[ind$train[[j]], ]
  h_j <- d$cell[ind$holdout[[j]], ]
  p_j <- pres[ind$train[[j]]]
  
  model <- maxent(x = d_j
                  , p = p_j
                  )
  
  pred <- dismo::predict(object = model,
                         x = h_j,
                         progress = ""
  )
  return(pred)
}


# Functions to extract accuracies

get_acc <- function(x, y, decimals = 3)
{
  out <- round(mean(x == y, na.rm = TRUE)
               , digits = decimals)
  return(out)
}


get_FN <- function(x, y, decimals = 3)
{
  FN <- mean(x == 1 & y == 0, na.rm = TRUE)
  AP <- mean(x == 1, na.rm = TRUE)
  out <- round(FN/AP, digits = decimals)
  return(out)
}


# Matrices for accuracies

metrics_cell <- matrix(numeric(), nrow = nrow(crops), ncol = 3
                       , dimnames = list(crops$NameUK, c('ACC', 'ROC', 'FN')))
metrics_field <- metrics_cell


# Number of cores to use

ncores <- 6


for(i in 1:nrow(crops))
{
  dir.create(paste0(dir, '/models_tmp/'))
  
  crop <- crops$Newcode[i]
  
  d <- readRDS(file = paste0(dir, '/tdata/tdata_', crop, '.rds'))
  
  ind <- readRDS(file = paste0(dir, '/tsplits/tsplits_', crop, '.rds'))
  
  pres <- as.numeric(d$field$target)
  pres[pres == 2] <- 0
  
  # Remove covariates with rasters
  vars1 <- colnames(d$field)[colnames(d$field) %in% names(cov_field[[9]])]
  d$field <- d$field[, colnames(d$field) %in% vars1]
  
  vars2 <- colnames(d$cell)[colnames(d$cell) %in% names(cov_cell)]
  d$cell <- d$cell[, colnames(d$cell) %in% vars2]
   
  js <- 1:length(ind$train)

  # 1 Field-based models
  addDefaultFileLogger(logFileName)  # Link to logger
  
  # cl <- makeCluster(12, "SOCK", outfile = "")
  cl <- makeCluster(numberOfThreads = ncores, divideFfMemory = FALSE)
  registerDoSNOW(cl)
  try(
    maxent.runs1 <- foreach(x = js, .packages = c("rJava", "dplyr")) %dopar% 
      {
        max.run.field(j = x)
      })
  stopCluster(cl)
  
  unlink(logFileName) # Clean up log file from the previous example
  clearLoggers() # Clean up the loggers from the previous example
  
  registerDoSEQ()
  
  # 1.2 Combined holdout dataset
  h_all <- sapply(1:length(ind$holdout), function(x)
  {
    out <- pres[ind$holdout[[x]]]
    return(out)
  }
  )
  h_all <- as.vector(h_all)
  
  # 1.3 Calculate accuracy
  try(p1 <- unlist(maxent.runs1))
  try(pcl1 <- round(p1, digits = 0))
  
  try(metrics_field[i, 1] <- get_acc(h_all, pcl1))
  try(metrics_field[i, 2] <- round(as.numeric(roc(h_all, pcl1)$auc)
                                  , digits = 3))
  try(metrics_field[i, 3] <- get_FN(h_all, pcl1))
  
  # 2 Cell-based models
  addDefaultFileLogger(logFileName)  # Link to logger
  
  # cl <- makeCluster(12, "SOCK", outfile = "")
  cl <- makeCluster(numberOfThreads = ncores, divideFfMemory = FALSE)
  registerDoSNOW(cl)
  try(
    maxent.runs2 <- foreach(x = js, .packages = c("rJava", "dplyr")) %dopar%
      {
        max.run.cell(j = x)
      })
  stopCluster(cl)
  
  unlink(logFileName) # Clean up log file from the previous example
  clearLoggers() # Clean up the loggers from the previous example
  
  registerDoSEQ()

  # 2.3 Calculate accuracy
  try(p2 <- unlist(maxent.runs2))
  try(pcl2 <- round(p2, digits = 0))
  
  try(metrics_cell[i, 1] <- get_acc(h_all, pcl2))
  try(metrics_cell[i, 2] <- round(as.numeric(roc(h_all, pcl2)$auc)
                                      , digits = 3))
  try(metrics_cell[i, 3] <- get_FN(h_all, pcl2))
  
  print(i)
}


# Print output from log

writeLines(readChar(logFileName, file.info(logFileName)$size))

# Write accuracies

write.table(metrics_cell, sep = ';', file = paste0(dir, '/Excel/metrics_cell_me_v3.csv'))
write.table(metrics_field, sep = ';', file = paste0(dir, '/Excel/metrics_field_me_v3.csv'))


# END