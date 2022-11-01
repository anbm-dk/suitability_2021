# Accuracy of ECOCROP models


# Get ready

library(raster)
library(caret)
library(ranger)
library(dplyr)
library(tidyr)
library(magrittr)
library(ROCR)

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


# Load conversion table

conv_df <- read.csv2(file = paste0(dir, '/Excel/ECOCROP_IMK.csv')
                     , dec = '.')

conv_df <- conv_df[conv_df$ECOCROP != -9, ]
conv_df$IMK_UK2 <- gsub("[[:space:]]", "", conv_df$IMK_UK)
conv_df$ECOCROP_NAME2 <- gsub("[[:space:]]", "", conv_df$ECOCROP_NAME)

library(stringr)
library(withr)
with_options(
  c(scipen = 999), 
  conv_df$ECOCROP2 <- str_pad(conv_df$ECOCROP, 4, pad = "0")
)

# Define constants

years <- 2011:2019


# Formulas to extract metrics

get_acc <- function(model, decimals = 3)
{
  out <- round(mean(model$pred$pred == model$pred$obs, na.rm = TRUE)
               , digits = decimals)
  return(out)
}

get_ROC <- function(model, decimals = 3)
{
  out <- round(model$results$ROC, digits = decimals)
  return(out)
}

accmat <- matrix(numeric(), nrow = nrow(conv_df), ncol = 2, dimnames = list(NULL, c('OA', 'AUC')))

for(i in 1:nrow(conv_df))
{
  crop <- conv_df$IMK[i]
  
  d <- readRDS(file = paste0(dir, '/tdata/tdata_', crop, '.rds'))
  
  ind <- readRDS(file = paste0(dir, '/tsplits/tsplits_', crop, '.rds'))
  
  df <- data.frame(cellindex = d$index_year$cellindex[unlist(ind$holdout)]
                   , target = d$cell$target[unlist(ind$holdout)]
  )
  
  r <- paste0(dir
              , '/ECOCROP_clim/ECOCROP_'
              , conv_df$ECOCROP2[i], '_'
              , conv_df$ECOCROP_NAME2[i]
              , '.tif') %>% raster()
  
  df$pred <- r[df$cellindex]
  
  df$predf <- base::cut(df$pred, breaks = c(-1, 0.5, 2), labels = c('X2', 'X1'))
  
  df$logic <- df$target == 'X1'
  
  accmat[i, 1] <- mean(df$target == df$predf, na.rm = TRUE)
  
  AUC <- prediction(predictions = df$pred, df$logic) %>% performance(measure = 'auc')
  
  accmat[i, 2] <- AUC@y.values[[1]]
}

results <- cbind(conv_df, accmat)

results

write.table(results
            , file = paste0(dir, '/Excel/ECOCROP_acc.csv')
            , sep = ';')

plot(results$OA, results$AUC)

mean(results$OA)
mean(results$AUC)

# END