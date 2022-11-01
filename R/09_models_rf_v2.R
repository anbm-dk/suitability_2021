# Train models for each crop

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
names(cov_cell)[names(cov_cell) == 'layer'] <- 'dirinsola'

cov_field <- list()

for(i in 1:length(years))
{
  cov_field[[i]] <- loadandstack(paste0(dir, '/COVARIATES/Y_', years[[i]]))
  cov_field[[i]] <- stack(cov_permanent, cov_field[[i]])
}
rm(cov_permanent, decimals, i)


# Training formulae

fm_cell <- as.formula(paste('target ~',
                            paste(names(cov_cell), collapse = ' + ')
))

fm_field <- as.formula(paste('target ~',
                             paste(names(cov_field[[1]]), collapse = ' + ')
))


# mtry values

mtry_cell  <- floor(sqrt(nlayers(cov_cell)))
mtry_field <- floor(sqrt(nlayers(cov_field[[1]])))


# Number of trees

ntrees <- 1000


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

get_FN <- function(model, decimals = 3)
{
  FN <- mean(model$pred$obs == 'X1' & model$pred$pred == 'X2', na.rm = TRUE)
  TN <- mean(model$pred$obs == 'X1', na.rm = TRUE)
  out <- round(FN/TN, digits = decimals)
  return(out)
}


# Matrices for metrics

metrics_cell <- matrix(numeric(), nrow = nrow(crops), ncol = 3
                       , dimnames = list(crops$NameUK, c('ACC', 'ROC', 'FN')))
metrics_field <- metrics_cell


# Lists for covariate importance

vi_cell <- list()
vi_field <- list()


# Directory for models

dir.create(paste0(dir, '/models_rf/'), showWarnings = FALSE)


# Run models for all crops

for(i in 1:length(crops$Newcode))
{
  # Load training data and indices

  crop <- crops$Newcode[i]
  
  d <- readRDS(file = paste0(dir, '/tdata/tdata_', crop, '.rds'))
  
  ind <- readRDS(file = paste0(dir, '/tsplits/tsplits_', crop, '.rds'))
  
  d_cell <- d$cell[, names(cov_cell)]
  d_cell$target <- d$cell$target
  
  d_field <- d$field[, names(cov_field[[9]])]
  d_field$target <- d$field$target
  
  
  # Minimum node size

  min_size <- floor(0.1*nrow(d$index_year))
  
  if(min_size < 1)
  {
    min_size <- 1
  }
  
  
  # tuning grids (just one row each)
  
  tg_cell <- data.frame(mtry = mtry_cell
                        , splitrule = 'gini'
                        , min.node.size = min_size
  )
  
  tg_field <- data.frame(mtry = mtry_field
                         , splitrule = 'gini'
                         , min.node.size = min_size
  )
  
  
  # Train control object
  
  tc <- trainControl(method = 'cv',
                     summaryFunction = twoClassSummary, 
                     classProbs      = TRUE,
                     savePredictions = 'final',
                     index           = ind$train,
                     indexOut        = ind$holdout
                     )
  
  
  # Models
  
  set.seed(1)
  
  rf_cell <- caret::train(fm_cell
                          , d_cell
                          , method     = 'ranger'
                          , tuneGrid   = tg_cell
                          , trControl  = tc
                          , importance = 'impurity'
                          , keep.inbag = TRUE
                          , num.trees  = ntrees
                          )
  
  set.seed(1)
  
  rf_field <- caret::train(fm_field
                           , d_field
                           , method     = 'ranger'
                           , tuneGrid   = tg_field
                           , trControl  = tc
                           , importance = 'impurity'
                           , keep.inbag = TRUE
                           , num.trees  = ntrees
                           )
  
  saveRDS(rf_cell, paste0(dir, '/models_rf/model_', crop, '_cell.rds'))
  saveRDS(rf_field, paste0(dir, '/models_rf/model_', crop, '_field.rds'))
  
  vi_cell[[i]] <- round(varImp(rf_cell)$importance, digits = 1)
  vi_field[[i]] <- round(varImp(rf_field)$importance, digits = 1)
  
  
  # Get metrics
  
  metrics_cell[i, 1] <- get_acc(rf_cell)
  metrics_cell[i, 2] <- get_ROC(rf_cell)
  metrics_cell[i, 3] <- get_FN(rf_cell)
  
  metrics_field[i, 1] <- get_acc(rf_field)
  metrics_field[i, 2] <- get_ROC(rf_field)
  metrics_field[i, 3] <- get_FN(rf_field)
}

write.table(metrics_cell, sep = ';', file = paste0(dir, '/Excel/metrics_cell_rf.csv'))
write.table(metrics_field, sep = ';', file = paste0(dir, '/Excel/metrics_field_rf.csv'))

vi_cell_df <- bind_cols(vi_cell)
vi_field_df <- bind_cols(vi_field)

colnames(vi_cell_df) <- crops$UK2
colnames(vi_field_df) <- crops$UK2

rownames(vi_cell_df) <- rownames(vi_cell[[1]])
rownames(vi_field_df) <- rownames(vi_field[[1]])

write.table(vi_cell_df, sep = ';', file = paste0(dir, '/Excel/vi_cell_rf.csv'))
write.table(vi_field_df, sep = ';', file = paste0(dir, '/Excel/vi_field_rf.csv'))


# END
