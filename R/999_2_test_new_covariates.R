# Try new covariates for potatoes, 2019

library(raster)
library(caret)
library(ranger)
library(dplyr)
library(tidyr)
library(plotROC)
library(ggplot2)

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

source(paste0(wd, '/sp_holdout.R'))


# Load target crops

crops <- read.csv(paste0(dir, '/Excel/Target_crops_2.csv')
                  , header = TRUE
                  , sep = ';'
                  , stringsAsFactors = FALSE
)
crops$UK2 <- gsub("[[:space:]]", "", crops$NameUK)



years <- 2011:2019

year <- years[[9]]


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

cov_field <- loadandstack(paste0(dir, '/COVARIATES/Y_', year))

# in the final script I will have to check for NAs for each year


# Training data

j <- 1

crop <- crops$Newcode[[j]]

pres <- readRDS(paste0(dir, '/crop_cells/cells_', crop, '.rds'))
pres <- pres[[9]]
pres <- pres[sample(nrow(pres)), ]
pres <- dplyr::sample_n(pres, 15000)

abs <- readRDS(paste0(dir, '/crop_cells/cells_rand.rds'))
abs <- abs[[9]]
abs <- abs[abs$cellvalue != crop, ]


# Extract cell values

pres_extr_perm <- cov_permanent[pres$cellindex]
pres_extr_cell <- cov_cell[pres$cellindex]
pres_extr_field <- cov_field[pres$cellindex]
pres_extr_cell_t <- tibble(cbind(pres_extr_perm, pres_extr_cell))
pres_extr_field_t <- tibble(cbind(pres_extr_perm, pres_extr_field))
pres_extr_all <- cbind(pres_extr_perm, pres_extr_cell, pres_extr_field)

abs_extr_perm <- cov_permanent[abs$cellindex]
abs_extr_cell <- cov_cell[abs$cellindex]
abs_extr_field <- cov_field[abs$cellindex]
abs_extr_cell_t <- tibble(cbind(abs_extr_perm, abs_extr_cell))
abs_extr_field_t <- tibble(cbind(abs_extr_perm, abs_extr_field))
abs_extr_all <- cbind(abs_extr_perm, abs_extr_cell, abs_extr_field)


# Remove NAs

pres_true <- apply(pres_extr_all, 1, FUN = function(x)
{
  out <- sum(is.na(x)) == 0
  return(out)
})

abs_true <- apply(abs_extr_all, 1, FUN = function(x)
{
  out <- sum(is.na(x)) == 0
  return(out)
})

pres_extr_cell_t <- pres_extr_cell_t[pres_true, ]
pres_extr_field_t <- pres_extr_cell_t[pres_true, ]

abs_extr_cell_t <- abs_extr_cell_t[abs_true, ]
abs_extr_field_t <- abs_extr_cell_t[abs_true, ]

pres_ind <- pres$cellindex[pres_true]
abs_ind <- abs$cellindex[abs_true]

# /Remove NAs


# What is this?:

pres_extr_cell_t <- pres_extr_cell
pres_extr_field_t <- pres_extr_field
abs_extr_cell_t <- abs_extr_cell
abs_extr_field_t <- abs_extr_field


# /What is this?


# Downsample to n for both pres and abs

set.seed(1)

n <- 10^4

if(n > length(pres_ind))
{
  n <- length(pres_ind)
  sel_pres <- 1:length(pres_ind)
} else {
  sel_pres <- sample(length(pres_ind), n)
}
sel_abs <- sample(length(abs_ind), n)

pres_extr_cell_t <- pres_extr_cell_t[sel_pres, ]
pres_extr_field_t <- pres_extr_field_t[sel_pres, ]

abs_extr_cell_t <- abs_extr_cell_t[sel_abs, ]
abs_extr_field_t <- abs_extr_field_t[sel_abs, ]

pres_ind <- pres$cellindex[sel_pres]
abs_ind <- abs$cellindex[sel_abs]
# /downsample


# Training indices

ind_all <- c(pres_ind, abs_ind)

xy <- xyFromCell(cov_permanent[[1]], ind_all)
  
tr_ind <- sp_holdout(xy = xy, n = 100, mindist = 1000, repeats = 30)

plot(xy[tr_ind$holdout[[1]], ], asp = 1)
  
points(xy[tr_ind$train[[1]], ], col = 'red')
# /training_indices


# Collate training data

pres_extr_cell_t <- tibble::as_tibble(pres_extr_cell_t)
pres_extr_field_t <- tibble::as_tibble(pres_extr_field_t)

abs_extr_cell_t <- tibble::as_tibble(abs_extr_cell_t)
abs_extr_field_t <- tibble::as_tibble(abs_extr_field_t)

pres_extr_cell_t$target <- 1
pres_extr_field_t$target <- 1

abs_extr_cell_t$target <- 2
abs_extr_field_t$target <- 2

data_cell <- rbind(pres_extr_cell_t, abs_extr_cell_t)
data_field <- rbind(pres_extr_field_t, abs_extr_field_t)

data_cell$target <- as.factor(data_cell$target)
data_field$target <- as.factor(data_field$target)

levels(data_cell$target) <- make.names(levels(data_cell$target))
levels(data_field$target) <- make.names(levels(data_field$target))


# Run this before predictions:

# cov_cell_p <- stack(cov_permanent, cov_cell)
# cov_field_p <- stack(cov_permanent, cov_field)


# Training formulae

fm_cell <- as.formula(paste('target ~',
                            paste(names(cov_cell), collapse = ' + ') # change name of cov
))

fm_field <- as.formula(paste('target ~',
                            paste(names(cov_field), collapse = ' + ') # change name of cov
))


# Models

set.seed(1)

rf_cell <- train(fm_cell
                 , data_cell
                 , method = "ranger"
                 , trControl = trainControl(method = 'cv',
                                            summaryFunction = twoClassSummary, 
                                            classProbs = TRUE,
                                            savePredictions = TRUE,
                                            index = tr_ind$train,
                                            indexOut = tr_ind$holdout
                 )
                 , importance = 'impurity'
                 , keep.inbag = TRUE
                 , num.trees = 100)

set.seed(1)

rf_field <- train(fm_field
                  , data_field
                  , method = "ranger"
                  , trControl = trainControl(method = 'cv',
                                             summaryFunction = twoClassSummary, 
                                             classProbs = TRUE,
                                             savePredictions = TRUE,
                                             index = tr_ind$train,
                                             indexOut = tr_ind$holdout
                  )
                  , importance = 'impurity'
                  , keep.inbag = TRUE
                  , num.trees = 100)

# Analyze models

rf_cell
rf_field

varImp(rf_cell)
varImp(rf_field)

selectedIndices <- rf_field$pred$mtry == 85 & rf_field$pred$splitrule == "gini"

ggplot(rf_field$pred[selectedIndices, ], 
       aes(m = X2, d = obs)) + 
  geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()

pred_field <- rf_field$pred[selectedIndices, ]

confusionMatrix(pred_field$pred, pred_field$obs)

selectedIndices2 <- rf_cell$pred$mtry == 50 & rf_field$pred$splitrule == "extratrees"

pred_cell <- rf_cell$pred[selectedIndices2, ]

confusionMatrix(pred_cell$pred, pred_cell$obs)

# Predict with best model

beginCluster(12)

predict.prune <- clusterR(cov_field,
                          predict,
                          args = list(model = rf_field,
                                      na.rm = TRUE,
                                      type  = 'prob'
                          ),
                          filename = paste0(dir, '/results/potatoes_field_2019.tif'),
                          overwrite = TRUE
)

endCluster()


# Test default values


rf_field <- train(fm_field
                  , data_field
                  , method = "ranger"
                  , trControl = trainControl(method = 'cv',
                                             summaryFunction = twoClassSummary, 
                                             classProbs = TRUE,
                                             savePredictions = TRUE,
                                             index = tr_ind$train,
                                             indexOut = tr_ind$holdout
                  )
                  , importance = 'impurity'
                  , keep.inbag = TRUE
                  , num.trees = 100)


# END