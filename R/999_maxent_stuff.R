# Maxent extra stuff

library(rJava)
options(java.parameters = "-Xmx50g")
library(dismo)
library(dplyr)
library(tidyr)
library(snow)
library(foreach)
library(doSNOW)
library(raster)
library(pROC)
library(ENMeval)

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


# Test variable importance for potatoes

i <- 1
j <- 1

crop <- crops$Newcode[i]

d <- readRDS(file = paste0(dir, '/tdata/tdata_', crop, '.rds'))

ind <- readRDS(file = paste0(dir, '/tsplits/tsplits_', crop, '.rds'))

pres <- as.numeric(d$field$target)
pres[pres == 2] <- 0

n_cells <- nrow(d$field[ind$train[[j]], -ncol(d$field)])
n_fields <- max(crops[i, 4:(length(years) + 3)])

d_j <- d$field[, -ncol(d$field)]
h_j <- d$field[, -ncol(d$field)]

model <- maxent(x = d_j
                , p = pres)

vi <- var.importance(model)

vars <- as.character(vi$variable[vi$percent.contribution > 0])

vars <- names(cov_field[[9]])[names(cov_field[[9]]) %in% vars]

d_j2 <- d_j[, colnames(d_j) %in% vars]

model2 <- model <- maxent(x = d_j2
                          , p = pres)

covs2 <- subset(cov_field[[9]], vars)

response(model, var = 'BIO14_P_driest_M')

# Write covariate importance

vi_cell_df <- bind_cols(vi_cell)
vi_field_df <- bind_cols(vi_field)

colnames(vi_cell_df) <- crops$UK2
colnames(vi_field_df) <- crops$UK2

rownames(vi_cell_df) <- rownames(vi_cell[[1]])
rownames(vi_field_df) <- rownames(vi_field[[1]])

write.table(vi_cell_df, sep = ';', file = paste0(dir, '/Excel/vi_cell_me_v2.csv'))
write.table(vi_field_df, sep = ';', file = paste0(dir, '/Excel/vi_field_me_v2.csv'))


# Run predictions

# beginCluster(12)
# 
# preds <- clusterR(cov_field[[9]]
#                   , raster::predict
#                   , args = list(model = model)
#                   , filename = paste0(dir, '/results_old/maxent_potatoes_2.tif'))
# 
# endCluster()

beginCluster(12)

preds <- clusterR(covs2
                  , raster::predict
                  , args = list(model = model2)
                  , filename = paste0(dir, '/results_old/maxent_potatoes_3.tif'))

endCluster()

# END