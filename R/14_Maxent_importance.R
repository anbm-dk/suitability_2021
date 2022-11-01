# Maxent covariate importance

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
library(stringr)
library(rlist)


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


# Extract covariate importance from models

tag <- 'cell'

impmat <- matrix(0, nrow = nlayers(cov_cell), ncol = nrow(crops), dimnames = list(names(cov_cell), crops$UK2))

cov_all <- names(cov_cell)

cov_all[cov_all == 'layer'] <- 'dirinsola'

for(i in 1:nrow(crops))
{
  crop <- crops$Newcode[i]
  cropname <- crops$UK2[i]
  
  results <- read.table(file = paste0(dir, '/models_me_v1/model_', crop, '_', tag, '/maxentResults.csv')
                        , sep = ','
                        , header = TRUE
                        , comment.char = '')
  
  
  results <- results[grep("permutation", names(results))]
  
  covnames <- str_split_fixed(names(results), "[.]", n = 2)[, 1]
  
  impmat[c(match(covnames, cov_all)), i] <- unlist(results)
}

write.table(impmat, file = paste0(dir, '/Excel/varimp_maxent.csv')
            , sep = ';')

# END