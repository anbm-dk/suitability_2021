# Correlation between ML and ECOCROP

library(raster)
library(tibble)
library(dplyr)
library(magrittr)

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

# Calculate correlation

# cor_m <- matrix(numeric(), ncol = 3, nrow = nrow(conv_df), dimnames = list(conv_df$IMK, c('RF_ME', 'RF_ECO', 'ME_ECO')))
# 
# for(i in 1:nrow(conv_df))
# {
#   
#   crop <- conv_df$IMK[i]
#   cropname <- conv_df$IMK_UK2[i]
#   ECOCROP <- conv_df$ECOCROP2[i]
#   ECOCROP_N <- conv_df$ECOCROP_NAME2[i]
#   
#   r1 <- raster(paste0(dir, '/results_rf_cell/pred_', crop, '_', cropname
#                       , '_cell.tif'))
#   
#   r2 <- raster(paste0(dir, '/results_me_cell_v1/pred_', crop, '_', cropname
#                       , '_cell.tif'))
#   
#   r3 <- raster(paste0(dir, '/ECOCROP_combined/ECOCROP_', ECOCROP, '_', ECOCROP_N
#                       , '.tif'))
#   
#   s <- stack(r1, r2, r3)
#   rcor <- layerStats(s
#                      , 'pearson'
#                      , na.rm = TRUE
#   )$'pearson correlation coefficient'
#   
#   cor_m[i, 1] <- rcor[1, 2]
#   cor_m[i, 2] <- rcor[1, 3]
#   cor_m[i, 3] <- rcor[2, 3]
# }
# 
# write.table(cor_m, file = paste0(dir, '/Excel/rastercorrelation.csv')
#             , sep = ';')


# Rank correlation

rcor <- function(a, b, c)
{
  rs <- stack(a, b, c) %>% 
    getValues() %>%
    cor(use = "complete.obs"
        , method = "spearman")

  out <- c(rs[1, 2], rs[1, 3], rs[2, 3])
  return(out)
}

cor_rank <- matrix(numeric(), ncol = 3, nrow = nrow(conv_df), dimnames = list(conv_df$IMK, c('RF_ME', 'RF_ECO', 'ME_ECO')))

for(i in 1:nrow(conv_df))
{
  crop <- conv_df$IMK[i]
  cropname <- conv_df$IMK_UK2[i]
  ECOCROP <- conv_df$ECOCROP2[i]
  ECOCROP_N <- conv_df$ECOCROP_NAME2[i]
  
  r1 <- raster(paste0(dir, '/results_rf_cell/pred_', crop, '_', cropname
                      , '_cell.tif'))
  
  r2 <- raster(paste0(dir, '/results_me_cell_v1/pred_', crop, '_', cropname
                      , '_cell.tif'))
  
  r3 <- raster(paste0(dir, '/ECOCROP_combined/ECOCROP_', ECOCROP, '_', ECOCROP_N
                      , '.tif'))
  
  cor_rank[i, ] <- rcor(r1, r2, r3)
  
  print(cor_rank)
}

write.table(cor_rank
            , file = paste0(dir, '/Excel/rastercorrelation_rank.csv')
            , sep = ';')

# END