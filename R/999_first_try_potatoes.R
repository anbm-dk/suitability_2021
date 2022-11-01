library(raster)
library(ranger)
library(caret)
library(mlbench)
library(plotROC)
library(ggplot2)

potatoes <- raster('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Crop_sums/potatoes.tif')

loadandstack <- function(dir = NULL)
{
  rlist<-list.files(dir, pattern="tif$", full.names = TRUE)
  for(r in rlist)
  {
    name <- unlist(strsplit(r, "[.]"))[length(unlist(strsplit(r, "[.]"))) - 1]
    assign(name, raster(r))
  }
  output <- stack(rlist)
  return(output)
}

covs_1 <- loadandstack('C:/Users/au542768/GEODATA/covariates/')

covs_2 <- loadandstack('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/CROP_COVS')

covs <- stack(covs_1, covs_2)

pts <- sampleRandom(potatoes, 20000, sp = TRUE)

pts <- pts[1:10000, ]

pts

table(as.factor(pts$potatoes))

extr <- extract(covs, pts, sp = TRUE)

head(extr)

names(covs)

factors <- c("geology", "georeg", "LU", "Terron_YER_group", "Terron_YER_nat"
             , "Terron_YER_reg", "Terron_YP_class", "WWheat_yield")

for (i in 1:length(factors)) 
{
  extr@data[, colnames(extr@data) == factors[i]] <- factor(extr@data[, colnames(extr@data) == factors[i]])
}
rm(i)

extr@data$class <- factor(as.numeric(extr$potatoes == 0) + 1)

fm <- as.formula(paste('class ~',
                       paste(names(covs), collapse = ' + ')
))

extr <- extr[complete.cases(extr@data), ]

rf1 <- ranger(fm, extr@data
             , importance = 'impurity'
             )


sort(importance(rf1), decreasing = TRUE)

cbind(names(importance(rf1)), unname(importance(rf1)))

names2 <- c('elevation', 'bluespot', 'Terron_YP_8_dist', 'BIO16_P_wettest_Q', 'DMI_solar' , 'valldepth', 'sagawi', 'sandc_b', 'asp_sin', 'Terron_YP_4_dist')

fm2 <- as.formula(paste('class ~',
                     paste(names2, collapse = ' + ')
))

levels(extr$class) <- c('L1', 'L2')

rf2 <- train(fm2, extr@data, method = "ranger"
            , trControl = trainControl(method = 'cv',
                                       summaryFunction = twoClassSummary, 
                                       classProbs = TRUE,
                                       savePredictions = TRUE
                                       )
            , keep.inbag = TRUE
            , num.trees = 100)

rf2

covs_10 <- subset(covs, names2)

selectedIndices <- rf2$pred$mtry == 6 & rf2$pred$splitrule == "extratrees"

plot.roc(rf2$pred$obs[selectedIndices],
         rf2$pred$L1[selectedIndices])

ggplot(rf2$pred[selectedIndices, ], 
       aes(m = L2, d = obs)) + 
  geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()

# beginCluster(4)
# 
# predict.prune <- clusterR(covs_10,
#                           predict,
#                           args = list(model = rf2,
#                                       na.rm = TRUE,
#                                       type  = 'prob'
#                           ),
#                           filename = 'C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/results/first_try_potatoes.tif',
#                           overwrite = TRUE
# )
# endCluster()

# END