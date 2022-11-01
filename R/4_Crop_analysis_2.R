# Analyze crop rasters

library(raster)
library(data.table)

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

crop_sums <- loadandstack(paste0(dir, '/Crop_sums/'))

IMK3 <- loadandstack(paste0(dir, '/IMK3_newcodes/'))

freq_all <- readRDS(paste0(dir,'/R/freq_all.rds'))



# Check general coincidence

general_freqs <- list()

for(i in 1:nlayers(crop_sums))
{
  maskraster <- calc(crop_sums[[i]], function(x)
  {
    out <- x
    out[out == 0] <- NA
    return(out)
  }
  , filename  = paste0(dir, '/temp/mask.tif')
  , overwrite = TRUE
  , datatype  = 'INT2U'
  )
  masked <- mask(IMK3
                 , maskraster
                 , filename = paste0(dir, '/temp/temp.tif')
                 , overwrite = TRUE
                 , datatype  = 'INT2U'
  )
  general_freqs[[i]] <- freq(masked, merge = TRUE)
}

saveRDS(general_freqs, paste0(dir, '/R/general_freqs.tif'))
general_freqs <- readRDS(paste0(dir, '/R/general_freqs.tif'))


# Remove NA as raster value

for (i in 1:length(general_freqs))
{
  general_freqs[[i]] <- general_freqs[[i]][is.na(general_freqs[[i]]$value) == FALSE, ]
}
rm(i)


# Set NA counts to 0

general_freqs <- lapply(general_freqs, function(x)
{
  out <- x
  out[is.na(out)] <- 0
  return(out)
})


# Calculate mean frequencies

maxrows <- max(unlist(lapply(general_freqs, FUN = function(x) nrow(x))))

sum_general_freq <- lapply(general_freqs, function(x)
{
  out <- data.frame(code = x[, 1]
                    , name = crop_codes_all$Afgrøde[match(x$value, crop_codes_all$Kode)]
                    , mean = apply(x[, 2:9], 1, mean))
  out <- out[order(out$mean, decreasing = TRUE), ]
  while(nrow(out) < maxrows) {
    newrow <- data.frame(code = NA, name = NA, mean = NA)
    out <- rbind(out, newrow)
  }
  return(out)
})

names(sum_general_freq) <- crops$UK2

df <- data.frame(sum_general_freq)

write.table(df, paste0(dir, '/Excel/sum_general_freqs.csv'), sep = ';')


# Gini impurity 2

gini_difs <- list()

percent_in <- list()

N_out <- apply(freq_all[, 2:9], 2, function(x)
{
  out <- sum(x) - x
  return(out)
})

for(i in 1:length(general_freqs))
{
  crop <- crops$Newcode[i]
  
  crop_prev <- general_freqs[[i]]$value
  
  p1 <- as.numeric(freq_all[freq_all$value == crop, 3:10]/
                     apply(freq_all[, 3:10], 2, function(x) sum(x
                                                                , na.rm = TRUE)
                     )
  )
  
  n_in <- freq_all[, 2:9]*0
  
  n_in[freq_all$value %in% general_freqs[[i]]$value, ] <- general_freqs[[i]][, 2:9]
  
  p2 <- n_in/freq_all[, 2:9]
  
  n_out <- apply(n_in, 2, function(x)
  {
    out <- sum(x) - x
    return(out)
  })
  
  p3 <- n_out/N_out
  
  gini1 <- p1*(1 - p1)
  
  gini2 <- (p2*(1 - p2)*freq_all[, 2:9] + (p3*(1 - p3)*N_out))/
    (freq_all[, 2:9] + N_out)
  
  gini_dif <- apply(gini2, 1, function(x) gini1 - x)
  
  if(is.list(gini_dif))
  {
    gini_dif <- rbindlist(gini_dif)
  }
  
  gini_difs[[i]] <- t(gini_dif)
  
  percent_in[[i]] <- 100*n_in/(n_in + n_out)
}
rm(i, crop, crop_prev, p1, p2, gini1, gini2, gini_dif)


# Mean gini reduction 2

for(i in 1:length(gini_difs)) {
  gini_difs[[i]] <- cbind(freq_all$value
                            , gini_difs[[i]])
  gini_difs[[i]][is.nan(gini_difs[[i]])] <- NA
  colnames(gini_difs[[i]])[1] <- 'code'
  gini_difs[[i]] <- cbind(gini_difs[[i]], percent_in[[i]])
}

maxrows <- max(unlist(lapply(gini_difs, FUN = function(x) nrow(x))))

mean_gini_dif_2 <- lapply(gini_difs, function(x)
{
  out <- data.frame(code = x[, 1]
                    , name = crop_codes_all$Afgrøde[match(x[, 1], crop_codes_all$Kode)]
                    , mean = apply(x[, 2:9], 1, function(x2) mean(x2, na.rm = TRUE))
                    , perc = apply(x[, 10:ncol(x)], 1, function(x2) mean(x2, na.rm = TRUE)))
  out <- out[order(out$mean, decreasing = TRUE), ]
  while(nrow(out) < maxrows) {
    newrow <- data.frame(code = NA, name = NA, mean = NA)
    out <- rbind(out, newrow)
  }
  return(out)
})

names(mean_gini_dif_2) <- crops$UK2

df <- data.frame(mean_gini_dif_2)

write.table(df, paste0(dir, '/Excel/general_mean_gini_difs.csv'), sep = ';')



# END