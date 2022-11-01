# Analyze crop rasters

# Start up

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

crop_sums <- loadandstack('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Crop_sums')

freq_all <- readRDS('freq_all.rds')

prev_freqs <- readRDS('prev_freqs.rds')

prev_freqs_2 <- readRDS('prev_freqs_2.rds')


# Merge previous prequencies

prev_freqs_3 <- prev_freqs_2

for (i in 1:length(prev_freqs))
{
  prev_freqs_3[[i]] <- prev_freqs[[i]]
}
rm(i)


# Crop counts

t <- freq(crop_sums, merge = TRUE, useNA = 'no', progress = 'text')

t

t[is.na(t)] <- 0

t

write.table(t
            , file = 'C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/crop_counts.csv'
            , sep = ';'
            )


# IMK with new codes

IMK2 <- loadandstack('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/IMK2_cleaned')

combinations <- read.csv('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/Combined_crops.csv'
                         , header = TRUE
                         , sep = ';'
)

newcodes <- unique(combinations$Newcode)

IMK3 <- IMK2

# for(i in 1:nlayers(IMK3))
for(i in 1:9)
{
  for(j in 1:length(newcodes))
  {
    oldcodes <- combinations$AfgKode[combinations$Newcode == newcodes[j]]
    IMK3[[i]][IMK3[[i]] %in% oldcodes] <- newcodes[j]
  }
  
  writeRaster(IMK3[[i]]
              , filename = paste0('C:/Users/au542768/Dropbox/AU/Papers_in_progress/'
                                  , 'Crop_suitability/IMK3_newcodes/'
                                  , names(IMK2)[i], '.tif'
              )
              , overwrite = TRUE
              , datatype  = 'INT2U'
              )
}
rm(i, j, oldcodes)

IMK3 <- loadandstack('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/IMK3_newcodes/')


# Crop in previous year

crops <- read.csv('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/Target_crops_2.csv'
                       , header = TRUE
                       , sep = ';'
                       , stringsAsFactors = FALSE
)
crops$UK2 <- gsub("[[:space:]]", "", crops$NameUK)

prev_freqs <- list()

for(i in 1:nrow(crops))
{
  previous <- list()
  for(j in 1:(nlayers(IMK3) - 1))
  {
    maskraster <- calc(IMK3[[j + 1]], function(x)
    {
      out <- x == crops$Newcode[i]
      out[out == 0] <- NA
      return(out)
    }
    , filename  = 'C:/Users/au542768/Desktop/temp/mask.tif'
    , overwrite = TRUE
    , datatype  = 'LOG1S'
    )
    previous[[j]] <- mask(IMK3[[j]]
                          , maskraster
                          , filename = paste0('C:/Users/au542768/Desktop/temp/'
                                              , 'temp_', j, '.tif'
                          )
                          , overwrite = TRUE
                          , datatype  = 'INT2U'
                          )
  }
  previous <- stack(previous)
  prev_freqs[[i]] <- freq(previous, useNA = 'no', merge = TRUE)
  rm(previous)
  print(i)
}
rm(i, j)

# saveRDS(prev_freqs, 'prev_freqs.rds')

prev_freqs <- readRDS('prev_freqs.rds')


# Prev freqs 2

prev_freqs_2 <- list()

for(i in (1 + length(prev_freqs)):nrow(crops))
{
  previous <- list()
  for(j in 1:(nlayers(IMK3) - 1))
  {
    maskraster <- calc(IMK3[[j + 1]], function(x)
    {
      out <- x == crops$Newcode[i]
      out[out == 0] <- NA
      return(out)
    }
    , filename  = 'C:/Users/au542768/Desktop/temp/mask.tif'
    , overwrite = TRUE
    , datatype  = 'INT2U'
    )
    previous[[j]] <- mask(IMK3[[j]]
                          , maskraster
                          , filename = paste0('C:/Users/au542768/Desktop/temp/'
                                              , 'temp_', j, '.tif'
                          )
                          , overwrite = TRUE
                          , datatype  = 'INT2U'
    )
  }
  previous <- stack(previous)
  prev_freqs_2[[i]] <- freq(previous, merge = TRUE)
  rm(previous)
  print(i)
}
rm(i, j)

# saveRDS(prev_freqs_2, 'prev_freqs_2.rds')

prev_freqs_2 <- readRDS('prev_freqs_2.rds')

# /prev freqs 2


# Merge previous prequencies

prev_freqs_3 <- prev_freqs_2

for (i in 1:length(prev_freqs))
{
  prev_freqs_3[[i]] <- prev_freqs[[i]]
}
rm(i)


# Remove NA as raster value

for (i in 1:length(prev_freqs_3))
{
  prev_freqs_3[[i]] <- prev_freqs_3[[i]][is.na(prev_freqs_3[[i]]$value) == FALSE, ]
}
rm(i)


# Set NA counts to 0

prev_freqs_3 <- lapply(prev_freqs_3, function(x)
{
  out <- x
  out[is.na(out)] <- 0
  return(out)
})


# Frequencies for all crops

freq_all <- freq(IMK3, useNA = 'no', merge = TRUE)
freq_all[is.na(freq_all)] <- 0

crop_codes_all <- read.csv('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/crop_codes_all.csv'
                           , header = TRUE
                           , sep = ';'
                           , stringsAsFactors = FALSE
)

# saveRDS(freq_all, 'freq_all.rds')
freq_all <- readRDS('freq_all.rds')


# Calculate mean frequencies

maxrows <- max(unlist(lapply(prev_freqs_3, FUN = function(x) nrow(x))))

mean_prev_freq <- lapply(prev_freqs_3, function(x)
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

names(mean_prev_freq) <- crops$UK2

df <- data.frame(mean_prev_freq)

write.table(df, 'C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/mean_prev_freqs.csv', sep = ';')


# gini impurity 1

gini_difs <- list()

for(i in 1:length(prev_freqs_3))
{
  crop <- crops$Newcode[i]
  
  crop_prev <- prev_freqs_3[[i]]$value
  
  p1 <- as.numeric(freq_all[freq_all$value == crop, 3:10]/
                     apply(freq_all[, 3:10], 2, function(x) sum(x
                                                                , na.rm = TRUE)
                           )
  )
  
  p2 <- prev_freqs_3[[i]][, 2:9]/freq_all[freq_all$value %in% crop_prev, 2:9]
  
  gini1 <- p1*(1 - p1)
  
  gini2 <- p2*(1 - p2)
  
  gini_dif <- apply(gini2, 1, function(x) x - gini1)
  
  if(is.list(gini_dif))
  {
    gini_dif <- rbindlist(gini_dif)
  }
  
  gini_difs[[i]] <- t(gini_dif)
}
rm(i, crop, crop_prev, p1, p2, gini1, gini2, gini_dif)


# Mean gini differences 1

for(i in 1:length(gini_difs)) {
  gini_difs[[i]] <- cbind(prev_freqs_3[[i]]$value[1:nrow(gini_difs[[i]])]
                          , gini_difs[[i]])
  gini_difs[[i]][is.nan(gini_difs[[i]])] <- NA
  colnames(gini_difs[[i]])[1] <- 'code'
}

maxrows <- max(unlist(lapply(gini_difs, FUN = function(x) nrow(x))))

mean_gini_dif <- lapply(gini_difs, function(x)
{
  out <- data.frame(code = x[, 1]
                    , name = crop_codes_all$Afgrøde[match(x[, 1], crop_codes_all$Kode)]
                    , mean = apply(x[, 2:9], 1, function(x2) mean(x2, na.rm = TRUE)))
  out <- out[order(out$mean, decreasing = TRUE), ]
  while(nrow(out) < maxrows) {
    newrow <- data.frame(code = NA, name = NA, mean = NA)
    out <- rbind(out, newrow)
  }
  return(out)
})

names(mean_gini_dif) <- crops$UK2

df <- data.frame(mean_gini_dif)

write.table(df, 'C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/mean_gini_difs.csv', sep = ';')


# Gini impurity 2

gini_difs_2 <- list()

percent_in <- list()

N_out <- apply(freq_all[, 2:9], 2, function(x)
  {
  out <- sum(x) - x
  return(out)
})

for(i in 1:length(prev_freqs_3))
{
  crop <- crops$Newcode[i]
  
  crop_prev <- prev_freqs_3[[i]]$value
  
  p1 <- as.numeric(freq_all[freq_all$value == crop, 3:10]/
                     apply(freq_all[, 3:10], 2, function(x) sum(x
                                                                , na.rm = TRUE)
                     )
  )
  
  n_in <- freq_all[, 2:9]*0
  
  n_in[freq_all$value %in% prev_freqs_3[[i]]$value, ] <- prev_freqs_3[[i]][, 2:9]
  
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
  
  gini_difs_2[[i]] <- t(gini_dif)
  
  percent_in[[i]] <- 100*n_in/(n_in + n_out)
}
rm(i, crop, crop_prev, p1, p2, gini1, gini2, gini_dif)


# Mean gini reduction 2

for(i in 1:length(gini_difs_2)) {
  gini_difs_2[[i]] <- cbind(freq_all$value
                          , gini_difs_2[[i]])
  gini_difs_2[[i]][is.nan(gini_difs_2[[i]])] <- NA
  colnames(gini_difs_2[[i]])[1] <- 'code'
  gini_difs_2[[i]] <- cbind(gini_difs_2[[i]], percent_in[[i]])
}

maxrows <- max(unlist(lapply(gini_difs_2, FUN = function(x) nrow(x))))

mean_gini_dif_2 <- lapply(gini_difs_2, function(x)
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

write.table(df, 'C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/mean_gini_difs_2.csv', sep = ';')


# END
