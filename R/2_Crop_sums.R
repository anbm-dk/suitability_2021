# Crop sums

library(raster)

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

IMK <- loadandstack('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/IMK_cleaned')

crops <- read.csv('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/Target_crops_2.csv'
                  , header = TRUE
                  , sep = ';'
                  )

crops$UK2 <- gsub("[[:space:]]", "", crops$NameUK)

crop_sums <- list()

beginCluster(2)

for(i in 1:nrow(crops))
{
  crop_sums[[i]] <- clusterR(IMK, calc, args = list(fun = function(x)
  {
    if (is.na(sum(x))) {
      out <- NA
    } else {
      out <- sum(x == crops$AfgKode[i], na.rm = TRUE)
      return(out)
    }
  })
  , filename = paste0('C:/Users/au542768/Dropbox/AU/Papers_in_progress/'
                      , 'Crop_suitability/Crop_sums/'
                      , 'sum_', crops$AfgKode[i], '_', crops$UK2[i], '.tif'
                      )
  , overwrite = TRUE
  , datatype  = 'INT2U'
  , export    = c('crops', 'i')
  )
}

endCluster()


# Combine four crops: Pumpkin, Plum, Sour cherry, Sweet cherry.

combinations <- read.csv('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/Combined_crops.csv'
                  , header = TRUE
                  , sep = ';'
)

combinations$UK2 <- gsub("[[:space:]]", "", combinations$UK)
combinations$Crop2 <- gsub("[[:space:]]", "", combinations$Crop)

newcodes <- unique(combinations$Newcode)

for (i in 1:length(newcodes)) # change this
{
  rs <- list()
  codes <- combinations$AfgKode[combinations$Newcode == newcodes[i]]
  names <- combinations$UK2[combinations$Newcode == newcodes[i]]
  for (j in 1:sum(combinations$Newcode == newcodes[i]))
  {
    rs[[j]] <- raster(paste0('C:/Users/au542768/Dropbox/AU/Papers_in_progress/'
                             , 'Crop_suitability/Crop_sums/Combined/'
                             , 'sum_', codes[j], '_', names[j], '.tif'
    ))
  }
  rs <- stack(rs)
  newname <- combinations$Crop2[combinations$Newcode == newcodes[i]][1]
  calc(rs, fun = function(x)
  {
    if (is.na(sum(x))) {
      out <- NA
    } else {
      out <- sum(x, na.rm = TRUE)
      return(out)
    }
  }
  , filename = paste0('C:/Users/au542768/Dropbox/AU/Papers_in_progress/'
                      , 'Crop_suitability/Crop_sums/'
                      , 'sum_', newcodes[i], '_', newname, '.tif'
  )
  , overwrite = TRUE
  , datatype  = 'INT2U'
  , export    = c('crops', 'i')
  )
}
rm(i, j, rs, codes, names, newname)


# Categories

categories <- read.csv('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/Categories.csv'
                       , header = TRUE
                       , sep = ';'
                       , stringsAsFactors = FALSE
)

catcodes <- unique(categories$Catcode)
catnames <- unique(categories$Catname)

beginCluster(2)

for(i in 1:length(catcodes))
{
  catcrops <- categories$Kode[categories$Catcode == catcodes[i]]
  clusterR(IMK, calc, args = list(fun = function(x)
  {
    if (is.na(sum(x))) {
      out <- NA
    } else {
      out <- sum(x %in% catcrops, na.rm = TRUE)
      return(out)
    }
  })
  , filename = paste0('C:/Users/au542768/Dropbox/AU/Papers_in_progress/'
                      , 'Crop_suitability/Crop_sums/Categories/'
                      , 'sum_', catcodes[i], '_', catnames[i], '.tif'
  )
  , overwrite = TRUE
  , datatype  = 'INT2U'
  , export    = c('catcrops')
  )
}

endCluster()


# Alternative categories

IMK3 <- loadandstack('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/IMK3_newcodes/')

alt_cats <- read.csv('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/Alt_cats.csv'
                       , header = TRUE
                       , sep = ';'
                       , stringsAsFactors = FALSE
)

catcodes <- unique(alt_cats$alt_cat_code)
catnames <- unique(alt_cats$Alt_cat)

beginCluster(2)

for(i in 1:length(catcodes))
{
  catcrops <- alt_cats$Kode[alt_cats$alt_cat_code == catcodes[i]]
  clusterR(IMK3, calc, args = list(fun = function(x)
  {
    if (is.na(sum(x))) {
      out <- NA
    } else {
      out <- sum(x %in% catcrops, na.rm = TRUE)
      return(out)
    }
  })
  , filename = paste0('C:/Users/au542768/Dropbox/AU/Papers_in_progress/'
                      , 'Crop_suitability/Crop_sums/Categories/'
                      , 'sum_', catcodes[i], '_', catnames[i], '.tif'
  )
  , overwrite = TRUE
  , datatype  = 'INT2U'
  , export    = c('catcrops')
  )
}

endCluster()


# END