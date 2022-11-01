# Rose hip variables from Bioclim 2, points from Invasive Species Compendium: https://www.cabi.org/

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

rs <- loadandstack('C:/Users/au542768/Desktop/wc2.0_10m_bio/')

plot(rs[[1]])

pts <- read.csv2('C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/rosa_rogusa_pts.csv', stringsAsFactors = FALSE)

pts[, 1] <- as.numeric(pts[, 1])
pts[, 2] <- as.numeric(pts[, 2])

coordinates(pts) <- ~Longitude+Latitude

crs(pts) <- crs(rs)

extr <- extract(rs, pts)

extr

write.table(extr, 'C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/Excel/rosa_rogusa_extr.csv')
