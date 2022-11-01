# FAO texture classes

# FAO 1974:
#
#   Textural classes:
#   1.	Coarse textured: sands, loamy sands and sandy loams with less than 18
#       percent clay, and more than 65 percent sand.
#   2.	Medium textured: sandy loams, loams, sandy clay loams, silt loams, silt,
#       silty clay loams and clay loams with less than 35 percent clay and less
#       than 65 percent sand; the sand fraction may be as high as 82 percent if
#       a minimum of 18 percent clay is present.
#   3.	Fine textured : clays, silty clays, sandy clays, clay loams and silty
#       clay loams with more than 35 percent clay.
#
#   Slope classes:
#   a.	level to gently undulating : dominant slopes ranging between 0 and 8
#       percent.
#   b.	rolling to hilly: dominant slopes ranging between 8 and 30 percent.
#   c.	steeply dissected to mountainous: dominant slopes are over 30 percent.
#
# Silt/sand threshold at 50 microns in FAO system, while it is 20 microns in
# the Danish classification.
#
# FAO fine sand fraction (as percentage of DK fine sand fraction):
# FAO (%) = (log(200) - log(50))/(log(200) - log(20)) = 60.2%
# FAO total sand = FAO fine sand + coarse sand

library(raster)

clay <- raster('C:/Users/au542768/GEODATA/covariates/clay_a.tif')
sandf <- raster('C:/Users/au542768/GEODATA/covariates/sandf_a.tif')
sandc <- raster('C:/Users/au542768/GEODATA/covariates/sandc_a.tif')

sand_FAO <- sandf*(log(200) - log(50))/(log(200) - log(20)) + sandc

tex_FAO <- clay > 35
tex_FAO <- tex_FAO + 2
tex_FAO[clay < 18 & sand_FAO > 65] <- 1

# '4' is organic soils

wetlands <- raster('C:/Users/au542768/GEODATA/covariates/wetland.tif')

tex_FAO[wetlands == 3] <- 4

writeRaster(tex_FAO
            , filename = 'C:/Users/au542768/Dropbox/AU/Papers_in_progress/Crop_suitability/ECOCROP_input/FAO_tex.tif'
            , datatype = 'INT2U'
)

# END