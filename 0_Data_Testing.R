# this project is intended to analyze land use change in montana
# and eventually develop a model of land use variables

# used libraries
library(tidyverse)
library(raster) # looks like this is the one we want to use to read in multiple bands
library(sf)

ncld24_rast <- raster::raster("I:\\COMMON\\Data\\DataSets\\Land Use & Water\\LandChange\\RasterTrade\\ExportsForKurt\\NLCD2024MTclip\\NLCD2024MTclip.tif")

chg_rast <- raster::raster(
  "I:\\COMMON\\Data\\DataSets\\Land Use & Water\\LandChange\\RasterTrade\\ExportsForKurt\\NCLD14_24DevChange.tif.vat\\NCLD14_24DevChange.tif"
)

# CODE BELOW USES MANY RESOURCES!!! MAYBE TRY TO CLIP TO A COUNTY FIRST?
# we can transform the chg_raster into a data.frame
chg_rast.df <- as.data.frame(chg_rast, xy = T)

# then we can plot with ggplot
ggplot() +
  geom_raster(data = chg_rast.df, 
              aes(x = x, y = y, alpha = Dev24)
              )
