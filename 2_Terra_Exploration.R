# this project is intended to analyze land use change in montana
# and eventually develop a model of land use variables

# used libraries
library(tidyverse)
library(terra) 
library(sf)
library(tidyterra)

# read in the raster files
ncld24_rast <- terra::rast("I:\\COMMON\\Data\\DataSets\\Land Use & Water\\LandChange\\RasterTrade\\ExportsForKurt\\NLCD2024MTclip\\NLCD2024MTclip.tif")
chg_rast <- terra::rast("I:\\COMMON\\Data\\DataSets\\Land Use & Water\\LandChange\\RasterTrade\\ExportsForKurt\\NCLD14_24DevChange.tif.vat\\NCLD14_24DevChange.tif")

# read in shapefiles
counties <- read_sf("I:\\COMMON\\_ANALYSTS\\KS\\GIS\\Political\\MontanaCounties_shp\\County.shp")
plot(st_geometry(counties))

# get coordinate reference systems
chg_rast.crs <- crs(chg_rast)
counties.crs <- st_crs(counties)

# set Dev24 to active category in this raster
activeCat(chg_rast)<- "Dev24" 

# reproject to the counties crs
dev24 <- project(chg_rast, counties.crs$wkt)
ncld24_rast <- project(ncld24_rast, counties.crs$wkt)

# crop to gallatin extent
dev24.gal <- crop(dev24, filter(counties, NAME == "GALLATIN"))
ncld24.gal <- crop(ncld24_rast, filter(counties, NAME == "GALLATIN"))

# mask non-gallatin parcels
dev24.gal <- mask(dev24.gal, filter(counties, NAME == "GALLATIN"))
ncld24.gal <- mask(ncld24.gal, filter(counties, NAME == "GALLATIN"))

# plot the masked dataset
plot(dev24.gal)
plot(ncld24.gal)

# we can transform the change raster into a data.frame
chg_rast.df.gal <- as.data.frame(dev24.gal, xy = T)

# then we can plot with ggplot
ggplot() +
  geom_tile(data = chg_rast.df.gal %>% filter(Dev24 != "Same"), 
              aes(x = x, y = y, fill = Dev24)
  )


# nice plots ===================

# plot of gallatin NCLD24 land cover
ggplot() +
  geom_spatraster(data = ncld24.gal)

