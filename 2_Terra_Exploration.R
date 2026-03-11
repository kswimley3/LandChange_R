# this project is intended to analyze land use change in montana
# and eventually develop a model of land use variables

# used libraries
library(tidyverse)
library(terra) 
library(tidyterra)

# DATA EXPLORATION =====================

# read in the raster files
ncld24_rast <- terra::rast("I:\\COMMON\\Data\\DataSets\\Land Use & Water\\LandChange\\RasterTrade\\ExportsForKurt\\NLCD2024MTclip\\NLCD2024MTclip.tif")
chg_rast <- terra::rast("I:\\COMMON\\Data\\DataSets\\Land Use & Water\\LandChange\\RasterTrade\\ExportsForKurt\\NCLD14_24DevChange.tif.vat\\NCLD14_24DevChange.tif")

# examine the crs of each raster
crs(ncld24_rast, describe = TRUE)
crs(chg_rast, describe = TRUE)

# get original extents of each raster
ext(ncld24_rast)
ext(chg_rast)

# crop the extent to the lowest of each min, and the highest of each mac
ext_mt <- c(
  min(ext(ncld24_rast)[1], ext(chg_rast)[1]),
  min(ext(ncld24_rast)[2], ext(chg_rast)[2]),
  min(ext(ncld24_rast)[3], ext(chg_rast)[3]),
  min(ext(ncld24_rast)[4], ext(chg_rast)[4])
)

# set the extent of each raster to the ext.mt
ncld24_rast <- crop(ncld24_rast, ext(ext_mt))
chg_rast <- crop(chg_rast, ext(ext_mt))

# set Dev24 to active category in this raster
activeCat(chg_rast)<- "Dev24" 
activeCat(ncld24_rast)<- "Category" # set category to active so we can read the plot

# bind both rasters together
combined.rast <- c(ncld24_rast, chg_rast)

# remove ncld24_rast to free up some space
rm(ncld24_rast)
gc()

# save combined raster to a file
writeRaster(combined.rast, "./output/data_out/merged_nlcd24_dev24.tif")
rm("combined.rast", overwrite = TRUE) # removes file
gc()

# check the levels of each raster
levels(chg_rast)
levels(ncld24_rast)

# create a chg_rast dataframe for easier viewing
cat.chg_rast <- cats(chg_rast)[[1]]
cat.ncld24_rast <- cats(ncld24_rast)[[1]]

# can we simplify the levels for the chg_rast for easier mapping?
levels.chg.simp <- levels(chg_rast)[[1]]
levels.chg.simp <- levels.chg.simp %>% 
  mutate(Dev24 = case_when(
    Dev24 == "Developed Open" ~ 1,
    Dev24 == "Developed Low" ~ 2, 
    Dev24 == "Developed Medium" ~ 3,
    Dev24 == "Developed High" ~ 4,
    Dev24 == "Other Change" ~ 5,
    Dev24 == "Same" ~ 6,
    .default = 0
  ))
levels.chg.matrix <- as.matrix(levels.chg.simp, ncol = 2)

# classify the change raster to just 6 values
chg_rast.simp <- classify(chg_rast, levels.chg.matrix)
levels(chg_rast.simp) <- data.frame(
  ID = c(1,2,3,4,5,6),
  category = c("Developed Open", "Developed Low", "Developed Medium",
                           "Developed High", "Other Change", "Same")
)
levels(chg_rast.simp)

# save the simplified raster for fewer steps in the future
writeRaster(chg_rast, "./output/data_out/simplified_landchange_raster.tif", overwrite = TRUE)

# convert to dataframe and get information
chg_rast.simp.df <- as.data.frame(chg_rast.simp, xy = TRUE)
table(chg_rast.simp.df$category)
gc()

# PRIMARY PLOTS  ======================================
  
# change default palette
options(terra.pal=NULL)

# plot simple raster with better colors
png(filename = "./output/plots/change_plot1.png", width = 750, height =500)
terra::plot(
  chg_rast.simp,
  mar = c(3.1, 3.1, 2.1, 9.0),
  col = c("skyblue", "orange", "red", "darkblue", "lightgray", "#FFFFFF"),
  plg = list(
    legend = c("Developed Open", "Developed Low", "Developed Medium",
               "Developed High","Other Change", "Same"),
    title = "Land Change")
)
dev.off()


# read in shapefiles
counties <- vect("I:\\COMMON\\_ANALYSTS\\KS\\GIS\\Political\\MontanaCounties_shp\\County.shp")
plot(counties)

towns <- vect("I:\\COMMON\\_ANALYSTS\\KS\\GIS\\Political\\MontanaCitiesTowns_shp\\MontanaIncorporatedCitiesTowns.shp")
lakes <- vect("I:\\COMMON\\_ANALYSTS\\KS\\GIS\\Water\\NamedStreamsandLakes_SHP\\Lakes24K_NamedOnly.shp")
roads <- vect("I:\\COMMON\\_ANALYSTS\\KS\\GIS\\Roads\\ALTIS_Statewide_Routes.shp")

# get coordinate reference systems
chg_rast.crs <- crs(chg_rast)
counties.crs <- crs(counties)

# reproject to the raster crs
counties <- project(counties, chg_rast.crs)
towns <- project(towns, chg_rast.crs)
lakes <- project(lakes, chg_rast.crs)
roads <- project(roads, chg_rast.crs)

# plot change raster with county boundaries overlaid
png(filename = "./output/plots/change3.png", width = 1200, height = 800)
terra::plot(
  chg_rast.simp,
  mar = c(3.1, 3.1, 2.1, 9.0),
  col = c("lightgreen", "gold", "red", "darkred", "lightgray", "#FFFFFF"),
  plg = list(
    legend = c("Developed Open", "Developed Low", "Developed Medium",
               "Developed High","Other Change", "Same"),
    title = "Land Change")
)

lines(lakes, col = "lightblue")
lines(counties, col = "black")
lines(towns, col = "darkgrey")
dev.off()


# crop to gallatin extent
dev24.gal <- crop(dev24, filter(counties, NAME == "GALLATIN"))
ncld24.gal <- crop(ncld24_rast, filter(counties, NAME == "GALLATIN"))

# mask non-gallatin parcels
dev24.gal <- mask(dev24.gal, filter(counties, NAME == "GALLATIN"))
ncld24.gal <- mask(ncld24.gal, filter(counties, NAME == "GALLATIN"))

# get the extent of each projection
ext(dev24.gal)
ext(ncld24.gal)


# plot the masked dataset
plot(dev24.gal, colNA = "white")
plot(ncld24.gal, colNA = "white")

# combine the datasets
gal.combined <- c(dev24.gal, ncld24.gal)

# we can transform the change raster into a data.frame
chg_rast.df.gal <- as.data.frame(dev24.gal, xy = T)

# then we can plot with ggplot
ggplot() +
  geom_tile(data = chg_rast.df.gal %>% filter(Dev24 != "Same"), 
              aes(x = x, y = y, fill = Dev24)
  )



