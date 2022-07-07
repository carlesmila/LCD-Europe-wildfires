#-----------------------------------------------------------------------------#
#                             Process fire danger                             #
#-----------------------------------------------------------------------------#

library("tidyverse")
library("sf")
library("raster")

pathroot <- ""

# danger file, rotate
danger <- stack(paste0(pathroot,
                       "data/firedanger/regional_cellmean_annual_fire_weather_mean_EU_map.nc4"))
extent(danger)[1] <- 0
extent(danger)[2] <- 360
danger <- rotate(danger)
# plot(danger)

# Derive countries, add regions
regions <- st_read(paste0(pathroot, "data/regions/Europe_regions.shp")) %>%
  rename(country = CNTR_CODE) %>%
  dplyr::filter(country != "TR") %>% # Turkey not included in the rest of the analyses
  dplyr::select(country, region) %>%
  filter(!duplicated(.))
regions <- group_by(regions, region) %>%
  summarise() %>%
  ungroup()
# plot(regions["region"])

# crop to EU
danger_euro <- raster::mask(danger, regions)
b <- as(extent(-30, 60, 30, 75), 'SpatialPolygons')
crs(b) <- crs(danger_euro)
danger_euro <- raster::crop(danger_euro, b)
# plot(danger_euro)

# regions
cellstats <- data.frame(year = 1981:2020)
danger_regions <- raster::extract(danger_euro, 
                                  fun = 'mean', na.rm = T,
                                  regions, weights = T, normalizeWeights = T)
danger_regions <- as.data.frame(t(danger_regions), row.names = F)
names(danger_regions) <- regions$region
cellstats <- bind_cols(cellstats, danger_regions)
# write_csv(cellstats, "data/processed/fdanger_region.csv")