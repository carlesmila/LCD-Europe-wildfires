#-----------------------------------------------------------------------------#
#  Sensitivity analysis: Non-population weighted exposure per European region #
#-----------------------------------------------------------------------------#

pathroot <- ""

# Read exposure and region data
expofiles <- list.files(paste0(pathroot, "SILAM/"), recursive = T, full.names = T)
expofiles <- expofiles[!grepl("2021", expofiles)]
regions <- st_read(paste0(pathroot, "data/regions/Europe_regions.shp")) %>%
  group_by(region) %>%
  summarise()

# Function to extract yearly average per area
silam_yavg <- function(silamfile, bound, year){
  print(year)
  silamyear <- brick(silamfile, var = "cnc_PM_FRP_m_17")
  silamyear <- silamyear*1e+9
  silamyear <- calc(silamyear, mean, na.rm=T)
  silammean <- data.frame(region = bound$region,
                          regmean = raster::extract(silamyear, bound, fun = mean, na.rm=T),
                          year = year)
  return(silammean)
}

# Execute per file
smean <- purrr::map2_df(.x = expofiles, .y = 2002:2020, .f = silam_yavg, bound = regions)
write_csv(smean, paste0(pathroot, "data/processed/regionmean_sensitivity.csv"))

