#-----------------------------------------------------------------------------#
#                 Clean NUTS data and create rasters with codes               #
#-----------------------------------------------------------------------------#

pathroot <- ""

# 1. Clean ----

## NUTS 2021 ----
# Warning: There are no areas for Bosnia and Herzegobina (BA) or Kosovo (XK)
nuts21 <- st_read(paste0(pathroot, "data/boundaries/NUTS_RG_01M_2021_3035.geojson"),
                  quiet=TRUE) %>%
  dplyr::select(NUTS_ID, LEVL_CODE) %>%
  st_transform(crs = 4326)
nuts21_0 <- dplyr::filter(nuts21, LEVL_CODE == 0) %>%
  rename(NUTS21_0 = NUTS_ID)%>%
  dplyr::select(NUTS21_0)
nuts21_1 <- dplyr::filter(nuts21, LEVL_CODE == 1) %>%
  rename(NUTS21_1 = NUTS_ID)%>%
  dplyr::select(NUTS21_1)
nuts21_2 <- dplyr::filter(nuts21, LEVL_CODE == 2) %>%
  rename(NUTS21_2 = NUTS_ID) %>%
  dplyr::select(NUTS21_2)
rm("nuts21")

# 2. Merge ----

# Exposure boundaries
expo <- raster(paste0(pathroot, "SILAM/europe_2003-sfc-daymean.nc4"), 
               var = "cnc_PM_FRP_m_17")
expo_polys <- st_as_sf(rasterToPolygons(expo)) %>%
  mutate(GRD_ID = 1:n()) %>%
  dplyr::select(GRD_ID)
expo_points <- st_centroid(expo_polys)

# Extract NUTS codes at the exposure grid geometries by largest overlap
add_nuts <- function(expolys, nutspolys){
  # st_join(largest = T) is not efficient, we can substantially speed it up by 
  # applying it only when necessary (more than one intersection)
  expolys_nuts <- st_join(expolys, nutspolys)
  expolys_1 <- expolys_nuts[!expolys_nuts$GRD_ID %in% expolys_nuts$GRD_ID[duplicated(expolys_nuts$GRD_ID)],]
  expolys_2 <- expolys[expolys$GRD_ID %in% expolys_nuts$GRD_ID[duplicated(expolys_nuts$GRD_ID)],]
  expolys_2 <- st_join(expolys_2, nutspolys, largest = T)
  expolys_nuts <- rbind(expolys_1, expolys_2) %>%
    arrange(GRD_ID)
  expolys_nuts
}

expo_points$NUTS21_0 <- add_nuts(expo_polys, nuts21_0)$NUTS21_0
expo_points$NUTS21_1 <- add_nuts(expo_polys, nuts21_1)$NUTS21_1
expo_points$NUTS21_2 <- add_nuts(expo_polys, nuts21_2)$NUTS21_2
expo_points <- expo_points[apply(st_drop_geometry(expo_points)[,2:4], 1, function(x) !all(is.na(x))),]

# Write
write_sf(expo_points, paste0(pathroot, "data/processed/expocentroids_nuts.geojson"))
# rm(list = ls())