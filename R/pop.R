#-----------------------------------------------------------------------------#
#           Clean GEOSTAT gridded population data and create rasters          #
#-----------------------------------------------------------------------------#

pathroot <- ""

# 1. Clean  ----

## GEOSTAT 2006 ----

# Read polygon grid geometries
pop06_geom <- 
  read_sf(paste0(pathroot, "data/population/2006/Grid_ETRS89_LAEA_1K_ref_GEOSTAT_2006.shp"),
          quiet = TRUE) %>%
  rename(GRD_ID = GRD_INSPIR)
# Read population counts
pop06_counts <- 
  read_delim(paste0(pathroot, "data/population/2006/GEOSTAT_grid_EU_POP_2006_1K_V1_1_1.csv"), 
             delim=";") %>%
  dplyr::select(GRD_ID, POP_TOT) %>%
  rename(POP06 = POP_TOT)
# Merge the two sources of information
pop06 <- left_join(pop06_geom, pop06_counts, by = "GRD_ID")
rm("pop06_geom", "pop06_counts")
# To centroids and WGS84
pop06 <- st_centroid(pop06)
pop06 <- st_transform(pop06, crs = 4326)


## GEOSTAT 2011 ----

# Read polygon grid geometries
pop11_geom <- 
  read_sf(paste0(pathroot,
                 "data/population/2011/GEOSTATReferenceGrid/Grid_ETRS89_LAEA_1K-ref_GEOSTAT_POP_2011_V2_0_1.shp"),
          quiet = TRUE) 
# Read population counts, there may be duplicated if a cell is part of several countries
pop11_counts <- read_csv(paste0(pathroot, 
                                "data/population/2011/GEOSTAT_grid_POP_1K_2011_V2_0_1.csv"))  %>%
  dplyr::select(GRD_ID, TOT_P) %>%
  rename(POP11 = TOT_P) 
# Read modelled population counts
pop11_modcounts <- read_csv(paste0(pathroot, 
                                   "data/population/2011/JRC-GHSL_AIT-grid-POP_1K_2011.csv"))  %>%
  dplyr::select(GRD_ID, TOT_P) %>%
  rename(POP11 = TOT_P) %>%
  filter(!GRD_ID %in% pop11_counts$GRD_ID) # Remove if available in the non-modelled dataset
pop11_counts <- bind_rows(pop11_counts, pop11_modcounts)
# Merge the two sources of information
pop11 <- left_join(pop11_geom, pop11_counts, by = "GRD_ID")
rm("pop11_geom", "pop11_counts")
# To centroids and WGS84
pop11 <- st_centroid(pop11)
pop11 <- st_transform(pop11, crs = 4326)


## GEOSTAT 2018 ----

# Read polygon grid geometries with population counts already included
pop18 <- read_sf(paste0(pathroot, "data/population/2018/JRC_POPULATION_2018.shp")) %>%
  dplyr::select(GRD_ID, TOT_P_2018) %>%
  rename(POP18 = TOT_P_2018)
# To centroids and WGS84
pop18 <- st_centroid(pop18)
pop18 <- st_transform(pop18, crs = 4326)


# 2. Rasterize ----
gridgeom <- raster(paste0(pathroot, "SILAM/europe_2003-sfc-daymean.nc4"), 
                   var = "cnc_PM_FRP_m_17")

## GEOSTAT 2006 ----
pop06_raster <- rasterize(pop06, gridgeom, pop06$POP06, fun=sum, background=0)
writeRaster(pop06_raster, paste0(pathroot, "data/processed/pop06.tif"))

## GEOSTAT 2011 ----
pop11_raster <- rasterize(pop11, gridgeom, "POP11", fun=sum, background=0)
writeRaster(pop11_raster, paste0(pathroot, "data/processed/pop11.tif"))

## GEOSTAT 2018 ----
pop18_raster <- rasterize(pop18, gridgeom, "POP18", fun=sum, background=0)
writeRaster(pop18_raster, paste0(pathroot, "data/processed/pop18.tif"))

## GEOSTAT 2006 imputed: Add 2011 counts where no data is available ----
pop06imp_raster <- pop06_raster
which0 <- which(getValues(pop06imp_raster) == 0)
pop06imp_raster[which0] <- pop11_raster[which0]
writeRaster(pop06imp_raster, paste0(pathroot, "data/processed/pop06_imp.tif"))
rm(list=ls())
