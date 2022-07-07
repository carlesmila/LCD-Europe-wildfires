#-----------------------------------------------------------------------------#
#                            Extract daily exposures                          #
#-----------------------------------------------------------------------------#

pathroot <- ""

# 1. Function ----
extract_expo <- function(expocentr, popfile, expofile, pathout){
  
  # Read wildfire PM2.5 file, units from kg/m3 to ug/m3
  expo <- brick(expofile, var = "cnc_PM_FRP_m_17")
  expo <- expo*1e+9
  names(expo) <- gsub(".11.30.00", "", names(expo))
  
  # Read population file, stack to the rest
  pop <- raster(popfile)
  names(pop) <- "pop"
  expo <- addLayer(expo, pop)
  
  # Extract by centroid
  exposure_extr <- raster::extract(expo, expocentr, df = T) %>%
    dplyr::select(-ID)
  
  # Drop geometries and write to disk
  exposure_extr <- cbind(st_drop_geometry(expocentr), exposure_extr) 
  write_csv(exposure_extr, pathout)
}

# 2. Extract exposures ----

expo_nuts <- st_read(paste0(pathroot, "data/processed/expocentroids_nuts.geojson"))

## 2003 (pop 2006)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop06_imp.tif"),
             paste0(pathroot, "SILAM/europe_2003-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata03.csv"))

## 2004 (pop 2006)
extract_expo(expo_nuts,
             paste0(pathroot, "data/processed/pop06_imp.tif"),
             paste0(pathroot, "SILAM/europe_2004-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata04.csv"))

## 2005 (pop 2006)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop06_imp.tif"),
             paste0(pathroot, "SILAM/europe_2005-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata05.csv"))

## 2006 (pop 2006)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop06_imp.tif"),
             paste0(pathroot, "SILAM/europe_2006-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata06.csv"))

## 2007 (pop 2006)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop06_imp.tif"),
             paste0(pathroot, "SILAM/europe_2007-sfc-daymean.nc4"),
             paste0(pathroot, "data/processed/HIAexposure/HIAdata07.csv"))

## 2008 (pop 2006)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop06_imp.tif"),
             paste0(pathroot, "SILAM/europe_2008-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata08.csv"))

## 2009 (pop 2011)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop11.tif"),
             paste0(pathroot, "SILAM/europe_2009-sfc-daymean.nc4"),
             paste0(pathroot, "data/processed/HIAexposure/HIAdata09.csv"))

## 2010 (pop 2011)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop11.tif"),
             paste0(pathroot, "SILAM/europe_2010-sfc-daymean.nc4"),
             paste0(pathroot, "data/processed/HIAexposure/HIAdata10.csv"))

## 2011 (pop 2011)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop11.tif"),
             paste0(pathroot, "SILAM/europe_2011-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata11.csv"))

## 2012 (pop 2011)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop11.tif"),
             paste0(pathroot, "SILAM/europe_2012-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata12.csv"))

## 2013 (pop 2011)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop11.tif"),
             paste0(pathroot, "SILAM/europe_2013-sfc-daymean.nc4"),
             paste0(pathroot, "data/processed/HIAexposure/HIAdata13.csv"))

## 2014 (pop 2011)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop11.tif"),
             paste0(pathroot, "SILAM/europe_2014-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata14.csv"))

## 2015 (pop 2018)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop18.tif"),
             paste0(pathroot, "SILAM/europe_2015-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata15.csv"))

## 2016 (pop 2018)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop18.tif"),
             paste0(pathroot, "SILAM/europe_2016-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata16.csv"))

## 2017 (pop 2018)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop18.tif"),
             paste0(pathroot, "SILAM/europe_2017-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata17.csv"))

## 2018 (pop 2018)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop18.tif"),
             paste0(pathroot, "SILAM/europe_2018-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata18.csv"))

## 2019 (pop 2018)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop18.tif"),
             paste0(pathroot, "SILAM/europe_2019-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata19.csv"))

## 2020 (pop 2018)
extract_expo(expo_nuts, 
             paste0(pathroot, "data/processed/pop18.tif"),
             paste0(pathroot, "SILAM/europe_2020-sfc-daymean.nc4"), 
             paste0(pathroot, "data/processed/HIAexposure/HIAdata20.csv"))