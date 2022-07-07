#-----------------------------------------------------------------------------#
#                LCD Europe Wildfire Smoke indicator: Makefile                #
#-----------------------------------------------------------------------------#

library("renv")
library("raster")
library("sf")
library("lubridate")
library("exactextractr")
library("ISOweek")
library("tidyverse")
library("data.table")

# 1. Clean GEOSTAT gridded population data and create raster at exposure geometries
source("code/pop.R")

# 2. Assign NUTS and region codes to each of the exposure geometries
source("code/nuts.R")

# 3. Extract daily exposures, population and NUTS codes (HPC)
source("code/exposure.R")

# 4. Clean EUROSTAT mortality time series
source("code/mortality.R")

# 5. Compute RRs, PAFs, attributable deaths and CIs (HPC)
source("code/HIA.R")

# 6. Additional analysis of fire weather index to complement main results
source("code/danger.R")