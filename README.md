# Lancet Countdown - Europe: Wildfire smoke indicator

This repository contains the code to generate the wildfire smoke indicators for the Lancet Countdown Europe 2022 edition, as well as the figures included in the report and appendix. 

## Analysis

The workflow to compute the indicators is structured in a sequential set of steps that can be run from a root [makefile](makefile.R) script. Note that some of the steps were performed in a HPC cluster due to high memory demands. The steps to be run are (a more detailed description can be found in the report appendix):

* [Population data processing](R/pop.R): Clean GEOSTAT gridded population data and create raster at exposure geometries.
* [NUTS boundary data processing](R/nuts.R): Assign NUTS and region codes to each of the exposure geometries.
* [Exposure data processing](R/exposure.R): Extract daily exposures, population and NUTS codes (HPC).
* [Mortality data processing](R/mortality.R): Clean EUROSTAT mortality time series.
* [Health Impact Assessment analysis](R/HIA.R): Compute RRs, PAFs, attributable deaths and CIs (HPC).
* [Fire weather index analysis](R/danger.R): Perform addition analysis of fire danger provided as supplementary information to the main indicators.
* [Sensitivity analysis 1](R/exposure_sensitivity.R): Trends in wildfire-PM2.5 by European region without population averaging.
* [Sensitivity analysis 2](R/HIA_sensitivity.R): Alternative health impact assessment using a wildfire-specific PM2.5 exposure-response function.`

## Visualizations

The code to generate the visualizations included in the Lancet Countdown Europe report 2022 edition can be found in the following [Rmarkdown document](reports/results.Rmd) and well as in [html form](reports/results.html).

## Preparation

The indicator uses a set of open datasets (NUTS boundaries, mortality time series, gridded population estimates) that can be easily obtained from NaturalEarth, Eurostat, Geostat and GISCO. Those should be placed in the folder [data/boundaries](data/boundaries), [data/mortality](data/mortality), and [data/population](data/population), respectively. These are not included in the repository due to large file sizes. Please find access paths in the text files included in the folders.

Furthermore, the indicator uses wildfire-PM2.5 exposure data derived from the Integrated System for wild-land Fires (IS4FIRES) and the System for Integrated modelling of Atmospheric composition (SILAM) models, as well as Fire Weather Index information computed using ERA5 data (see report appendix for further details). Please contact the authors of the indicator (carles.mila@isglobal.com, Mikhail.Sofiev@fmi.fi, cathryn.tonne@isglobal.org) for details about how to get access to the data. SILAM data should be placed in the [SILAM](SILAM/) folder and fire danger data should be placed in the [data/firedanger](data/firedanger) folder.
