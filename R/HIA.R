#-----------------------------------------------------------------------------#
#                  Population-weighted exposures, RR and PAFs                 #
#-----------------------------------------------------------------------------#

pathroot <- ""

# 1. RRs ----
# Short-term exposure to particulate matter (PM10 and PM2.5), nitrogen
# dioxide (NO2), and ozone (O3  and all-cause and cause-specific mortality:
# Systematic review and meta-analysis. Orellano et al. (2020).
set.seed(1234)
point_RR10 <- 1.0065
sim_RR10 <- exp(rnorm(200, mean = log(point_RR10), sd = log(1.0086/1.0044)/(qnorm(0.975)*2)))
names(sim_RR10) <- paste0("RR10_", str_pad(1:200, 3, "left", "0"))

# 2. Regions
regions <- st_read(paste0(pathroot, "data/regions/Europe_regions.shp")) %>%
  st_drop_geometry() %>%
  rename(country = CNTR_CODE) %>%
  dplyr::select(country, region) %>%
  filter(!duplicated(.))

# 2. Mortality ----
mortality <- fread(paste0(pathroot, "data/processed/mortality.csv"), 
                   colClasses = c("character", "numeric", "character"))


# 3. HIA Function ----
hia <- function(hiafile, pRR10 = point_RR10, sRR10 = sim_RR10, 
                mort = mortality, reg = regions){
  
  # Read data for NUTS2. Drop:
  # Turkey (TR) and Jan Mayen and Svalbard (NO0B) - no population data
  # Madeira (PT30) - no Atlantic islands
  # Germany (DE), Ireland (IE), Croatia (HR), Slovenia (SI) - mortality at the NUTS1 level
  # Bosnia and Herzegovina and Kosovo - No NUTS data
  hiadata <- read_csv(hiafile)
  hiadata_nuts2 <- hiadata %>%
    dplyr::select(GRD_ID, NUTS21_2, pop, starts_with("X20")) %>%
    dplyr::filter(!is.na(NUTS21_2)) %>%
    filter(!grepl("TR|NO0B|PT30|DE|IE|HR|SI", NUTS21_2)) %>% 
    rename(NUTS = NUTS21_2)
  # Read data for NUTS1 for selected countries (DE, IE, HR, SI), stack
  hiadata_nuts1 <- hiadata %>%
    dplyr::select(GRD_ID, NUTS21_1, pop, starts_with("X20")) %>%
    dplyr::filter(!is.na(NUTS21_1)) %>%
    filter(grepl("DE|IE|HR|SI", NUTS21_1)) %>% 
    rename(NUTS = NUTS21_1)
  hiadata <- bind_rows(hiadata_nuts1, hiadata_nuts2)
  # Derive countries, add regions
  hiadata$country <- substr(hiadata$NUTS, 1, 2)
  hiadata <- left_join(hiadata, reg, by = "country")
  rm("hiadata_nuts1", "hiadata_nuts2")
  
  # Population check. No NUTS can have a population of 0.
  if(any(tapply(hiadata$pop, hiadata$NUTS, sum) == 0)){
    stop("Some NUTS have a population of 0. Please revise the data.")
  }
  
  # Mortality check:  Macedonia (MK00) doesn't have mortality data
  if(length(setdiff(unique(hiadata$NUTS), c("MK00", unique(mort$NUTS)))>0)){
    stop("Exposure and mortality files should have the same unique identifiers.")
  }
  
  # Remove empty population cells, long format, parse dates 
  hiadata <- filter(hiadata, pop != 0) %>%
    pivot_longer(-c("GRD_ID", "pop", "NUTS", "country", "region"),
                 names_to = "date", values_to = "pm25") %>%
    mutate(date = gsub("X", "", date, fixed = T),
           date = gsub(".", "-", date, fixed = T))
  
  # Compute RRs  
  hiadata$RR <- exp(log(pRR10)*hiadata$pm25/10)
  hiadata_sims <- map_dfc(sRR10, function(x) exp(log(x)*hiadata$pm25/10))
  names(hiadata_sims) <- gsub("RR10", "RR", names(hiadata_sims))
  hiadata <- cbind(hiadata, hiadata_sims)
  rm("hiadata_sims")
  
  # Compute population weights by NUTS, daily population-weighted exposure + PAFs
  hia_nuts <- hiadata
  setDT(hia_nuts)
  hia_nuts[, popw := pop/sum(pop), by = .(NUTS, country, region, date)]
  RRcols <- names(hia_nuts)[grepl("RR", names(hia_nuts))]
  hia_nuts <- hia_nuts[, c(pm25w=sum(popw*pm25),
                           lapply(.SD, function(x) (sum(popw*x)-1)/sum(popw*x))), 
                       by = .(NUTS, country, region, date), .SDcols = RRcols]
  names(hia_nuts) <- gsub("RR", "PAF", names(hia_nuts))
  hia_nuts[, year := year(as.Date(date))]
  expodist_nuts <- hia_nuts[, .(NUTS, country, region, year, date, pm25w)]
  setDF(expodist_nuts)
  
  # Compute annual NUTS exposure indicators ----
  hia_nuts_year <- hia_nuts[, .(pm25w_avg = mean(pm25w)), 
                            by = .(NUTS, country, region, year)]
  setDF(hia_nuts_year)
  
  # Compute population weights by country, daily population-weighted exposure 
  hia_country <- hiadata
  setDT(hia_country)
  hia_country[, popw := pop/sum(pop), by = .(country, region, date)]
  hia_country <- hia_country[, .(pm25w = sum(popw*pm25)),
                             by = .(country, region, date)]
  
  # Compute annual country exposure indicators ----
  hia_country[, year := year(as.Date(date))]
  hia_country_year <- hia_country[, .(pm25w_avg = mean(pm25w)),
                                  by = .(country, region, year)]
  setDF(hia_country_year)
  rm("hia_country")
  
  # Compute population weights by region, daily population-weighted exposure 
  hia_region <- hiadata
  setDT(hia_region)
  hia_region[, popw := pop/sum(pop), by = .(region, date)]
  hia_region <- hia_region[, .(pm25w = sum(popw*pm25)), by = .(region, date)]
  
  # Compute annual region exposure indicators ----
  hia_region[, year := year(as.Date(date))]
  hia_region_year <- hia_region[, .(pm25w_avg = mean(pm25w)), by = .(region, year)]
  setDF(hia_region_year)
  hia_region_year <- hia_region_year[!is.na(hia_region_year$region),]
  rm("hia_region")
  
  # Compute Europe-wide population weights, daily population-weighted exposure 
  hia_euro <- hiadata
  setDT(hia_euro)
  hia_euro[, popw := pop/sum(pop), by = .(date)]
  hia_euro <- hia_euro[, .(pm25w = sum(popw*pm25)), by = .(date)]
  
  # Compute annual Europe-wide exposure indicators ----
  hia_euro[, year := year(as.Date(date))]
  hia_euro_year <- hia_euro[, .(pm25w_avg = mean(pm25w)), by = .(year)]
  setDF(hia_euro_year)
  rm("hia_region")
  
  # Merge mortality and exposure, estimate attributable mortality ----
  setkey(mort, date, NUTS)
  setkey(hia_nuts, date, NUTS)
  mort_nuts <- mort[hia_nuts, nomatch = 0]
  PAFcols <- names(mort_nuts)[grepl("PAF", names(mort_nuts))]
  mort_nuts <- mort_nuts[, lapply(.SD, function(x) x * deaths), 
                         by = .(NUTS, country, region, year, date), .SDcols = PAFcols]
  names(mort_nuts) <- gsub("PAF", "attr", names(mort_nuts))
  
  # Aggregate to years ----
  attrcols <- names(mort_nuts)[grepl("attr", names(mort_nuts))]
  mort_nuts_year <- mort_nuts[, c(N = .N, lapply(.SD, sum)), 
                              by = .(NUTS, country, region, year),
                              .SDcols = attrcols]
  setDF(mort_nuts_year)
  mort_nuts_year <- filter(mort_nuts_year, N >= 365) %>%
    dplyr::select(-N) %>%
    complete(NUTS, year)
  
  # Aggregate to countries ----
  mort_country_year <- mort_nuts_year %>%
    mutate(country = substr(NUTS, 1, 2)) %>%
    group_by(year, country, region) %>%
    summarise(across(starts_with("attr"), sum)) %>%
    ungroup()
  
  # Aggregate to Europe ----
  mort_euro_year <- mort_nuts_year %>%
    mutate(country = substr(NUTS, 1, 2)) %>%
    group_by(year) %>%
    summarise(across(starts_with("attr"), sum)) %>%
    ungroup()
  
  # Compute CIs ----
  mort_nuts_year$attrlower <- 
    apply(dplyr::select(mort_nuts_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.025, na.rm=T))
  mort_nuts_year$attrupper <- 
    apply(dplyr::select(mort_nuts_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.975, na.rm=T))
  mort_country_year$attrlower <-
    apply(dplyr::select(mort_country_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.025, na.rm=T))
  mort_country_year$attrupper <- 
    apply(dplyr::select(mort_country_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.975, na.rm=T))
  mort_euro_year$attrlower <-
    apply(dplyr::select(mort_euro_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.025, na.rm=T))
  mort_euro_year$attrupper <- 
    apply(dplyr::select(mort_euro_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.975, na.rm=T))  
  mort_nuts_year <- dplyr::select(mort_nuts_year, -contains("_"))
  mort_country_year <- dplyr::select(mort_country_year, -contains("_"))
  mort_euro_year <- dplyr::select(mort_euro_year, -contains("_"))
  
  # Return data
  list("expo_dist" = expodist_nuts,
       "expo_nuts" = hia_nuts_year, 
       "expo_country" = hia_country_year,
       "expo_region" = hia_region_year,
       "expo_euro" = hia_euro_year,
       "mortality_nuts" = mort_nuts_year,
       "mortality_country" = mort_country_year,
       "mortality_euro" = mort_euro_year)
}


# 4. Execute  ----
hia03 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata03.csv"))
hia04 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata04.csv"))
hia05 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata05.csv"))
hia06 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata06.csv"))
hia07 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata07.csv"))
hia08 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata08.csv"))
hia09 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata09.csv"))
hia10 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata10.csv"))
hia11 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata11.csv"))
hia12 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata12.csv"))
hia13 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata13.csv"))
hia14 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata14.csv"))
hia15 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata15.csv"))
hia16 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata16.csv"))
hia17 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata17.csv"))
hia18 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata18.csv"))
hia19 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata19.csv"))
hia20 <- hia(paste0(pathroot, "data/processed/HIAexposure/HIAdata20.csv"))

# Exposure distribution
expo_dist <- bind_rows(hia03[[1]], hia04[[1]], hia05[[1]], hia06[[1]], hia07[[1]], 
                       hia08[[1]], hia09[[1]], hia10[[1]], hia11[[1]], hia12[[1]], 
                       hia13[[1]], hia14[[1]], hia15[[1]], hia16[[1]], hia17[[1]], 
                       hia18[[1]], hia19[[1]], hia20[[1]])
write_csv(expo_dist, paste0(pathroot, "data/processed/expo_dist.csv"))

# Exposure level by NUTS
expo_nuts <-  bind_rows(hia03[[2]], hia04[[2]], hia05[[2]], hia06[[2]], hia07[[2]], 
                        hia08[[2]], hia09[[2]], hia10[[2]], hia11[[2]], hia12[[2]], 
                        hia13[[2]], hia14[[2]], hia15[[2]], hia16[[2]], hia17[[2]], 
                        hia18[[2]], hia19[[2]], hia20[[2]])
write_csv(expo_nuts, paste0(pathroot, "data/processed/expo_nuts.csv"))

# Exposure level by country
expo_country <- bind_rows(hia03[[3]], hia04[[3]], hia05[[3]], hia06[[3]], hia07[[3]], 
                          hia08[[3]], hia09[[3]], hia10[[3]], hia11[[3]], hia12[[3]], 
                          hia13[[3]], hia14[[3]], hia15[[3]], hia16[[3]], hia17[[3]], 
                          hia18[[3]], hia19[[3]], hia20[[3]])
write_csv(expo_country, paste0(pathroot, "data/processed/expo_country.csv"))

# Exposure level by region
expo_region <- bind_rows(hia03[[4]], hia04[[4]], hia05[[4]], hia06[[4]], hia07[[4]], 
                         hia08[[4]], hia09[[4]], hia10[[4]], hia11[[4]], hia12[[4]], 
                         hia13[[4]], hia14[[4]], hia15[[4]], hia16[[4]], hia17[[4]], 
                         hia18[[4]], hia19[[4]], hia20[[4]])
write_csv(expo_region, paste0(pathroot, "data/processed/expo_region.csv"))

# Exposure level Europe-wide
expo_euro <- bind_rows(hia03[[5]], hia04[[5]], hia05[[5]], hia06[[5]], hia07[[5]], 
                       hia08[[5]], hia09[[5]], hia10[[5]], hia11[[5]], hia12[[5]], 
                       hia13[[5]], hia14[[5]], hia15[[5]], hia16[[5]], hia17[[5]], 
                       hia18[[5]], hia19[[5]], hia20[[5]])
write_csv(expo_euro, paste0(pathroot, "data/processed/expo_europe.csv"))

# Attributable mortality by NUTS
attr_nuts <- bind_rows(hia03[[6]], hia04[[6]], hia05[[6]], hia06[[6]], hia07[[6]], 
                       hia08[[6]], hia09[[6]], hia10[[6]], hia11[[6]], hia12[[6]], 
                       hia13[[6]], hia14[[6]], hia15[[6]], hia16[[6]], hia17[[6]], 
                       hia18[[6]], hia19[[6]], hia20[[6]])
write_csv(attr_nuts, paste0(pathroot, "data/processed/attributable_nuts.csv"))

# Attributable mortality by country
attr_country <- bind_rows(hia03[[7]], hia04[[7]], hia05[[7]], hia06[[7]], hia07[[7]], 
                          hia08[[7]], hia09[[7]], hia10[[7]], hia11[[7]], hia12[[7]], 
                          hia13[[7]], hia14[[7]], hia15[[7]], hia16[[7]], hia17[[7]], 
                          hia18[[7]], hia19[[7]], hia20[[7]])
write_csv(attr_country, paste0(pathroot, "data/processed/attributable_country.csv"))

# Attributable mortality euro
attr_euro <- bind_rows(hia03[[8]], hia04[[8]], hia05[[8]], hia06[[8]], hia07[[8]], 
                       hia08[[8]], hia09[[8]], hia10[[8]], hia11[[8]], hia12[[8]], 
                       hia13[[8]], hia14[[8]], hia15[[8]], hia16[[8]], hia17[[8]], 
                       hia18[[8]], hia19[[8]], hia20[[8]])
write_csv(attr_euro, paste0(pathroot, "data/processed/attributable_euro.csv"))