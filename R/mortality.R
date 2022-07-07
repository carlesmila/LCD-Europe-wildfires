#-----------------------------------------------------------------------------#
#                     Clean and add weekly mortality data                     #
#-----------------------------------------------------------------------------#

# Warning:
# Germany (DE), Ireland (IE), Croatia (HR), Slovenia (SI) available at the NUTS1 level
# No data for Macedonia (MK)
# The rest of the countries are available at NUTS 2 level
# Estonia (EE) mortality data is for NUTS16, but that makes no difference at the
# NUTS2 level.
pathroot <- ""

# 1. Clean mortality ----

# Read data
mort <- read_tsv(paste0(pathroot, "data/mortality/demo_r_mwk2_ts.tsv"))
# Take years 2003-2020
mort <- dplyr::select(mort, 1, contains(as.character(2003:2020)))
# Disentangle 1st column, take both sexes and NUTS codes
names(mort)[1] <- "meta"
mort <- group_by(mort, meta) %>%
  mutate(sex = strsplit(meta, ",")[[1]][1],
         NUTS = strsplit(meta, ",")[[1]][3]) %>%
  ungroup() %>%
  filter(sex == "T") %>%
  filter(nchar(NUTS)==4|(nchar(NUTS) == 3 & grepl("DE|IE|HR|SI", NUTS))) %>% # NUTS2 or NUTS1 for selected countries
  dplyr::select(-meta, -sex) 
# Wide to long format, parse NAs and remove provisional flags
mort <- pivot_longer(mort, -NUTS, names_to = "week", values_to = "deaths")
mort <- mutate(mort,
               deaths = ifelse(deaths == ":", NA, deaths),
               deaths = gsub(" p", "", deaths),
               deaths = as.numeric(deaths)) %>%
  filter(!is.na(deaths))
# Discard XX NUTS2: unclassified and minor counts
mort <- mort[!grepl("XX", mort$NUTS),]
# Discard NO0B NUTS2 (Jan Mayen and Svalbard): always 0
mort <- mort[!grepl("NO0B", mort$NUTS),]
# Discard extra Norway NUTS2: superseded (merged into larger NUTS2) in 2021 version
mort <- mort[!grepl("NO01|NO03|NO04|NO05", mort$NUTS),]
# Discard more NUTS2 for which we have no exposure data: Canary Islands (ES70), 
# Guadeloupe (FRY1), Martinique (FRY2), Guyane (FRY3), La Reunion (FRY4), 
# Mayotte (FRY5), Acores (PT20), Madeira (PT30)
mort <- mort[!grepl("ES70|FRY1|FRY2|FRY3|FRY4|FRY5|PT20|PT30", mort$NUTS),]


# 2. Weekly to daily counts (ISO 8601) ----
# https://ec.europa.eu/eurostat/data/database?node_code=demomwk
# WARNING: provisional data + 2020W52 and 53 not available for the UK

# Expand weeks to days by dividing all death counts by 7 and stacking 7 times
mort_day <- mutate(mort, week = paste0(substr(week, 1, 4), "-", substr(week, 5, 7)))
mort_day <- mutate(mort_day, deaths = deaths/7)   
mort_day <- bind_rows(mutate(mort_day, week_day = paste0(week, "-1")),
                      mutate(mort_day, week_day = paste0(week, "-2")),
                      mutate(mort_day, week_day = paste0(week, "-3")),
                      mutate(mort_day, week_day = paste0(week, "-4")),
                      mutate(mort_day, week_day = paste0(week, "-5")),
                      mutate(mort_day, week_day = paste0(week, "-6")),
                      mutate(mort_day, week_day = paste0(week, "-7")))
mort_day <- mutate(mort_day, date = ISOweek2date(week_day))
mort_day <- arrange(mort_day, NUTS, date) %>%
  dplyr::select(-week, -week_day)
# Check
if(sum(mort$deaths) != sum(mort_day$deaths)){
  stop("Something has gone wrong when processing the weekly death counts.")
}

mort_day <- filter(mort_day, between(date, as.Date("2003-01-01"), as.Date("2020-12-31")))
write_csv(mort_day, paste0(pathroot, "data/processed/mortality.csv"))
