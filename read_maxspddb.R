library(tidyverse)
library(devtools)
library(rrtools)

readxl::read_excel("analysis/data/raw_data/maxspeeddatabase.xlsx") %>%
  dplyr::rename(tax_grp = `taxonomic group`,
                locomotion = `locomotion mode`,
                mass_kg = `body mass [kg]`,
                mass_ref = `mass ref.`,
                max_spd_kmh = `max. speed [km/h]`,
                speed_ref = `speed ref.`,
                diet = `primary diet`) %>%
  saveRDS("analysis/data/derived_data/maxspeed.RDS")
