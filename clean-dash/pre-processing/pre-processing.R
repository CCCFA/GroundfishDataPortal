## ---------------------------
## Code re-purposed / updated by Danielle Quinn (2021) to align with dummy data for dashboard
##
## Purpose of script: Brings together VTR, EM, and Dealer Data for FY2019 for
##    analytics and reporting
##
## Original Author: George A. Maynard
##
## Original Date Created: 2020-10-19
##
## Original Copyright (c) George Alphonse Maynard, 2020
## Email: galphonsemaynard@gmail.com
##

# Set Options ----
options(scipen = 6, digits = 4) # eliminate scientific notation

# Load Packages ----
library(lubridate)
library(stringdist)
library(dplyr)
library(tidyr)

# Species Data ----
species <- read.csv("https://raw.githubusercontent.com/gamaynard/ElectronicMonitoring/master/species.csv")

# Process Dealer Data ----
Dealer <- read_excel("orig-data/FY19 Dealer Data SIMM.xlsx",
                     .name_repair = "universal") %>% 
  select("Mri", "Vessel.Permit.No", "Vessel.Name",
                "Vessel.Reg.No", "Vtr.Serial.No",
                "State.Land", "Port.Land", "Species.Itis",
                "Landed.Weight", "Live.Weight", "Date.Sold") %>%
  mutate(
    # Permit numbers should be character strings to maintain leading zeros
    PERMIT = as.character(Vessel.Permit.No),
    # Vessel names should be all caps
    VESSEL = toupper(Vessel.Name),
    # VTR numbers should be character strings to maintain leading zeros
    VTR = as.character(Vtr.Serial.No),
    # Dates should be converted to POSIX values
    DATE = ymd(Date.Sold)
  ) %>%
  # Species names can be converted directly from ITIS numbers
  left_join(species %>% select(species = AFS, ITIS) %>% distinct(),
            by = c("Species.Itis" = "ITIS")) %>%
  select(-Vtr.Serial.No, -Vessel.Name, -Vessel.Permit.No)%>%
  rename_with(str_to_lower)

# Process VTR Data ----
VTR <- read.csv("orig-data/FY2019-eVTRdata-GARFO.csv") %>%
  select("DATE_SAIL", "DATE_LAND", "VESSEL_PERMIT_NUM",
         "SERIAL_NUM", "GEARCODE", "GEARQTY", "GEARSIZE", "AREA",
         "LAT_DEGREE", "LAT_MINUTE", "LAT_SECOND",
         "LON_DEGREE", "LON_MINUTE", "LON_SECOND",
         "NTOWS", "DATETIME_HAUL_START", "DATETIME_HAUL_END",
         "SPECIES_ID", "KEPT", "DISCARDED", "PORT_LANDED") %>%
  # Lowercase variable names
  rename_with(tolower) %>%
  # Paper trip reports only have 8 characters and can be omitted
  filter(nchar(serial_num) == 16) %>%
  # Create columns with updated variables
  mutate(
    # Date sail and date land times should be POSIX formatted values
    DATE.SAIL = mdy_hms(date_sail),
    DATE.LAND = mdy_hms(date_land),
    PERMIT = as.character(vessel_permit_num),
    # Check for NA values in the seconds columns of 
    # both latitude and longitude and if they exist,
    # replace them with zeros
    lat_second = replace_na(lat_second, 0),
    lon_second = replace_na(lon_second, 0),
    # All longitude values should be west of the Prime Meridian (negative)
    lon_degree = ifelse(lon_degree > 0, lon_degree * -1, lon_degree),
    # Combine degrees, minutes, and seconds into decimal 
    # degrees for both latitude and longitude
    LAT = lat_degree + lat_minute/60 + lat_second/(60^2),
    LON = lon_degree - lon_minute/60 - lon_second/(60^2),
    # Replace gear codes with human-readable values
    gearcode = case_when(
      gearcode == "GNS" ~ "GILLNET",
      gearcode == "HND" ~ "JIG",
      gearcode == "LLB" ~ "LONGLINE",
      gearcode == "OTF" ~ "TRAWL",
      gearcode == "PTL" ~ "LOBSTER POT",
      TRUE ~ ""
    ),
    # Ensure stat areas are reported as numbers
    AREA = as.numeric(as.character(area)),
    # Trim serial numbers to generate VTR numbers
    VTR = ifelse(nchar(serial_num) == 16, substr(serial_num, 1, 14), NA),
    # Haul start and end times should be POSIX formatted values
    HAULSTART = mdy_hms(datetime_haul_start),
    HAULEND = mdy_hms(datetime_haul_end),
    # Kept and discarded weights should be numeric
    KEPT = as.numeric(as.character(kept)),
    DISCARDED = as.numeric(as.character(discarded))
  ) %>%
  # Convert permit numbers to vessel names using the
  # reference values available in the Dealer data
  left_join(Dealer %>% select(vessel, permit) %>% distinct(),
            by = c("PERMIT" = "permit")) %>%
  # Drop records with missing vessel
  filter(!is.na(vessel)) %>%
  select(-vessel_permit_num, -area, -kept, -discarded,
         -datetime_haul_start, -datetime_haul_end,
         -date_sail, -date_land)

# Standardize species names
VTR$SPECIES <- NA
for(i in 1:nrow(VTR)) {
  VTR$SPECIES[i] <- species$AFS[which.max(stringsim(VTR$species_id[i], species$PEBKAC))]
}

VTR <- VTR %>%
  rename_with(str_to_lower)

# Process EM Data ----
EM <- read_excel("orig-data/Example of an FY19EM Summary File-preAPI calcus - flattened from JSN to Excel Format.xlsx",
               .name_repair = "universal") %>%
  separate(Location, into = c("LAT", "LON"), sep = ",", convert = TRUE) %>%
  mutate(
    # The start time column needs to be converted to a POSIX value
    STARTTIME = ymd_hms(Start.Timestamp),
    ENDTIME = ymd_hms(End.Timestamp),
    # Remove abs LAT < 5, LON < 5
    across(c(LAT, LON), ~ifelse(abs(.) < 5, NA, .)),
    # Discard Count needs to be a number
    DiscardCount = Quantity,
    # DiscardWeight needs to be a number
    DiscardWeight = Estimated.weight
  )

# Standardize species names
EM$SPECIES <- NA
for(i in 1:nrow(EM)) {
  EM$SPECIES[i] <- ifelse(!is.na(EM$Species[i]),
                          species$AFS[which.max(stringsim(EM$Species[i], species$PEBKAC))],
                          NA)
}

EM <- EM %>%
  select(-Species) %>%
  rename_with(str_to_lower)

# Export Data ----
write.csv(VTR, "data/evtr_processed.csv", row.names = FALSE)
write.csv(EM, "data/em_processed.csv", row.names = FALSE)
write.csv(Dealer, "data/simm_processed.csv", row.names = FALSE)
