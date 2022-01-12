# Import EM Data ----
em <- read.csv("data/em_processed.csv") %>%
  mutate(start.timestamp = force_tz(ymd_hms(start.timestamp), tzone = "America/New_York"),
         start.date = date(start.timestamp),
         evtr = as.character(evtr),
         species = str_to_title(species))

# Import Dealer Data ----
simm <- read.csv("data/simm_processed.csv") %>%
    mutate(date.sold = ymd(date.sold),
           dummy.date = ymd(paste(2048, month(date.sold), day(date.sold), sep = "-")),
           year = year(date.sold),
           species = str_to_title(species),
           port.land = str_trim(port.land),
           port.land = str_replace(port.land, "Portlane", "Portland"))

# Operator Information
operator <- read.csv("data/operator_ref.csv")

# Import eVTR Data ----
# ISSUE: Not all gear types have haul start times; decide what to use as a date instead
# TEMPORARY SOLUTION: When haulstart not available, use date.sail
evtr <- read.csv("data/evtr_processed.csv") %>%
    rename_with(~str_replace_all(., "_", ".")) %>%
    mutate(datetime.sail = date.sail,
           date.sail = date(datetime.sail),
           datetime.land = date.land,
           date.land = date(datetime.land),
           haulstarttime = haulstart,
           haulstart = date(haulstarttime),
           haulendtime = haulend,
           haulend = date(haulendtime),
           usedate = if_else(!is.na(haulstart), haulstart, date.sail),
           gear = str_to_title(gearcode),
           gear = ifelse(gear == "", "Other", gear),
           port.land = str_to_title(port.landed),
           port.land = ifelse(port.land == "Harwichport", "Harwich Port", port.land),
           week = week(usedate),
           month = month(usedate),
           species = str_to_title(species)) %>%
    group_by(vtr, haulstarttime, haulendtime) %>%
    mutate(haul.id = cur_group_id()) %>%
    ungroup()

# Import Environmental Data ----
# Updated
suppressWarnings(
  enviro_data <- read.csv(textConnection(readLines("data/enviro/A01_sbe37_all_dab4_6226_5825.csv")[-2])) %>%
  bind_rows(read.csv(textConnection(readLines("data/enviro/B01_sbe37_all_dab4_6226_5825.csv")[-2]))) %>%
  bind_rows(read.csv(textConnection(readLines("data/enviro/E01_sbe37_all_dab4_6226_5825.csv")[-2]))) %>%
  bind_rows(read.csv(textConnection(readLines("data/enviro/F01_sbe37_all_dab4_6226_5825.csv")[-2]))) %>%
  mutate(usedate = ymd(str_sub(time, 1, 10)),
         month = month(usedate),
         week = week(usedate),
         name = case_when(
           station == "A01" ~ "Massachusettes Bay",
           station == "B01" ~ "Western Maine Shelf",
           station == "E01" ~ "Central Maine Shelf",
           station == "F01" ~ "Penobscot Bay"
         ),
         link = case_when(
           station == "A01" ~ "https://mariners.neracoos.org/platform/A01%20-%2044029",
           station == "B01" ~ "https://mariners.neracoos.org/platform/B01%20-%2044030",
           station == "E01" ~ "https://mariners.neracoos.org/platform/E01%20-%2044032",
           station == "F01" ~ "https://mariners.neracoos.org/platform/F01%20-%2044033"
         )) %>%
  group_by(name, station, depth, usedate, month, week, latitude, longitude, link) %>%
  summarise(across(c(temperature, conductivity, salinity),
                   list(mean = mean, min = min, max = max), na.rm = TRUE)) %>%
  ungroup()
)
