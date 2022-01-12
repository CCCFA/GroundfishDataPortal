# Load packages ----
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)
library(lubridate)
library(forcats)
library(tibble)
library(tidyr)
library(stringr)
library(leaflet.extras)
library(mapview)
library(readr)
library(purrr)

# Options ----
options(scipen = 6, digits = 4)

# Import Data ----
source("global-components/data-import-prep.R")

# Scales ----
source("global-components/import-scales.R")
