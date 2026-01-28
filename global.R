# Packages to be loaded.
library(tidyverse)
library(shiny)
library(scales)
library(shinythemes)
library(shinyWidgets)
library(cowplot)
library(data.table)

# ============================================================================
# CONSTANTS
# ============================================================================

# Plot colors
COLOR_REGIONAL <- "#3B9AB2"
COLOR_NATIONAL <- "#02401B"
COLOR_PROVINCE_DAILY <- "#899DA4"
COLOR_PROVINCE_AVG <- "#F21A00"

# Visual parameters
PLOT_ALPHA <- 0.9
PLOT_ALPHA_DAILY <- 0.2
PLOT_ALPHA_AREA <- 0.1
PLOT_LINE_SIZE <- 0.75
PLOT_FONT_SIZE <- 18

# Rolling average window
ROLLING_WINDOW <- 7

# Geographic constants
ISO_CODE_MAPPING <- list("IT-SD" = "IT-SU")
NATIONAL_LABEL <- "Italy"

# Function to read and assemble Google Mobility Report data by selected country.
# This function will be used to update Google data weekly.
# url = zip URL to be downloaded
# files_to_read = vector of CSV filenames to extract from the zip
read_google <- function(url, files_to_read) {

  # Assigning path class to url.
  url <- file.path(url)

  # Creating a temporary file.
  temp <- tempfile()

  # Download zip with error handling.
  tryCatch({
    download.file(url, temp)
  }, error = function(e) {
    stop(paste("Failed to download file from URL:", url, "\nError:", e$message))
  })

  # Unzip files with error handling.
  fls <- tryCatch({
    unzip(temp, files_to_read)
  }, error = function(e) {
    unlink(temp)
    stop(paste("Failed to unzip file. The archive may be corrupted.\nError:", e$message))
  })
  
  # Read CSV files with error handling.
  dfr <<- tryCatch({
    rbindlist(lapply(fls, fread, encoding = "UTF-8"))
  }, error = function(e) {
    unlink(c(temp, fls))
    stop(paste("Failed to read CSV files. Data may be malformed.\nError:", e$message))
  }) %>%
    
    # Deleting useless columns.
    select(-c("country_region_code", "country_region", "metro_area", "census_fips_code")) %>%
    
    # Renaming variables for understandability.
    rename(
      iso31662 = iso_3166_2_code,
      region = sub_region_1,
      province = sub_region_2,
      retail_recreation = retail_and_recreation_percent_change_from_baseline,
      grocery_pharmacy = grocery_and_pharmacy_percent_change_from_baseline,
      parks = parks_percent_change_from_baseline,
      transit_stations = transit_stations_percent_change_from_baseline,
      workplace = workplaces_percent_change_from_baseline,
      residential = residential_percent_change_from_baseline
    ) %>%
    
    # Indexes to percent values.
    mutate(
      "retail_recreation" = retail_recreation / 100,
      "grocery_pharmacy" = grocery_pharmacy / 100,
      "parks" = parks / 100,
      "transit_stations" = transit_stations / 100,
      "workplace" = workplace / 100,
      "residential" = residential / 100,
      # Trimming province names.
      province = str_replace(province, "Metropolitan City of ", ""),
      province = str_replace(province, "Province of ", ""),
      province = str_replace(province, "Free municipal consortium of ", ""),
      province = stringr::str_trim(province),
      region = stringr::str_trim(region),
      iso31662 = stringr::str_trim(iso31662),
      # Fixing empty cells (Italy and regions are missing).
      province = ifelse(province == "", region, province),
      iso31662 = ifelse(iso31662 == "IT-SD", ISO_CODE_MAPPING[["IT-SD"]], iso31662),
      province = ifelse(province == "", NATIONAL_LABEL, province)
    )
  
  # Subsetting alphabetically-ordered region and province labels. 
  # Relational DB that connects labels with province and region names. 
  regpro <<- dfr %>%
    distinct(region, province) %>%
    distinct(province, .keep_all = TRUE) %>%
    mutate(prolab = ifelse(province != region | region == "Aosta", province, NA),
           reglab = ifelse(is.na(prolab), region, NA))
  
  # Cleaning spaces and other characters that ggplot cannot handle as names.
  regpro <<- regpro %>%
    mutate(region = str_replace_all(region, c(" " = "" , "'" = "",  "-" = "")),
           province = str_replace_all(province, c(" " = "" , "'" = "",  "-" = "")))
  
  # Cleaning the database for the same purpose.
  dfr <<- dfr %>%
    mutate(region = str_replace_all(region, c(" " = "" , "'" = "",  "-" = "")),
                         province = str_replace_all(province, c(" " = "" , "'" = "",  "-" = "")))
  # Removing temporary items.
  unlink(c(temp, fls))
  rm(temp, fls)
}

# Reading data from Google Mobility Reports by geographical area.
path <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"
# Only for local testing:
# path <- "file://data/new file.zip"

# Vector of pertinent csv files.
files <- c("2020_IT_Region_Mobility_Report.csv",
           "2021_IT_Region_Mobility_Report.csv")

# Reading and manipulating Google Mobility Data in one large dataframe according
# on 2 inputs (url and vector of csv files).
read_google(path, files)

# Creating a db to link plot variables, displayed names and text to
# explain mobility variables to be shown in the summary.
# Using tibble constructor for cleaner, more maintainable code.
nam <- tibble(
  var = colnames(dfr[, 6:11]) %>% sort(),
  namlab = c(
    "Groceries and pharmacies",
    "Parks",
    "Residences",
    "Retail and recreation",
    "Transit stations",
    "Workplaces"
  ),
  text = c(
    " shows mobility trends for places like grocery markets, food warehouses, farmers markets, specialty food shops, drug stores, and pharmacies.",
    " shows mobility trends for places like national parks, public beaches, marinas, dog parks, plazas, and public gardens.",
    " shows mobility trends for places of residence.",
    " shows mobility trends for places like restaurants, cafes, shopping centers, theme parks, museums, libraries, and movie theaters.",
    " shows mobility trends for places like public transport hubs such as subway, bus, and train stations.",
    " shows mobility trends for places of work."
  )
)

# Plot description text that will be concatenated to text summaries above
plotdescr <- "This plot displays daily variations from baseline in grey and a 7-day rolling average in red. Dots on the left add 7 day regional or national rolling averages. Data is updated to the latest available week."

