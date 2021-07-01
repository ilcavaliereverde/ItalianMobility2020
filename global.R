# Packages to be loaded.
library(tidyverse)
library(magrittr)
library(shiny)
library(stringr)
library(scales)
library(shinythemes)
library(shinyWidgets)
library(cowplot)
library(data.table)

# Function to read and assemble Google Mobility Report data by selected country.
# This function will be used to update Google data weekly.
# x = zip to be downloaded;
# y = file to be read within the zip file.
read_google <- function(x, y) {
  
  # Assigning path class to x.
  x <- file.path(x)
  
  # Count how many files are in the y vector.
  length(y)
  
  # Creating a temporary file for every.
  temp <- tempfile()
  
  # Downloading zip from the link.
  download.file(x, temp)
  
  # Unzipping files.
  fls <- unzip(temp, y)
  
  # Assigning dataframe.
  dfr <<- rbindlist(lapply(fls, fread, encoding = "UTF-8")) %>%
    
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
      iso31662 = ifelse(iso31662 == "IT-SD", "IT-SU", iso31662),
      province = ifelse(province == "", "Italy", province)
    )
  
  # Subsetting alphabetically-ordered region and province labels. 
  # Relational DB that connects labels with province and region names. 
  regpro <<- dfr %>%
    distinct(region, province) %>%
    distinct(province, .keep_all = T) %>%
    mutate(
      prolab = ifelse(province != region | region == "Aosta", province, NA),
      reglab = ifelse(is.na(prolab), region, NA))
  
  # Cleaning spaces and other characters that ggplot cannot handle as names.
  regpro <<- regpro %>% mutate(region = str_replace_all(region, c(" " = "" , "'" = "",  "-" = "")),
                              province = str_replace_all(province, c(" " = "" , "'" = "",  "-" = "")))
  
  # Cleaning the database for the same purpose.
  dfr <<- dfr %>% mutate(region = str_replace_all(region, c(" " = "" , "'" = "",  "-" = "")),
                         province = str_replace_all(province, c(" " = "" , "'" = "",  "-" = "")))
  # Removing temporary items.
  unlink(c(temp, file))
  rm(temp)
}

# Reading data from Google Mobility Reports by geographical area.
path <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"
# Only for local testing:
# path <- "file://data/ file.zip" 

# Vector of pertinent csv files.
files <- c("2020_IT_Region_Mobility_Report.csv",
           "2021_IT_Region_Mobility_Report.csv")

# Reading and manipulating Google Mobility Data in one large dataframe according
# on 2 inputs (url and vector of csv files).
read_google(path, files)

# Creating a db to link plot variables, displayed names and text to 
# explain mobility variables to be shown in the summary.

# Plot variables
nam = colnames(dfr[, 5:10]) %>% sort () %>% tibble() 

# Labels to be displayed in UI
nam[1, 2] = "Groceries and pharmacies"
nam[2, 2] = "Parks"
nam[3, 2] = "Residences"
nam[4, 2] = "Retail and recreation"
nam[5, 2] = "Transit stations"
nam[6, 2] = "Workplaces"

# Summaries for UI
nam[1, 3] = " shows mobility trends for places like grocery markets, food warehouses, farmers markets, specialty food shops, drug stores, and pharmacies."
nam[2, 3] = " shows mobility trends for places like national parks, public beaches, marinas, dog parks, plazas, and public gardens."
nam[3, 3] = " shows mobility trends for places of residence."
nam[4, 3] = " shows mobility trends for places like restaurants, cafes, shopping centers, theme parks, museums, libraries, and movie theaters."
nam[5, 3] = " shows mobility trends for places like public transport hubs such as subway, bus, and train stations."
nam[6, 3] = " shows mobility trends for places of work."

# Colnames to call in app
colnames(nam) = c("var", "namlab", "text")

# Plot description text that will be concatenated to text summaries above
plotdescr = "This plot displays daily variations from baseline in grey and a 7-day rolling average in red. Dots on the left add 7 day regional or national rolling averages. Data is updated to the latest available week."

