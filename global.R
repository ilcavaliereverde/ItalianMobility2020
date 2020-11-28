#packages for the plots tab
library(tidyverse)
library(magrittr)
library(shiny)
library(stringr)
library(scales)
library(shinythemes)
library(shinyWidgets)
library(cowplot)
library(data.table)

#Function to read and assemble Google Mobility Report data by selected country. This function
#will be used to update Google data weekly
readgoogle <- function(x, y) {
  #Assigning path to x
  x <- file.path(x)
  
  dfr <<-
    #x can be the Google Mobility Reports url or a local csv file
    data.table::fread(x, encoding = "UTF-8") %>%
    #Filtering the dataset by selected country
    filter(country_region_code == y) %>%
    #Deleting useless columns
    select(-c("metro_area", "census_fips_code")) %>%
    #Renaming variables for understandability
    rename(
      iso = country_region_code,
      iso31662 = iso_3166_2_code,
      country = country_region,
      region = sub_region_1,
      province = sub_region_2,
      retail_recreation = retail_and_recreation_percent_change_from_baseline,
      grocery_pharmacy = grocery_and_pharmacy_percent_change_from_baseline,
      parks = parks_percent_change_from_baseline,
      transit_stations = transit_stations_percent_change_from_baseline,
      workplace = workplaces_percent_change_from_baseline,
      residential = residential_percent_change_from_baseline
    ) %>%
    #Indexes to percent values
    mutate(
      "retail_recreation" = retail_recreation / 100,
      "grocery_pharmacy" = grocery_pharmacy / 100,
      "parks" = parks / 100,
      "transit_stations" = transit_stations / 100,
      "workplace" = workplace / 100,
      "residential" = residential / 100,
      #Trimming province names
      province = str_replace(province, "Metropolitan City of ", ""),
      province = str_replace(province, "Province of ", ""),
      province = str_replace(province, "Free municipal consortium of ", ""),
      province = stringr::str_trim(province),
      region = stringr::str_trim(region),
      iso31662 = stringr::str_trim(iso31662),
      #Fixing empty cells (missing Italy and regions)
      region = ifelse(region == "", country, region),
      province = ifelse(province == "", region, province),
      iso31662 = ifelse(iso31662 == "IT-SD", "IT-SU", iso31662),
    )
}

#Reading data from Google Mobility Reports
# path <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
country <- "IT"

#Only for local use
path <- "data/Global_Mobility_Report.csv"

#Reading and manipulating data
readgoogle(path, country)

##The following code need not be inside the function to work: region, province, descriptions do not change if the function is updated by the shiny app

#Subsetting alphabetically-ordered region and province labels for later
prre = dfr %>% distinct(region, province) %>% distinct(province, .keep_all = T)
# pro = sort(prre$province)
prre <- prre %>% mutate(provonly = ifelse(province != region | region == "Aosta", province, NA))
reg = sort(unique(prre$region))

#Creating a db to link plot variables, diplayed names and text to explain mobility variables to be shown in the summary
#Plot variables
nam = colnames(dfr[, 7:12]) %>% sort () %>% tibble() 
#Labels to be displayed in UI
nam[1, 2] = "Groceries and pharmacies"
nam[2, 2] = "Parks"
nam[3, 2] = "Residences"
nam[4, 2] = "Retail and recreation"
nam[5, 2] = "Transit stations"
nam[6, 2] = "Workplaces"
#Summaries for UI
nam[1, 3] = "Groceries & pharmacies shows mobility trends for places like grocery markets, food warehouses, farmers markets, specialty food shops, drug stores, and pharmacies."
nam[2, 3] = "Parks shows mobility trends for places like national parks, public beaches, marinas, dog parks, plazas, and public gardens."
nam[3, 3] = "Residential shows mobility trends for places of residence."
nam[4, 3] = "Retail & recreation shows mobility trends for places like restaurants, cafes, shopping centers, theme parks, museums, libraries, and movie theaters."
nam[5, 3] = "Transit stations shows mobility trends for places like public transport hubs such as subway, bus, and train stations."
nam[6, 3] = "Workplaces shows mobility trends for places of work."
#Colnames to call in app
colnames(nam) = c("var", "name", "text")

#Plot description text that will be concatenated to text summaries above
plotdescr = "This plot displays daily variations from baseline in grey and a 7-day rolling average in red. You can check the box on the left to add the regional 7-day rolling average. Data is updated to the latest available week."