library(tidyverse)
library(dplyr)
library(tidycensus)
library(tidyselect)
library(tidygeocoder)

install.packages('tidyselect')

install.packages('tidygeocoder')

#code to read in the csv gov contracting files
tempall <- read.csv("GSA_Federal_Contracts_Addresses.csv", header = TRUE,
                    stringsAsFactors = FALSE, colClass ="factor")


census_full <- AL %>% geocode(street = recipient_address_line_1, city = recipient_city_name, state = recipient_state_code, 
                              method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies'))
