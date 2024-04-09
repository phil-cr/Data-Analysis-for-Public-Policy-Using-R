library(tidyverse)
library(dplyr)
library(tidycensus)

#code to read in the csv gov contracting files
temp21_1 <- read.csv("Data/Hubzones/2021/FY2021_All_Contracts_Full_1.csv", header = TRUE,
                     stringsAsFactors = FALSE, colClass ="factor")

temp21_2 <- read.csv("Data/Hubzones/2021/FY2021_All_Contracts_Full_2.csv", header = TRUE,
                     stringsAsFactors = FALSE, colClass ="factor")

temp21_3 <- read.csv("Data/Hubzones/2021/FY2021_All_Contracts_Full_3.csv", header = TRUE,
                     stringsAsFactors = FALSE, colClass ="factor")

temp21_4 <- read.csv("Data/Hubzones/2021/FY2021_All_Contracts_Full_4.csv", header = TRUE,
                     stringsAsFactors = FALSE, colClass ="factor")

temp21_5 <- read.csv("Data/Hubzones/2021/FY2021_All_Contracts_Full_5.csv", header = TRUE,
                     stringsAsFactors = FALSE, colClass ="factor")

temp21_6 <- read.csv("Data/Hubzones/2021/FY2021_All_Contracts_Full_6.csv", header = TRUE,
                     stringsAsFactors = FALSE, colClass ="factor")

temp21_7 <- read.csv("Data/Hubzones/2021/FY2021_All_Contracts_Full_7.csv", header = TRUE,
                     stringsAsFactors = FALSE, colClass ="factor")

#code to remove the necessary columns (repeat for all three dataframes in a given year)
temp2021_7 <- temp21_7 %>% select(alaskan_native_corporation_owned_firm, american_indian_owned_business, 
                                  indian_tribe_federally_recognized, native_hawaiian_organization_owned_firm, tribally_owned_firm, 
                                  minority_owned_business, asian_pacific_american_owned_business, action_date_fiscal_year, 
                                  subcontinent_asian_asian_indian_american_owned_business, black_american_owned_business,
                                  hispanic_american_owned_business, native_american_owned_business,
                                  other_minority_owned_business, contracting_officers_determination_of_business_size_code,
                                  historically_underutilized_business_zone_hubzone_firm, recipient_city_name, recipient_name, 
                                  awarding_agency_name, federal_action_obligation, total_dollars_obligated, recipient_country_code, 
                                  recipient_county_name, recipient_state_code, recipient_state_name, number_of_actions, recipient_zip_4_code, 
                                  recipient_address_line_1, recipient_address_line_2)

#code to combine all three dataframes from a single year
temp21<- temp2021_1 %>% rbind(temp2021_2) %>% 
  rbind(temp2021_3) %>% 
  rbind(temp2021_4) %>% 
  rbind(temp2021_5) %>% 
  rbind(temp2021_6) %>% 
  rbind(temp2021_7)

#code to download the newly binded dataframe from one single year into a .csv file on your computer
write.csv(temp21, file = "FederalContracts2021.csv")

#code for reading in the final singular and cleaned dataframes from each year
temp2023 <- read.csv("FederalContracts2023.csv", header = TRUE,
                     stringsAsFactors = FALSE, colClass ="factor")

# code to combine all 10 cleaned years
temp21<- temp2023 %>% rbind(temp2022) %>% 
  rbind(temp2021) %>% 
  rbind(temp2020) %>% 
  rbind(temp2019) %>% 
  rbind(temp2018) %>% 
  rbind(temp2017) %>% 
  rbind(temp2016) %>% 
  rbind(temp2015) %>% 
  rbind(temp2014) %>% 
  rbind(temp2013) 
  
# removing all non-US addresses
temp2017 %>% filter(recipient_country_code, method = c("USA"))

                    
