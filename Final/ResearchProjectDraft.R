library(tidyverse)
library(dplyr)
library(tidycensus)

#code for reading in the final singular and cleaned dataframes from each year
temp2013 <- read.csv("FederalContracts2013.csv", header = TRUE,
                         stringsAsFactors = FALSE, colClass ="factor")

temp2014 <- read.csv("FederalContracts2014.csv", header = TRUE,
                         stringsAsFactors = FALSE, colClass ="factor")

temp2015 <- read.csv("FederalContracts2014.csv", header = TRUE,
                         stringsAsFactors = FALSE, colClass ="factor")

#code to read in the csv gov contracting files
temp13_1 <- read.csv("FY2013_All_Contracts_Full_2013_1.csv", header = TRUE,
                         stringsAsFactors = FALSE, colClass ="factor")

temp13_2 <- read.csv("FY2013_All_Contracts_Full_2013_2.csv", header = TRUE,
                         stringsAsFactors = FALSE, colClass ="factor")

temp15 <- read.csv("FY2013_All_Contracts_Full_2013_3.csv", header = TRUE,
                         stringsAsFactors = FALSE, colClass ="factor")

#code to remove the necessary columns (repeat for all three dataframes in a given year)
temp2013_1 <- temp13_1 %>% select(alaskan_native_corporation_owned_firm, american_indian_owned_business, 
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
temp14<- temp2014_1 %>% rbind(temp2014_2) %>% 
  rbind(temp2014_3)

#code to download the newly binded dataframe from one single year into a .csv file on your computer
write.csv(temp13, file = "FederalContracts2014.csv")

# getting rid of the dollar signs
contract <- contracttemp %>% 
  mutate(across(ends_with("Dollars"), ~gsub("\\$", "", .)))

# getting rid of the commas
contracts <- contract %>% 
  mutate(across(ends_with("Dollars"), ~gsub("\\,", "", .))) %>% 
  mutate(across(ends_with("Actions"), ~gsub("\\,", "", .))) 

# treating it as a factor in order to do summary stats
summary <- contracts %>% 
  group_by(American.Indian.Owned.Dollars)
  summarise(as.factor(American.Indian.Owned.Dollars))

?as.numeric

