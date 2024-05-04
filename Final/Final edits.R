library(tidyverse)
library(dplyr)
library(tidycensus)
library(stringr)
library(panelr)

install.packages("panelr")
install.packages()
install.packages(tidygeocoder)
install.packages('stringr')

# group or filter by year, create a new column, filter to just be population in a given year
#if year == "2018" then, 

#code to read in the csv gov contracting files

census <- read.csv("Census_Hubzone_Master.csv", header = TRUE,
                   stringsAsFactors = FALSE, colClass ="factor")

contracts <- read.csv("General_Services_Administration_Federal_Contracts.csv", header = TRUE,
                      stringsAsFactors = FALSE, colClass ="factor")

# try the match and filter and then join by two states or something to see if the issue is in the code or
# if it's in the 

group_by()


contractsfinal <- contractsfinal %>% 
  mutate(`2018_DP05_0001E` = as.numeric(`2018_DP05_0001E`)) %>% 
  mutate(action_date_fiscal_year = as.numeric(action_date_fiscal_year)) %>% 
  
  
  
  
  
  contractsfinal <- contractsfinal %>% mutate(total_population_year = `2018_DP05_0001E`[action_date_fiscal_year == 2018],
                                              total_population_year = `2019_DP05_0001E`[action_date_fiscal_year == 2019],
                                              total_population_year = `2020_DP05_0001E`[action_date_fiscal_year == 2020],
                                              total_population_year = `2021_DP05_0001E`[action_date_fiscal_year == 2021],
                                              total_population_year = `2022_DP05_0001E`[action_date_fiscal_year == 2022]) 

#code to read in the files 

allcontracts <- read.csv("/Users/lenaharris/Desktop/Data Analysis R/Research Project/All_Contracts_Final.csv", header = TRUE,
                         stringsAsFactors = FALSE, colClass ="factor")

businesses <- read.csv("/Users/lenaharris/Desktop/ABSCS2021.AB2100CSA01_2024-05-01T142816/ABSCS2021.AB2100CSA01-Data.csv", header = TRUE,
                       stringsAsFactors = FALSE, colClass ="factor")

c2020 <- read.csv("/Users/lenaharris/Desktop/ACSDP5Y2020.DP03_2024-04-30T115136/ACSDP5Y2020.DP03-Data.csv", header = TRUE,
                  stringsAsFactors = FALSE, colClass ="factor")

c2021 <- read.csv("/Users/lenaharris/Desktop/ACSDP5YSPT2021.DP03_2024-04-30T121202/ACSDP5YSPT2021.DP03-Data.csv", header = TRUE,
                  stringsAsFactors = FALSE, colClass ="factor")

c2022 <- read.csv("/Users/lenaharris/Desktop/ACSDP5Y2022.DP03_2024-04-30T121223/ACSDP5Y2022.DP03-Data.csv", header = TRUE,
                  stringsAsFactors = FALSE, colClass ="factor")

#code to remove the necessary columns (repeat for all year dataframes)
c18 <- c18 %>% select(GEO_ID, NAME, DP05_0001E, DP05_0018E, DP05_0037E, DP05_0038E, DP05_0039E, DP05_0044E, DP05_0052E,
                      DP05_0058E, DP05_0070E) 

c2018 <- c2018 %>% select(GEO_ID, NAME, DP03_0008E, DP03_0009E, DP03_0049E, DP03_0051E, DP03_0062E, DP03_0063E,
                          DP03_0072E, DP03_0088E, DP03_0092E, DP03_0128E)

#adding columns with years 

paste(2019, colnames(c2019), sep = "_")

#adding years to each column, this just adds it in front of each observation

c18$DP05_0001E <-paste("2018", c18$DP05_0001E, sep = "_")

#joining all demographic data
censusecon <-  inner_join(c2018,c2019, by = c("GEO_ID"), relationship = "one-to-one") %>% 
  inner_join(c2020, by = c("GEO_ID"), relationship = "one-to-one") %>% 
  inner_join(c2021, by = c("GEO_ID"), relationship = "one-to-one") %>% 
  inner_join(c2022, by = c("GEO_ID"), relationship = "one-to-one") 

#dropping unecessary columns
censusecon = subset(censusecon, select = -c(NAME.x, Year.x, NAME.y, Year.y, NAME.x.x, Year.x.x, NAME.y.y, Year.y.y))

censusall <- inner_join(censusdem, censusecon, by = "GEO_ID", relationship = "many-to-many") 

censusdem <- c18 %>% rbind(c19) %>% rbind(c20) %>% rbind(c21) %>% rbind(c22) 

censusecon <- c2018 %>% rbind(c2019) %>% 
  rbind(c2020) %>% rbind(c2021) %>% rbind(c2022)

censusall$Year.x <- as.factor(censusall$Year.x) 

str(censusall)

censusall = subset(censusall, select = -c(`as.factor(Year.x)`))

allcontracts$Full.Tract.ID = paste(rep(0), allcontracts$Full.Tract.ID, sep = "")

allcontracts = subset(allcontracts, select = -c(X, recipient_state_code.y, recipient_state_code.x, 
                                                recipient_city_name.y, recipient_zip_4_code, 
                                                awarding_agency_name, recipient_country_code, 
                                                recipient_state_name))

contractsfinal <- inner_join(allcontracts, censusall, by =c("Full.Tract.ID" = "GEO_ID"), relationship = "one-to-many")

contractsfinal <- inner_join(fedcontracts, cencusdistinct, by =c("Full.Tract.ID" = "GEO_ID", "action_date_fiscal_year" = "Year.x"),
                             relationship = "many-to-many")

censusall$GEO_ID <- substr(censusall$GEO_ID, 1, nchar(as.character(censusall$GEO_ID)))

#code to remove all years besides 2018-2022

contracts <- allcontracts %>% filter(!action_date_fiscal_year %in% c("2013", "2014", "2015", "2016", "2017"))

#code to remove a row 
censusall <- censusall[-1,]

censusall = subset(censusall, filter = -c(NAME.x != "Geographic Area Name")) 

#code to have no duplicatest of rows
cencusdistinct <- censusall %>% distinct(GEO_ID, .keep_all = TRUE)

str(censusall)

#code to add a leading zero when states equal AL, AK, AZ, AR, CA, CO, CT

fedcontracts <- contracts %>%
  mutate(Full.Tract.ID = case_when(
    State %in% c("AL", "AK", "AZ", "AR", "CA", "CO", "CT") ~ paste0("0", Full.Tract.ID),
    TRUE ~ Full.Tract.ID))


write.csv(contractsfinal, file = "/Users/lenaharris/Desktop/Census_Dem_Econ_Contracts_Final.csv")

#code to merge the two

#code to get rid of columns dataframes 

c18 = subset(c18, select = -c(Year))

c18 <- as_tibble(c18)

c22 <- c22 %>% rename(`DP05_0001E` = "2022_DP05_0001E", 
                      `DP05_0018E` = "2022_DP05_0018E", 
                      `DP05_0037E` = "2022_DP05_0037E", 
                      `DP05_0038E` = "2022_DP05_0038E", 
                      `DP05_0039E` = "2022_DP05_0039E", 
                      `DP05_0044E` = "2022_DP05_0044E", 
                      `DP05_0052E` = "2022_DP05_0052E", 
                      `DP05_0058E` = "2022_DP05_0058E", 
                      `DP05_0070E` = "2022_DP05_0070E")

c2018 <- c2018 %>% rename(`DP03_0088E` = "2018_DP03_0088E",
                          `DP03_0008E` = "2018_DP03_0008E",
                          `DP03_0009E` = "2018_DP03_0009E",       
                          `DP03_0008E` = "2018_DP03_0008E",
                          `DP03_0049E` = "2018_DP03_0049E", 
                          `DP03_0051E` = "2018_DP03_0051E", 
                          `DP03_0062E` = "2018_DP03_0062E", 
                          `DP03_0063E` = "2018_DP03_0063E",
                          `DP03_0072E` = "2018_DP03_0072E", 
                          `DP03_0088E` = "2018_DP03_0088E", 
                          `DP03_0092E` = "2018_DP03_0092E", 
                          `DP03_0128E` = "2018_DP03_0128E")


ALcontracts <- contracts %>% filter(recipient_state_code == "AL") 

ALcensus <- census %>% filter(recipient_state_code == "AL") 

ALallcontracts <- inner_join(distinct, ALcontracts, by = c("recipient_address_line_1"), relationship = "many-to-many")

distinct <- ALcensus %>% distinct()

levels(federalcontracts$hubzone)

# code to drop a column
census = subset(census, select = -c(recipient_zip_4_code))



str(census)
str(contracts)

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

states <- read.csv("censusFirst25.csv", header = TRUE,
                   stringsAsFactors = FALSE, colClass ="factor")

hubzones <- read.csv(".csv", header = TRUE,
                     stringsAsFactors = FALSE, colClass ="factor")

#code to download the newly binded dataframe from one single year into a .csv file on your computer
write.csv(temp13, file = "FederalContracts2014.csv")

#code for reading in the final singular and cleaned dataframes from each year
tempall <- read.csv("General_Services_Administration_Federal_Contracts.csv", header = TRUE,
                    stringsAsFactors = FALSE, colClass ="factor")

#getting census tract ids from addresses
addresses <- tempall %>% 
  select(recipient_address_line_1,
         recipient_city_name, 
         recipient_state_code,
         recipient_zip_4_code) %>%
  mutate(unique_id = row_number()) %>%
  select(unique_id, everything())

write.csv(addresses, file = "GSA_Federal_Contracts_Addresses.csv")

temp13 %>% mutate(unique_id = row_number())

sum13 <- summary(temp13$recipient_country_code)

as.factor(temp13)

contracts <- allcontracts %>% 
  mutate(Full.Tract.ID = as.numeric(as.character(Full.Tract.ID)))

str(contracts)

tempcontracts10 <- contracts %>% filter(Full.Tract.ID < 10000000000)

tempcontracts11 <- contracts %>% filter(Full.Tract.ID > 10000000000)

contracts <- tempcontracts11 %>% rbind(tempcontracts10)

tempcontracts10 <- tempcensus10 %>% paste0("0", Full.Tract.ID)

summary <- summarise(tempcensus10$Full.Tract.ID)

tempcensus0s <- tempcensus10 %>% str_pad(Full.Tract.ID, width = "", side ="left", pad = "0")

tempcensus10$Full.Tract.ID = paste(rep(0), tempcensus10$Full.Tract.ID, sep = "")

write.csv(contractsfinal, file = "/Users/lenaharris/Desktop/Contracts_Census_Final_Dataset.csv")

#coding the linear regression

reg1 <- contractsfinal %>% lm(total_dollars_obligated ~ hubzone, 
                              alaskan_native_firm, 
                              american_indian_firm, 
                              indian_tribe_firm,
                              native_hawaiian_firm, 
                              tribal_firm, asian_pacific_firm, 
                              subcontinentasian_asian_indianamerican_tribe_firm,
                              black_firm, 
                              hispanic_american_firm, 
                              native_american_firm, 
                              other_minority_firm, 
                              DP05_0001E, 
                              DP05_0018E)


reg2 <- ALallcontracts %>% lm(total_dollars_obligated ~ hubzone, black_firm)

?lm()

#Code to make the column 10 digits
decennial$GEO_ID <- substr(decennial$GEO_ID, 10, nchar(decennial$GEO_ID))
