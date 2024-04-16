library(tidyverse)
library(dplyr)
library(tidycensus)
library(tidyselect)
library(tidygeocoder)
library(lmtest)
library(fixest)
library(sandwich)

install.packages('tidyselect')
install.packages("lmtest")
install.packages('tidygeocoder')
install.packages("fixest")
install.packages("sandwich")


#code to read in the csv gov contracting files
gsa <- read.csv("Code/GSA_Federal_Contracts_Addresses.csv")

first25 <- read.csv("Code/CensusFirst25.csv", header = TRUE,
                    stringsAsFactors = FALSE, colClass ="factor")

gsa = subset(gsa, select = -c(X))

first25 = subset(first25, select = -c(X))

MT <- gsa %>% filter(recipient_state_code == "MT")

census_MT <- MT %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


NE <- gsa %>% filter(recipient_state_code == "NE")

census_NE <- NE %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


NV <- gsa %>% filter(recipient_state_code == "NV")

census_NV <- NV %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


NH <- gsa %>% filter(recipient_state_code == "NH")

census_NH <- NH %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


NJ <- gsa %>% filter(recipient_state_code == "NJ")

census_NJ <- NJ %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


NM <- gsa %>% filter(recipient_state_code == "NM")

census_NM <- NM %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


NY <- gsa %>% filter(recipient_state_code == "NY")

census_NY <- NY %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


NC <- gsa %>% filter(recipient_state_code == "NC")

census_NC <- NC %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


ND <- gsa %>% filter(recipient_state_code == "ND")

census_ND <- ND %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


MP <- gsa %>% filter(recipient_state_code == "MP")

census_MP <- MP %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


GU <- gsa %>% filter(recipient_state_code == "GU")

census_GU <- GU %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


OH <- gsa %>% filter(recipient_state_code == "OH")

census_OH <- OH %>% geocode(street = recipient_address_line_1, 
                           city = recipient_city_name, 
                           state = recipient_state_code, 
                           method = "census", full_results = TRUE, 
                           api_options = list(census_return_type = 'geographies'))


OK <- gsa %>% filter(recipient_state_code == "OK")

census_OK <- OK %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


OR <- gsa %>% filter(recipient_state_code == "OR")

census_OR <- OR %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


PA <- gsa %>% filter(recipient_state_code == "PA")

census_PA <- PA %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


PR <- gsa %>% filter(recipient_state_code == "PR")

census_PR <- PR %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


RI <- gsa %>% filter(recipient_state_code == "RI")

census_RI <- RI %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


SC <- gsa %>% filter(recipient_state_code == "SC")

census_SC <- SC %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


SD <- gsa %>% filter(recipient_state_code == "SD")

census_SD <- SD %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


TN <- gsa %>% filter(recipient_state_code == "TN")

census_TN <- TN %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


TX <- gsa %>% filter(recipient_state_code == "TX")

census_TX <- TX %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


UT <- gsa %>% filter(recipient_state_code == "UT")

census_UT <- UT %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


VT <- gsa %>% filter(recipient_state_code == "VT")

census_VT <- VT %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


VA <- gsa %>% filter(recipient_state_code == "VA")

census_VA <- VA %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


VI <- gsa %>% filter(recipient_state_code == "VI")

census_VI <- VI %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))

WA <- gsa %>% filter(recipient_state_code == "WA")

census_WA <- WA %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


WV <- gsa %>% filter(recipient_state_code == "WV")

census_WV <- WV %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


WI <- gsa %>% filter(recipient_state_code == "WI")

census_WI <- WI %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


WY <- gsa %>% filter(recipient_state_code == "WY")

census_WY <- WY %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


AS <- gsa %>% filter(recipient_state_code == "AS")

census_AS <- AS %>% geocode(street = recipient_address_line_1, 
                            city = recipient_city_name, 
                            state = recipient_state_code, 
                            method = "census", full_results = TRUE, 
                            api_options = list(census_return_type = 'geographies'))


census50 <- census_AS %>% rbind(census_GU) %>% 
  rbind(census_MP) %>% 
  rbind(census_MT) %>% 
  rbind(census_NC) %>% 
  rbind(census_ND) %>% 
  rbind(census_NE) %>% 
  rbind(census_NH) %>% 
  rbind(census_NJ) %>% 
  rbind(census_NM) %>% 
  rbind(census_NV) %>% 
  rbind(census_NY) %>% 
  rbind(census_OH) %>% 
  rbind(census_OK) %>% 
  rbind(census_OR) %>% 
  rbind(census_PA) %>% 
  rbind(census_PR) %>% 
  rbind(census_RI) %>% 
  rbind(census_SC) %>% 
  rbind(census_SD) %>% 
  rbind(census_TN) %>% 
  rbind(census_TX) %>% 
  rbind(census_UT) %>% 
  rbind(census_VA) %>% 
  rbind(census_VI) %>% 
  rbind(census_VT) %>% 
  rbind(census_WA) %>% 
  rbind(census_WI) %>% 
  rbind(census_WV) %>% 
  rbind(census_WY)

census_all <- first25 %>% rbind(census50)
  
  
write.csv(census50, file = "CensusLast25.csv")

census50 <- read.csv("Code/Censuslast25.csv", header = TRUE,
                    stringsAsFactors = FALSE, colClass ="factor")

census50 = subset(census50, select = -c(X))


hubzones <- read.csv("Code/CensusDesignations_Cleaned.csv", header = TRUE,
                     stringsAsFactors = FALSE, colClass = "factor")


census50 <- census50 %>% unite(Full.Tract.ID, state_fips:census_tract, sep = '', remove = TRUE) 

census_all <- first25 %>% rbind(census50)

write.csv(census_all, file = "CensusAll.csv")

census_all <- read.csv("Code/CensusAll.csv", header = TRUE,
                       stringsAsFactors = FALSE)

census_all = subset(census_all, select = -c(X))

# code to filter by state

CA <- tempall %>% filter(recipient_state_code == "CA")

# code to remove a column

tempall =  subset(tempall, select = -c(X))

# code to join to dataframes

census_hubzone <- merge(census_all, hubzones, by = "Full.Tract.ID")

# write the new .csv

write.csv(census_hubzone, file = "Census_Hubzone_Master.csv")

 
census <- read.csv("Code/Census_Hubzone_Master.csv")
contracts <- read.csv("Code/General_Services_Administration_Federal_Contracts.csv")

#federalcontracts <- left_join(contracts, census, by = c("recipient_city_name", 
                                                        "recipient_state_code", 
                                                        "recipient_zip_4_code", 
                                                        "recipient_address_line_1"))

census = subset(census, select = -c(unique_id,X, X.1, X.2,id, census_block, 
                                    tiger_side, tiger_line_id, recipient_zip_4_code))

contracts = subset(contracts, select = -c(X, X.1))

census_distinct <- census %>% distinct()
#contracts_distinct <- contracts %>% distinct()

federalcontracts <- merge(contracts, census_distinct, by.x = "recipiant_address_line_1", by.y = "recipiant_address_line_1")

federalcontracts <- match(contracts, census)

allcontracts <- inner_join(census_distinct, contracts, by = c("recipient_address_line_1"), relationship = "many-to-many")

allcontracts = subset(allcontracts, select = -c(recipient_address_line_2, 
                                                input_address, match_type, 
                                                match_indicator, matched_address))

write.csv(allcontracts, file = "All_Contracts_Final.csv")

ols1l <- lm(federal_action_obligation ~ hubzone + black_firm + hubzone*black_firm, data = allcontracts)
ols3l <- lm(federal_action_obligation ~ historically = black_firm + hubzone*black_firm, data = allcontracts)
ols2l <- lm(number_of_actions ~ hubzone + black_firm + hubzone*black_firm, data = allcontracts)
summary(ols1l)
summary(ols2l)

coeftest(ols1l, vcov = vcovHC(ols1l, type = "HC1"))