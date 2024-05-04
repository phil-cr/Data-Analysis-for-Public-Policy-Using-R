library(tidyverse)
library(dplyr)
library(tidycensus)
library(stringr)
library(panelr)

install.packages("panelr")
install.packages()
install.packages(tidygeocoder)
install.packages('stringr')

federalcontracts <- read.csv("Data/Federal_Contracts_Census_Demographics_Final.csv", header = TRUE,
                             stringsAsFactors = FALSE, colClass ="factor")

ols1l <- lm(federal_action_obligation ~ hubzone + black_firm + hubzone*black_firm, data = allcontracts)
ols3l <- lm(federal_action_obligation ~ historically = black_firm + hubzone*black_firm, data = allcontracts)
ols2l <- lm(number_of_actions ~ hubzone + black_firm + hubzone*black_firm, data = allcontracts)
summary(ols1l)
summary(ols2l)

coeftest(ols1l, vcov = vcovHC(ols1l, type = "HC1"))

reg1 <- federalcontracts %>% lm(total_dollars_obligated ~ hubzone, 
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
                              small_business,
                              total_pop,
                              median_age,
                              median_household_income,
                              median_earnings)