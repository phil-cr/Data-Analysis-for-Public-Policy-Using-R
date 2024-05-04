library(tidyverse)
library(dplyr)
library(tidycensus)
library(stringr)
library(panelr)
library(fixest)
library(stargazer)

install.packages("panelr")
install.packages()
install.packages(tidygeocoder)
install.packages('stringr')
install.packages("fixest")

federalcontracts <- read.csv("Data/Federal_Contracts_Census_Demographics_Final.csv")

#create dummy variables
federalcontracts$alaskan_native_firm <- factor(federalcontracts$alaskan_native_firm, labels = c("No", "Yes"))
federalcontracts$american_indian_firm <- factor(federalcontracts$american_indian_firm, labels = c("No", "Yes"))
federalcontracts$indian_tribe_firm <- factor(federalcontracts$indian_tribe_firm, labels = c("No", "Yes"))
federalcontracts$native_hawaiian_firm <- factor(federalcontracts$native_hawaiian_firm, labels = c("No", "Yes"))
federalcontracts$tribal_firm <- factor(federalcontracts$tribal_firm, labels = c("No", "Yes"))
federalcontracts$minority_firm <- factor(federalcontracts$minority_firm, labels = c("No", "Yes"))
federalcontracts$asian_pacific_firm <- factor(federalcontracts$asian_pacific_firm, labels = c("No", "Yes"))
federalcontracts$subcontinentasian_asian_indianamerican_tribe_firm <- factor(federalcontracts$subcontinentasian_asian_indianamerican_tribe_firm, labels = c("No", "Yes"))
federalcontracts$black_firm <- factor(federalcontracts$black_firm, labels = c("No", "Yes"))
federalcontracts$hispanic_american_firm <- factor(federalcontracts$hispanic_american_firm, labels = c("No", "Yes"))
federalcontracts$native_american_firm <- factor(federalcontracts$native_american_firm, labels = c("No", "Yes"))
federalcontracts$other_minority_firm <- factor(federalcontracts$other_minority_firm, labels = c("No", "Yes"))
federalcontracts$small_business <- factor(federalcontracts$small_business, labels = c("No", "Yes"))
federalcontracts$certified_hubzone_firm <- factor(federalcontracts$certified_hubzone_firm, labels = c("No", "Yes"))




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

reg1 = lm(total_dollars_obligated ~ hubzone +
            small_business +
            alaskan_native_firm +
            american_indian_firm +
            indian_tribe_firm +
            other_minority_firm +
            median_age +
            median_household_income +
            median_earnings +
            native_hawaiian_firm +
            tribal_firm +
            asian_pacific_firm +
            subcontinentasian_asian_indianamerican_tribe_firm +
            black_firm +
            hispanic_american_firm +
            native_american_firm +
            self_employed_status +
            hispanic_latino_pop +
            multiracial_pop +
            asian_pop +
            native_hawaiian_pacific_islander_pop +
            american_indian_alaskan_native +
            black_pop +
            white_pop +
            median_age,
          data = federalcontracts, weight = total_pop)

summary(reg1)

#specify model with FEs for country and wave, w/ robust SEs
reg_fe1 <- feols(total_dollars_obligated ~ hubzone + hubzone*black_firm, 
                 data = federalcontracts,
                 weight = federalcontracts$total_pop)

summary(reg_fe1)

stargazer(reg_fe1, type = "html")

stargazer(reg_fe1)