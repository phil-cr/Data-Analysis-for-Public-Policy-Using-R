library(tidyverse)
library(dplyr)
library(eeptools)

# reading csv file
temp13 <- read.csv("Awards By Contractor Type (FY2013).csv", header = TRUE,
                         stringsAsFactors = FALSE, colClass ="factor")
# add column for year
temp13$year <- 2013

# reading csv file
temp14 <- read.csv("Awards By Contractor Type (FY2014).csv", header = TRUE,
                         stringsAsFactors = FALSE, colClass ="factor")
# add column for year

temp14$year <- 2014

# reading csv file
temp15 <- read.csv("Awards By Contractor Type (FY2015).csv", header = TRUE,
                         stringsAsFactors = FALSE, colClass ="factor")
# add column for year

temp15$year <- 2015

# reading csv file
temp16 <- read.csv("Awards By Contractor Type (FY2016).csv", header = TRUE,
                         stringsAsFactors = FALSE, colClass ="factor")
# add column for year

temp16$year <- 2016

# reading csv file
temp17 <- read.csv("Awards By Contractor Type (FY2017).csv", header = TRUE,
                   stringsAsFactors = FALSE, colClass ="factor")
# add column for year

temp17$year <- 2017

# reading csv file
temp18 <- read.csv("Awards By Contractor Type (FY2018).csv", header = TRUE,
                   stringsAsFactors = FALSE, colClass ="factor")
# add column for year

temp18$year <- 2018

# reading csv file
temp19 <- read.csv("Awards By Contractor Type (FY2019).csv", header = TRUE,
                   stringsAsFactors = FALSE, colClass ="factor")
# add column for year

temp19$year <- 2019

# reading csv file
temp20 <- read.csv("Awards By Contractor Type (FY2020).csv", header = TRUE,
                   stringsAsFactors = FALSE, colClass ="factor")

# add column for year 2020

temp20$year <- 2020

# reading csv file
temp21 <- read.csv("Awards By Contractor Type (FY2021).csv", header = TRUE,
                   stringsAsFactors = FALSE, colClass ="factor")
# add column for year

temp21$year <- 2021

# reading csv file
temp22 <- read.csv("Awards By Contractor Type (FY2022).csv", header = TRUE,
                   stringsAsFactors = FALSE, colClass ="factor")
# add column for year

temp22$year <- 2022

# reading csv file
temp23 <- read.csv("Awards By Contractor Type (FY2023).csv", header = TRUE,
                   stringsAsFactors = FALSE, colClass ="factor")
# add column for year

temp23$year <- 2023

summary(contracttemp)

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

