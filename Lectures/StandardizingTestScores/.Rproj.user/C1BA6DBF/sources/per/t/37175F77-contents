################################################################################
##
## [ PROJ ] Standardizing variables 
## [ FILE ] StandardizingTestScores.r
## [ AUTH ] < Philip Crane >
## [ INIT ] < March 19, 2024 >
##
################################################################################

# This short exercise uses survey data from the Japanese goverment
# for each of 47 prefectures from 2007-2021 (with some missing years).
# It was obtained as a part of a recent student project exploring the effect
# of the 2011 earthquake on educational outcomes.

# Our focus is on average scores (percent correct) on a standardized Japanese
# language test for elementary school students.


## -----------------------------------------------------------------------------
## libraries
## -----------------------------------------------------------------------------

library(tidyverse)


## -----------------------------------------------------------------------------
## check directory paths
## -----------------------------------------------------------------------------




## -----------------------------------------------------------------------------
## describe distribution of test scores by year & construct standardized measures
## -----------------------------------------------------------------------------

#read in data w/info on standardized Japanese test scores by prefecture-year

scores <- read_csv("Data/elementary_score.csv") %>% 
  select(wave, Prefecture, Japanese) %>% 
  filter(wave != 2011, wave != 2020) %>% 
  filter(wave < 2010) %>% 
  filter(Prefecture != "AverageAll") %>% 
  filter(Prefecture != "AveragePublic")

    #is this wide-form or long-form panel data?
  
  
#explore the distribution of test scores by year (wave)
  
  #get summary statistics to describe distribution

scores %>% 
  group_by(wave) %>% 
  summarise(wave_mean = mean(Japanese, na.rm = TRUE),
            wave_sd = sd(Japanese, na.rm = TRUE))
  
  #visualize distribution

scores %>% 
  ggplot() +
  aes(x = Japanese) +
  geom_histogram() +
  facet_wrap(~ wave, nrow = 3)
  

#get *standardized* measure of test scores
#this changes the units to standard deviations of test scores (by year)
#what problem does this solve?
  
  #manually construct standardized measure

scores_z <- scores %>% 
  group_by(wave) %>% 
  summarise(wave_mean = mean(Japanese, na.rm = TRUE),
            wave_sd = sd(Japanese, na.rm = TRUE),
            Japanese_z = (Japanese - wave_mean) / wave_sd) %>% 
  mutate_if(is.numeric, round, 2)
  
    #can also use the scale() function along with standardize()
  
    #check distribution of new standardized measure by year

scores_z %>% 
  group_by(wave) %>% 
  summarise(wave_mean = mean(Japanese_z),
            wave_sd = sd(Japanese_z))
  
  #visualize new, standardized distribution of scores by year

scores_z %>% 
  ggplot() +
  aes(x = Japanese_z) +
  geom_histogram() +
  facet_wrap(~wave, nrow = 3)
  
    #what sort of visualization would help us see variation across all 14 years?
 
   
  #question: what if we didn't group by year?
    #i.e. what if we did the standardization by pooling all years?

    
    #in subsequent regression analysis, 
    #how would wave FEs effectively limit the test score variation that is used?
    
    #how about prefecture FEs?
    
    
    