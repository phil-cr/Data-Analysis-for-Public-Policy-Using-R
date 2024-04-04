################################################################################
##
## [ PROJ ] < Assessing gender wage gaps using the Current Population Survey >
## [ FILE ] < Lecture2-startclass.R >
## [ INIT ] < Jan 23, 2024 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------

library(tidyverse)

## ---------------------------
## directory paths
## ---------------------------

getwd()


## -----------------------------------------------------------------------------
## 1. load and inspect CPS data: 
##    
##    a. inspect the data frame and data types for each column
##          - make sure to inspect the age, sex, race, college columns
##
##    b. use the mutate function to create new column for sex:
##        sex.fac = as.factor(sex),
##        check if it worked by calling the str() function
##
##    c. include sex.fac in a new data frame called cps.temp1, 
##       also create factors for race and college education,
##        use a pipe to exclude the columns for serial and ind
##        after creating cps.temp1, print the first 5 observations
##
##    d. inspect race.fac, sex.fac, and college.fac using the levels() function,
##        what package is the levels() function located in?
##
##    e. use filter() to only include rows only for June 2022,
##        store as a new object cps_2022,
##        print the first 5 observations,
##        confirm your data only includes observations for 2022
##
##    f. remove the cps.temp1 object from memory using the rm() function
## -----------------------------------------------------------------------------

# load the CPS data frame and store as an object 'cps'
  cps <- read.csv("cps_june_22-23.csv")

# this time we will remove NA values from the outset in the interest of time
# general rule: need to understand why NA values arise before deciding what to do 
  cps <- na.omit(cps) #remove all observations with NA values
  
  
# 1a. 
  str(cps)
  View(cps)
  summary(cps$age)
  summary(cps$race)
  
  #we can also inspect the data frame by double-clicking in the Environment tab
  
 
  
  
# 1b.
  mutate(cps, sex.fac = as.factor(sex))
  str(mutate(cps, sex.fac = as.factor(sex)))
  
# 1c.
  cps.temp1 <- cps %>% 
    mutate(sex.fact = as.factor(sex),
           race.fac = as.factor(race),
           college.fac = as.factor(college)) %>% 
    select(-serial, -ind)
  
  head(cps.temp1, n = 5)
  
  #some helpful syntax for later: 
  #subset the first row of cps.temp1
  cps.temp1[1,]
  #subset the cell in the first row, 4th column (i.e. first obs for age)
  cps.temp1[1,4]
  
  
# 1d.
  levels(cps.temp1$sex.fac)

  
  
  
# 1e.
  cps_2022 <- cps.temp1 %>% 
    filter(year == 2022)

  head(cps_2022)
  
  
# 1f.
  rm(cps.temp1)

  
## -----------------------------------------------------------------------------
## 2. Describe the cps_2022 data frame
##
##    a. what is the unit of observation (or unit of analysis)? 
##
##    b. how many individuals are observed? from how many households?
##
##    c. what is the average age of individuals in the sample? youngest and oldest person?
## -----------------------------------------------------------------------------
  
# 2a.
 
  
  #NOTE: don't include all of your inspection commands in your R Markdown submission
  #      I've included view() as a reminder, but it should never be in your submission
  #      neither should clunky str() output, be selective about the output you show!
  
  
# 2b. HINT: we need to calculate a statistic for a given column of data
  ?summarise
  
  summarise(cps_2022, n_distinct(personid))
  summarise(cps_2022, n_distinct(hhid))
  
  #summarise is in the dplyr package which is part of the tidyverse
  cps_2022 %>% FILL IN CODE HERE

  
  #short way: use in your .rmd file to reference code inline to answer questions
  n_distinct(cps_2022$personid)
  n_distinct(cps_2022$hhid)
  #NOTE: you'll lose points for hardcoding numerical answers in your assignments!
  
  
# 2c. 
  #use summarise with multiple arguments, one for each statistic
  #try ?summarise to find the syntax for different summary statistics
  
  cps_2022 %>% 
    summarise(avg_age = mean(age),
              min_age = min(age),
              max_age = max(age))

  
## -----------------------------------------------------------------------------
## 3. Let's now look at earnings per week for different groups in June 2022
##
##    a. find the observation for the top weekly earnings using the summarise() function, 
##        assign this to a new object called max_earnings
##
##    b. find max weekly earnings using the arrange function instead of summarise
##
##    c. use the filter function to subset for the observation with max weekly earnings
##        (don't hardcode the max earnings to filter on, refer to the max_earnings object from a),
##        store in new data frame cps_max_earn,
##        confirm it worked
##
##    d. what is the age, sex, and race of the top weekly earner in the sample?
##
##    e. list the age, sex, and race of the top 10 weekly earners in the sample.
##
##    f. how many individuals earned more than $2000 in weekly earnings?
## -----------------------------------------------------------------------------
  
# 3a. 
  cps_2022 %>% summarise(max_earning = max(earnweek))
  #Documentation: https://cps.ipums.org/cps-action/variables/EARNWEEK#codes_section
  
  #assign to object
  max_earnings <- cps_2022 %>% 
    summarise(max_earning = max(earnweek))

    
# 3b. 
  cps_2022 %>% 
    arrange(desc(earnweek)) %>% 
    select(earnweek) %>% 
    head(n=1)
  
  ?select
  
# 3c. 
  
  #HINT: your condition needs to refer to the max weekly earnings.
  # you created max_earnings as a data frame in part a,
  # so in your filter() call refer to the value from max_earnings that you want to filter on
  # this requires subsetting the appropriate element from that data frame
  # (in this case just the 1st row of a 1-row data frame).
  # See Lecture2.1/Section 4.2 for examples of how to subset matrix/df elements
  
  cps_max_earn <- cps_2022 %>% 
    filter(earnweek == max_earnings[1,])
  
  summary(cps_max_earn$earnweek)
  
# 3d. 

  
  #obviously better to refer to column names when you know them as we do here
  #alternatively, you could refer to the elements you want within the cps_max_earn object
  #by calling the 4th, 5th, and 6th columns from the first row
  cps_max_earn[1,4:6]
  
  
# 3e.
  
  cps_2022 %>% 
    arrange(desc(earnweek)) %>% 
    select(age, sex, race) %>% 
    head(n=10)
  
# 3f.
  
  #HINT: use the nrow() function to return the number of observations

  cps_2022 %>% 
    filter(earnweek > 2000) %>% 
    nrow()
  
## -----------------------------------------------------------------------------
## 4. Let's look at wage gaps between males and females:
##
##    a. use the filter function to subset observations for males, 
##        assign to new data frame, cps_2022_male,
##        sort in descending order of weekly earnings
##        check if it worked
##
##    b. repeat part a for females and create a new data frame, cps_2022_female
##
##    c. use summarise to find mean, min & max for males and females, separately
##        name each statistic appropriately (i.e. name each column in the 1-row table of stats)
##        what is the gender gap in mean weekly earnings?
##
##    d. What is the wage gap in weekly earnings ($) between 
##        white males and Black females?
##
##    e. What is the wage gap in weekly earnings ($) between 
##        college educated white males and college educated Black females? 
## -----------------------------------------------------------------------------
  
# 4a. 
  
  #HINT: try levels(cps_2022$sex.fac) function to get the factor labels to filter on
  
  cps_2022_male <- FILL IN CODE HERE

  
# 4b.
  cps_2022_female <- FILL IN CODE HERE

  
# 4c.

  #HINTt: use the newly created data frames for males and females to generate stats
  

  
  #calculate gender gap in weekly earnings:
  #HINT: use base R to calculate the wage gap directly in your .rmd file

  
# 4d. 
  
  #HINT: use levels() again to get the factor labels
  
  #create object w/observations for white males
  cps_2022_wh_male <-  FILL IN CODE HERE
  
  
  #create object w/observations for Black females
  cps_2022_bl_female <-  FILL IN CODE HERE 
  
  
  #calculate wage gap between white males and Black females

  
  #Q: do you see any potential issues with the demographic groups we just used?
  
  

# 4e. 
  
  #create object w/observations for white college educated males
  cps_2022_wh_male_college <- FILL IN CODE HERE

  
  #create object w/observations for Black college educated females
  cps_2022_bl_female_college <- FILL IN CODE HERE
  
  
  #calculate wage gap


#NOTE: above exercises are done w/weekly earnings, but can easily be converted to hourly wages
