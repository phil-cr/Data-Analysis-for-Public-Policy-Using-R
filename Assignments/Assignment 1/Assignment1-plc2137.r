################################################################################
##
## [ PROJ ] Assignment1
## [ FILE ] Assignment1-PLC2137.r
## [ AUTH ] Philip Crane
## [ DATE ] 1/17/24
##
################################################################################

## Assignment 1: Getting familiar with R & RStudio

## Include the code to answer the following
## Use comments to organize and provide your responses to questions
## Submit only this R script (Assignment1.r)


## -----------------------------------------------------------------------------
## 0. create a new R project called Assignment1
## -----------------------------------------------------------------------------

# This is for your internal project management, do not submit your .rproj file

  getwd()

## -----------------------------------------------------------------------------
## 1. load the gapminder package (to access the gapminder data)
## -----------------------------------------------------------------------------

  install.packages("gapminder")
  library(gapminder)

## -----------------------------------------------------------------------------
## 2. a) use the str() function to give an overview of the gapminder data frame
##        (found in the gapminder package)
##    b) how many observations and variables are there?
## -----------------------------------------------------------------------------

# a
  gap <- gapminder
  str(gap)  
  
#b 
  ## This dataframe contains 1,704 observations across 6 variables.



## -----------------------------------------------------------------------------
## 3. a) what is the average gdpPercap across all observations in the dataset
##    b) use ?gapminder to view the documentation & find the units for gdpPercap
##    c) how would you intepret this mean, what is it the mean of?
##       HINT: try executing View(gapminder)
## -----------------------------------------------------------------------------

#a
  summary(gap$gdpPercap)
  mean(gap$gdpPercap)
  ## The mean of gpdPercap is 7215.327.
  
#b. 
  ?gapminder
  ## "GDP per capita (US$, inflation-adjusted)" Source: R Documentation
  
#c
  View(gapminder)
  # $7,215.3 is the mean GDP per capita in US dollars (inflation-adjusted)
  # of all observed nations pulled every 5 years from 1952 to 2007.

## -----------------------------------------------------------------------------
## 4. a) plot year vs. gdpPercap
##    b) what does the plot say about economic growth over time (1 sentence)
##    c) describe what a better visualization might look like 
##       (don't worry about how to do this in R... we'll get there!)
##       TIP: try typing your answer using ENTER to fit in the margins,
##            then select the text & use CTRL+SHIFT+C to comment multiple lines
## -----------------------------------------------------------------------------

#a
  plot(gdpPercap ~ year, gap)

#b
  # In general, GDP per capita increases every five years across all observed
  # countries

#c
  # A better way to visualize this could be via a line plot, which makes more
  # sense if tracking changes for certain countries over time. Depending on the
  # purpose behind the visualization, it could be helpful to color code each 
  # nation by continent to identify more generalization trends.


## -----------------------------------------------------------------------------
## 5. create a barplot showing the number of observations in each continent
##    a) start by using the table function with continent as its argument,
##       assign the results of this function to a new object
##    b) next pass the object you created as an argument to the barplot function
## -----------------------------------------------------------------------------

#a
  table(gap$continent)
  freq_bycont <- table(gap$continent)
  
#b
  barplot(freq_bycont)
