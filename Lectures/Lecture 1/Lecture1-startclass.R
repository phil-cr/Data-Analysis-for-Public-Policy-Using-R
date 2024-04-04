################################################################################
##
## [ PROJ ] Lecture1-startclass: Getting familiar with RStudio
## [ FILE ] Lecture1-startclass.r
## [ AUTH ] INSTRUCTOR FILE 
## [ INIT ] Jan 16, 2024 
##
################################################################################

## -----------------------------------------------------------------------------
## 0. create an R project that includes this R script (Lecture1-startclass.r)
## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
#1. look around and get our bearings.
## -----------------------------------------------------------------------------

#get working directory
  getwd()


## -----------------------------------------------------------------------------
## 2. install and load the gapminder package 
## -----------------------------------------------------------------------------

#first we have to install the package
  install.packages("gapminder")

#load this package
  library(gapminder)

## -----------------------------------------------------------------------------
## 3. Inspect gapminder data frame (in the gapminder package) w base R functions
##    (this exercise is based on STAT545 by Jenny Bryan)
## -----------------------------------------------------------------------------

#let's assign the gapminder data frame to a new data frame called gap
  gap <- gapminder

#let's use some functions to inspect gapminder dataframe (an object in the gapminder package)
  str(gap)
  
  #access built in help function for str()
    ?str

  #the function class() tells us what class(es) an object is assigned to
    class(gap)
    
  #some other functions
    head(gap)
    head(gap, n = 10)
    View(gap)
    
    
## -----------------------------------------------------------------------------
## 4. Use some base R functions to perform some very basic exploratory analysis
## -----------------------------------------------------------------------------

#let's use some more base R functions to understand the data structure
  dim(gap)
  ncol(gap)
  nrow(gap)
  names(gap)

#assign the number of columns in gap to a new object 
  num_of_vars <- ncol(gap)
  num_of_vars
  
#get summary statistics
  summary(gap)

#let's plot the relationship of year (x) vs lifeExp (y) using base R
  plot(lifeExp ~ year, gap, ylab = 'life expectancy')
  plot(lifeExp ~ year, gap, ylab = 'life expectancy')
  plot((y = lifeExp) ~ (x = year), data = gap)
  plot(gap$year, gap$lifeExp)
  
#what is a factor? let's look at some base R functions to figure it out
  summary(gap$continent)
  levels(gap$continent)
  nlevels(gap$continent)

  table(gap$year)  
  freq_byyear <- table(gap$year)  

  barplot(freq_byyear)  
  