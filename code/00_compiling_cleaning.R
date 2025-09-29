#This script is for cleaning and combinining bird survey files into a single data frame

library(tidyverse)

#One issue: there are duplicated csv files for the same survey date on ArcGIS Online

#Tools for comparing data sets:

# dataCompareR package
# waldo package
# daff package- creates visualization of differences 

# or just use all.equal function from base R

library(dataCompareR)
library(waldo)
library(daff)

#test these tools duplicated csvs

CL_a <- read_csv(file = "")

CL_b <- read_csv(file = "")

all.equal(CL_a, CL_b)
#returns TRUE

#compare from waldo package
waldo::compare(CL_a, CL_b)

#returns "No differences"

