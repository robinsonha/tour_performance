## Additional updated scripts and functions for customer 
#
# The purpose of this script (and the partner functions script -> function_code_UMBIZO_120525.r) is 
# to more flexibly handle the current tour datasets and datasets in the future 
# The original request was focussed on set relaxed and active tours; what if this was to change in the future? 
# i.e. more/less tours to be considered 
#
# The purpose of this script is to account for this scenario 
#


## Set the working directory
local.dir <- "/path_to_packages/"
.libPaths(new = local.dir)

work.dir <- "/path_to_workdir/"
setwd(work.dir)

dir.create("functions_testing_120525")
setwd("functions_testing_120525")

library(data.table)
library(dplyr)
library(ggplot2)

source("../function_code_UMBIZO_120525.r")




## Sanity-check -> 
# Same analysis as initial request (relaxed vs active)
# Produces the same results 
sanity_check <- function(){
dir.create("sanity_check")
setwd("sanity_check")

tour.info <- data.frame(
Tours = c(
    "Ultimate Tour of Ireland (Jewel Tour)",
    "Wild West and North Tour (Jewel Tour)", 
    "Emerald Explorer Tour (Jewel Tour)",
    "Magical Southern Explorer (Jewel Tour)",      
    "The Full Irish",
    "Ireland to Island Tour (Jewel and Zest Tour)",
    "Great Atlantic Adventure Tour (Zest Tour)",
    "Spectacular South & West Tour (Zest Tour)"
),
TourType = c(rep("Relaxed", 4), rep("Active", 4))
)

## The first input "../../data" is the location of the data -> Needs to be either csv or tsv format 

data.df <- data.processing.tour.v2("../../data/", tour.info)

## Which days are popular 
popular_days.v2(data.df, "popular_day_function_test")

## Comparing tours 
tour_comparison_plot.v2(data.df, "relaxed_vs_active_original")

## Comparing Lead times 
lead_time_plot.v2(data.df, "lead_times_relaxed_vs_active_original")

## Clashing days 
clashing_tours.v2(data.df, "clashing_tours_relaxed_vs_active_original")




}






## Testing processing function when tour of interest is small 
short_tour_test <- function(){
dir.create("short_tour_test")
setwd("short_tour_test")

tour.df <- data.frame(
Tours = c(
"The Full Irish",
"10-Day Ultimate Self-Drive",
"Frisco Lakes goes to Ireland",
"Friso Lakes Travel Club Ireland Tour",
"April and Amber's Family Private Tour",
"Private Ultimate Tour - Jasmine Mcmaster",
"Robin Autio Private Tour",
"Christina Simon Family Tour"
),
TourType = c(rep("test",4), rep("test2", 4))
)

data.df <- data.processing.tour.v2("../../data/", tour.df)

## Which days are popular 
popular_days.v2(data.df, "popular_day_function_test")

## Comparing tours 
tour_comparison_plot.v2(data.df, "tour_short")

## Comparing Lead times 
lead_time_plot.v2(data.df, "lead_short")

## Clashing days 
clashing_tours.v2(data.df, "clash_short")


}


## The updated code is able to handle different tour inputs now 
big_vs_small_sellers <- function(){
dir.create("big_vs_small_sellers")
setwd("big_vs_small_sellers")

tour.df <- data.frame(
Tours = c(
    "Ultimate Tour of Ireland (Jewel Tour)",
    "Wild West and North Tour (Jewel Tour)", 
    "Magical Southern Explorer (Jewel Tour)",      
    "Ireland to Island Tour (Jewel and Zest Tour)",
    "Great Atlantic Adventure Tour (Zest Tour)",
    "Spectacular South & West Tour (Zest Tour)",
    "10-Day Ultimate Self-Drive",
    "Frisco Lakes goes to Ireland",
    "Friso Lakes Travel Club Ireland Tour",
    "April and Amber's Family Private Tour",
    "Private Ultimate Tour - Jasmine Mcmaster",
    "Robin Autio Private Tour",
    "Christina Simon Family Tour"
),
TourType = c(rep("Big Sellers", 6), rep("Smaller Sellers", 7))
)

data.df <- data.processing.tour.v2("../../data/", tour.df)

## Which days are popular 
popular_days.v2(data.df, "popular_day")

## Comparing tours 
tour_comparison_plot.v2(data.df, "big_vs_small")

## Comparing Lead times 
lead_time_plot.v2(data.df, "lead_test_big_vs_small")

## Clashing days 
clashing_tours.v2(data.df, "clash_test_big_vs_small")




}
