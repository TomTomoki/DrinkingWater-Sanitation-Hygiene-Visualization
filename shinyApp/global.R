library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggiraph)

source("www/functions/helper_functions.R")

df_water_map <- read_csv('www/datasets/water_map_reduced.csv')

water1 <- read_csv('www/datasets/water1.csv')
water2 <- read_csv('www/datasets/water2.csv')

sanitation1 <- read_csv('www/datasets/sanitation1.csv')
sanitation2 <- read_csv('www/datasets/sanitation2.csv')

hygiene1 <- read_csv('www/datasets/hygiene1.csv')