library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggiraph)

source("www/functions/helper_functions.R")

df_water_map <- read_csv('www/datasets/water_map_reduced.csv')

df_water <- read_csv('www/datasets/water1.csv')
