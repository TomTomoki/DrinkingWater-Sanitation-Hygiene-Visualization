library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggiraph)

source("www/functions/helper_functions.R")

df_water_map <- read_csv('www/datasets/water_map_reduced.csv')
df_sanitation_map <- read_csv('www/datasets/sanitation_map_reduced.csv')
df_hygiene_map <- read_csv('www/datasets/hygiene_map_reduced.csv')