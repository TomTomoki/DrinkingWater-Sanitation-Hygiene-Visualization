library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggiraph) #interactive ploting
library(shinythemes)
library(fontawesome) #add icons
library(plotly) #interactive ploting
library(forecast) #for forecasting
library(tsbox) #for forecasting
library(ggfortify) #for forecasting plot
library(markdown)

source("www/functions/helper_functions.R")

df_water_map <- read_csv('www/datasets/water_map_reduced.csv')
df_sanitation_map <- read_csv('www/datasets/sanitation_map_reduced.csv')
df_hygiene_map <- read_csv('www/datasets/hygiene_map_reduced.csv')

df_water <- read_csv('www/datasets/water1.csv')
df_sanitation <- read_csv('www/datasets/sanitation1.csv')
df_hygiene <- read_csv('www/datasets/hygiene1.csv')

df_water2 <- read_csv('www/datasets/water2.csv')
df_sanitation2 <- read_csv('www/datasets/sanitation2.csv')
