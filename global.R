library(leaflet)
library(dplyr)
library(shiny)
library(ggplot2)
library(stringr)
library(plotly)
library(shinythemes)
library(DT)
library(reshape2)
library(class)
library(FNN)

# Data Load

# year <- as.numer(str_extract(input$team, '[0-9]+'))
# Team Arenas
team_arena <- read.csv("https://raw.githubusercontent.com/jjenki22/KNN-NCAA-Team-Matcher/master/Data/Teams_Arenas.csv", stringsAsFactors = FALSE)

# Table Data
teams_table <- read.csv("https://raw.githubusercontent.com/jjenki22/KNN-NCAA-Team-Matcher/master/Data/Teams_Shiny_2008_2019.csv", stringsAsFactors = FALSE)
teams_table <- teams_table %>% 
  arrange(-Year, Unique_Identifier)