library(readxl)
library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
setwd("C:/Users/crix9_000/Desktop/Hands On/Company Tasks/LimeTray Task/")
country_code_data <- read_xlsx("Country-Code.xlsx")
names(country_code_data)[1] <- "Country.Code"
zomato_data <- read.csv("zomato.csv", stringsAsFactors = FALSE)
zomato_data <- left_join(zomato_data, country_code_data, by = 'Country.Code')
unique(zomato_data$Rating.color)
unique(zomato_data$Rating.text)

cntry_rest_count <- zomato_data %>% group_by(Country) %>%
  summarise('Count' = length(Restaurant.ID))

india_city_rstrnt_count <- zomato_data %>% filter(Country.Code == 1) %>%
  group_by(City) %>%
  summarise('Count' = length(Restaurant.ID))

rating_grp <- zomato_data %>%
  group_by(Rating.text, Rating.color, Aggregate.rating) %>%
  summarise(
    rest_count = uniqueN(Restaurant.ID),
    # rating_bucket = paste(min(Aggregate.rating), max(Aggregate.rating),
    #                       sep = "-"),
    Votes = sum(Votes))

country_grp <- zomato_data %>%
  group_by(Country.Code) %>%
  summarise(
    rest_count = uniqueN(Restaurant.ID))

hist(zomato_data$Average.Cost.for.two, breaks = 1000)
# since most of the data is under average cost 8000, it's because of difference in currency.
