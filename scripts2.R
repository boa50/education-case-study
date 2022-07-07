# loading libraries
library(tidyverse)
library(janitor)
library(scales)

# set working directory
# putting this way to become easier to manage the variables
setwd("/home/boa50/Desenvolvimento/education-case-study")

# data obtained from https://datacatalog.worldbank.org/search/dataset/0037712/World-Development-Indicators at 04/07/2022
df <- read_csv('dataset/WDIData.csv')
df_countries <- read_csv('dataset/WDICountry.csv')

# get data until 2019 to ignore pandemic situation
df_cleaned <- df %>%
  select(c(`Country Code`, `Indicator Code`, `2010`:`2019`))

# removing rows where all year values are null
df_filtered <- df_cleaned %>%
  filter(rowSums(is.na(df_cleaned)) < 10)
# remove_empty(select(df_cleaned, `2010`:`2019`), which = "rows")

# # chosing countries to compare to Brazil (based on Income Group of Countries.csv file)
# BRA
# # Upper middle income
# CHN
# # High income
# FIN, JPN
# # Lower middle income
# BOL, IND
# # Low income
# RWA, SOM

countries_codes <- c('BRA','CHN','FIN','JPN','BOL','IND','RWA','SOM')

## Analyzing education inputs

# getting money invested in education based on GDP (https://data.oecd.org/gdp/gross-domestic-product-gdp.htm)
df_gdp_spent <- df_filtered %>%
  filter(`Country Code` %in% (countries_codes)) %>%
  filter(`Indicator Code` == 'SE.XPD.TOTL.GD.ZS') %>%
  select(c(`Country Code`, `2010`:`2019`)) %>%
  remove_empty(which = "cols") %>%
  # getting useful Country names to the plots
  merge(df_countries, by='Country Code') %>%
    select(c(`Short Name`, `2010`:`2019`)) %>%
  # pivoting table to make it easier to plot
  pivot_longer(!`Short Name`, names_to='year', values_to='gdp_percent') %>%
  # filling years where the value does not exist with previous year value
  fill(`gdp_percent`)

ggplot(df_gdp_spent, aes(x=`year`, y=`gdp_percent`, group=`Short Name`)) +
  geom_line(aes(color=`Short Name`)) +
  expand_limits(y = 0)

# getting GDP per capita changes over time
df_gdp <- df_filtered %>%
  filter(`Country Code` %in% (countries_codes)) %>%
  filter(`Indicator Code` == 'NY.GDP.PCAP.KD') %>%
  select(c(`Country Code`, `2010`:`2019`)) %>%
  remove_empty(which = "cols") %>%
  # getting useful Country names to the plots
  merge(df_countries, by='Country Code') %>%
  select(c(`Short Name`, `2010`:`2019`)) %>%
  # pivoting table to make it easier to plot
  pivot_longer(!`Short Name`, names_to='year', values_to='GDP per capita (constant 2015 US$)')

ggplot(df_gdp, aes(x=`year`, y=`GDP per capita (constant 2015 US$)`, group=`Short Name`)) +
  geom_line(aes(color=`Short Name`)) +
  scale_y_continuous(labels=scales::comma) +
  # making the chart to start at the 0 point
  expand_limits(y=0) +
  # changing to more distinctive colors
  scale_colour_brewer(palette = "Set1")

# Analyze education outputs