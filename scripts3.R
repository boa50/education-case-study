library(tidyverse)
library(janitor)

# set working directory
# putting this way to become easier to manage the variables
setwd("/home/boa50/Desenvolvimento/education-case-study")

# data obtained from https://datacatalog.worldbank.org/search/dataset/0037712/World-Development-Indicators at 04/07/2022
# cleaning column names to become easier to work with the data
read_dataset <- function(file_name, dataset_folder='dataset/') {
  file_path <- paste(dataset_folder, file_name, sep='')
  df <- read_csv(file_path) %>% clean_names()
  return(df)
}

df <- read_dataset('WDIData.csv')
df_countries <- read_dataset('WDICountry.csv')
# df_series <- read_dataset('WDISeries.csv')

df_cleaned <- df %>%
  # get data until 2019 to ignore pandemic situation
  select(c(country_code, indicator_code, x2010:x2019)) 

# removing rows where all year values are null
df_cleaned <- df_cleaned %>% filter(rowSums(is.na(df_cleaned)) < 10)

# enrollment indicators
# SE.PRE.ENRR: School enrollment, preprimary (% gross) There are a lot of differences with this type of data, probably because parents do not put children at school by this age
## Primary and secondary indicators were net indicators so they could reflect better the students that needed to be at each level
# SE.PRM.NENR: School enrollment, primary (% net)
# SE.SEC.NENRR: School enrollment, secondary (% net)
# SE.TER.ENRR: School enrollment, tertiary (% gross)

enrollment_indicators <- c('SE.PRM.NENR', 'SE.SEC.NENR', 'SE.TER.ENRR')

df_enrollment <- df_cleaned %>%
  filter(indicator_code %in% enrollment_indicators) %>%
  # getting useful Country names to the plots
  merge(df_countries, by='country_code') %>%
  # removing countries not associated with any Income Group
  filter(!is.na(income_group)) %>%
  select(c(country_code, income_group, indicator_code, x2015:x2019)) %>%
  remove_empty(which='cols') %>%
  # creating a different group to Brazil, to make it comparable with other groups
  mutate(income_group=if_else(country_code == 'BRA', 'Brazil', income_group)) %>%
  # pivoting table to make it easier to plot
  pivot_longer(x2015:x2019, names_to='year', values_to='enrollment')

# cleaning year values to make it easier to use as label
df_enrollment <- df_enrollment %>%
  mutate(year=gsub('x', '', df_enrollment$year))

# above of 20% of the data has NA values
# because of this we will use the strategy to fill years with previous values or next values
print(sum(is.na(df_enrollment$enrollment)) / nrow(df_enrollment))

df_enrollment <- df_enrollment %>%
  group_by(country_code, indicator_code) %>%
  fill(enrollment, .direction='downup') %>%
  # filtering to get only the countries with at least 1 not NA value for each indicator
  filter(!is.na(enrollment))

df_enrollment_grouped <- df_enrollment %>%
  group_by(income_group, indicator_code, year) %>%
  summarise(mean=mean(enrollment), sd=sd(enrollment))

legend_order <- c('Brazil', 'High income', 'Upper middle income', 'Lower middle income', 'Low income')
color_scale <- 'Set2'

ggplot(df_enrollment_grouped, aes(x=year, y=mean, group=income_group)) +
  geom_line(aes(color=income_group), size=1) +
  geom_ribbon(aes(ymin=mean - sd, ymax=mean + sd, fill=income_group), alpha=.15) +
  expand_limits(y = 0) +
  # changing the order of the legend values
  scale_fill_brewer(breaks=legend_order, palette=color_scale) +
  scale_colour_brewer(breaks=legend_order, palette=color_scale) +
  facet_wrap(vars(indicator_code))

# checking the distribution of values about secondary education enrolling for the Upper middle income countries
brazil_value <- df_enrollment %>%
  filter(income_group == 'Brazil' & indicator_code == 'SE.SEC.NENR' & year == '2019')
brazil_value <- brazil_value$enrollment

df_enrollment %>%
  filter(income_group %in% c('Brazil', 'Upper middle income') & 
           indicator_code == 'SE.SEC.NENR' & year == '2019') %>%
  ggplot(aes(x=enrollment)) + 
  geom_histogram(binwidth=5, fill='#80b1d3') +
  geom_vline(aes(xintercept=brazil_value, color='teste'), color='#757575', size=1, linetype='dashed') +
  geom_text(aes(x=brazil_value+2.5, y=-.2, label='Brazil'))

# checking the investment in education
# getting money invested in education based on GDP (https://data.oecd.org/gdp/gross-domestic-product-gdp.htm)
df_gdp_spent_percent <- df_cleaned %>%
  filter(indicator_code  == 'SE.XPD.TOTL.GD.ZS') %>%
  # getting useful Country names to the plots
  merge(df_countries, by='country_code') %>%
  # removing countries not associated with any Income Group
  filter(!is.na(income_group)) %>%
  select(c(country_code, income_group, indicator_code, x2010:x2019)) %>%
  remove_empty(which='cols') %>%
  # creating a different group to Brazil, to make it comparable with other groups
  mutate(income_group=if_else(country_code == 'BRA', 'Brazil', income_group)) %>%
  # pivoting table to make it easier to plot
  pivot_longer(x2010:x2019, names_to='year', values_to='gdp_percent') %>%
  group_by(country_code, indicator_code) %>%
  fill(gdp_percent, .direction='downup') %>%
  # filtering to get only the countries with at least 1 not NA value for each indicator
  filter(!is.na(gdp_percent))

df_gdp_spent_grouped <- df_gdp_spent_percent %>%
  group_by(income_group, indicator_code, year) %>%
  summarise(mean=mean(gdp_percent), sd=sd(gdp_percent)) %>%
# cleaning year values to make it easier to use as label
  mutate(year=gsub('x', '', year))


ggplot(df_gdp_spent_grouped, aes(x=year, y=mean, group=income_group)) +
  geom_line(aes(color=income_group)) +
  expand_limits(y = 0)


# getting GDP per capita changes over time
df_gdp <- df_cleaned %>%
  filter(indicator_code  == 'NY.GDP.PCAP.KD') %>%
  # getting useful Country names to the plots
  merge(df_countries, by='country_code') %>%
  # removing countries not associated with any Income Group
  filter(!is.na(income_group)) %>%
  select(c(country_code, income_group, indicator_code, x2010:x2019)) %>%
  remove_empty(which='cols') %>%
  # creating a different group to Brazil, to make it comparable with other groups
  mutate(income_group=if_else(country_code == 'BRA', 'Brazil', income_group)) %>%
  # pivoting table to make it easier to plot
  pivot_longer(x2010:x2019, names_to='year', values_to='gdp_per_capita') %>%
  group_by(country_code, indicator_code) %>%
  fill(gdp_per_capita, .direction='downup') %>%
  # filtering to get only the countries with at least 1 not NA value for each indicator
  filter(!is.na(gdp_per_capita))

df_gdp_grouped <- df_gdp %>%
  group_by(income_group, indicator_code, year) %>%
  summarise(mean=mean(gdp_per_capita), sd=sd(gdp_per_capita)) %>%
  # cleaning year values to make it easier to use as label
  mutate(year=gsub('x', '', year))
  
ggplot(df_gdp_grouped, aes(x=year, y=mean, group=income_group)) +
  geom_line(aes(color=income_group)) +
  expand_limits(y = 0)
  
df_gdp_spent_per_capita <- df_gdp_spent_percent %>%
  # joining with gdp data to be able to get the gpd expenditure per capita
  merge(df_gdp, by=c('country_code', 'year')) %>%
  rename(income_group=income_group.x) %>%
  select(c(country_code, year, income_group, gdp_percent, gdp_per_capita)) %>%
  # getting the gdp spent per capita
  mutate(gdp_spent_per_capita=(gdp_percent/100) * gdp_per_capita) %>%
  group_by(income_group, year) %>%
  summarise(mean=mean(gdp_spent_per_capita), sd=sd(gdp_spent_per_capita)) %>%
  # cleaning year values to make it easier to use as label
  mutate(year=gsub('x', '', year))

ggplot(df_gdp_spent_per_capita, aes(x=year, y=mean, group=income_group)) +
  geom_line(aes(color=income_group), size=1) +
  geom_ribbon(aes(ymin=mean - sd, ymax=mean + sd, fill=income_group), alpha=.15) +
  expand_limits(y = 0) +
  # changing the order of the legend values
  scale_fill_brewer(breaks=legend_order, palette=color_scale) +
  scale_colour_brewer(breaks=legend_order, palette=color_scale)

# ggplot(df_gdp, aes(x=`year`, y=`GDP per capita (constant 2015 US$)`, group=`Short Name`)) +
#   geom_line(aes(color=`Short Name`)) +
#   scale_y_continuous(labels=scales::comma) +
#   # making the chart to start at the 0 point
#   expand_limits(y=0) +
#   # changing to more distinctive colors
#   scale_colour_brewer(palette = "Set1")