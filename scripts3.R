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

legend_order <- c('High income', 'Upper middle income', 'Lower middle income', 'Low income', 'Brazil')
color_scale <- 'Set2'

ggplot(df_enrollment_grouped, aes(x=year, y=mean, group=income_group)) +
  geom_line(aes(color=income_group), size=1) +
  geom_ribbon(aes(ymin=mean - sd, ymax=mean + sd, fill=income_group), alpha=.15) +
  expand_limits(y = 0) +
  # changing the order of the legend values
  scale_fill_discrete(breaks=legend_order) +
  scale_color_discrete(breaks=legend_order) +
  scale_fill_brewer(palette=color_scale) +
  scale_colour_brewer(palette=color_scale) +
  facet_wrap(vars(indicator_code))

