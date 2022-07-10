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

df_cleaned <- read_dataset('WDIData.csv') %>%
  # get data until 2019 to ignore pandemic situation
  select(c(country_code, indicator_code, x2010:x2019)) %>%
  # Removing rows where all year values are null
  filter(rowSums(is.na(.)) < 10) %>%
  # Getting income groups of each country
  merge(read_dataset('WDICountry.csv'), by='country_code') %>%
  select(c(country_code, income_group, indicator_code, x2010:x2019)) %>%
  # removing countries not associated with any Income Group
  filter(!is.na(income_group)) %>%
  # creating a different group to Brazil, to make it comparable with other groups
  mutate(income_group=if_else(country_code == 'BRA', 'Brazil', income_group))

### Global variables
legend_order <- c('Brazil', 'High income', 'Upper middle income', 'Lower middle income', 'Low income')
color_scale <- 'Set2'

# enrollment indicators
# SE.PRE.ENRR: School enrollment, preprimary (% gross) There are a lot of differences with this type of data, probably because parents do not put children at school by this age
## Primary and secondary indicators were net indicators so they could reflect better the students that needed to be at each level
# SE.PRM.NENR: School enrollment, primary (% net)
# SE.SEC.NENR: School enrollment, secondary (% net)
# SE.TER.ENRR: School enrollment, tertiary (% gross)
enrollment_indicators <- c('SE.PRM.NENR', 'SE.SEC.NENR', 'SE.TER.ENRR')

df_enrollment <- df_cleaned %>%
  filter(indicator_code %in% enrollment_indicators) %>%
  # Getting only the last 5 year to get results based on previous years investments
  select(c(country_code, income_group, indicator_code, x2015:x2019)) %>%
  # pivoting table to make it easier to plot
  pivot_longer(x2015:x2019, names_to='year', values_to='enrollment') %>%
  # cleaning year values to make it easier to use as label
  mutate(year=gsub('x', '', .$year))

# above of 20% of the data has NA values
# because of this we will use the strategy to fill years with previous values or next values
print(
  paste(
    'Percentage of NA values on enrollment column: ',
    trunc(sum(is.na(df_enrollment$enrollment)) / nrow(df_enrollment)*100), 
    '%', 
    sep=''
  )
)

df_enrollment <- df_enrollment %>%
  group_by(country_code, indicator_code) %>%
  fill(enrollment, .direction='downup') %>%
  # filtering to get only the countries with at least 1 not NA value for each indicator
  filter(!is.na(enrollment))

# setting representative labels to the indicators
indicators_labels <- c(
  'SE.PRM.NENR' = 'Primary Education',
  'SE.SEC.NENR' =  'Secondary Education',
  'SE.TER.ENRR' = 'Tertiary Education'
)

df_enrollment %>%
  group_by(income_group, indicator_code, year) %>%
  summarise(mean=mean(enrollment), sd=sd(enrollment)) %>%
  ggplot(aes(x=year, y=mean, group=income_group)) +
  theme_minimal() +
  geom_line(aes(color=income_group), size=1) +
  geom_ribbon(aes(ymin=mean - sd, ymax=mean + sd, fill=income_group), alpha=.15) +
  expand_limits(y = 0) +
  # changing the order of the legend values
  scale_fill_brewer(breaks=legend_order, palette=color_scale) +
  scale_colour_brewer(breaks=legend_order, palette=color_scale) +
  # formatting the y labels to include a percent sign
  scale_y_continuous(labels=function(y) paste(y, '%', sep='')) +
  facet_wrap(vars(indicator_code), labeller=as_labeller(indicators_labels)) +
  labs(title='Average enrollment per Income Group and Education level', 
       subtitle='The enrollment is calculated based on the age group of the specific education level',
       x='Year', y='Enrollment', color='Income Group', fill='Income Group')

# checking the distribution of values about secondary education enrolling for the Upper middle income countries
brazil_value <- df_enrollment %>%
  filter(income_group == 'Brazil' & indicator_code == 'SE.SEC.NENR' & year == '2019')
brazil_value <- brazil_value$enrollment

df_enrollment %>%
  filter(income_group %in% c('Brazil', 'Upper middle income') & 
           indicator_code == 'SE.SEC.NENR' & year == '2019') %>%
  ggplot(aes(x=enrollment)) + 
  geom_histogram(binwidth=5, fill='#80b1d3') +
  geom_vline(aes(xintercept=brazil_value), color='#757575', size=1, linetype='dashed') +
  geom_text(aes(x=brazil_value+2.5, y=-.2, label='Brazil'))

# checking the investment in education
# getting money invested in education based on GDP (https://data.oecd.org/gdp/gross-domestic-product-gdp.htm)
df_gdp_spent_percent <- df_cleaned %>%
  filter(indicator_code  == 'SE.XPD.TOTL.GD.ZS') %>%
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
  mutate(
    gdp_spent_per_capita=(gdp_percent/100) * gdp_per_capita, 
    year=gsub('x', '', year))
  
df_gdp_spent_per_capita_grouped <- df_gdp_spent_per_capita %>%
  group_by(income_group, year) %>%
  summarise(mean=mean(gdp_spent_per_capita), sd=sd(gdp_spent_per_capita)) 


ggplot(df_gdp_spent_per_capita_grouped, aes(x=year, y=mean, group=income_group)) +
  geom_line(aes(color=income_group), size=1) +
  geom_ribbon(aes(ymin=mean - sd, ymax=mean + sd, fill=income_group), alpha=.15) +
  expand_limits(y = 0) +
  # changing the order of the legend values and setting default colors
  scale_fill_brewer(breaks=legend_order, palette=color_scale) +
  scale_colour_brewer(breaks=legend_order, palette=color_scale)

# let's compare how far we are from the majority of high income countries
brazil_value_gdp_per_capita <- df_gdp_spent_per_capita %>%
  filter(income_group == 'Brazil' & year == '2019')
brazil_value_gdp_per_capita <- brazil_value_gdp_per_capita$gdp_spent_per_capita

df_gdp_spent_per_capita %>%
  filter(income_group %in% c('Brazil', 'High income') & year == '2019') %>%
  ggplot(aes(x=gdp_spent_per_capita)) + 
  geom_histogram(binwidth=200, fill='#80b1d3') +
  geom_vline(aes(xintercept=brazil_value_gdp_per_capita), color='#757575', size=1, linetype='dashed') +
  geom_text(aes(x=brazil_value_gdp_per_capita+250, y=-.2, label='Brazil'))


# getting data on children in employment 7 - 14 year
# SL.TLF.0714.ZS
df_children_employment <- df_cleaned %>%
  filter(indicator_code  == 'SL.TLF.0714.ZS') %>%
  # getting only the last year from the last five because there is a lot of NA values
  mutate(children_employment=do.call(coalesce, rev(across(x2015:x2019)))) %>%
  select(c(country_code, income_group, children_employment)) %>%
  filter(!is.na(children_employment))

ggplot(df_children_employment, aes(x=income_group, y=children_employment)) + 
  geom_boxplot(aes(fill=income_group)) +
  # removing the legend as it is not necessary on this chart
  theme(legend.position='none') +
  # changing the order of the legend values and setting default colors
  scale_x_discrete(limits=legend_order) +
  scale_fill_brewer(palette=color_scale)


# getting data about poverty !caution
# distribution of income share per group
# SI.DST.05TH.20: Income share held by highest 20%
# SI.DST.04TH.20: Income share held by fourth 20%
# SI.DST.03RD.20: Income share held by third 20%
# SI.DST.02ND.20: Income share held by second 20%
# SI.DST.FRST.20: Income share held by lowest 20%
poverty_indicators <- c('SI.DST.FRST.20', 'SI.DST.02ND.20' , 'SI.DST.03RD.20', 'SI.DST.04TH.20', 'SI.DST.05TH.20')

df_poverty <- df_cleaned %>%
  filter(indicator_code  %in% poverty_indicators) %>%
  # pivoting table to make it easier to plot
  pivot_longer(x2010:x2019, names_to='year', values_to='income_share') %>%
  group_by(country_code, indicator_code) %>%
  # filling NA values so that each year could have the same number of countries
  fill(income_share, .direction='downup') %>%
  filter(!is.na(income_share))

df_poverty_grouped <- df_poverty %>%
  group_by(income_group, indicator_code, year) %>%
  summarise(mean=mean(income_share), sd=sd(income_share)) %>%
  # cleaning year values to make it easier to use as label
  mutate(year=gsub('x', '', year)) %>%
  # getting the difference of values between years per group
  mutate(years_difference=abs(mean - lag(mean)))

# as the difference of values between years are so small, we are
# comparing values based only on the last year
print(max(df_poverty_grouped$years_difference, na.rm=TRUE))
ggplot(df_poverty_grouped, aes(x=years_difference)) + 
  geom_histogram(fill='#80b1d3')

df_poverty_grouped %>%
  filter(year == '2019') %>%
  ggplot(aes(
    # changing the order of the legend values
    fill=factor(indicator_code, levels=rev(poverty_indicators)),
    y=mean, x=income_group)) + 
  geom_bar(position=position_dodge(), stat='identity') +
  # inserting the standard deviation bars https://bioinformatics.stackexchange.com/questions/11222/stacked-bargraph-with-error-bars
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.3, position=position_dodge(0.9), color='#757575') +
  # setting other colors to differentiate from previous charts
  # got this custom pallet based on https://colors.dopely.top/palette-generator/ToL9uxh2Egw
  scale_fill_manual(values=c('#82AC85','#A9C7AC','#BFD8C4','#E2CFC9','#D1ACA5')) + 
  scale_x_discrete(limits=legend_order) +
  labs(fill='indicator_code') +
  theme_classic()
