#include necessary packages
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

# pull in data
#years_of_schooling <- read_csv('./years-of-schooling.csv')
years_of_education <- read_csv('./years-of-education.csv', na = "..")
unemployment <- read_csv('./unemployment.csv')
wages <- read_csv('./wages.csv')

# clean up years of schooling data for later use
years_of_education$Average_Years_Of_Education <-
  rowMeans(subset(years_of_education, select = c(`2000 [YR2000]`, `2005 [YR2005]`, `2010 [YR2010]`)), na.rm = TRUE)

years_of_education_avg <- years_of_education %>%
  select(`Country Name`, Average_Years_Of_Education)

# clean up unemployment data for later use
unemployment_avg <- unemployment %>%
  select(ref_area.label, sex.label, obs_value) %>%
  group_by(ref_area.label) %>%
  summarise(Average_Unemployment = mean(obs_value))

# clean up wage data for later use
# normalizing by setting to USD
wages_avg_adjusted_usd <- wages %>%
  filter(classif1 == "CUR_TYPE_USD") %>%
  select(ref_area.label, obs_value) %>%
  group_by(ref_area.label) %>%
  summarise(Average_Wages = mean(obs_value))

# merge tables for ease of use in plots
# also makes it easy to tell what we are looking at
years_vs_unemployment <- merge(years_of_education_avg, unemployment_avg, by.x = 'Country Name', by.y = 'ref_area.label')
years_vs_wage <- merge(years_of_education_avg, wages_avg_adjusted_usd, by.x = 'Country Name', by.y = 'ref_area.label')

# STATISTICAL TESTS
# t-test
t.test(years_vs_unemployment$Average_Unemployment, years_vs_unemployment$Average_Years_Of_Education)
t.test(years_vs_wage$Average_Wages, years_vs_wage$Average_Years_Of_Education)

# spearman's rank correlation test
cor.test(years_vs_unemployment$Average_Unemployment, years_vs_unemployment$Average_Years_Of_Education, method = "spearman")
cor.test(years_vs_wage$Average_Wages, years_vs_wage$Average_Years_Of_Education, method = "spearman")

# plots
ggplot(data = years_vs_unemployment, aes(x = Average_Years_Of_Education, y = Average_Unemployment)) + 
  geom_point(aes( color = Average_Unemployment))+ geom_smooth(method = lm) +
  #scale_color_gradient(low = 'red', high= 'yellow') +
  ggtitle('Average Years Of Schooling vs. Average Unemployment in the World')

ggplot(data = years_vs_wage, aes(x = Average_Years_Of_Education, y = Average_Wages)) + 
  geom_point(aes( color = Average_Wages))+ geom_smooth(method = lm) +
  #scale_color_gradient(low = 'red', high= 'yellow') +
  ggtitle('Average Years Of Schooling vs. Average Wages in the World')
