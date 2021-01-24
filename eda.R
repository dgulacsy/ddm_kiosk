# Initialize environment --------------------------------------------------
rm(list=ls())

library(tidyverse)
library(readxl)
library(janitor)

# read data
fname <- 'Czech newspaperkiosk case dataset 20210104_v1.0.xlsx'
turnover <- readxl::read_excel(fname, sheet = 'Turnover') %>% janitor::clean_names() %>% mutate(store_name = as.factor(store_name))
store <- readxl::read_excel(fname, sheet = 'Store data') %>% janitor::clean_names() 
costs <- readxl::read_excel(fname, sheet = 'Costs') %>% janitor::clean_names()


# AVG performance YoY
turnover_avg <- turnover %>% 
  group_by(store_name, product_category, year) %>% 
  summarise(
    avg_gm = mean(gm)
  )
ggplot(turnover_avg, aes(year, avg_gm)) + geom_boxplot(position = 'dodge') + facet_grid(scales = 'free', rows = . ~ product_category)

# AVG Monthly performance by category 
turnover_mon <- turnover %>% 
  group_by(year, month, product_category) %>% 
  summarise(
    avg_gm = mean(gm)
  )
ggplot(turnover_mon, aes(year, avg_gm)) + geom_boxplot(position = 'dodge') + facet_grid(scales = 'free', rows = vars(product_category), cols = vars(month))


# join all tables together for modelling
df <- turnover %>% 
  left_join(costs, by = c('store_name', 'year')) %>% 
  left_join(store, by = c('store_name'))

glimpse(df)
