# Initialize environment --------------------------------------------------
rm(list=ls())

library(tidyverse)
library(readxl)
library(janitor)

# read data
fname <- 'data/Czech newspaperkiosk case dataset 20210104_v1.0.xlsx'
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


# calculate NM ( = GM - NCOGS)
df <- df %>% 
  group_by(year, store_name) %>% 
  summarise(
    gm_y = sum(gm)
  ) %>% 
  right_join(df, by = c('year', 'store_name')) %>% 
  mutate(
    nm = gm_y - costs
  )

# Shops that are underperformers ------------------------------------------

nm_avg_y <- df %>% 
  group_by(year) %>% 
  summarise(
    avg_nm = mean(nm)
  )

nm_avg_0 <- as.numeric(nm_avg_y[1, 2])
nm_avg_1 <- as.numeric(nm_avg_y[2, 2])

df_up <- df %>% 
  mutate(
    underperformer_0 = ifelse(year == "Last year" & nm < nm_avg_0, 1, ifelse(year == "This year", NA, 0)),
    underperformer_1 = ifelse(year == "This year" & nm < nm_avg_1, 1, ifelse(year == "Last year", NA, 0))
  )

df_up %>% 
  summarise(
    underperformers_last_year = sum(underperformer_0, na.rm = T),
    underperformers_this_year = sum(underperformer_1, na.rm = T),
    total = n(),
    perc_total_0 = underperformers_last_year / total * 100,
    perc_total_1 = underperformers_this_year / total * 100
  )

df %>% ggplot(aes(nm)) + geom_histogram(bins = 50)
# gambling ----------------------------------------------------------------



# seasonality -------------------------------------------------------------



# sales performance factors per product category --------------------------


