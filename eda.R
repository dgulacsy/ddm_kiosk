# Initialize environment --------------------------------------------------
rm(list=ls())

library(tidyverse)
library(readxl)
library(janitor)

# read data
fname <- 'data/Czech newspaperkiosk case dataset 20210104_v1.0.xlsx'
turnover <- readxl::read_excel(fname, sheet = 'Turnover') %>% janitor::clean_names() %>% mutate(store_name = as.factor(store_name))
store <- readxl::read_excel(fname, sheet = 'Store data') %>% janitor::clean_names() 
costs <- readxl::read_excel(fname, sheet = 'Costs') %>% janitor::clean_names() %>% mutate(costs = -costs)

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
  left_join(store, by = c('store_name')) %>% 
  mutate(year = fct_recode(as.factor(year), "0" = "Last year", "1" = "This year"))

# calculate NM ( = GM + NCOGS)
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
# create yearly aggregated average net-margin variable
nm_avg_y <- df %>% 
  group_by(year) %>% 
  summarise(
    avg_nm = mean(nm)
  )

nm_avg_0 <- as.numeric(nm_avg_y[1, 2])
nm_avg_1 <- as.numeric(nm_avg_y[2, 2])

# filter based on yearly nm 
df_up <- df %>% 
  mutate(
    underperformer_0 = ifelse(year == 0 & nm < nm_avg_0, 1, ifelse(year == 1, NA, 0)),
    underperformer_1 = ifelse(year == 1 & nm < nm_avg_1, 1, ifelse(year == 0, NA, 0))
  )

df_up %>% 
  summarise(
    underperformers_last_year = sum(underperformer_0, na.rm = T),
    underperformers_this_year = sum(underperformer_1, na.rm = T),
    total = n(),
    perc_total_0 = underperformers_last_year / total * 100,
    perc_total_1 = underperformers_this_year / total * 100
  )

# histogram of nm and gm
df_hist <- df %>% group_by(year, store_name) %>% summarise(gm = mean(gm), nm = mean(nm), costs = mean(costs))

df_hist %>% ggplot(aes(nm)) + geom_histogram(bins = 50)
df_hist %>% ggplot(aes(gm)) + geom_histogram(bins = 50)

# scatter plot of costs ~ nm/gm
df_hist %>% ggplot(aes(nm, costs)) + geom_point()
df_hist %>% ggplot(aes(gm, costs)) + geom_point()
# gambling ----------------------------------------------------------------
# create total gross-margin and total gross-margin per category
df_g <- df %>% 
  group_by(store_name, product_category, year) %>% 
  summarise(
    total_gm_cat = sum(gm)
  ) %>% 
  right_join(df, by = c("store_name", "product_category", "year"))
df_g <- df_g %>% 
  group_by(store_name, year) %>% 
  summarise(
    total_gm = sum(gm)
  ) %>% 
  right_join(df_g, by = c("store_name", "year"))

# calculate percentage of total gross margin per category
df_g <- df_g %>% 
  group_by(store_name, year, product_category) %>% 
  summarise(
    total_gm_cat_perc = total_gm_cat / total_gm * 100
  ) %>% 
  right_join(df_g, by = c("store_name", "year", "product_category"))

# create average perc of total gm per category per year
prod_cat_becnh <- df_g %>% 
  group_by(year, product_category) %>% 
  summarise(
    avg_perc_gm = mean(total_gm_cat_perc)
  ) 

# count number of high impact gambling stores per year
df_g %>% 
  filter(product_category == "Sazka") %>% 
  mutate(gambling_high_imp = ifelse(total_gm_cat_perc > 25, 1, 0)) %>% 
  group_by(store_name, year) %>% 
  summarise(
    gambl_hi_count = mean(gambling_high_imp)
  ) %>% 
  group_by(year) %>% 
  summarise(
    total_hi_gamb = sum(gambl_hi_count)
  )

# create high impact gambling dummy
df_g <- df_g %>% 
  mutate(gamb_hi_imp = ifelse(product_category == "Sazka" & total_gm_cat_perc > 25, 1, ifelse(product_category != "Sazka", NA, 0))) 

# seasonality -------------------------------------------------------------



# sales performance factors per product category --------------------------


