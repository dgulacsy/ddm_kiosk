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
  mutate(year = ifelse(year == "This year", "1", "0")) %>% 
  group_by(year, month, product_category) %>% 
  summarise(
    avg_gm = mean(gm)
  ) %>% 
  arrange(year, month, product_category)

grouping <- rep(seq(1, 60), 2)
turnover_mon['grouping'] <- grouping

ggplot(turnover_mon, aes(year, avg_gm)) + geom_point() + facet_grid(scales = 'free', rows = vars(product_category), cols = vars(month)) + geom_line(aes(group = grouping))


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

df_hist %>% ggplot(aes(nm)) + geom_histogram(bins = 50) + geom_vline(xintercept = median(df_hist$nm), color = "red")
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

df_g <- df_g[!duplicated(df_g),]


# create average perc of total gm per category per year
prod_cat_becnh <- df_g %>% 
  group_by(year, product_category) %>% 
  summarise(
    avg_perc_gm = mean(total_gm_cat_perc)
  ) %>% 
  arrange(year, desc(avg_perc_gm)) %>% 
  mutate(avg_perc_gm = paste0(round(avg_perc_gm, 2), " %"),
         year = ifelse(year == 0, "Last year", "This year")) %>% 
  rename("Year" = "year", "Product Category" = "product_category", "Average Gross Margin (%)"="avg_perc_gm")

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

turnover_mon <- turnover_mon %>% 
  arrange(year, product_category, month) 

grouping_mon <- c(rep(1, 12), rep(2, 12), rep(3, 12), rep(4, 12), rep(5, 12), rep(6, 12), rep(7, 12), rep(8, 12), rep(9, 12), rep(10, 12))

turnover_mon['grouping_mon'] <- grouping_mon

turnover_mon %>% ggplot(aes(month, avg_gm)) + facet_grid(rows = vars(product_category), cols = vars(year), scales = "free") + geom_point() + geom_line(aes(group = grouping_mon))

# sales performance factors per product category --------------------------

glimpse(df)
