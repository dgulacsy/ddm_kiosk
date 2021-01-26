# Initialize environment --------------------------------------------------
library(dplyr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(janitor)
require(scales)
source("sum_stat.R")

rm(list=ls())

path<-"/Users/Dominik/OneDrive - Central European University/2nd_trimester/DDM/ddm_kiosk/"

# Read in data ------------------------------------------------------------
fname <- 'data/Czech newspaperkiosk case dataset 20210104_v1.0.xlsx'
turnover <- read_excel(fname, sheet = 'Turnover') %>% clean_names() %>% mutate(store_name = as.factor(store_name))
store <- read_xlsx(fname, sheet = 'Store data') %>% clean_names() 
costs <- read_xlsx(fname, sheet = 'Costs') %>% clean_names() %>% mutate(costs = -costs)

# convert nominal variables to factors
store$prague<-as.factor(store$prague)
store$price_category<- as.factor(store$price_category)
store$lottery_pos<- as.factor(store$lottery_pos)
store$pudo_pos<- as.factor(store$pudo_pos)

# convert numeric variables stored as character to numeric
store$sunday_opening_hours <- as.numeric(store$sunday_opening_hours)
store$real_estate_price_sqm <- as.numeric(store$real_estate_price_sqm)
store$x0_50m_avg <- as.numeric(store$x0_50m_avg)
store$x50_100m_avg <- as.numeric(store$x50_100m_avg)
store$x100_plusm_avg <- as.numeric(store$x100_plusm_avg)
store$newspaper_shelves <- as.numeric(store$newspaper_shelves)
store$non_newspaper_shelves <- as.numeric(store$non_newspaper_shelves)

# convert store names to strings
turnover$store_name <- as.character(turnover$store_name)
costs$store_name <- as.character(costs$store_name)

# change #missing values to NA
store[store == "#MISSING"] <- NA

# round numbers

# change sign of costs
costs$costs <- -costs$costs

# Create Year-Store Aggregated Dataset ------------------------------------
# Aggregate data
ys_t <- turnover %>% group_by(year,store_name) %>% 
  summarise(
    gm = sum(gm)
  )

ys_c <- costs %>% group_by(year,store_name) %>% 
  summarise(
    cost = sum(costs)
  )

# Join cost and turnover
ys <- left_join(ys_t,ys_c,by=c("year","store_name"))

# Join df and store data
ys <- left_join(ys,store, by="store_name")

# Calculate net margin (Net margin = Gross margin - Non-COGS costs)
ys$nm <- ys$gm-ys$cost

# Create Year-Store-ProductCat Aggregated Dataset ------------------------
ysp_t <- turnover %>% group_by(year,store_name, product_category) %>% 
  summarise(
    gm = sum(gm)
  )

ysp <- left_join(ysp_t,store,by="store_name")

# Create Year-Store-ProductCat-Month Aggregated Dataset ------------------------
yspm <- left_join(turnover, store, by="store_name")

# General EDA ----------------------------------------------------------------------

store %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_bw() + 
  scale_fill_wsj()

# Descriptive statistics
dstats<-sum_stat(store, store %>%
           keep(is.numeric) %>% 
           colnames(),
         c('mean','median','min','max','1st_qu.','3rd_qu','sd','range'),
         num_obs = F
         )

# Add gm and costs
dstats <- dstats %>%
  left_join(sum_stat(turnover,
                     c("gm"),
                     c('mean','median','min','max','1st_qu.','3rd_qu','sd','range'),
                     num_obs = F),
            by="statistics") %>%
  left_join(sum_stat(costs,
                     c("costs"),
                     c('mean','median','min','max','1st_qu.','3rd_qu','sd','range'),
                     num_obs = F),
            by="statistics")

# Distributions of GM by product categories and years
ggplot(ysp_t, aes(gm)) + geom_histogram(position = 'dodge') + facet_grid(year ~ product_category,scales = 'free')

# Distributions of GM by year
ggplot(ysp, aes(gm)) + geom_histogram(position = 'dodge') + facet_grid(scales = 'free', rows = . ~ year)

# Distributions of Non-COGS Costs by years
ggplot(ys_c, aes(cost)) + geom_histogram(position = 'dodge') + facet_grid(scales = 'free', rows = . ~ year)

# Select This year
ys <- filter(ys, year=="This year")
ysp <- filter(ysp, year=="This year")
yspm <- filter(yspm, year=="This year")

# Weekend Opening Hours ------------------------

ggplot(ys, aes(x = round(sunday_opening_hours,1), y = gm)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs( title = "Pattern of association between Gross Margin and Sunday Opening Hours",
        y = "Gross Margin",
        x = "Sunday Opening Hours")

# Create Sunday open dummy variable
ys$sunday_open<-as.factor(ifelse(ys$sunday_opening_hours>0,1,0))
yspm$sunday_open<-as.factor(ifelse(yspm$sunday_opening_hours>0,1,0))

gm_s <- ys %>% group_by(sunday_open) %>% 
  summarise(
    avg_gm = mean(gm)
  )

ggplot(gm_s, aes(fill=sunday_open, y=avg_gm, x=sunday_open)) + 
  geom_bar(position="dodge", stat="identity")

gm_mps<- yspm %>% group_by(product_category,month,sunday_open) %>% 
  summarise(
    avg_gm = mean(gm)
  )

ggplot(gm_mps, aes(fill=sunday_open, y=avg_gm, x=sunday_open)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(product_category ~ month,scales = 'free')

# PUDO Services -----------------------------------------------------------
gm_p<- yspm %>% group_by(product_category,month) %>% 
  summarise(
    avg_gm = mean(gm)
  )

ggplot(gm_p, aes(fill=product_category, y=avg_gm, x=product_category)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(product_category ~ month,scales = 'free')

# Newspaper Focus (newspaper focus ratio) --------
#calculate newspaper ratio
ys$npr<- ys$newspaper_shelves/(ys$newspaper_shelves+ys$non_newspaper_shelves)

ggplot(ys, aes(x = npr, y = gm)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs( title = "Pattern of association between Gross Margin and Newspaper focus ratio",
        y = "Gross Margin",
        x = "Newspaper focus ratio")


