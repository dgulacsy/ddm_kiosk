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
fname <- 'data/raw/Czech newspaperkiosk case dataset 20210104_v1.0.xlsx'
turnover <- read_xlsx(fname, sheet = 'Turnover') %>% clean_names()
stores <- read_xlsx(fname, sheet = 'Store data') %>% clean_names() 
cost <- read_xlsx(fname, sheet = 'Costs') %>% clean_names()

write_csv(turnover,paste0(path,"data/clean/turnover.csv"))
write_csv(stores,paste0(path,"data/clean/stores.csv"))
write_csv(cost,paste0(path,"data/clean/cost.csv"))


# Data cleaning -----------------------------------------------------------
str(stores)
str(turnover)
str(cost)

# convert store_name column to string
turnover$store_name <- as.character(turnover$store_name)
cost$store_name <- as.character(cost$store_name)
stores$store_name <- as.character(stores$store_name)

# convert nominal variables to factors
stores$prague<-as.factor(stores$prague)
stores$price_category<- as.factor(stores$price_category)
stores$lottery_pos<- as.factor(stores$lottery_pos)
stores$pudo_pos<- as.factor(stores$pudo_pos)

# convert numeric variables stored as character to numeric
stores$sunday_opening_hours <- as.numeric(stores$sunday_opening_hours)
stores$real_estate_price_sqm <- as.numeric(stores$real_estate_price_sqm)
stores$x0_50m_avg <- as.numeric(stores$x0_50m_avg)
stores$x50_100m_avg <- as.numeric(stores$x50_100m_avg)
stores$x100_plusm_avg <- as.numeric(stores$x100_plusm_avg)
stores$newspaper_shelves <- as.numeric(stores$newspaper_shelves)
stores$non_newspaper_shelves <- as.numeric(stores$non_newspaper_shelves)

# replace #missing values with NA
stores[stores == "#MISSING"] <- NA

# change sign of costs
cost$costs <- -cost$costs

# change unit of measurement for gm, cost
cost$costs <- cost$costs/10^5
turnover$gm <- turnover$gm/10^5

# Create Year-Store Aggregated Dataset ------------------------------------
# Aggregate data
ys_t <- turnover %>% group_by(year,store_name) %>% 
  summarise(
    gm = sum(gm)
  )

# Join cost and turnover
ys <- left_join(ys_t,cost,by=c("year","store_name"))

# Join df and store data
ys <- left_join(ys,stores, by="store_name")

# Calculate net margin (Net margin = Gross margin - Non-COGS costs)
ys$nm <- ys$gm-ys$costs

# Create Year-Store-ProductCat Aggregated Dataset ------------------------
ysp_t <- turnover %>% group_by(year,store_name, product_category) %>% 
  summarise(
    gm = sum(gm)
  )

ysp <- left_join(ysp_t,stores,by="store_name")

# Create Year-Store-ProductCat-Month level Dataset ------------------------
yspm <- left_join(turnover, stores, by="store_name")

# General EDA ----------------------------------------------------------------------

stores %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_bw() + 
  scale_fill_wsj()

# Descriptive statistics
dstats<-sum_stat(stores, stores %>%
           keep(is.numeric) %>% 
           colnames(),
         c('mean','median','min','max','1st_qu.','3rd_qu','sd','range'),
         num_obs = F
         ) %>% 
        left_join(sum_stat(turnover,
                           c("gm"),
                           c('mean','median','min','max','1st_qu.','3rd_qu','sd','range'),
                           num_obs = F),
                  by="statistics") %>%
        left_join(sum_stat(cost,
                           c("costs"),
                           c('mean','median','min','max','1st_qu.','3rd_qu','sd','range'),
                           num_obs = F),
                  by="statistics")

# Distributions of GM by product categories and years
ggplot(ysp_t, aes(gm)) + geom_histogram(position = 'dodge') + facet_grid(year ~ product_category,scales = 'free')

# Distributions of GM by year
ggplot(ys, aes(gm)) + geom_histogram(position = 'dodge') + facet_grid(scales = 'free', rows = . ~ year)

# Distributions of Non-COGS Costs by years
ggplot(cost, aes(costs)) + geom_histogram(position = 'dodge') + facet_grid(scales = 'free', rows = . ~ year)

# Cost - Gross Margin Scatterplot
suspicous <- subset(ys, costs<0)

ggplot(ys , aes(x = costs, y = gm)) +
  geom_point() +
  geom_point(data=suspicous, colour="red")+
  geom_text(data=suspicous, label="Error?", vjust=-1, hjust=0.25, color="red")+
  theme_bw() +
  geom_smooth(method="loess" , formula = y ~ x ) +
  labs( title = "Relationship between Non-COGS Costs and Gross Margin",
        y = "Gross Margin (100K)",
        x = "Costs (100K)")+ 
  facet_grid(scales = 'free', rows = . ~ year)

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
        y = "Gross Margin (100K)",
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
  facet_grid(product_category ~ month,scales = 'free')+
  theme(axis.text.x=element_blank(),
        legend.position = "none")

# Newspaper Focus (newspaper focus ratio) --------
# calculate newspaper ratio
ys$npr<- ys$newspaper_shelves/(ys$newspaper_shelves+ys$non_newspaper_shelves)

ggplot(ys, aes(x = npr, y = gm)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs( title = "Pattern of association between Gross Margin and Newspaper focus ratio",
        y = "Gross Margin (100K)",
        x = "Newspaper focus ratio")

ggplot(ys, aes(x = npr, y = nm)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs( title = "Pattern of association between Gross Margin and Newspaper focus ratio",
        y = "Net Margin (100K)",
        x = "Newspaper focus ratio")


