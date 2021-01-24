# Initialize environment --------------------------------------------------
library(dplyr)
library("readxl")
library(ggplot2)
library(ggthemes)
library(tidyverse)
require(scales)
source("sum_stat.R")

rm(list=ls())

path<-"/Users/Dominik/OneDrive - Central European University/2nd_trimester/DDM/ddm_kiosk/"

# Read in data ------------------------------------------------------------
read_excel_allsheets <- function(filename) {
  sheets <- excel_sheets(filename)
  for (sheet in sheets){
    df <- read_excel(filename, sheet = sheet)
    name <- tolower(gsub(" ", "_", sheet))
    assign(name, df, pos=1)
  }
}

read_excel_allsheets(paste0(path, "data/Czech newspaperkiosk case dataset 20210104_v1.0.xlsx"))

rm(description)

# convert nominal variables to factors
store_data$Prague<-as.factor(store_data$Prague)
store_data$`Price category`<- as.factor(store_data$`Price category`)
store_data$`Lottery POS`<- as.factor(store_data$`Lottery POS`)
store_data$`PUDO POS`<- as.factor(store_data$`PUDO POS`)

# change sign of costs
costs$Costs <- -costs$Costs

# Create Year-Store Aggregated Dataset ------------------------------------
# Aggregate data
ys_t <- turnover %>% group_by(Year,`Store name`) %>% 
  summarise(
    gm = sum(GM)
  )

ys_t$`Store name` <- as.character(ys_t$`Store name`)

ys_c <- costs %>% group_by(Year,`Store name`) %>% 
  summarise(
    cost = sum(Costs)
  )

ys_c$`Store name` <- as.character(ys_c$`Store name`)

# Join cost and turnover
ys <- left_join(ys_t,ys_c,by=c("Year","Store name"))

# Join df and store data
ys <- left_join(ys,store_data, by="Store name")

# Calculate net margin
ys$nm <- ys$gm-ys$cost

# Create Year-Store-ProductCat Aggregated Dataset ------------------------
ysp_t <- turnover %>% group_by(Year,`Store name`, `Product category`) %>% 
  summarise(
    gm = sum(GM)
  )

ysp_t$`Store name` <- as.character(ysp_t$`Store name`)

ysp <- left_join(ysp_t,store_data,by="Store name")

# EDA ----------------------------------------------------------------------

str(store_data)

store_data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_bw() + 
  scale_fill_wsj()

dstats<-sum_stat(store_data, store_data %>%
           keep(is.numeric) %>% 
           colnames(),
         c('mean','median','min','max','1st_qu.','3rd_qu','sd','range'),
         num_obs = F
         )

ysp %>%
  select(gm) %>% 
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_bw() + 
  scale_fill_wsj()

ys %>%
  select(c(gm,cost,nm)) %>% 
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_bw() + 
  scale_fill_wsj()

