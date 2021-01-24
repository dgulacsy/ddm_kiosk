# Initialize environment --------------------------------------------------
library(dplyr)
library("readxl")

path<-"/Users/Dominik/OneDrive - Central European University/2nd_trimester/DDM/ddm_kiosk/"

df <- read_xlsx(paste0(path, "data/Czech newspaperkiosk case dataset 20210104_v1.0.xlsx"), col_names = T)
mysheets <- read_excel_allsheets(paste0(path, "data/Czech newspaperkiosk case dataset 20210104_v1.0.xlsx"))
read_excel_

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- excel_sheets(filename)
  x <- lapply(sheets, function(X) read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

dfs<-read_excel_allsheets(paste0(path, "data/Czech newspaperkiosk case dataset 20210104_v1.0.xlsx"))

