# prepare environment
rm(list=ls())
library(readxl)
library(tidyverse)
library(shiny)
path <- "./R-Shiny-Code-Sample/"
data_path <- "./R-Shiny-Code-Sample/Multiple Job Holding Data/"

# load data
data_files <- list.files(data_path)
excel_to_df <- function(fname, col_names) {
  df <- read_excel(paste0(data_path, fname),
                   range = cell_rows(8:34), # exclude multi-index and footer
                   col_names = col_names)
  df_name <- substr(fname, 1, 8) # exclude .xlsx from df name
  assign(x = df_name, value = df, envir = globalenv())
}
names_1415 <- c("characteristic", "total_count_14", "total_count_15", "total_rate_14", "total_rate_15",
                "men_count_14", "men_count_15", "men_rate_14", "men_rate_15","wom_count_14", 
                "wom_count_15", "wom_rate_14", "wom_rate_15")
names_1617 <- c("characteristic", "total_count_16", "total_count_17", "total_rate_16", "total_rate_17",
                "men_count_16", "men_count_17", "men_rate_16", "men_rate_17","wom_count_16", 
                "wom_count_17", "wom_rate_16", "wom_rate_17")
names_1819 <- c("characteristic", "total_count_18", "total_count_19", "total_rate_18", "total_rate_19",
                "men_count_18", "men_count_19", "men_rate_18", "men_rate_19","wom_count_18", 
                "wom_count_19", "wom_rate_18", "wom_rate_19")
all_col_names <- c(list(names_1415), list(names_1617), list(names_1819))
for (i in 1:length(data_files)) {
  excel_to_df(data_files[i], unlist(all_col_names[i]))
}
view(`mjh14-15`)

# clean data



