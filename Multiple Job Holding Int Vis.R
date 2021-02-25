# prepare environment
rm(list=ls())
library(readxl)
library(tidyverse)
library(dplyr)
library(shiny)
path <- "./R-Shiny-Code-Sample/Multiple Job Holding Data/"

# load data
data_files <- list.files(path)
excel_to_df <- function(fname, col_names) {
  df <- read_excel(paste0(path, fname),
                   range = cell_rows(8:34), # exclude multi-index and footer
                   col_names = col_names)
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
char_rename <- c("16+", "16-19", "20+", "20-24", "25+", "25-54", "55+", "55-64", "65+",
                     "white", "black", "asian", "hispanic/latino", "married",
                     "widowed/divorced/separated", "never married", "full time/part time",
                     "both part time", "both full time", "hrs vary")
                  # there are inconsistencies in variable naming across waves of data, which presents
                  # an error when joining waves on characteristic
# clean data
clean_df<- function(df, vars, df_name) {
  cleaned <- df %>%
    na.omit() %>%
    mutate_at(vars, as.numeric)
  cleaned$characteristic <- char_rename
  assign(df_name, cleaned, envir = globalenv())
}
numeric_cols <- c(list(names_1415[2:13]), list(names_1617[2:13]), list(names_1819[2:13]))
    # all cols except first contain numeric data 
for (i in 1:length(data_files)) {
  temp <- excel_to_df(data_files[i], unlist(all_col_names[i]))  # load data
  df_name <- substr(data_files[i], 1, 8) # use fname for df_name, but exclude .xlsx
  temp_cleaned <- clean_df(temp, unlist(numeric_cols[i]), df_name) # clean data 
}
dfs <- list(`mjh14-15`, `mjh16-17`, `mjh18-19`)
merged <- as.data.frame(dfs[1])
for (i in range(2:length(dfs))) {
  merged <- inner_join(merged, as.data.frame(dfs[i]), 
                       by = c("characteristic" = "characteristic"))  # merge all waves by characteristic
}

ui <- fluidPage(
  
)

server <- function(input, output) {
  
}






