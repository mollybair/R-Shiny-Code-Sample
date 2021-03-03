# prepare environment
rm(list=ls())
library(readxl)
library(tidyverse)
library(dplyr)
library(shiny)
path <- "./R-Shiny-Code-Sample/Multiple Job Holding Data/"

# load data
data_files <- list.files(path)
#data_files <- data_files[2:length(data_files)] #BANDAID FIX COME BACK TO THIS!!!
excel_to_df <- function(fname, col_names) {
  df <- read_excel(paste0(path, fname),
                   range = cell_rows(8:28), # exclude multi-index and footer, and last group of characteristics
                                            # (hours worked) because rates are missing
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
char_rename <- c("16-19 years", "20-24 years", "25-54 years", "55-64 years", "65+",
                 "White", "Black or African American", "Asian", "Hispanic or Latino ethnicity", 
                 "Married", "Widowed, Divorced, or Separated", "Never Married")
                  # there are inconsistencies in variable naming across waves of data, which presents
                  # an error when joining waves on characteristic

# clean data
clean_df<- function(df, vars, drop_vals, df_name) {
  cleaned <- df %>%
    na.omit() %>%
    mutate_at(vars, as.numeric)
  cleaned <- cleaned[!cleaned$characteristic %in% drop_vals, ]
  cleaned$characteristic <- char_rename
  assign(df_name, cleaned, envir = globalenv())
}
numeric_cols <- c(list(names_1415[2:13]), list(names_1617[2:13]), list(names_1819[2:13]))
    # all cols except first contain numeric data 
for (i in 1:length(data_files)) {
  temp <- excel_to_df(data_files[i], unlist(all_col_names[i]))  # load data
  df_name <- substr(data_files[i], 1, 8) # use fname for df_name, but exclude .xlsx
  age_drop <- c("Total, 16 years and over(2)", "20 years and over", "25 years and over",
                "55 years and over") # drop redundant age measurements
  temp_cleaned <- clean_df(temp, unlist(numeric_cols[i]), age_drop, df_name) # clean data 
}

# merge data for annual comparisons
dfs <- list(`mjh14-15`, `mjh16-17`, `mjh18-19`)
merged <- as.data.frame(dfs[1])
for (i in range(2:length(dfs))) {
  merged <- inner_join(merged, as.data.frame(dfs[i]), 
                       by = c("characteristic" = "characteristic"))  # merge all waves by characteristic
}

# separate data by year
years <- c("14", "15", "16", "17", "18", "19")
year_dfs <- c("mjh14", "mjh15", "mjh16", "mjh17", "mjh18", "mjh19")
clean_colnames <- c("characteristic", "total_count", "total_rate", "men_count", "men_rate",
                    "wom_count", "wom_rate")
for (i in seq_along(years)) {
  col_names <- colnames(merged)[2:length(colnames(merged))]
  keep <- c()
  for (col_name in col_names) {
    if (grepl(years[i], col_name) == TRUE) {
      keep <- c(keep, col_name)
    }
  }
  keep <- c("characteristic", keep)
  data <- merged[keep]
  colnames(data) <- clean_colnames
  assign(year_dfs[i], data, envir = globalenv())
}
view(mjh14)
ggplot(mjh14) +
  geom_col(aes(total_rate, characteristic))

# create shiny app
ui <- fluidPage(
  titlePanel("Multiple Job Holding Trends"),
  
  selectInput("year", "Select a dataset to view",
              list("Yearly Totals", "2014 Demographics", "2015 Demographics", "2016 Demographics",
                   "2017 Demographics", "2018 Demographics", "2019 Demographics")),
  
  plotOutput("plot", click = "plot_click"),
  
  verbatimTextOutput("text")
  
)

server <- function(input, output) {
  data <- reactive({
    if ("Yearly Totals" %in% input$year) return (merged)
    if ("2014" %in% input$year) return (mjh14)
    if ("2015" %in% input$year) return (mjh15)
    if ("2016" %in% input$year) return (mjh16)
    if ("2017" %in% input$year) return (mjh17)
    if ("2018" %in% input$year) return (mjh18)
    if ("2019" %in% input$year) return (mjh19)
  })
  
  output$plot <- renderPlot({
    if ("Yearly Totals" %in% input$year) {
      # line graph of annual trends
      
    } else {
      # bar graph comparing demographic participation in mjh
      ggplot(data) +
        geom_bar(aes(total_rate))
    }
    
  })
  
  output$text <- renderText({
    paste0()
  })

}

shinyApp(ui = ui, server = server)






