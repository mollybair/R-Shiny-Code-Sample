# prepare environment
rm(list=ls())
library(readxl)
library(tidyverse)
library(dplyr)
library(shiny)
library(forcats)
path <- "./R-Shiny-Code-Sample/Multiple Job Holding Data/"

# load data
data_files <- list.files(path)
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

# add column that indicates type of characteristic (age, race, etc.)
# add levels to characteristic col so that bars in bar chart will be in correct order
merged$char_cat <- c("Age Group", "Age Group", "Age Group", "Age Group", "Age Group", "Race/Ethnicity",
                     "Race/Ethnicity", "Race/Ethnicity", "Race/Ethnicity", "Marital Status",
                     "Marital Status", "Marital Status")
char_levels <- char_rename
merged$characteristic <- factor(char_levels, levels = char_levels)

# separate data by year
years <- c("14", "15", "16", "17", "18", "19")
year_dfs <- c("mjh14", "mjh15", "mjh16", "mjh17", "mjh18", "mjh19")
clean_colnames <- c("characteristic", "char_cat", "total_count", "total_rate", "men_count", "men_rate",
                    "wom_count", "wom_rate")

for (i in seq_along(years)) {
  col_names <- colnames(merged)[2:length(colnames(merged))]
  keep <- c()
  for (col_name in col_names) {
    if (grepl(years[i], col_name) == TRUE) {
      keep <- c(keep, col_name)
    }
  }
  keep <- c("characteristic", "char_cat", keep)
  data <- merged[keep]
  colnames(data) <- clean_colnames
  assign(year_dfs[i], data, envir = globalenv())
}

# create shiny app
ui <- fluidPage(
  fluidRow(
    column(width = 12,
           align = "left",
           tags$h1("Characteristics of Multiple Jobholders, 2014-2019"))
  ),
  
  fluidRow(
    column(width = 3,
           align = "right",
           tags$h2("Data on multiple jobholders comes from the Current Population Survey conducted by the United States Bureau of Labor Statistics."))
  ),
  
  selectInput("year", "Select a year",
              list("2014", "2015", "2016", "2017", "2018", "2019")
    
  ),
  
  plotOutput("plot")
  
)

server <- function(input, output) {
  my_data <- reactive({
    if (input$year == "2014") return (mjh14)
    if (input$year == "2015") return (mjh15)
    if (input$year == "2016") return (mjh16)
    if (input$year == "2017") return (mjh17)
    if (input$year == "2018") return (mjh18)
    if (input$year == "2019") return (mjh19)
  })
  
  output$plot <- renderPlot({
    ggplot(data = my_data()) +
      geom_col(mapping = aes(x = total_rate, y = characteristic, fill = char_cat)) +
      scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
      labs(x = "Percentage of Employed Persons in Group with Multiple Jobs",
           y = "",
           caption = "Source: U.S. Bureau of Labor Statistics") +
      theme_minimal()
  })

}

shinyApp(ui = ui, server = server)

