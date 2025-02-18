# PREAMBLE ----------------------------------------------------------------
# TITLE: construct_psymaptic_caseload.R
# DESCRIPTION: construct provider-level caseloads from psympatic data

rm(list=ls())

# Load packages
library(dplyr)
library(readxl)
library(stringr)
library(purrr)
library(tidyr)


# READ PSYMAPTIC OUTPUT ---------------------------------------------------

# Function to get file list
get_psymaptic_files <- function(directory, type = "count") {
  dir(directory, 
      pattern = paste0("^ccg-.*-\\d{4}-", type, ".xlsx$"),
      full.names = TRUE)
}


# Function to process each file
process_file <- function(filepath) {
  # Extract year and type from filename
  filename <- basename(filepath)
  year <- str_extract(filename, "\\d{4}")
  type <- str_extract(filename, "(?<=ccg-)[^-]+(?=-\\d{4})")
  
  # Read Excel file
  # Use sheet name that matches filename without extension
  sheet_name <- str_replace(filename, ".xlsx$", "")
  
  df <- read_excel(
    filepath,
    sheet = sheet_name
  ) %>%
    # Add year and type columns
    mutate(
      year = year,
      prediction_type = type
    ) %>%
    select(ccg19cd, ccg19nm, year, prediction_type, everything())
  
  return(df)
}


# Function to reshape combined data
reshape_psymaptic_data <- function(df) {
  df %>%
    # Convert to long format
    pivot_longer(
      cols = -c(ccg19cd, ccg19nm, year, prediction_type),
      names_to = "variable",
      values_to = "value"
    ) %>%
    # Split the variable column into components
    separate(
      variable,
      into = c("age_group", "sex", "ethnicity", "estimate_type"),
      sep = "_",
      fill = "right"
    ) %>%
    # Convert estimate type to wide format
    pivot_wider(
      names_from = estimate_type,
      values_from = value
    ) %>%
    # Clean up columns
    mutate(
      age_group = str_replace(age_group, "age", ""),
      sex = str_replace(sex, "sex", ""),
      ethnicity = str_replace(ethnicity, "eth", "")
    ) %>%
    # Arrange the data
    arrange(ccg19cd, year, prediction_type, age_group, sex, ethnicity)
}

# Main function to process either counts or rates
process_psymaptic_data <- function(directory, type = "count") {
  # Get file list
  files <- get_psymaptic_files(directory, type)
  
  # Read and combine all files
  combined_df <- map_df(files, process_file)
  
  # Reshape the data
  reshaped_df <- reshape_psymaptic_data(combined_df)
  
  return(reshaped_df)
}

# Process counts and rates
count_df <- process_psymaptic_data("data/raw/Psymaptic/", "count")
rate_df <- process_psymaptic_data("data/raw/Psymaptic/", "rate")


saveRDS(count_df, file="data/processed/psymaptic_ccg_counts.Rds")
saveRDS(rate_df, file="data/processed/psymaptic_ccg_rates.Rds")

