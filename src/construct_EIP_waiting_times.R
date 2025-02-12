# PREAMBLE ----------------------------------------------------------------
# TITLE: construct_EIP_waiting_times.R
# DESCRIPTION: construct waiting times for EIP teams for model validation purposes

rm(list=ls())

# Load packages
library(dplyr)
library(tidyr)
library(purrr)


# PRE-SEPTEMBER 2019 ------------------------------------------------------

# Create data of following format:
#   Date: Date of month start
#   Provider_Org_code: Unique EIP provider (trust) organisation code
#   Provider_name: EIP provider name
#   total_complete: Total pathways completed at month end
#   total_incomplete: Total pathways incomplete at month end
#   median_wait_complete: Median wait for pathways completed in month
#   median_wait_incomplete: Median wait for pathways incomplete at month end
#   twoweek_complete: Proportion of pathways completed within 2 week target at month end
#   twoweek_incomplete: Proportion of incomplete pathways at month end outside 2 week target

EIPtimeseries_df <- read.csv("data/raw/EIP Waiting Times/EIP-Timeseries-History-File-December-2015-to-September-2019-1.csv") %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  # Collapse by provider
  group_by(Date, Provider_Org_code, Provider_name, Description) %>%
  summarise(across(c(starts_with("Gt_"), Total_Pathways), sum), .groups = "drop") %>%
  # Calculate metrics at the provider level
  mutate(twoweek = (Gt_0_1_Weeks + Gt_1_2_Weeks) / Total_Pathways,
         cum_props = pmap(
           select(., starts_with("Gt_")), 
           ~cumsum(c(...)) / sum(c(...))
         ),
         # Calculate median wait with linear interpolation
         median_wait = map_dbl(cum_props, function(props) {
           bins <- c(0:12)
           median_bin <- which(props >= 0.5)[1]
           if (is.na(median_bin)) return(13)
           prev_prop <- if(median_bin == 1) 0 else props[median_bin - 1]
           bins[median_bin] + (0.5 - prev_prop) / (props[median_bin] - prev_prop)
         }),
         Description = case_when(
           Description == "EIP pathways completed this month" ~ "complete",
           Description == "EIP pathways incomplete at month end" ~ "incomplete"
         )
  ) %>%
  # Select final columns before pivoting
  select(Date, Provider_Org_code, Provider_name, Description, 
         total=Total_Pathways, median_wait, twoweek) %>%
  # Pivot to wide format  
  pivot_wider(
    id_cols = c(Date, Provider_Org_code, Provider_name),
    names_from = Description,
    values_from = c(total, median_wait, twoweek),
    names_glue = "{.value}_{tolower(Description)}"
  )


# POST-SEPTEMBER 2019 -----------------------------------------------------




# SAVE --------------------------------------------------------------------


         