# Load necessary libraries
library(dplyr)
library(openxlsx)
library(lubridate)
library(knitr)
library(kableExtra)
library(readxl)
library(tidyr)
library(webshot)
library(purrr)
library(ggplot2)

# Load the custom R script
source("C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/05_Radar_Back-Analysis_Alarming_Approach_Failure_Event/05_Radar_Back-Analysis_Alarming_Approach_Failure_Event.R")

# Install PhantomJS (if not already installed)
if (!dir.exists(system.file("phantomjs", package = "webshot"))) {
  webshot::install_phantomjs(force = TRUE)
}

# Assuming closest_times is already defined in the sourced script
mean_closest_time_deformation <- closest_times %>%
  group_by(Description, Timestamp, Interval, Target_Time) %>%
  summarise(across(starts_with("Closest_Deformation_Rate"), \(x) mean(x, na.rm = TRUE)), .groups = 'drop') %>%
  rename_with(~ sub("Closest_", "Mean_Closest_", .), starts_with("Closest_")) %>%
  pivot_longer(
    cols = starts_with("Mean_Closest_Deformation_Rate"),
    names_to = "Deformation_Rate_Interval",
    values_to = "Mean_Closest_Deformation_Rate"
  ) %>%
  mutate(Deformation_Rate_Interval = sub("Mean_Closest_Deformation_Rate_", "", Deformation_Rate_Interval))


# Create tables for each unique Description and Timestamp
unique_combinations <- unique(failure_alarms %>% select(Description, Timestamp))
tables <- list()
for (i in 1:nrow(unique_combinations)) {
  description <- unique_combinations$Description[i]
  timestamp <- unique_combinations$Timestamp[i]
  
  table <- mean_closest_time_deformation %>%
    filter(Description == description & Timestamp == timestamp) %>%
    select(Target_Time, Interval, Deformation_Rate_Interval, Mean_Closest_Deformation_Rate) %>%
    mutate(Interval = case_when(
      Interval == "Time_Minus_01_Hour" ~ "01 Hour",
      Interval == "Time_Minus_04_Hours" ~ "04 Hours",
      Interval == "Time_Minus_12_Hours" ~ "12 Hours",
      Interval == "Time_Minus_24_Hours" ~ "24 Hours",
      TRUE ~ Interval
    )) %>%
    mutate(Interval = factor(Interval, levels = c("01 Hour", "04 Hours", "12 Hours", "24 Hours"))) %>%
    arrange(Interval) %>%
    pivot_wider(names_from = Deformation_Rate_Interval, values_from = Mean_Closest_Deformation_Rate, names_prefix = "MDR_")
  
  tables[[paste(description, timestamp, sep = "_")]] <- table
}

# Define the output directory
output_dir <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/07_Radar_Back-Analysis_Alarming_Approach_Velocity/"
dir.create(output_dir, showWarnings = FALSE)  # Ensure the directory exists

# Display and save the tables as .png
for (i in 1:length(tables)) {
  description <- unique_combinations$Description[i]
  timestamp <- unique_combinations$Timestamp[i]
  table <- tables[[paste(description, timestamp, sep = "_")]]
  readable_timestamp <- format(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  caption <- paste("Table for:", description, "- Time of occurrence:", readable_timestamp)
  
  kable_output <- kable(table, digits = 2, align = "c", caption = caption) %>%
    kable_styling("striped", full_width = FALSE)
  
  # Save the table as HTML
  temp_html <- tempfile(fileext = ".html")
  save_kable(kable_output, file = temp_html)
  
  # Convert the HTML to PNG
  temp_png <- file.path(output_dir, paste0(gsub(" ", "_", description), "_", format(timestamp, "%Y-%m-%d_%H-%M-%S"), ".png"))
  webshot(temp_html, file = temp_png, selector = "table")
  
  cat("\nSaved table for:", description, "at", timestamp, "as", temp_png, "\n")
}
