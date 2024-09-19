# Load necessary libraries
library(lubridate)
library(openxlsx)
library(readxl)
library(tidyr)
library(dplyr)

# Define the directory containing radar data
directory_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/01_Radar_Data"
output_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/02_Radar_Data_Processed/radar_data_processed.xlsx"
average_output_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/02_Radar_Data_Processed/radar_data_process_average.xlsx"

# Get all Excel files in the directory
excel_files <- list.files(directory_path, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty data frame to store combined data
combined_data <- data.frame()

# Loop through each Excel file
for (file_path in excel_files) {
  # Get all sheet names
  sheets <- excel_sheets(file_path)
  
  for (sheet in sheets) {
    # Read the data from the current sheet
    data <- read_excel(file_path, sheet = sheet, col_types = c("date", "numeric"))
    
    # Get the name of the second column (value column)
    value_column_name <- colnames(data)[2]
    
    # Step 2: Reshape the data into the desired format
    long_data <- data %>%
      pivot_longer(cols = all_of(value_column_name), names_to = "Pixel", values_to = "displacement_mm") %>%
      select(Time, Pixel, displacement_mm) %>%
      mutate(SourceFile = basename(file_path))
    
    # Combine the reshaped data
    combined_data <- bind_rows(combined_data, long_data)
  }
}

# Function to calculate rolling average for a given window in hours
rolling_avg_func <- function(df, window_hours) {
  map_dbl(1:nrow(df), function(i) {
    start_time <- df$Time[i] - hours(window_hours)
    subset_df <- df[df$Time >= start_time & df$Time <= df$Time[i], ]
    mean(subset_df$displacement_mm, na.rm = TRUE)
  })
}

# Calculate rolling averages and deformation rate for each SourceFile
combined_data <- combined_data %>%
  group_by(SourceFile, Pixel) %>%
  arrange(Time) %>%
  mutate(
    Rolling_Avg_1hr = rolling_avg_func(cur_data(), 1),
    Rolling_Avg_4hr = rolling_avg_func(cur_data(), 4),
    Rolling_Avg_12hr = rolling_avg_func(cur_data(), 12),
    Rolling_Avg_24hr = rolling_avg_func(cur_data(), 24),
    Time_Diff = c(NA, diff(as.numeric(Time)) / 3600),  # in hours
    Deformation_Rate_1hr = c(NA, diff(Rolling_Avg_1hr)) / Time_Diff,
    Deformation_Rate_4hr = c(NA, diff(Rolling_Avg_4hr)) / Time_Diff,
    Deformation_Rate_12hr = c(NA, diff(Rolling_Avg_12hr)) / Time_Diff,
    Deformation_Rate_24hr = c(NA, diff(Rolling_Avg_24hr)) / Time_Diff
  )


# Calculate rolling averages and deformation rate for each SourceFile
combined_data <- combined_data %>%
  group_by(SourceFile, Pixel) %>%
  arrange(Time) %>%
  mutate(
    Inverse_Velocity_1hr = if_else(is.infinite(1 / Deformation_Rate_1hr) | is.na(Deformation_Rate_1hr), 0, 1 / Deformation_Rate_1hr),
    Inverse_Velocity_4hr = if_else(is.infinite(1 / Deformation_Rate_4hr) | is.na(Deformation_Rate_4hr), 0, 1 / Deformation_Rate_4hr),
    Inverse_Velocity_12hr = if_else(is.infinite(1 / Deformation_Rate_12hr) | is.na(Deformation_Rate_12hr), 0, 1 / Deformation_Rate_12hr),
    Inverse_Velocity_24hr = if_else(is.infinite(1 / Deformation_Rate_24hr) | is.na(Deformation_Rate_24hr), 0, 1 / Deformation_Rate_24hr)
  )

# Function to calculate acceleration for a given column of deformation rates
calc_acceleration <- function(rate_column, time_diff_column) {
  c(NA, diff(rate_column) / time_diff_column[-1])
}

# Calculate accelerations
combined_data <- combined_data %>%
  group_by(SourceFile, Pixel) %>%
  mutate(
    Acceleration_1hr = calc_acceleration(Deformation_Rate_1hr, Time_Diff),
    Acceleration_4hr = calc_acceleration(Deformation_Rate_4hr, Time_Diff),
    Acceleration_12hr = calc_acceleration(Deformation_Rate_12hr, Time_Diff),
    Acceleration_24hr = calc_acceleration(Deformation_Rate_24hr, Time_Diff)
  )

# Rearrange columns as specified
combined_data <- combined_data %>%
  select(Time, Time_Diff, SourceFile, Pixel, displacement_mm, 
         Deformation_Rate_1hr, Deformation_Rate_4hr, Deformation_Rate_12hr, Deformation_Rate_24hr, 
         Inverse_Velocity_1hr, Inverse_Velocity_4hr, Inverse_Velocity_12hr, Inverse_Velocity_24hr, 
         Acceleration_1hr, Acceleration_4hr, Acceleration_12hr, Acceleration_24hr)

# Write the result to a new Excel file
write.xlsx(combined_data, output_path)

# Calculate the average for each column based on all Pixels
average_data <- combined_data %>%
  group_by(Time, Time_Diff, SourceFile) %>%
  summarise(
    Avg_displacement_mm = mean(displacement_mm, na.rm = TRUE),
    Avg_Deformation_Rate_1hr = mean(Deformation_Rate_1hr, na.rm = TRUE),
    Avg_Deformation_Rate_4hr = mean(Deformation_Rate_4hr, na.rm = TRUE),
    Avg_Deformation_Rate_12hr = mean(Deformation_Rate_12hr, na.rm = TRUE),
    Avg_Deformation_Rate_24hr = mean(Deformation_Rate_24hr, na.rm = TRUE),
    Avg_Inverse_Velocity_1hr = mean(Inverse_Velocity_1hr, na.rm = TRUE),
    Avg_Inverse_Velocity_4hr = mean(Inverse_Velocity_4hr, na.rm = TRUE),
    Avg_Inverse_Velocity_12hr = mean(Inverse_Velocity_12hr, na.rm = TRUE),
    Avg_Inverse_Velocity_24hr = mean(Inverse_Velocity_24hr, na.rm = TRUE),
    Avg_Acceleration_1hr = mean(Acceleration_1hr, na.rm = TRUE),
    Avg_Acceleration_4hr = mean(Acceleration_4hr, na.rm = TRUE),
    Avg_Acceleration_12hr = mean(Acceleration_12hr, na.rm = TRUE),
    Avg_Acceleration_24hr = mean(Acceleration_24hr, na.rm = TRUE)
  )

# Write the average data to a new Excel file
write.xlsx(average_data, average_output_path)

# Find the first and last timestamps for each unique "SourceFile"
timestamp_summary <- combined_data %>%
  group_by(SourceFile) %>%
  summarise(
    First_Timestamp = min(Time, na.rm = TRUE),
    Last_Timestamp = max(Time, na.rm = TRUE)
  )

# Print the sentence for each SourceFile
for (row in 1:nrow(timestamp_summary)) {
  source_file <- timestamp_summary$SourceFile[row]
  first_timestamp <- timestamp_summary$First_Timestamp[row]
  last_timestamp <- timestamp_summary$Last_Timestamp[row]
  cat(sprintf("L'analyse contient les données de %s du %s jusqu'au %s.\n", 
              source_file, first_timestamp, last_timestamp))
}
