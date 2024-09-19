# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(openxlsx)
library(purrr)
library(ggplot2)
library(tidyr)
library(readr)

# Process the radar data
input_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/02_Radar_Data_Processed/radar_data_processed.xlsx"
combined_data <- read_excel(input_path)

output_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/05_Radar_Back-Analysis_Alarming_Approach_Failure_Event/radar_combined_data.xlsx"
write.xlsx(combined_data, output_path)

# Process the event data
event_data_path <- "C:/Users/rlangevin/Desktop/WIP/events/00_Processed_event/processed_event.xlsx"
event <- read_excel(event_data_path)

event <- event %>%
  mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

# Generate unique Event_ID based on Timestamp
event <- event %>%
  mutate(Event_ID = dense_rank(Timestamp))

# Add Event_ID as a prefix to Description
event <- event %>%
  mutate(Description = paste(Event_ID, Description, sep = "_"))

failure_alarms <- event %>%
  filter(grepl("failure", Description, ignore.case = TRUE))

failure_alarms <- failure_alarms %>%
  mutate(`01 hour prior to Alarm` = Timestamp - hours(1),
         `04 hours prior to Alarm` = Timestamp - hours(4),
         `12 hours prior to Alarm` = Timestamp - hours(12),
         `24 hours prior to Alarm` = Timestamp - hours(24),
         `72 hours prior to Alarm` = Timestamp - hours(72)) %>%
  pivot_longer(cols = starts_with("01 hour") | starts_with("04 hours") | starts_with("12 hours") | starts_with("24 hours") | starts_with("72 hours"),
               names_to = "Interval",
               values_to = "Target_Time")

event_output_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/05_Radar_Back-Analysis_Alarming_Approach_Failure_Event/failure_alarms.xlsx"
write.xlsx(failure_alarms, event_output_path, rowNames = FALSE)

# Define a function to find the closest time and corresponding value for a specific pixel
find_closest_value <- function(target_time, times, values) {
  closest_row <- which.min(abs(difftime(times, target_time, units = "secs")))
  return(values[closest_row])
}

# Generate closest times for each failure event, time interval, and pixel
closest_times <- failure_alarms %>%
  crossing(Pixel = unique(combined_data$Pixel)) %>%
  rowwise() %>%
  mutate(Closest_Displacement = find_closest_value(Target_Time,
                                                   combined_data$Time[combined_data$Pixel == Pixel],
                                                   combined_data$displacement_mm[combined_data$Pixel == Pixel]),
         Closest_Deformation_Rate_1hr = find_closest_value(Target_Time,
                                                           combined_data$Time[combined_data$Pixel == Pixel],
                                                           combined_data$Deformation_Rate_1hr[combined_data$Pixel == Pixel]),
         Closest_Deformation_Rate_4hr = find_closest_value(Target_Time,
                                                           combined_data$Time[combined_data$Pixel == Pixel],
                                                           combined_data$Deformation_Rate_4hr[combined_data$Pixel == Pixel]),
         Closest_Deformation_Rate_12hr = find_closest_value(Target_Time,
                                                            combined_data$Time[combined_data$Pixel == Pixel],
                                                            combined_data$Deformation_Rate_12hr[combined_data$Pixel == Pixel]),
         Closest_Deformation_Rate_24hr = find_closest_value(Target_Time,
                                                            combined_data$Time[combined_data$Pixel == Pixel],
                                                            combined_data$Deformation_Rate_24hr[combined_data$Pixel == Pixel]),
         Closest_Inverse_Velocity_1hr = find_closest_value(Target_Time,
                                                           combined_data$Time[combined_data$Pixel == Pixel],
                                                           combined_data$Inverse_Velocity_1hr[combined_data$Pixel == Pixel]),
         Closest_Inverse_Velocity_4hr = find_closest_value(Target_Time,
                                                           combined_data$Time[combined_data$Pixel == Pixel],
                                                           combined_data$Inverse_Velocity_4hr[combined_data$Pixel == Pixel]),
         Closest_Inverse_Velocity_12hr = find_closest_value(Target_Time,
                                                            combined_data$Time[combined_data$Pixel == Pixel],
                                                            combined_data$Inverse_Velocity_12hr[combined_data$Pixel == Pixel]),
         Closest_Inverse_Velocity_24hr = find_closest_value(Target_Time,
                                                            combined_data$Time[combined_data$Pixel == Pixel],
                                                            combined_data$Inverse_Velocity_24hr[combined_data$Pixel == Pixel]),
         SourceFile = find_closest_value(Target_Time,
                                         combined_data$Time[combined_data$Pixel == Pixel],
                                         combined_data$SourceFile[combined_data$Pixel == Pixel])) %>%
  ungroup()

# Add the Failure_Displacement values
failure_times <- failure_alarms %>%
  crossing(Pixel = unique(combined_data$Pixel)) %>%
  rowwise() %>%
  mutate(Failure_Displacement = find_closest_value(Timestamp,
                                                   combined_data$Time[combined_data$Pixel == Pixel],
                                                   combined_data$displacement_mm[combined_data$Pixel == Pixel])) %>%
  ungroup() %>%
  select(Description, Timestamp, Interval, Pixel, Failure_Displacement) %>%
  distinct()

# Merge the closest times with the failure displacement
closest_times <- closest_times %>%
  left_join(failure_times, by = c("Description", "Timestamp", "Interval", "Pixel"))

closest_times_output_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/05_Radar_Back-Analysis_Alarming_Approach_Failure_Event/closest_times.xlsx"
write.xlsx(closest_times, closest_times_output_path, rowNames = FALSE)
