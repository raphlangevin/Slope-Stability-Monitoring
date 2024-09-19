library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
library(paletteer)

# File paths
blast_data_path <- "C:/Users/rlangevin/Desktop/WIP/events/01_Blast_event/02_blast_data_processed/blast_data_processed.xlsx"
radar_data_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/02_Radar_Data_Processed/radar_data_process_average.xlsx"
cumulative_precip_path <- "C:/Users/rlangevin/Desktop/WIP/Météo/02_Cumulative_Precipitation/cumulative_precipitation.xlsx"
temperature_data_path <- "C:/Users/rlangevin/Desktop/WIP/Météo/03_Temperature/Temperature.xlsx"
processed_event_path <- "C:/Users/rlangevin/Desktop/WIP/events/00_Processed_event/processed_event.xlsx"
output_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/12_Radar_Back-Analysis_Triggering_Factors/plots/"

# Read data
blast_data_processed <- read_excel(blast_data_path)
radar_data_process_average <- read_excel(radar_data_path, col_types = c("date", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
cumulative_precipitation <- read_excel(cumulative_precip_path, col_types = c("date", "numeric", "numeric"))
temperature_data <- read_excel(temperature_data_path, col_types = c("date", "numeric"))
processed_event <- read_excel(processed_event_path)

# Central position and radius
central_easting <- 716469.318
central_northing <- 5334584.166
central_elevation <- 252.777
radius <- 250

# Calculate distance from the central point
blast_data_processed <- blast_data_processed %>%
  mutate(distance = sqrt((.[[5]] - central_easting)^2 + (.[[6]] - central_northing)^2 + (.[[7]] - central_elevation)^2))

# Filter data within the specified radius
blast_data_filtered <- blast_data_processed %>%
  filter(distance <= radius)

# Ensure Timestamp is of POSIXct type
blast_data_filtered <- blast_data_filtered %>%
  mutate(Timestamp = as.POSIXct(Timestamp, format="%Y-%m-%d %H:%M:%S"))

processed_event <- processed_event %>%
  mutate(Timestamp = as.POSIXct(Timestamp, format="%Y-%m-%d %H:%M:%S"))

# Ensure the Timestamp column in cumulative_precipitation is correctly formatted as Date
cumulative_precipitation <- cumulative_precipitation %>%
  mutate(Timestamp = as.Date(Timestamp))

temperature_data <- temperature_data %>%
  rename(Timestamp = Recorded) %>%
  mutate(Timestamp = as.Date(Timestamp))

# Merge precipitation and temperature data with radar data by date using left_join
radar_data_process_average <- radar_data_process_average %>%
  mutate(Date = as.Date(Time)) %>%
  left_join(cumulative_precipitation, by = c("Date" = "Timestamp")) %>%
  left_join(temperature_data, by = c("Date" = "Timestamp"))

# Fill NA values in cumulative precipitation and temperature with the last observed value
radar_data_process_average <- radar_data_process_average %>%
  arrange(Time) %>%
  fill(Cumulative_Precipitation, Temperature, .direction = "down")

# Adjust cumulative precipitation to start from zero
initial_precipitation <- first(na.omit(radar_data_process_average$Cumulative_Precipitation))
radar_data_process_average <- radar_data_process_average %>%
  mutate(Cumulative_Precipitation = Cumulative_Precipitation - initial_precipitation)

# Find the first and last timestamps for each unique "SourceFile"
timestamp_summary <- radar_data_process_average %>%
  group_by(SourceFile) %>%
  summarise(
    First_Timestamp = min(Time, na.rm = TRUE),
    Last_Timestamp = max(Time, na.rm = TRUE)
  )

# Merge the timestamp summary with the radar data
radar_data_with_timestamps <- radar_data_process_average %>%
  left_join(timestamp_summary, by = "SourceFile")

# Function to create the plot for a given SourceFile
plot_for_SourceFile <- function(SourceFile) {
  SourceFile_data <- radar_data_with_timestamps %>% filter(SourceFile == SourceFile)
  SourceFile_data_long <- SourceFile_data %>%
    pivot_longer(cols = c("Avg_displacement_mm", "Avg_Deformation_Rate_1hr", "Avg_Acceleration_1hr"),
                 names_to = "variable", values_to = "value")
  SourceFile_data_long$variable <- factor(SourceFile_data_long$variable, levels = c("Avg_displacement_mm", "Avg_Deformation_Rate_1hr", "Avg_Acceleration_1hr"))
  first_timestamp <- unique(SourceFile_data$First_Timestamp)
  last_timestamp <- unique(SourceFile_data$Last_Timestamp)
  
  p1 <- ggplot() +
    geom_line(data = SourceFile_data_long, aes(x = Time, y = value, color = variable)) +
    geom_vline(data = blast_data_filtered, aes(xintercept = as.numeric(Timestamp), linetype = "Blast Event"), color = "red") +
    facet_wrap(~ variable, scales = "free_y", ncol = 1, labeller = as_labeller(c(
      Avg_displacement_mm = "Cumulative Displacement over time [mm]",
      Avg_Deformation_Rate_1hr = "Displacement rate over an hourly running window [mm/h]",
      Avg_Acceleration_1hr = "Acceleration over an hourly running window [mm/h^2]"
    ))) +
    scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Precipitation [mm]")) +
    geom_line(data = SourceFile_data, aes(x = Time, y = Cumulative_Precipitation, group = 1, color = "Cumulative Precipitation"), size = 1) +
    labs(
      title = "Average Cumulative displacement, velocity, acceleration, and precipitation over time based on selected Pixels",
      subtitle = paste(unique(SourceFile_data$SourceFile), "dataset from", format(first_timestamp, "%e %B %Y"), "to", format(last_timestamp, "%e %B %Y")),
      x = "Time",
      y = "Value",
      color = "Variable",
      linetype = "Event",
      caption = paste("Average LOS cumulative displacement, deformation rate, and acceleration captured by the ground-based radar SSR624XT from", format(first_timestamp, "%e %B %Y"), "to", format(last_timestamp, "%e %B %Y"), "\n compared to the cumulative precipitation and blast events within", radius, "meters radius from instabilities indicated by vertical red lines")
    ) +
    scale_color_manual(values = c("Avg_displacement_mm" = "blue", "Avg_Deformation_Rate_1hr" = "green", "Avg_Acceleration_1hr" = "orange", "Cumulative Precipitation" = "purple")) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p1)
  ggsave(filename = paste0("SourceFile_", SourceFile, "_plot_with_cumulative_precipitation_without_processed_event.png"), plot = p1, path = output_path, width = 10, height = 12)
  
  processed_event_filtered <- processed_event %>% filter(Source != "Radar Event")
  
  p2 <- ggplot(SourceFile_data_long, aes(x = Time, y = value, color = variable)) +
    geom_line() +
    geom_vline(data = processed_event_filtered, aes(xintercept = as.numeric(Timestamp), color = Source), linetype = "solid") +
    scale_colour_paletteer_d("ggthemes::Classic_10") +
    facet_wrap(~ variable, scales = "free_y", ncol = 1, labeller = as_labeller(c(
      Avg_displacement_mm = "Cumulative Displacement over time [mm]",
      Avg_Deformation_Rate_1hr = "Displacement rate over an hourly running window [mm/h]",
      Avg_Acceleration_1hr = "Acceleration over an hourly running window [mm/h^2]"
    ))) +
    scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Precipitation [mm]")) +
    geom_line(data = SourceFile_data, aes(x = Time, y = Cumulative_Precipitation, group = 1, color = "Cumulative Precipitation"), size = 1) +
    labs(
      title = "Average Cumulative displacement, velocity and acceleration, and precipitation over time based on selected Pixels",
      subtitle = paste(unique(SourceFile_data$SourceFile), "dataset from", format(first_timestamp, "%e %B %Y"), "to", format(last_timestamp, "%e %B %Y")),
      x = "Time",
      y = "Value",
      caption = paste("Average LOS cumulative displacement, deformation rate, and acceleration captured by the ground-based radar SSR624XT from", format(first_timestamp, "%e %B %Y"), "to", format(last_timestamp, "%e %B %Y"), "\n compared to the cumulative precipitation and blast events within", radius, "meters radius from instabilities indicated by vertical red lines")
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p2)
  ggsave(filename = paste0("SourceFile_", SourceFile, "_plot_with_cumulative_precipitation_with_processed_event.png"), plot = p2, path = output_path, width = 10, height = 12)
  
  p3 <- ggplot() +
    geom_line(data = SourceFile_data_long, aes(x = Time, y = value, color = variable)) +
    geom_vline(data = blast_data_filtered, aes(xintercept = as.numeric(Timestamp), linetype = "Blast Event"), color = "red") +
    facet_wrap(~ variable, scales = "free_y", ncol = 1, labeller = as_labeller(c(
      Avg_displacement_mm = "Cumulative Displacement over time [mm]",
      Avg_Deformation_Rate_1hr = "Displacement rate over an hourly running window [mm/h]",
      Avg_Acceleration_1hr = "Acceleration over an hourly running window [mm/h^2]"
    ))) +
    scale_y_continuous(sec.axis = sec_axis(~ ., name = "Daily Total Precipitation [mm]")) +
    geom_line(data = SourceFile_data, aes(x = Time, y = Precipitation_mm, group = 1, color = "Daily Total Precipitation"), size = 1) +
    labs(
      title = paste("Average Cumulative displacement, velocity, acceleration, and precipitation over time based on selected Pixels"),
      subtitle = paste(unique(SourceFile_data$SourceFile), "dataset from", format(first_timestamp, "%e %B %Y"), "to", format(last_timestamp, "%e %B %Y")),
      x = "Time",
      y = "Value",
      color = "Variable",
      linetype = "Event",
      caption = paste("Average LOS cumulative displacement, deformation rate, and acceleration captured by the ground-based radar SSR624XT from", format(first_timestamp, "%e %B %Y"), "to", format(last_timestamp, "%e %B %Y"), "\n compared to the daily total precipitation and blast events within", radius, "meters radius from instabilities indicated by vertical red lines")
    ) +
    scale_color_manual(values = c("Avg_displacement_mm" = "blue", "Avg_Deformation_Rate_1hr" = "green", "Avg_Acceleration_1hr" = "orange", "Daily Total Precipitation" = "purple")) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p3)
  ggsave(filename = paste0("SourceFile_", SourceFile, "_plot_with_daily_cumulative_precipitation_without_processed_event.png"), plot = p3, path = output_path, width = 10, height = 12)
  
  p4 <- ggplot(SourceFile_data_long, aes(x = Time, y = value, color = variable)) +
    geom_line() +
    geom_vline(data = processed_event_filtered, aes(xintercept = as.numeric(Timestamp), color = Source), linetype = "solid") +
    scale_colour_paletteer_d("ggthemes::Classic_10") +
    facet_wrap(~ variable, scales = "free_y", ncol = 1, labeller = as_labeller(c(
      Avg_displacement_mm = "Cumulative Displacement over time [mm]",
      Avg_Deformation_Rate_1hr = "Displacement rate over an hourly running window [mm/h]",
      Avg_Acceleration_1hr = "Acceleration over an hourly running window [mm/h^2]"
    )))  +
    scale_y_continuous(sec.axis = sec_axis(~ ., name = "Daily Total Precipitation [mm]")) +
    geom_line(data = SourceFile_data, aes(x = Time, y = Precipitation_mm, group = 1, color = "Daily Total Precipitation"), size = 1) +
    labs(
      title = paste("Average Cumulative displacement, velocity, acceleration, and precipitation over time based on selected Pixels"),
      subtitle = paste(unique(SourceFile_data$SourceFile), "dataset from", format(first_timestamp, "%e %B %Y"), "to", format(last_timestamp, "%e %B %Y")),
      x = "Time",
      y = "Value",
      color = "Variable",
      linetype = "Event",
      caption = paste("Average LOS cumulative displacement, deformation rate, and acceleration captured by the ground-based radar SSR624XT from", format(first_timestamp, "%e %B %Y"), "to", format(last_timestamp, "%e %B %Y"), "\n compared to the daily total precipitation and blast events within", radius, "meters radius from instabilities indicated by vertical red lines")
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p4)
  
  ggsave(filename = paste0("SourceFile_", SourceFile, "_plot_with_daily_cumulative_precipitation_with_processed_event.png"), plot = p4, path = output_path, width = 10, height = 12)
  
  # Normalize temperature data
  temp_range <- range(SourceFile_data$Temperature, na.rm = TRUE)
  y_range <- c(-30, 30)
  scale_factor <- diff(y_range) / diff(temp_range)
  offset <- y_range[1] - temp_range[1] * scale_factor
  
  SourceFile_data$Normalized_Temperature <- SourceFile_data$Temperature * scale_factor + offset
  
  # Plot 5: Cumulative displacement with Temperature
  p5 <- ggplot(SourceFile_data_long, aes(x = Time, y = value, color = variable)) +
    geom_line() +
    geom_vline(data = processed_event_filtered, aes(xintercept = as.numeric(Timestamp), color = Source), linetype = "solid") +
    scale_colour_paletteer_d("ggthemes::Classic_10") +
    facet_wrap(~ variable, scales = "free_y", ncol = 1, labeller = as_labeller(c(
      Avg_displacement_mm = "Cumulative Displacement over time [mm]",
      Avg_Deformation_Rate_1hr = "Displacement rate over an hourly running window [mm/h]",
      Avg_Acceleration_1hr = "Acceleration over an hourly running window [mm/h^2]"
    ))) +
    scale_y_continuous(
      sec.axis = sec_axis(~ (. - offset) / scale_factor, name = "Temperature (degC)")
    ) +
    geom_line(data = SourceFile_data, aes(x = Time, y = Normalized_Temperature, group = 1, color = "Temperature (degC)"), size = 1) +
    labs(
      title = paste("Average Cumulative displacement, velocity, acceleration, and temperature over time based on selected Pixels"),
      subtitle = paste(unique(SourceFile_data$SourceFile), "dataset from", format(first_timestamp, "%e %B %Y"), "to", format(last_timestamp, "%e %B %Y")),
      x = "Time",
      y = "Value",
      color = "Variable",
      linetype = "Event",
      caption = paste("Average LOS cumulative displacement, deformation rate, and acceleration captured by the ground-based radar SSR624XT from", format(first_timestamp, "%e %B %Y"), "to", format(last_timestamp, "%e %B %Y"), "\n compared to the temperature and blast events within", radius, "meters radius from instabilities indicated by vertical red lines")
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p5)
  
  ggsave(filename = paste0("SourceFile_", SourceFile, "_plot_cumulative_precipitation_temperature.png"), plot = p5, path = output_path, width = 10, height = 6)
}

# Loop over each unique SourceFile and create the plots
unique_SourceFiles <- unique(radar_data_process_average$SourceFile)
for (SourceFile in unique_SourceFiles) {
  plot_for_SourceFile(SourceFile)
}

print("Plots have been successfully created and saved.")
