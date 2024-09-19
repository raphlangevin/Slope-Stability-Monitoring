# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(pracma)
library(paletteer)  # Load the paletteer package

# Read the radar data
radar_data <- read_excel("C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/02_Radar_Data_Processed/radar_data_processed.xlsx", 
                         col_types = c("date", "numeric", "text", 
                                       "text", "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric"))

# Define file paths
processed_event_file_path <- "C:/Users/rlangevin/Desktop/WIP/events/00_Processed_event/processed_event.xlsx"

# Read the data from the Excel file
processed_event <- read_excel(processed_event_file_path)

# Convert Timestamps to POSIXct
processed_event$Timestamp <- as.POSIXct(processed_event$Timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Get the unique pixel values
unique_pixels <- unique(radar_data$Pixel)

# Function to find inflection points where both deformation rate and acceleration change to negative signs
find_inflexion_points <- function(time, deformation_rate, acceleration) {
  # Identify points where both deformation rate and acceleration change to negative signs
  inflection_points <- which(diff(sign(deformation_rate)) != 0 & diff(sign(acceleration)) != 0 &
                               sign(deformation_rate[-1]) == -1 & sign(acceleration[-1]) == -1)
  
  # Return the times of the inflection points
  return(time[inflection_points + 1])
}

# Function to create the plot for a given pixel with inflection points
plot_for_pixel_with_inflection <- function(pixel) {
  # Filter the data for the current pixel
  pixel_data <- radar_data %>% filter(Pixel == pixel)
  
  # Reshape the data for faceting
  pixel_data_long <- pixel_data %>%
    pivot_longer(cols = c("displacement_mm", "Deformation_Rate_24hr", "Acceleration_24hr"),
                 names_to = "variable", values_to = "value")
  
  # Convert 'variable' to a factor and specify the desired order
  pixel_data_long$variable <- factor(pixel_data_long$variable, levels = c("displacement_mm", "Deformation_Rate_24hr", "Acceleration_24hr"))
  
  # Find inflection points where both Deformation_Rate_24hr and Acceleration_24hr change to negative signs
  inflection_points <- find_inflexion_points(pixel_data$Time, pixel_data$Deformation_Rate_24hr, pixel_data$Acceleration_24hr)
  
  # Define the start and end time for captions
  start_time <- min(pixel_data$Time)
  end_time <- max(pixel_data$Time)
  
  # Define labels for facets
  facet_labels <- c(
    displacement_mm = "Cumulative Displacement over time [mm]",
    Deformation_Rate_24hr = "Displacement rate over a 24-hours running window [mm/h]",
    Acceleration_24hr = "Acceleration over a 24-hours running window [mm/^2]"
  )
  
  # Create the plot without x-axis limit
  p <- ggplot(pixel_data_long, aes(x = Time, y = value, color = variable)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(inflection_points), linetype = "dotted", color = "darkgrey") +
    geom_vline(data = processed_event, aes(xintercept = as.numeric(Timestamp), color = Source), linetype = "solid") +
    scale_colour_paletteer_d("ggthemes::Classic_10") +
    facet_wrap(~ variable, scales = "free_y", ncol = 1, labeller = as_labeller(facet_labels)) +
    labs(
      title = paste("Pixel", pixel, "Cumulative displacement, velocity and acceleration over time"),
      subtitle = paste(unique(radar_data$SourceFile), "dataset from",
                       format(start_time, "%Y-%m-%d"), "to", format(end_time, "%Y-%m-%d")),
      x = "Time",
      y = "Value",
      caption = paste(
        pixel, "Cumulative displacement, velocity and acceleration over time from",
        format(start_time, "%Y-%m-%d"), "to", format(end_time, "%Y-%m-%d"),
        "with vertical lines delineating inflection time of negative velocity and acceleration.\n The inflection point method enables the identification of the specific moment when change in both velocity and acceleration occur.\n The first set of multiple inflection points are the Onset-of-acceleration (OOA)."
      )
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
  
  # Save the plot
  ggsave(filename = paste0("Pixel_", pixel, "_plot.png"), plot = p, path = "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/10_Radar_Back-Analysis_TOF_Analysis_OOA_Selection", width = 10, height = 12)
  
  # Define the specific date range for the second pipeline
  specific_start_time <- as.POSIXct("2024-03-26 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")
  specific_end_time <- as.POSIXct("2024-03-30 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")
  
  # Create the plot with the specific date range
  p_specific <- ggplot(pixel_data_long, aes(x = Time, y = value, color = variable)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(inflection_points), linetype = "dashed", color = "darkgrey") +
    geom_vline(data = processed_event, aes(xintercept = as.numeric(Timestamp), color = Source), linetype = "solid") +
    scale_colour_paletteer_d("ggthemes::Classic_10") +
    facet_wrap(~ variable, scales = "free_y", ncol = 1, labeller = as_labeller(facet_labels)) +
    labs(
      title = paste("Pixel", pixel, "Cumulative displacement, velocity and acceleration over time"),
      subtitle = paste(unique(radar_data$SourceFile), "dataset from",
                       format(specific_start_time, "%Y-%m-%d"), "to", format(specific_end_time, "%Y-%m-%d")),
      x = "Time",
      y = "Value",
      caption = paste(pixel,"Cumulative displacement, velocity and acceleration over time from",
                      format(specific_start_time, "%Y-%m-%d"), "to", format(specific_end_time, "%Y-%m-%d"),
                      "with vertical lines delineating inflection time of negative velocity and acceleration.\n The inflection point method enables the identification of the specific moment when change in both velocity and acceleration occur.\n The first set of multiple inflection points are the Onset-of-acceleration (OOA)."
      )
    ) +
    scale_x_datetime(limits = c(specific_start_time, specific_end_time)) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Save the plot with the specific date range
  ggsave(filename = paste0("Pixel_", pixel, "_plot_specific.png"), plot = p_specific, path = "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/10_Radar_Back-Analysis_TOF_Analysis_OOA_Selection", width = 10, height = 12)
  
  print(p_specific)
}

# Function to create the plot for a given pixel without inflection points
plot_for_pixel_without_inflection <- function(pixel) {
  # Filter the data for the current pixel
  pixel_data <- radar_data %>% filter(Pixel == pixel)
  
  # Reshape the data for faceting
  pixel_data_long <- pixel_data %>%
    pivot_longer(cols = c("displacement_mm", "Deformation_Rate_24hr", "Acceleration_24hr"),
                 names_to = "variable", values_to = "value")
  
  # Convert 'variable' to a factor and specify the desired order
  pixel_data_long$variable <- factor(pixel_data_long$variable, levels = c("displacement_mm", "Deformation_Rate_24hr", "Acceleration_24hr"))
  
  # Define the start and end time for captions
  start_time <- min(pixel_data$Time)
  end_time <- max(pixel_data$Time)
  
  # Define labels for facets
  facet_labels <- c(
    displacement_mm = "Cumulative Displacement over time [mm]",
    Deformation_Rate_24hr = "Displacement rate over a 24-hours running window [mm/h]",
    Acceleration_24hr = "Acceleration over a 24-hours running window [mm/^2]"
  )
  
  # Create the plot without inflection points and without x-axis limit
  p <- ggplot(pixel_data_long, aes(x = Time, y = value, color = variable)) +
    geom_line() +
    geom_vline(data = processed_event, aes(xintercept = as.numeric(Timestamp), color = Source), linetype = "solid") +
    scale_colour_paletteer_d("ggthemes::Classic_10") +
    facet_wrap(~ variable, scales = "free_y", ncol = 1, labeller = as_labeller(facet_labels)) +
    labs(
      title = paste("Pixel", pixel, "Cumulative displacement, velocity and acceleration over time"),
      subtitle = paste(unique(radar_data$SourceFile), "dataset from",
                       format(start_time, "%Y-%m-%d"), "to", format(end_time, "%Y-%m-%d")),
      x = "Time",
      y = "Value",
      caption = paste(
        pixel, "Cumulative displacement, velocity and acceleration over time from",
        format(start_time, "%Y-%m-%d"), "to", format(end_time, "%Y-%m-%d")
      )
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
  
  # Save the plot
  ggsave(filename = paste0("Pixel_", pixel, "_plot_without_inflection.png"), plot = p, path = "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/10_Radar_Back-Analysis_TOF_Analysis_OOA_Selection", width = 10, height = 12)
  
  # Define the specific date range for the second pipeline
  specific_start_time <- as.POSIXct("2024-03-26 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")
  specific_end_time <- as.POSIXct("2024-03-30 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")
  
  # Create the plot with the specific date range without inflection points
  p_specific <- ggplot(pixel_data_long, aes(x = Time, y = value, color = variable)) +
    geom_line() +
    geom_vline(data = processed_event, aes(xintercept = as.numeric(Timestamp), color = Source), linetype = "solid") +
    scale_colour_paletteer_d("ggthemes::Classic_10") +
    facet_wrap(~ variable, scales = "free_y", ncol = 1, labeller = as_labeller(facet_labels)) +
    labs(
      title = paste("Pixel", pixel, "Cumulative displacement, velocity and acceleration over time"),
      subtitle = paste(unique(radar_data$SourceFile), "dataset from",
                       format(specific_start_time, "%Y-%m-%d"), "to", format(specific_end_time, "%Y-%m-%d")),
      x = "Time",
      y = "Value",
      caption = paste(pixel,"Cumulative displacement, velocity and acceleration over time from",
                      format(specific_start_time, "%Y-%m-%d"), "to", format(specific_end_time, "%Y-%m-%d")
      )
    ) +
    scale_x_datetime(limits = c(specific_start_time, specific_end_time)) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Save the plot with the specific date range without inflection points
  ggsave(filename = paste0("Pixel_", pixel, "_plot_specific_without_inflection.png"), plot = p_specific, path = "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/10_Radar_Back-Analysis_TOF_Analysis_OOA_Selection", width = 10, height = 12)
  
  print(p_specific)
}

# Loop over each unique pixel and create the plots
for (pixel in unique_pixels) {
  plot_for_pixel_with_inflection(pixel)
  plot_for_pixel_without_inflection(pixel)
}

print("Plots have been successfully created and saved.")

