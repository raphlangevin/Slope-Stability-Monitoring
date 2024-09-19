# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(openxlsx)
library(purrr)
library(ggplot2)

# Read the consolidated data from the specified file
input_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/02_Radar_Data_Processed/radar_data_processed.xlsx"
output_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/04_Radar_Velocity_Over_Time/radar_velocity.xlsx"
combined_data <- read_excel(input_path)

# Read the event data
event_path <- "C:/Users/rlangevin/Desktop/WIP/events/event.csv"
event <- read.csv(event_path, colClasses = c("Timestamp" = "POSIXct"))

# Function to calculate rolling average for a given window in hours
rolling_avg_func <- function(df, window_hours) {
  map_dbl(1:nrow(df), function(i) {
    start_time <- df$Time[i] - hours(window_hours)
    subset_df <- df[df$Time >= start_time & df$Time <= df$Time[i], ]
    mean(subset_df$displacement_mm, na.rm = TRUE)
  })
}

# Calculate rolling averages and deformation rate for each SourceFile
velocity_data <- combined_data %>%
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

# Write the result to a new Excel file
write.xlsx(velocity_data, output_path)
View(velocity_data)

# Create plots for each "Pixel"
unique_pixels <- unique(velocity_data$Pixel)
for (pixel in unique_pixels) {
  df_to_plot <- velocity_data %>% filter(Pixel == pixel)
  
  p1 <- ggplot(df_to_plot, aes(x = Time)) +
    geom_line(aes(y = Deformation_Rate_1hr, colour = "60 minutes")) +
    geom_line(aes(y = Deformation_Rate_4hr, colour = "240 minutes")) +
    geom_line(aes(y = Deformation_Rate_12hr, colour = "720 minutes")) +
    geom_line(aes(y = Deformation_Rate_24hr, colour = "1440 minutes")) +
    geom_vline(data = event, aes(xintercept = as.numeric(Timestamp), linetype = Type, colour = Type)) +
    scale_colour_manual("Velocity over Time period", 
                        values = c("60 minutes" = "blue", "240 minutes" = "gray", 
                                   "720 minutes" = "yellow", "1440 minutes" = "red")) +
    labs(x = "Time", y = "Velocity (mm/hr)", title = "Velocity curves with different dt Values", caption = "Fig. Velocity along LOS over time of points considered representative of the behavior of different sectors of the failure", subtitle = paste("Pixel", pixel, "- Data from", df_to_plot$SourceFile[1])) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2))
  
  # Display the plot
  print(p1)
  
  # Save the plot
  file_name <- paste0("Radar_Velocity_Plot_", pixel, "_", df_to_plot$SourceFile[1], ".png")
  ggsave(filename = file_name, plot = p1, path = dirname(output_path))
}
