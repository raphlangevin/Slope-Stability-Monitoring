# Load necessary libraries
library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)

# Read the radar velocity data
input_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/04_Radar_Velocity_Over_Time/radar_velocity.xlsx"
output_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/09_Radar_Back-Analysis_TOF_Approach_Acceleration/radar_acceleration.xlsx"
velocity_data <- read_excel(input_path)

# Function to calculate acceleration for a given column of deformation rates
calc_acceleration <- function(rate_column, time_diff_column) {
  c(NA, diff(rate_column) / time_diff_column[-1])
}

# Calculate accelerations
acceleration_data <- velocity_data %>%
  group_by(SourceFile, Pixel) %>%
  mutate(
    Acceleration_1hr = calc_acceleration(Deformation_Rate_1hr, Time_Diff),
    Acceleration_4hr = calc_acceleration(Deformation_Rate_4hr, Time_Diff),
    Acceleration_12hr = calc_acceleration(Deformation_Rate_12hr, Time_Diff),
    Acceleration_24hr = calc_acceleration(Deformation_Rate_24hr, Time_Diff)
  )

# Write the result to a new Excel file
write.xlsx(acceleration_data, output_path)

# Generate plots for each "Pixel"
unique_pixels <- unique(acceleration_data$Pixel)
for (pixel in unique_pixels) {
  df_to_plot <- acceleration_data %>% filter(Pixel == pixel)
  
  p1 <- ggplot(df_to_plot, aes(x = Time)) +
    geom_line(aes(y = Acceleration_1hr, colour = "01hr")) +
    geom_line(aes(y = Acceleration_4hr, colour = "04hr")) +
    geom_line(aes(y = Acceleration_12hr, colour = "12hr")) +
    geom_line(aes(y = Acceleration_24hr, colour = "24hr")) +
    labs(x = "Time", y = "Acceleration (mm/hr^2)", title = paste("Acceleration over Time for Pixel", pixel)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2)) +
    facet_wrap(~ Pixel)
  
  # Display the plot
  print(p1)
  
  # Save the plot
  file_name <- paste0("Radar_Acceleration_Plot_", pixel, ".png")
  ggsave(filename = file_name, plot = p1, path = dirname(output_path))
}
