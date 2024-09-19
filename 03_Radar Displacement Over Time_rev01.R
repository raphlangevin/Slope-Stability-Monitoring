# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr) # For filling missing values
library(patchwork) # For combining plots

# Define file paths
output_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/02_Radar_Data_Processed/radar_data_processed.xlsx"
precip_path <- "C:/Users/rlangevin/Desktop/WIP/Météo/02_Cumulative_Precipitation/cumulative_precipitation.xlsx"
temp_path <- "C:/Users/rlangevin/Desktop/WIP/Météo/03_Temperature/Temperature.xlsx"
save_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/03_Radar_Displacement_Over_Time/Plots/"

# Create the output directory if it doesn't exist
if (!dir.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

# Read the data
combined_data <- read_excel(output_path)
cumulative_precipitation <- read_excel(precip_path, col_types = c("date", "numeric", "numeric"))
temperature <- read_excel(temp_path, col_types = c("date", "numeric"))

# Ensure the Timestamp column in cumulative_precipitation is correctly formatted as Date
cumulative_precipitation <- cumulative_precipitation %>%
  mutate(Timestamp = as.Date(Timestamp))

# Ensure the Recorded column in temperature is correctly formatted as Date
temperature <- temperature %>%
  rename(Timestamp = Recorded) %>%
  mutate(Timestamp = as.Date(Timestamp))

# Merge precipitation and temperature data with combined data by date using left_join
combined_data <- combined_data %>%
  mutate(Date = as.Date(Time)) %>%
  left_join(cumulative_precipitation, by = c("Date" = "Timestamp")) %>%
  left_join(temperature, by = c("Date" = "Timestamp"))

# Fill NA values in cumulative precipitation and temperature with the last observed value
combined_data <- combined_data %>%
  arrange(Time) %>%
  fill(Cumulative_Precipitation, .direction = "down") %>%
  fill(Temperature, .direction = "down")

# Find the first Date and apply an offset to the Cumulative_Precipitation values
first_date <- min(combined_data$Date, na.rm = TRUE)
initial_precipitation <- combined_data %>%
  filter(Date == first_date) %>%
  pull(Cumulative_Precipitation) %>%
  first()

combined_data <- combined_data %>%
  mutate(Cumulative_Precipitation = Cumulative_Precipitation - initial_precipitation)

# Display and save individual plots
unique_files <- unique(combined_data$SourceFile)
for (file in unique_files) {
  plot_data <- combined_data %>% filter(SourceFile == file)
  
  # Plot with displacement and cumulative precipitation data
  p1 <- ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = displacement_mm, colour = Pixel), show.legend = TRUE) +
    geom_line(aes(y = Cumulative_Precipitation / 0.25, linetype = "Cumulative Precipitation"), linewidth = 1, color = "blue") +
    scale_y_continuous(
      name = "Cumulative displacement [mm]",
      sec.axis = sec_axis(~ . * 0.25, name = "Cumulative Precipitation (mm)")
    ) +
    scale_color_hue(direction = 1, name = "Pixel") +
    scale_linetype_manual(values = c("Cumulative Precipitation" = "solid"), name = "") +
    labs(x = "Time",
         caption = "Fig. Cumulative displacement along LOS over time of points considered representative of the behavior of different sectors of the failure",
         title = "Cumulative Displacement with Precipitation Data",
         subtitle = paste("Data from -", file)) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2))
  
  print(p1)
  
  # Plot with displacement and temperature data
  p2 <- ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = displacement_mm, colour = Pixel), show.legend = TRUE) +
    geom_line(aes(y = Temperature / 0.01, linetype = "Temperature"), linewidth = 1, color = "red") +
    scale_y_continuous(
      name = "Cumulative displacement [mm]",
      sec.axis = sec_axis(~ . * 0.01, name = "Temperature (degC)")
    ) +
    scale_color_hue(direction = 1, name = "Pixel") +
    scale_linetype_manual(values = c("Temperature" = "solid"), name = "") +
    labs(x = "Time",
         caption = "Fig. Cumulative displacement along LOS over time of points considered representative of the behavior of different sectors of the failure",
         title = "Cumulative Displacement with Temperature Data",
         subtitle = paste("Data from -", file)) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2))
  
  # Combine the two plots using patchwork
  combined_plot <- p1 / p2
  
  print(combined_plot)
  
  # Save the combined plot
  ggsave(filename = paste0(save_path, file, "_combined_plot.png"), plot = combined_plot, width = 10, height = 12)
}
