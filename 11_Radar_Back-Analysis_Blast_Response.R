library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(lubridate)

# Read in the Excel file
blast_data_processed <- read_excel("C:/Users/rlangevin/Desktop/WIP/events/01_Blast_event/02_blast_data_processed/blast_data_processed.xlsx")

# Central position coordinates and radius
central_easting <- 716469.318
central_northing <- 5334584.166
central_elevation <- 252.777
radius <- 250

# Calculate distance from central point
blast_data_processed <- blast_data_processed %>%
  mutate(distance = sqrt((blast_data_processed[[5]] - central_easting)^2 + 
                           (blast_data_processed[[6]] - central_northing)^2 + 
                           (blast_data_processed[[7]] - central_elevation)^2))

# Filter based on distance within the specified radius
blast_data_filtered <- blast_data_processed %>%
  filter(distance <= radius)

# Ensure Timestamp is of POSIXct type
blast_data_filtered <- blast_data_filtered %>%
  mutate(Timestamp = as.POSIXct(Timestamp, format="%Y-%m-%d %H:%M:%S"))

# Specify the output file path
output_file_path <- "C:/Users/rlangevin/Desktop/WIP/events/01_Blast_event/02_blast_data_processed/blast_data_filtered.xlsx"

# Write the filtered data frame to a new Excel file
write_xlsx(blast_data_filtered, path = output_file_path)

# Define file paths
output_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/02_Radar_Data_Processed/radar_data_processed.xlsx"
save_path <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/11_Radar_Back-Analysis_Blast_Response/plots/" # Ensure this directory exists


radar_data_processed <- read_excel("C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/02_Radar_Data_Processed/radar_data_processed.xlsx", 
                                   col_types = c("date", "numeric", "text", 
                                                 "text", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric"))
# Ensure the save path directory exists
if (!dir.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

# Display and save individual plots
unique_files <- unique(radar_data_processed$SourceFile)

for (file in unique_files) {
  plot_data <- radar_data_processed %>% filter(SourceFile == file)
  
  # Ensure Time is of POSIXct type
  plot_data <- plot_data %>%
    mutate(Time = as.POSIXct(Time, format="%Y-%m-%d %H:%M:%S"))
  
  # Filter blast_data_filtered to only include timestamps within the range of plot_data$Time
  blast_data_filtered_for_plot <- blast_data_filtered %>%
    filter(Timestamp >= min(plot_data$Time) & Timestamp <= max(plot_data$Time)) %>%
    mutate(EventType = "Blast Event",
           hjust_value = ifelse(row_number() %% 2 == 0, 1, 2))  # Add a column for alternating hjust
  
  # Calculate the rounded number of months
  start_time <- min(plot_data$Time)
  end_time <- max(plot_data$Time)
  months_diff <- interval(start_time, end_time) %/% months(1)
  rounded_months <- round(months_diff)
  time_period <- paste(rounded_months, ifelse(rounded_months == 1, "month", "months"))
  
  # Plot with event data
  p1 <- ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = displacement_mm, colour = Pixel)) +
    geom_vline(data = blast_data_filtered_for_plot, aes(xintercept = Timestamp, linetype = EventType), color = "#D62728FF") +
    geom_text(data = blast_data_filtered_for_plot, aes(x = Timestamp, y = Inf, label = Nom, hjust = hjust_value), angle = 90, vjust = -0.5) +
    scale_color_hue(direction = 1) +
    scale_linetype_manual(values = c("Blast Event" = "dashed"), name = "Event Type") +
    labs(x = "Time", y = "Cumulative displacement [mm]",
         caption = paste("Cumulative displacement plot of different pixels measured by SSR624XT Radar from", 
                         format(start_time, "%Y-%m-%d"), 
                         "to", 
                         format(end_time, "%Y-%m-%d"), 
                         "with vertical lines delineating blast events within", 
                         radius, 
                         "meters"),
         title = "Cumulative Displacement with Blast Event",
         subtitle = paste("Data from -", file)) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p1)
  
  print(paste("The figure above shows the cumulative displacement plots for the selected control points for the complete data set",file, "over the period" , format(start_time, "%Y-%m-%d %H:%M:%S"), "to", format(end_time, "%Y-%m-%d %H:%M:%S"),". These trends represent the local response to mining activity during this period."))
  
  # Save the plot with event data
  ggsave(filename = paste0(save_path, file, "_event_plot.png"), plot = p1, width = 10, height = 6)
}

