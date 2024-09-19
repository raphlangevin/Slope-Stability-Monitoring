library(rlang)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(gganimate)
library(gifski)
library(ggpp)  # For geom_table and stat_fmt_table
library(tibble)
library(gridExtra)
library(ggplotify)
library(grid)

# Source external script
source("C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/05_Radar_Back-Analysis_Alarming_Approach_Failure_Event/05_Radar_Back-Analysis_Alarming_Approach_Failure_Event.R")

# Create tables for each unique Description and Timestamp
unique_combinations <- unique(failure_alarms %>% select(Description, Timestamp))
tables <- list()

for (i in 1:nrow(unique_combinations)) {
  description <- unique_combinations$Description[i]
  timestamp <- unique_combinations$Timestamp[i]
  
  table <- closest_times %>%
    filter(Description == description & Timestamp == timestamp) %>%
    select(Target_Time, Interval, Pixel, Closest_Displacement) %>%
    mutate(Interval = case_when(
      Interval == "Time_Minus_01_Hour" ~ "01 Hour",
      Interval == "Time_Minus_04_Hours" ~ "04 Hours",
      Interval == "Time_Minus_12_Hours" ~ "12 Hours",
      Interval == "Time_Minus_24_Hours" ~ "24 Hours",
      TRUE ~ Interval
    )) %>%
    mutate(Interval = factor(Interval, levels = c("01 Hour", "04 Hours", "12 Hours", "24 Hours"))) %>%
    arrange(Interval) %>%
    pivot_wider(names_from = Pixel, values_from = Closest_Displacement)
  
  tables[[paste(description, timestamp, sep = "_")]] <- table
}

# Define the output directory
output_dir <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/06_Radar_Back-Analysis_Alarming_Approach_Displacement/"
dir.create(output_dir, showWarnings = FALSE)  # Ensure the directory exists

# Display and save the tables as .png
for (i in 1:nrow(unique_combinations)) {
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

# Merge Failure_Displacement with closest_times and calculate Delta_displacement
plot_data <- closest_times %>%
  mutate(Delta_displacement = Failure_Displacement - Closest_Displacement)

# Define the output directory for full timeline plots
output_dir_full <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/06_Radar_Back-Analysis_Alarming_Approach_Displacement/"
dir.create(output_dir_full, showWarnings = FALSE)  # Ensure the directory exists

# Create the full timeline plot with subtable and inset plot
for (desc in unique(failure_alarms$Description)) {
  for (interval in unique(failure_alarms$Interval)) {
    data <- plot_data %>%
      filter(Description == desc & Interval == interval)
    
    # Create a table with all pixels
    pixel_table <- data %>%
      select(Pixel, Closest_Displacement, Failure_Displacement, Delta_displacement)
    
    # Define the zoomed-in plot (24 hours before the failure event)
    inset_plot <- ggplot(data) +
      geom_segment(aes(x = Timestamp, xend = Target_Time, y = Failure_Displacement, yend = Closest_Displacement, colour = Pixel), linewidth = 1) +
      geom_line(data = combined_data %>% filter(Time >= unique(data$Timestamp) - hours(24) & Time <= unique(data$Timestamp)), aes(x = Time, y = displacement_mm, group = Pixel, colour = Pixel), alpha = 0.5) +
      labs(x = NULL, y = NULL) +
      theme_bw(base_size = 8) +
      scale_colour_discrete(guide = "none")
    
    # Find the beginning timestamp of the plot
    beginning_timestamp <- min(combined_data$Time)
    
    # Create a tibble with the inset plot positioned at 600 hours after the beginning of the plot
    data_tb <- tibble(x = beginning_timestamp + hours(600), y = 0, plot = list(inset_plot))
    
    # Create the main plot with the inset plot
    main_plot <- ggplot(data) +
      geom_segment(aes(x = Timestamp, xend = Target_Time, y = Failure_Displacement, yend = Closest_Displacement, colour = Pixel), linewidth = 1) +
      geom_line(data = combined_data, aes(x = Time, y = displacement_mm, group = Pixel, colour = Pixel), alpha = 0.5) +
      geom_plot(data = data_tb, aes(x, y, label = plot), vjust = 0, hjust = 0) +
      annotate(geom = "rect", xmin = unique(data$Timestamp) - hours(24), xmax = unique(data$Timestamp), ymin = min(data$Closest_Displacement), ymax = max(data$Failure_Displacement), linetype = "dotted", fill = NA, colour = "black") +
      labs(x = "Time", y = "Cumulative displacement [mm]",
           title = paste("Displacement from Failure Event\nto Target Time for", desc),
           subtitle = paste("Interval:", interval),
           caption = "Lines represent displacement from failure event time to target time for each pixel and interval.\nOverlaid cumulative displacement data.") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Create a table grob
    table_grob <- tableGrob(pixel_table)
    
    # Convert the plot to a grob
    plot_grob <- ggplotify::as.grob(main_plot)
    
    # Arrange the plot and table together
    combined_grob <- grid.arrange(plot_grob, table_grob, nrow = 2, heights = c(4/5, 1/5))
    
    # Save the combined plot and table
    ggsave(filename = file.path(output_dir_full, paste0(gsub(" ", "_", desc), "_", interval, "_inset.png")), plot = combined_grob, width = 10, height = 8)
  }
}
