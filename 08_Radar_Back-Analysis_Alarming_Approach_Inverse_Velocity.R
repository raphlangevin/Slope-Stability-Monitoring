# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(openxlsx)
library(purrr)
library(ggplot2)
library(tidyr)
library(webshot)
library(readr)
library(gtable)
library(grid)

# Load the custom R script
source("C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/05_Radar_Back-Analysis_Alarming_Approach_Failure_Event/05_Radar_Back-Analysis_Alarming_Approach_Failure_Event.R")

failure_event <- read_excel("C:/Users/rlangevin/Desktop/WIP/events/02_failure_event/failure_event.xlsx", 
                            col_types = c("date", "text", "text", 
                                          "numeric", "numeric", "numeric"))

# Extract timestamps for each event (excluding "SW Failure Event")
timestamps <- failure_event %>%
  select(Timestamp, Name, Description) %>%
  mutate(Timestamp = ymd_hms(Timestamp)) %>%
  filter(Description != "SW Failure Event")

# Extract the South-West failure timestamp
sw_failure_event <- failure_event %>%
  filter(Name == "Barnat pit South-West Failure" & Description == "Barnat pit South-West Failure") %>%
  mutate(Timestamp = ymd_hms(Timestamp))
sw_failure_timestamp <- sw_failure_event %>%
  pull(Timestamp)

# Create plots for each "Description"
unique_descriptions <- unique(failure_alarms$Description)
for (event in unique_descriptions) {
  event_data <- failure_alarms %>% filter(Description == event)
  
  for (interval in c("01 hour prior to Alarm", "04 hours prior to Alarm", "12 hours prior to Alarm", "24 hours prior to Alarm", "72 hours prior to Alarm")) {
    target_time <- event_data %>% filter(Interval == interval) %>% pull(Target_Time)
    if (is.na(target_time) || length(target_time) == 0) next
    
    # Extract the interval hours correctly
    interval_hours <- as.numeric(gsub(".*(\\d{2}) hour.*", "\\1", interval)) # Extract the interval hours
    
    # Create a zoomed-in plot for the specified period before each failure event
    zoomed_plot <- ggplot(combined_data %>% filter(Time >= target_time & Time <= event_data$Timestamp[1]), aes(x = Time)) +
      geom_line(aes(y = Inverse_Velocity_1hr, colour = "0060 minutes"), linewidth = 0.5) +
      geom_line(aes(y = Inverse_Velocity_4hr, colour = "0240 minutes"), linewidth = 0.5) +
      geom_line(aes(y = Inverse_Velocity_12hr, colour = "0720 minutes"), linewidth = 0.5) +
      geom_line(aes(y = Inverse_Velocity_24hr, colour = "1440 minutes"), linewidth = 0.5) +
      geom_vline(aes(xintercept = as.numeric(event_data$Timestamp[1]), linetype = "Failure Alarm", colour = "Failure Alarm"), linewidth = 1) +
      geom_vline(aes(xintercept = as.numeric(sw_failure_timestamp), linetype = "SW Failure Event", colour = "SW Failure Event"), linewidth = 1) +
      geom_vline(data = timestamps, aes(xintercept = as.numeric(Timestamp), linetype = Description, colour = Description), linewidth = 0.5) + # Add vertical lines for all events
      scale_colour_manual(name = "Inverse Velocity over time period", 
                          values = c("0060 minutes" = "blue", "0240 minutes" = "gray", 
                                     "0720 minutes" = "yellow", "1440 minutes" = "red", 
                                     "Failure Alarm" = "black",
                                     "Barnat pit South-West Failure" = "purple",
                                     "Trend Update Point (TU)" = "orange",
                                     "Onset-Of-Acceleration (OOA)" = "brown"),
                          breaks = c("0060 minutes", "0240 minutes", "0720 minutes", "1440 minutes", 
                                     "Failure Alarm", "Barnat pit South-West Failure", 
                                     "Trend Update Point (TU)", "Onset-Of-Acceleration (OOA)")) +
      scale_linetype_manual(name = "Failure Alarm & Event", 
                            values = c("Failure Alarm" = "dashed",
                                       "Barnat pit South-West Failure" = "dotted",
                                       "Trend Update Point (TU)" = "dotted",
                                       "Onset-Of-Acceleration (OOA)" = "dotted"),
                            breaks = c("Failure Alarm", "Barnat pit South-West Failure", 
                                       "Trend Update Point (TU)", "Onset-Of-Acceleration (OOA)")) +
      labs(x = "Time", y = "Inverse Velocity [h/mm]", 
           title = paste("Inverse Velocity over time", interval, event),
           subtitle = paste(unique(combined_data$SourceFile), "dataset from time period between", format(target_time,"%d %b %Y"), "and", format(event_data$Timestamp[1],"%d %b %Y")), 
           caption = paste("Trend Update Point (TU) selection based on inverse-velocity trends leading to", sw_failure_event$Name,"from time period between", format(target_time,"%d %b %Y %H:%M"), "and", format(event_data$Timestamp[1],"%d %b %Y %H:%M.\n Before the Onset-of-Acceleration (OOA), pre-failure behavior was observed. Between the OOA and the Trend Update Point (TU), an asymptotic trend was evident. Subsequently, the inverse velocity transitioned to a linear trend, ultimately leading to slope failure."))) + # Add SourceFile to the subtitle
      theme_minimal() +
      facet_wrap(vars(Pixel)) +
      theme(legend.position = "bottom",
            panel.grid.major = element_line(size = 0.1, colour = "gray"),
            panel.grid.minor = element_line(size = 0.05, colour = "lightgray"),
            axis.text.x = element_text(size = 8, angle = 90, hjust = 1)) + # Make x-axis labels vertical
      guides(colour = guide_legend(order = 1, override.aes = list(linetype = c("solid", "solid", "solid", "solid", "solid", "solid", "solid","solid"))), 
             linetype = guide_legend(order = 2)) + # Arrange legend items
      scale_x_datetime(limits = c(target_time, event_data$Timestamp[1]), 
                       breaks = seq(from = target_time, to = event_data$Timestamp[1], length.out = 4), # Ensure at least 4 labels
                       date_labels = "%Y-%m-%d\n%H:%M") + # Consistent grid and datetime labels
      scale_y_continuous(limits = c(0, 20)) # Ensure the y-axis does not exceed 4 h/mm
    
    # Convert the plot to a gtable
    gt <- ggplot_gtable(ggplot_build(zoomed_plot))
    
    # Get the position of the legends in the gtable
    leg1 <- which(sapply(gt$grobs, function(x) x$name) == "guide-box")
    
    # Display the plots with the modified legend
    grid.newpage()
    grid.draw(gt)
    
    # Define the output folder for plots
    plot_output_folder <- "C:/Users/rlangevin/Desktop/WIP/Radar/Radar_Analysis/08_Radar_Back-Analysis_Alarming_Approach_Inverse_Velocity"
    
    # Save the plots
    output_filename <- paste0("Zoomed_Radar_Inverse_Velocity_Plot_", event, "_", interval, ".png")
    ggsave(filename = output_filename, plot = zoomed_plot, path = plot_output_folder)
    
    
  }
}

