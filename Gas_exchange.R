library(tidyverse)
library(scico)
library(ggplot2)
library(ggtext)
library(ggpubr)
library(ggbreak) 
library(dplyr)
library(lubridate)
library(gridExtra)
library(paletteer)
library(writexl)
library(grid)
options(digits = 2)

#################################################################################
#############################Read #Me############################################
#################################################################################
#we want to look at the ambient/abiotic data collected (Ca, RH, VPD, TempLEaf, PARTOp)
#and we will plot it as a panel of 6 plots.
#we want to clean data that doesn't make sense biologically, meaning negative gs, ci higher than 450 or negative.
#and we want to identify errors in what is left (outliers via z-score) and remove it. 
#the blocks below help you go through it


#Explanation about z-scores for identifying the outliers
#In our context: We calculate the z-scores for each parameter (A, E, ci, GH2O) in the dataset.
#We identify the outliers as those data points with a z-score greater than 2 or less than -2.
# In the plots, these outliers are highlighted in red to distinguish them from the rest of the data.
# The original values are plotted on the graph, and outliers are marked in red to show how far they deviate from the norm.

#################################################################################
#################################################################################
#################################################################################

#####Tables for inital checkups of ambient parameters######
# Read column names from csv (careful: single quotation in header and ISO 8859-1 encoding)
GFScolumns <- read.csv(file="control.csv", sep=';', nrows=1, header=FALSE, quote="", na.strings=";;", stringsAsFactors=FALSE,fileEncoding="ISO-8859-1")
# Load data and add column names
GFSdata <- read.csv(file="control.csv", sep=';', skip=2, header=FALSE, col.names=GFScolumns, na.strings=";;", stringsAsFactors=FALSE,fileEncoding="ISO-8859-1")
# Exclude ZP Mode rows and select only numeric columns
GFSdf <- GFSdata %>%
  filter(Code == 'MP_010') %>%
  rowid_to_column() %>%
  rename(Step=rowid)

# Select columns I wanna keep
GFSdf[,c(1,2,3,15,20,23,25,27,28,29,30,31,32,33)] -> GFSdf_select

# Filter the dataframe to keep rows where ci is within the range of 0 to 450 and GH2O is greater than 0
GFSdf_filter_ci_gs <- GFSdf

#### checking abiotic parameters ####

# Create a new column for the date and hour (ignoring minutes)
GFSdf_filter_ci_gs <- GFSdf_filter_ci_gs %>%
  mutate(DateHour = floor_date(ymd_hms(paste(Date, Time)), unit = "hour"))

# Generate the dataframe with mean, median, and SD for each DateHour
summary_abiotic_all <- GFSdf_filter_ci_gs %>%
  group_by(DateHour) %>%
  summarise(
    mean_Flow = mean(Flow, na.rm = TRUE),
    median_Flow = median(Flow, na.rm = TRUE),
    sd_Flow = sd(Flow, na.rm = TRUE),
    mean_PARtop = mean(PARtop, na.rm = TRUE),
    median_PARtop = median(PARtop, na.rm = TRUE),
    sd_PARtop = sd(PARtop, na.rm = TRUE),
    mean_rh = mean(rh, na.rm = TRUE),
    median_rh = median(rh, na.rm = TRUE),
    sd_rh = sd(rh, na.rm = TRUE),
    mean_VPD = mean(VPD, na.rm = TRUE),
    median_VPD = median(VPD, na.rm = TRUE),
    sd_VPD = sd(VPD, na.rm = TRUE),
    mean_Tleaf = mean(Tleaf, na.rm = TRUE),
    median_Tleaf = median(Tleaf, na.rm = TRUE),
    sd_Tleaf = sd(Tleaf, na.rm = TRUE),
    mean_ca = mean(ca, na.rm = TRUE),
    median_ca = median(ca, na.rm = TRUE),
    sd_ca = sd(ca, na.rm = TRUE)
  )

# Short version df
summary_abiotic_all[,c(1,2,5,8,11,14,17)] -> summary_abiotic_means

# Save summary tables as Excel files
write_xlsx(summary_abiotic_all, "ambient_summary_all.xlsx")
write_xlsx(summary_abiotic_means, "ambient_summary_means.xlsx")


####Plotting ambient######

# Function to create line plots
create_plot <- function(data, parameter, y_label) {
  p <- ggplot(data, aes(x = DateHour)) +
    geom_line(aes(y = get(paste0("mean_", parameter)), color = "Mean"), size = 1.2) +  # Thicker lines
    geom_errorbar(aes(ymin = get(paste0("mean_", parameter)) - get(paste0("sd_", parameter)), 
                      ymax = get(paste0("mean_", parameter)) + get(paste0("sd_", parameter))), 
                  width = 0.2, color = "gray") +
    labs(title = paste0("Time Series Plot of ", parameter),
         x = "Hour",
         y = y_label) +
    scale_color_manual(values = c("Mean" = "black")) +
    theme_minimal(base_family = "sans") +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),  
          axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    scale_x_datetime(date_labels = "%H:%M", date_breaks = "6 hours")  # Set x-axis to 6-hour intervals
  
  return(p)
}

# Create line plots for each parameter (excluding Flow)
plot_PARtop <- create_plot(summary_abiotic_all, "PARtop", "PARtop")
plot_rh <- create_plot(summary_abiotic_all, "rh", "Relative Humidity (%)")
plot_VPD <- create_plot(summary_abiotic_all, "VPD", "VPD")
plot_Tleaf <- create_plot(summary_abiotic_all, "Tleaf", "Tleaf")
plot_ca <- create_plot(summary_abiotic_all, "ca", "Ca")

# Save the individual plots with increased dimensions
ggsave("ambient_plot_PARtop.tiff", plot = plot_PARtop, device = "tiff", dpi = 300, units = "cm", width = 25, height = 20)
ggsave("ambient_plot_rh.tiff", plot = plot_rh, device = "tiff", dpi = 300, units = "cm", width = 25, height = 20)
ggsave("ambient_plot_VPD.tiff", plot = plot_VPD, device = "tiff", dpi = 300, units = "cm", width = 25, height = 20)
ggsave("ambient_plot_Tleaf.tiff", plot = plot_Tleaf, device = "tiff", dpi = 300, units = "cm", width = 25, height = 20)
ggsave("ambient_plot_ca.tiff", plot = plot_ca, device = "tiff", dpi = 300, units = "cm", width = 25, height = 20)

# Combine all parameters into a single summary plot with secondary y-axis
summary_plot <- ggplot(summary_abiotic_all, aes(x = DateHour)) +
  geom_line(aes(y = mean_PARtop, color = "PARtop"), size = 1.2) +
  geom_line(aes(y = mean_rh, color = "RH (%)"), size = 1.2) +
  geom_line(aes(y = mean_VPD, color = "VPD"), size = 1.2) +
  geom_line(aes(y = mean_Tleaf, color = "Tleaf"), size = 1.2) +
  geom_line(aes(y = mean_ca, color = "Ca"), size = 1.2) +
  scale_y_continuous(
    name = "RH (%), VPD, Tleaf",
    sec.axis = sec_axis(~ ., name = "Ca, PARtop"),  # Adjust secondary axis scale
    limits = c(0, 500)
  ) +
  scale_color_paletteer_d("MetBrewer::Austria") +
  labs(title = "Summary Plot of All Parameters",
       x = "Hour",
       y = "Values") +
  theme_minimal(base_family = "sans") +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = rel(1.2)),  # Increase font size for axis text
        axis.title = element_text(size = rel(1.2)),  # Increase font size for axis titles
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "6 hours")  # Set x-axis to 6-hour intervals

# Arrange the plots into a single figure
summary_figure <- grid.arrange(plot_PARtop, plot_rh, plot_VPD, plot_Tleaf, plot_ca, summary_plot, ncol = 2)

# Save the individual summary plot
ggsave("ambient_summary_plot.tiff", plot = summary_plot, device = "tiff", dpi = 300, units = "cm", width = 20, height = 15)

# Save the summary figure with all plots
ggsave("ambient_summary_all.tiff", plot = summary_figure, device = "tiff", dpi = 300, units = "cm", width = 40, height = 30)


###############################################################################################
####Looking at the gas exchange/ photosynthesis data#####

#####cleaning outliers for x-axis in hours####

#Part 1: Loading Libraries, Calculating Z-Scores, and Identifying Outliers

# Assuming your data frame is named GFSdf_filter_ci_gs
# Convert Date and Time to a single datetime object
GFSdf_filter_ci_gs$Datetime <- as.POSIXct(paste(GFSdf_filter_ci_gs$Date, GFSdf_filter_ci_gs$Time), format = "%Y-%m-%d %H:%M:%S")

# Create a new column for the date and hour (ignoring minutes)
GFSdf_filter_ci_gs <- GFSdf_filter_ci_gs %>%
  mutate(DateHour = floor_date(ymd_hms(paste(Date, Time)), unit = "hour"))

# Find the first and last dates in your dataset
first_date <- min(as.Date(GFSdf_filter_ci_gs$Datetime))
last_date <- max(as.Date(GFSdf_filter_ci_gs$Datetime))

# Create datetime objects for 19:00 on the first and last days
start_datetime <- as.POSIXct(paste(first_date, "18:00:00"), format = "%Y-%m-%d %H:%M:%S")
end_datetime <- as.POSIXct(paste(last_date, "8:00:00"), format = "%Y-%m-%d %H:%M:%S")

# Filter the dataset to only keep entries within this datetime range
filtered_data <- GFSdf_filter_ci_gs %>%
  filter(Datetime >= start_datetime & Datetime <= end_datetime)
filtered_data -> GFSdf_filter_ci_gs 

# Convert Date and Time to a single datetime object
GFSdf_filter_ci_gs$Datetime <- as.POSIXct(paste(GFSdf_filter_ci_gs$Date, GFSdf_filter_ci_gs$Time), format = "%Y-%m-%d %H:%M:%S")

# Calculate z-scores for each parameter
parameters <- c("A", "E", "ci", "GH2O")
for (param in parameters) {
  GFSdf_filter_ci_gs[[paste0(param, "_z")]] <- scale(GFSdf_filter_ci_gs[[param]])
}

# Identify outliers for each parameter (z-score > 2 or < -2)
for (param in parameters) {
  GFSdf_filter_ci_gs[[paste0("Outlier_", param)]] <- abs(GFSdf_filter_ci_gs[[paste0(param, "_z")]]) > 2
}




#Part 2: Creating and Saving the Plots with Outliers Highlighted

# Find the first and last dates in your dataset
first_date <- min(as.Date(GFSdf_filter_ci_gs$Datetime))
last_date <- max(as.Date(GFSdf_filter_ci_gs$Datetime))

# Create datetime objects for 19:00 on the first and last days
start_datetime <- as.POSIXct(paste(first_date, "19:00:00"), format = "%Y-%m-%d %H:%M:%S")
end_datetime <- as.POSIXct(paste(last_date, "7:00:00"), format = "%Y-%m-%d %H:%M:%S")

# Function to create individual plots with day and night shading and highlight outliers
create_plot_with_outliers <- function(data, parameter, y_label, title) {
  outlier_column <- paste0("Outlier_", parameter)
  p <- ggplot(data, aes(x = Datetime)) +
    geom_line(aes(y = get(parameter)), color = "black", size = 1.2) +
    geom_point(aes(y = get(parameter), color = get(outlier_column)), size = 2) +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +  # Color outliers in red
    labs(title = title,
         x = "Time (hours)",
         y = y_label) +
    theme_minimal(base_family = "sans") +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = rel(1.5)),  # Increase font size for axis text
          axis.title = element_text(size = rel(1.5)),  # Increase font size for axis titles
          plot.title = element_text(size = rel(1.8)),  # Increase font size for plot title
          axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    scale_x_datetime(date_labels = "%H:%M", date_breaks = "6 hours", limits = c(start_datetime, end_datetime), expand = c(0, 0))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))  # Double the number of y-axis ticks
  
  # Add the night bar with grey and white shading for day and night
  p <- p + geom_rect(data = data, 
                     aes(xmin = Datetime, xmax = lead(Datetime, default = last(Datetime)), 
                         ymin = -Inf, ymax = Inf, fill = PARtop < 10), 
                     inherit.aes = FALSE, alpha = 0.2, color = NA) +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "#1a1a1a"), guide = "none") +  # Use darker grey for night
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5)  # Add a continuous line at y=0
  
  return(p)
}

# Create and save plots for each parameter with outliers highlighted
parameters_labels <- list(
  A = expression(A~"(µmol m"^-2~"s"^-1~")"),
  ci = expression(c[i]~"(ppm)"),
  E = expression(E~"(mmol m"^-2~"s"^-1~")"),
  GH2O = expression(g[s]~"(mmol m"^-2~"s"^-1~")")
)

for (param in names(parameters_labels)) {
  plot <- create_plot_with_outliers(GFSdf_filter_ci_gs, param, parameters_labels[[param]], paste0("Plot of ", param, " with Outliers"))
  ggsave(paste0("outlier_plot_", param, ".tiff"), plot = plot, device = "tiff", dpi = 300, units = "cm", width = 25, height = 20)
  #print(plot)
}


#Part 3: Removing Outliers, Re-Plotting the Data, and Saving the New Plots

# Remove outliers from the data
GFSdf_filter_ci_gs_clean <- GFSdf_filter_ci_gs
for (param in parameters) {
  outlier_column <- paste0("Outlier_", param)
  GFSdf_filter_ci_gs_clean <- GFSdf_filter_ci_gs_clean[!GFSdf_filter_ci_gs_clean[[outlier_column]], ]
}
# Save the cleaned data frame to an Excel file
write_xlsx(GFSdf_filter_ci_gs_clean, "GFS_final_clean.xlsx")


# Function to create individual plots without outliers
create_plot_without_outliers <- function(data, parameter, y_label, title) {
  p <- ggplot(data, aes(x = Datetime)) +
    geom_line(aes(y = get(parameter)), color = "black", size = 1.2) +
    geom_point(aes(y = get(parameter)), color = "black") +
    labs(title = title,
         x = "Time (hours)",
         y = y_label) +
    theme_minimal(base_family = "sans") +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = rel(1.5)),  # Increase font size for axis text
          axis.title = element_text(size = rel(1.5)),  # Increase font size for axis titles
          plot.title = element_text(size = rel(1.8)),  # Increase font size for plot title
          axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    scale_x_datetime(date_labels = "%H:%M", date_breaks = "6 hours", limits = c(start_datetime, end_datetime), expand = c(0, 0))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))  # Double the number of y-axis ticks
  
  # Add the night bar with grey and white shading for day and night
  p <- p + geom_rect(data = data, 
                     aes(xmin = Datetime, xmax = lead(Datetime, default = last(Datetime)), 
                         ymin = -Inf, ymax = Inf, fill = PARtop < 10), 
                     inherit.aes = FALSE, alpha = 0.2, color = NA) +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "#1a1a1a"), guide = "none") +  # Use darker grey for night
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5)  # Add a continuous line at y=0
  
  return(p)
}

# Create and save plots for each parameter without outliers
for (param in names(parameters_labels)) {
  plot_clean <- create_plot_without_outliers(GFSdf_filter_ci_gs_clean, param, parameters_labels[[param]], paste0("Plot of ", param, " without Outliers"))
  ggsave(paste0("indiv1_plot_", param, "_clean.tiff"), plot = plot_clean, device = "tiff", dpi = 300, units = "cm", width = 25, height = 20)
  #print(plot_clean)
}




#################Adjusting font size in individual plots for paper############
# Part 1: Creating and Saving the Clean Plots without Outliers
# Find the first and last dates in your dataset
first_date <- min(as.Date(GFSdf_filter_ci_gs_clean$Datetime))
last_date <- max(as.Date(GFSdf_filter_ci_gs_clean$Datetime))

# Create datetime objects for 19:00 on the first and last days
start_datetime <- as.POSIXct(paste(first_date, "18:00:00"), format = "%Y-%m-%d %H:%M:%S")
end_datetime <- as.POSIXct(paste(last_date, "8:00:00"), format = "%Y-%m-%d %H:%M:%S")

# Function to create individual clean plots with uniform and larger fonts
create_clean_plot <- function(data, parameter, y_label) {
  p <- ggplot(data, aes(x = Datetime)) +
    geom_line(aes(y = get(parameter)), color = "black", size = 0.6) +  # Thinner lines connecting the dots
    geom_point(aes(y = get(parameter)), color = "black") +
    labs(x = "Time (hours)",
         y = y_label) +
    theme_minimal(base_family = "sans") +
    theme(panel.background = element_rect(fill = "white", colour = NA),  # Remove black line around the plot
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 30),  # Set font size for axis text
          axis.title = element_text(size = 30),  # Set font size for axis titles
          axis.text.x = element_text(angle = 45, hjust = 1, size = 30),  # Set font size for rotated x-axis labels
          plot.margin = margin(t = 20, r = 20, b = 20, l = 20)) +  # Increase plot area
    scale_x_datetime(date_labels = "%H:%M", date_breaks = "12 hours", limits = c(start_datetime, end_datetime), expand = c(0, 0)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))  # Ensure same number of y-axis ticks
  
  # Add the night bar with grey and white shading for day and night
  p <- p + geom_rect(data = data, 
                     aes(xmin = Datetime, xmax = lead(Datetime, default = last(Datetime)), 
                         ymin = -Inf, ymax = Inf, fill = PARtop < 10), 
                     inherit.aes = FALSE, alpha = 0.2, color = NA) +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "#1a1a1a"), guide = "none") +  # Use darker grey for night
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5)  # Add a continuous line at y=0
  
  return(p)
}

# Create and save clean plots for each parameter
parameters_labels <- list(
  A = expression(A~"(µmol m"^-2~"s"^-1~")"),
  ci = expression(c[i]~"(ppm)"),
  E = expression(E~"(mmol m"^-2~"s"^-1~")"),
  GH2O = expression(g[s]~"(mmol m"^-2~"s"^-1~")")
)

for (param in names(parameters_labels)) {
  assign(paste0("plot_", param, "_clean"), create_clean_plot(GFSdf_filter_ci_gs_clean, param, parameters_labels[[param]]))
  ggsave(paste0("PHOT_summary_", param, "_clean.tiff"), get(paste0("plot_", param, "_clean")), device = "tiff", dpi = 300, units = "cm", width = 25, height = 20)
  print(get(paste0("plot_", param, "_clean")))
}



#reducing number of ticks in y-axis for plot
# Create and save clean plot for parameter A with 8 y-axis ticks
plot_A_clean <- create_clean_plot(GFSdf_filter_ci_gs_clean, "A", expression(A~"(µmol m"^-2~"s"^-1~")")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))  # Set the number of y-axis ticks to 8

# Save the adjusted plot for A
ggsave("PHOT_summary_A_clean2.tiff", plot_A_clean, device = "tiff", dpi = 300, units = "cm", width = 25, height = 20)
print(plot_A_clean)


