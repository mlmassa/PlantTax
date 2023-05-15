# Load required packages
library(dplyr)
library(plotKML)

files <- list.files(path = "fwdataconversion/", full.names = T)

# Initialize empty data frame
wp_full <- NULL

for (i in 1:length(files)) {
  # Select file list and import data into R object
  wp_list <- readGPX(files[i], way = T)
  # Extract lat, long, elev, time, name, symbol and append to R dataframe
  wp_df <- wp_list$waypoints
  # Append dataframe from last index to a full waypoint object
  wp_full <- bind_rows(wp_full, wp_df)
}

# Export object with all waypoints to csv file
write.csv(wp_full, "hwa_waypoints.csv") 