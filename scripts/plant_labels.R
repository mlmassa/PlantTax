# This script generates a PDF of FSU herbarium labels.
# It must be run before generating a report because it imports data.

# Setup -------------------------------------------------------------------

# Load necessary packages
library(googledrive) # Data import
library(googlesheets4) # Date import
library(tidyverse)
library(glue)
library(ggtext)
library(gridExtra)
library(sf)

# Load data
# Fetch from Google Sheets
plants <- 
  drive_get("Plant collections") |> 
  gs4_get() |> 
  read_sheet(1) |> 
  # Remove any records with no coordinates
  filter(!is.na(easting)) |> 
  # In case this is re-run in the future when I have more collections
  filter(as.numeric(str_sub(id, -3, -1)) <= 20) |> 
  # Convert to spatial object
  st_as_sf(
    coords = c("easting", "northing"),
    remove = F,
    # Coordinate reference system is UTM 17N (EPSG code 32617)
    crs = 32617) |> 
  # Transform to longlat
  st_transform(crs = 4326) |> 
  # Get longitude and latitude
  mutate(
    long = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]) |> 
  st_drop_geometry()
  
write_csv(plants, "data/processed/plant_collections.csv")
glimpse(plants)

# Set label dimensions ----------------------------------------------------

# Set page dimensions
page_width <- 8.5
page_height <- 11

# How many labels do you want per page
n_wide <- 2
n_high <- 3

# Margin from text to edge of label
box_margin <- 0.125

# Calculate size of label
label_margin <- 0.125
label_width <- (page_width / n_wide) - label_margin * 2
label_height <- (page_height / n_high) - label_margin * 2

# Alternatively, explicitly set label size (override line 55-56)
label_width <- 4.25 - box_margin
label_height <- 3.5 - box_margin

# Geom_text uses font size in mm. Set the size you want in pts and convert
text_size_pt <- 10
text_size <- text_size_pt * 0.352777777777777
font_family <- "serif"

# Generate labels ---------------------------------------------------------

# Create formatted, combined text for labels
text_grobs <-
  plants |> 
  mutate(
    # Round lat/long to 5 digits (~1m accuracy)
    lat = round(lat, 5),
    long = round(long, 5),
    # Capitalize state and county
    state = str_to_upper(state),
    county = str_to_upper(county),
    # Convert date to format "March 5, 2023"
    date = format(date, "%B %e, %Y"),
    # Create header text
    start_text = glue(.sep = "<br>",
      "Frostburg State University Herbarium",
      "{state}, USA",
      "{date}"),
    # Create formatted text
    text = glue(.sep = "<br>",
      "<b><br><i>{species}</i></b>  {authority}",
      "{family}<br>",
      "<b>{county} COUNTY:</b> {site}. {location} Elev: {elev_m}m. 
        ({lat}, {long})<br>",
      "Species Description: {species_description} {frequency} 
       Growing in association with <i>{associates}</i>.<br>",
      "Collector: {collector}"),
    # Create right-justified collection ID text
    right_text = glue("#{id}")
  )

# Create herbarium label cards as text box plots
plots <-
  text_grobs |> 
  # Do the following for each row of the data...
  rowwise() |> 
  mutate(
    # Create combined text plot
    text_plot = list(
      # Initialize ggplot
      ggplot() +
      theme_void() +
      labs(x = NULL, y = NULL) +
      scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
      # Header text
      geom_textbox(
        data = tibble(start_text = start_text),
        aes(label = start_text),
        family = font_family,
        box.colour = NA,
        fill = NA,
        halign = 0.5, # Center
        valign = 1, # Align with top
        x = 0.5, y = 0.5, 
        size = text_size,
        width = unit(label_width, "in"), 
        height = unit(label_height, "in"),
        box.margin = unit(c(box_margin, box_margin, box_margin, box_margin), "in")
      ) +
      # Main label text
      geom_textbox(
        data = tibble(text = text), 
        aes(label = text),
        box.colour = NA,
        fill = NA,
        family = font_family,
        # Align with bottom; to align with top it would need enough <br> at start of grob
        valign = 0, 
        x = 0.5, y = 0.5,
        size = text_size,
        width = unit(label_width, "in"), 
        height = unit(label_height, "in"),
        box.margin = unit(c(box_margin, box_margin, box_margin, box_margin), "in")
      ) +
      # Collection ID (right-justified)
      geom_textbox(
        data = tibble(right_text = right_text), 
        aes(label = right_text),
        box.colour = NA,
        fill = NA,
        family = font_family,
        valign = 0, # Align with bottom
        halign = 1, # Right-justify
        x = 0.5, y = 0.5,
        size = text_size,
        width = unit(label_width - box_margin, "in"), 
        height = unit(label_height, "in"),
        box.margin = unit(c(box_margin, box_margin, box_margin, box_margin), "in")
      ) + 
      # Box outline (text transparent)
      geom_textbox(
        data = tibble(text = text), 
        aes(label = text),
        color = NA,
        # Outline to cut along
        box.colour = "gray70",
        # Sharp corners
        box.r = unit(0, "pt"),
        fill = NA,
        family = font_family,
        valign = 0,
        x = 0.5, y = 0.5,
        size = text_size,
        width = unit(label_width + box_margin, "in"), 
        height = unit(label_height + box_margin, "in"),
        box.margin = unit(c(box_margin, box_margin, box_margin, box_margin), "in")
      )
    )
  ) |> 
  pull(text_plot)

# Preview a plot
plots[[19]]

# If the text is overlapping, try adjusting the box size or font size.

# Save PDF for printing

ggsave(
  "output/plantlabels.pdf", 
  plot = marrangeGrob(
    plots, 
    ncol = n_wide, 
    nrow = n_high,
    as.table = F,
    padding = unit(0.125, "in"),
    top = NULL), 
  width = page_width, 
  height = page_height, 
  units = "in"
)