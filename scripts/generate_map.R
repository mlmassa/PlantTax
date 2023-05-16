# This script maps specimen locations with a hillshade basemap.

# Setup -------------------------------------------------------------------

# Load required packages
library(tidyverse)
library(sf)
library(tigris) # Fetch county/state/road/water shapefiles
options(tigris_use_cache = TRUE) # Don't redownload shapefiles
library(ggpubr) # Combine figure parts
library(terra) # Handle raster files
library(elevatr) # Fetch elevation data
library(tidyterra) # Make rasters play nice with ggplot

# Set ggplot theme
theme_set(
  theme_bw(base_size = 11.5) +
  theme(text = element_text(family = "Calibri")))


# Import data -------------------------------------------------------------

# Load data
plants <-
  read_csv("data/processed/plant_collections.csv") |> 
  # In case this is re-run in the future when I have more collections
  filter(as.numeric(str_sub(id, -3, -1)) <= 20) |> 
  # Convert to spatial object
  st_as_sf(
    coords = c("long", "lat"),
    remove = F,
    crs = 4326)

# Get one point per site (for large map)
sites <-
  plants |> 
  group_by(site) |> 
  summarise(st_union(geometry)) |> 
  st_centroid() 

# Set map parameters ------------------------------------------------------

# Get window for large-area map
bbox_large <-
  st_bbox(plants)

# Actually, I played with it and changed my mind
bbox_large[[1]] <- -80.5
bbox_large[[2]] <- 34.5
bbox_large[[3]] <- -76.1
bbox_large[[4]] <- 40.5

# Make bbox a plottable object (for inset map)
bbox_large_window <-
  st_as_sfc(bbox_large)

# Get window for small-area map
bbox_small <-
  plants |> 
  dplyr::filter(!state == "North Carolina") |> 
  st_buffer(dist = 2000) |> 
  st_bbox()

# Slightly expand
bbox_small <-
  bbox_small + c(-0.01, -0.051, 0.01, 0.03)

# Make bbox a plottable object
bbox_small_window <-
  st_as_sfc(bbox_small)

# Set hillshade palette
grays <- 
  colorRampPalette(c("gray30", "gray50", "#E8EAED", "white"))

# Get vector of hillshade colors
pal_gray <- 
  grays(1000)

# Symbol colors: color-blind friendly palette
# Chose to eliminate blues to prevent confusion with water bodies
okabe <-
  palette.colors(palette = "Okabe-Ito")[c(1, 2, 4, 5, 7, 8)] |> 
  unname() |> 
  rev()

water_col <- "#5295c9" 


# Get map data ------------------------------------------------------------

# Get states for map
# More than I ended up using, but could replot more zoomed out if needed
states <- 
  states() |> 
  dplyr::filter(
    NAME %in% c(
      "Virginia", "West Virginia", "Maryland", "District of Columbia", 
      "North Carolina", "Pennsylvania", "Delaware", "Ohio", 
      "South Carolina", "New Jersey")) |> 
  st_transform(crs = st_crs(plants))

# Get counties
counties <-
  counties(state = states$STUSPS) |> 
  st_transform(crs = st_crs(plants))

# Get lakes and large rivers
water <-
  bind_rows(
    area_water(
      state = "MD", 
      county = "Garrett"),
    area_water(
      state = "MD", 
      county = "Allegany"),
    area_water(
      state = "PA", 
      county = "Somerset"),
    area_water(
      state = "PA", 
      county = "Bedford"),
    area_water(
      state = "WV", 
      county = "Mineral")) |>
  st_transform(crs = st_crs(plants))

# Get rivers and streams
water_line <-
  bind_rows(
    linear_water(
      state = "MD", 
      county = "Garrett"), 
    linear_water(
      state = "MD", 
      county = "Allegany"),
    linear_water(
      state = "PA", 
      county = "Somerset"),
    linear_water(
      state = "PA", 
      county = "Bedford"),
    linear_water(
      state = "WV", 
      county = "Mineral")) |>
  st_transform(crs = st_crs(plants))

# Get main roads
roads <-
  bind_rows(
    primary_secondary_roads(state = "MD"),
    primary_secondary_roads(state = "PA"),
    primary_secondary_roads(state = "WV")) |> 
  st_transform(crs = st_crs(plants)) |> 
  st_crop(bbox_small + c(-0.1, -0.1, 0.1, 0.1))

# Get coarse elevation (large map) ----------------------------------------

# Download coarse elevation
elev_large <-
  get_elev_raster(
    as_Spatial(bbox_large_window), 
    neg_to_na = T, # Large negative numbers: eliminate
    z = 8) |> 
  # Downloads as raster; convert to terra
  rast()

# Convert elevation 0 (water, in this dataset) to NA. Imperfect, but we're not mapping coastline
elev_large <-
  # if/else for terra spatrasters
  ifel(elev_ <= 0, NA, elev_large)

# Create hillshade effect
hill_large <- 
  shade(
    terrain(elev_large, "slope", unit = "radians"), 
    terrain(elev_large, "aspect", unit = "radians"), 
    30, 270)

# Scale raster to # of colors in gray palette
index_large <- 
  hill_large |> 
  mutate(index_col = scales::rescale(hillshade, to = c(1, length(pal_gray)))) |>
  mutate(index_col = round(index_col)) |> 
  pull(index_col)

# Set color palette of hillshade
vector_cols_large <- pal_gray[index_large]


# Get fine elevation (inset map) ------------------------------------------

# Download fine elevation
elev_small <-
  get_elev_raster(
    as_Spatial(bbox_small_window), 
    neg_to_na = T, # Large negative numbers: eliminate
    z = 11) |> 
  rast()

# Create hillshade effect
hill_small <- 
  shade(
    terrain(elev_small, "slope", unit = "radians"), 
    terrain(elev_small, "aspect", unit = "radians"), 
    30, 270)

index_small <- 
  hill_small |> 
  mutate(
    index_col = scales::rescale(hillshade, to = c(1, length(pal_gray)))) |>
  mutate(index_col = round(index_col)) |> 
  pull(index_col)

# Get cols
vector_cols_small <- pal_gray[index_small]


# Generate large map ------------------------------------------------------

map_large <-
  ggplot() +
  ## Hillshade ----
  geom_spatraster(
    data = hill_large, 
    fill = vector_cols_large, 
    maxcell = Inf,
    alpha = 1) +
  ## Counties ----
  geom_sf(
    data = counties, 
    fill = NA,
    color = alpha("gray30", 0.4),
    linewidth = 0.1) +
  ## US state borders ----
  geom_sf(
    data = states, 
    fill = NA,
    linewidth = 0.25,
    color = "black") +
  ## Sites ----
  geom_sf(
    data = sites,
    aes(shape = site, fill = site),
    size = 2.25,
    stroke = 0.3) +
  scale_shape_manual(values = c(21, 21:25)) +
  scale_fill_manual(values = okabe) +
  ## Zoom box ----
  geom_sf(
    data = bbox_small_window,
    fill = NA,
    linewidth = 0.25,
    color = "red") +
  ## Extras ----
  coord_sf(
    xlim = c(bbox_large[["xmin"]], bbox_large[["xmax"]]),
    ylim = c(bbox_large[["ymin"]], bbox_large[["ymax"]]),
    expand = F) +
  labs(
    shape = "Site",
    fill = "Site") +
  theme(
    panel.background = element_rect(fill = water_col),
    panel.grid = element_blank()) 


# Generate small map ------------------------------------------------------

map_small <-
  ggplot() +
  ## Hillshade ----
  geom_spatraster(
    data = hill_small, 
    fill = vector_cols_small, 
    maxcell = Inf,
    alpha = 1) +
  ## Water area ----
  geom_sf(
    data = water,
    fill = water_col,
    color = water_col) +
  ## Water linear ----
  geom_sf(
    data = water_line,
    color = water_col,
    linewidth = 0.1,
    alpha = 0.9) +
  ## Roads ----
  geom_sf(
    data = roads,
    # Scale line width width by road type
    aes(linewidth = MTFCC),
    color = "white") +
  scale_linewidth_manual(values = c(0.5, 0.25)) +
  ## Counties ----
  geom_sf(
    data = counties, 
    fill = NA,
    color = alpha("gray30", 0.4),
    linewidth = 0.3) +
  ## US state borders ----
  geom_sf(
    data = states, 
    fill = NA,
    linewidth = 0.25,
    color = "black") +
  ## Sites ----
  geom_sf(
    data = plants,
    aes(shape = site, fill = site),
    size = 2,
    stroke = 0.3) +
  scale_shape_manual(values = c(21, 21:25)) +
  scale_fill_manual(values = okabe) +
  ## Extras ----
  coord_sf(
    xlim = c(bbox_small[["xmin"]], bbox_small[["xmax"]]),
    ylim = c(bbox_small[["ymin"]], bbox_small[["ymax"]]),
    expand = F) +
  labs(
    shape = "Site",
    fill = "Site") +
  guides(linewidth = "none") +
  theme(panel.grid = element_blank()) 

# Combine maps ------------------------------------------------------------

ggarrange(
  map_large + 
    theme(legend.position = "none"), 
  map_small + 
    theme(legend.position = "bottom") + 
    guides(
      fill = guide_legend(nrow = 3), 
      shape = guide_legend(nrow = 3)), 
  common.legend = F, 
  widths = c(1, 1.5), 
  align = "v") +
bgcolor("white") +
border("white")

ggsave(
  "output/plots/sample_map.png",
  width = 7.5,
  height = 4,
  units = "in",
  dpi = "retina")

