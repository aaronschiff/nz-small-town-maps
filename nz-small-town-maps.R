# Make maps of NZ's smaller urban areas using LINZ primary parcels data

# -----------------------------------------------------------------------------
# Setup
rm(list = ls())
library(parallel)
library(magrittr)
library(tidyverse)
library(sf)
library(png)
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Configuration
max_area_size <- 25               # Square kilometres, the maximum size of urban area to map
plot_resolution <- 4              # Metres per pixel, the output image resolution
bbox_expansion_factor <- 1.25     # Ratio to increase bounding boxes used for mapping, to fit edges more nicely
urban_area_buffer <- 25           # Metres of buffer around urban area boundary, to ensure features on the boundary are not clipped
min_plot_parcel_size <- 200       # Square metres, the minimum area of parcels to map
max_plot_parcel_size <- 2000      # Square metres, the maximum area of parcels to map

# Map colours and settings
background_colour <- "black"
parcel_colour <- "white"
parcel_lwd <- 2
overview_map_width <- 150
overview_map_height <- 200

# Utility function to expand a bounding box by the bbox_expansion_factor
expand_bbox <- function(b) {
  b_width_delta <- as.numeric((b["xmax"] - b["xmin"]) * (bbox_expansion_factor - 1) / 2)
  b_height_delta <- as.numeric((b["ymax"] - b["ymin"]) * (bbox_expansion_factor - 1) / 2)
  b_new <- b + c(-b_width_delta, -b_height_delta, b_width_delta, b_height_delta)
  return(b_new)
}
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Load data
print("Loading data")

# Urban areas
urban_areas <- read_sf("data/2017 Digital Boundaries Generalised Clipped/UA2017_GV_Clipped.shp")
names(urban_areas) <- tolower(names(urban_areas))

# Primary parcels
parcels_1 <- read_sf("data/nz-primary-parcels/nz-primary-parcels-1.shp") %>%
  select(id, parcel_int, geometry)
parcels_2 <- read_sf("data/nz-primary-parcels/nz-primary-parcels-2.shp") %>%
  select(id, parcel_int, geometry)
parcels_3 <- read_sf("data/nz-primary-parcels/nz-primary-parcels-3.shp") %>%
  select(id, parcel_int, geometry)
parcels <- rbind(parcels_1, parcels_2, parcels_3)
rm(parcels_1, parcels_2, parcels_3)

# Coastline and lakes for outline maps
coastline <- read_sf("data/nz-coastlines-topo-1500k/nz-coastlines-topo-1500k.shp") %>%
  st_cast("POLYGON")
lakes <- read_sf("data/nz-lake-polygons-topo-1500k/nz-lake-polygons-topo-1500k.shp")
lakes$area <- as.numeric(st_area(lakes) / 1000000)
lakes %<>% filter(area > 100)
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Filter and process data
print("Filtering areas and processing data")

# Filter out non-urban areas and remove unncessary columns from urban_areas
urban_areas %<>%
  filter(!ua2017_nam %in% c("Rural Centre", 
                            "Inland Water not in Urban Area", 
                            "Inlet-in TA but not in Urban Area", 
                            "Rural (Incl.some Off Shore Islands)")) %>%
  select(-area_sq_km, -land_sq_km)

# Combine central Auckland urban areas into one
auckland_areas <- c("Northern Auckland Zone", 
                    "Western Auckland Zone", 
                    "Central Auckland Zone", 
                    "Southern Auckland Zone")
auckland_geometry <- urban_areas %>%
  filter(ua2017_nam %in% auckland_areas) %>%
  st_union()
auckland <- st_as_sf(tibble(ua2017 = "999", 
                            ua2017_nam = "Auckland", 
                            geometry = auckland_geometry))
urban_areas %<>%
  filter(!ua2017_nam %in% auckland_areas)
urban_areas %<>% rbind(auckland)
rm(auckland_geometry, auckland)

# Find areas smaller than size limit
urban_areas$area <- as.numeric(st_area(urban_areas) / 1000000)
small_urban_areas <- filter(urban_areas, area < max_area_size)
small_urban_areas$centroid <- st_centroid(small_urban_areas)

# Buffer small urban areas so that features on the edges will be included properly
small_urban_areas_buffered <- st_buffer(small_urban_areas, urban_area_buffer)
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Intersect urban areas with other geographic data
small_urban_area_names <- small_urban_areas$ua2017_nam

# Function to find the boundary of a named area, optionally returning the bounding box
find_area_boundary <- function(area_name, use_bbox = FALSE) {
  area_boundary <- small_urban_areas_buffered %>%
    filter(ua2017_nam == area_name)
  
  if (use_bbox) {
    area_boundary <- st_as_sfc(expand_bbox(st_bbox(area_boundary)))
  }
  
  return(area_boundary)
}

# Parcels
# Note just using two cores here due to memory requirements for parcels data
print("Intersecting parcels")
plot_parcels <- mclapply(small_urban_area_names, 
                         function(i) { 
                           st_intersection(parcels, find_area_boundary(i, use_bbox = FALSE))
                         }, 
                         mc.cores = 2)
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Plotting
print("Plotting")

# Function to plot a little overview map for an area
plot_overview_map <- function(i, output_directory) {
  area_name <- small_urban_area_names[i]
  area_shape <- small_urban_areas_buffered[i, ]
  area_centroid <- st_centroid(area_shape)
  
  # Plotting setup
  png(paste0(output_directory, "/map-", area_name, ".png"), 
      width = overview_map_width, 
      height = overview_map_height)
  par(mar = c(0, 0, 0, 0), 
      bg = background_colour)
  plot(st_geometry(coastline), 
       col = parcel_colour, 
       border = NA, 
       xaxs = "i",
       yaxs = "i")
  plot(st_geometry(lakes), 
       col = background_colour, 
       border = NA, 
       add = TRUE)
  plot(st_geometry(st_centroid(area_shape)), 
       pch = 21, 
       cex = 2.5, 
       col = rgb(255/255, 255/255, 255/255), 
       bg = rgb(255/255, 0/255, 0/255), 
       lwd = 2, 
       add = TRUE)
  dev.off()
}

# Function to plot an urban area
plot_area <- function(i, output_directory) {
  # Gather data for the selected area
  area_name <- small_urban_area_names[i]
  area_shape <- small_urban_areas_buffered[i, ]
  area_parcels <- plot_parcels[[i]]
  
  # Set up output fule
  area_bbox <- expand_bbox(st_bbox(area_shape))
  output_width <- as.numeric(round((area_bbox["xmax"] - area_bbox["xmin"]) / plot_resolution))
  output_height <- as.numeric(round((area_bbox["ymax"] - area_bbox["ymin"]) / plot_resolution))
  
  # Plotting setup
  png(paste0(output_directory, "/", area_name, ".png"), width = output_width, height = output_height)
  par(mar = c(0, 0, 0, 0), 
      bg = background_colour)
  plot(st_as_sfc(area_bbox), col = NA, border = NA, xaxs = "i", yaxs = "i")
  
  # Plot property parcels
  area_parcels_property <- filter(area_parcels, !(parcel_int %in% c("Road", 
                                                                    "Road Strata", 
                                                                    "Hydro", 
                                                                    "Streambed", 
                                                                    "Riverbed")))
  area_parcels_property$area <- as.numeric(st_area(area_parcels_property))
  area_parcels_residential <- filter(area_parcels_property, 
                                     area < max_plot_parcel_size, 
                                     area > min_plot_parcel_size)
  plot(st_geometry(area_parcels_residential), 
       col = parcel_colour, 
       border = background_colour, 
       lwd = parcel_lwd, 
       add = TRUE)
  
  # Plot finished 
  dev.off()
  
  # Plot summary map
  plot_overview_map(i, output_directory)
  
  # Done
  return(c(name = area_name, width = output_width, height = output_height))
}

# For some reason, need to call plot_area on one area before plotting all with parallel processing
# I don't know why this makes it work properly
plot_area(1, "outputs")

plot_results <- mclapply(1:length(small_urban_area_names),
                         plot_area,
                         output_directory = "outputs", 
                         mc.cores = detectCores() - 2)

plot_info <- as_tibble(matrix(unlist(plot_results), nrow = length(plot_results), ncol = 3, byrow = TRUE))
names(plot_info) <- c("area_name", "width", "height")
plot_info$width <- as.numeric(plot_info$width)
plot_info$height <- as.numeric(plot_info$height)
plot_info %<>% arrange(area_name)
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Web gallery
output_html <- function(content, output_filename) {
  # Load HTML template parts
  template_header <- read_file("header.html")
  template_footer <- read_file("footer.html")
  
  output <- paste(template_header, content, template_footer, sep = "\n")
  write_file(output, output_filename)
}

wrap_html_tag <- function(x, tag, params = NULL) {
  output <- paste0("<", tag)
  if (!is.null(params)) output <- paste(output, paste(params, collapse = " "))
  output <- paste0(output, ">", x, "</", tag, ">")
  return(output)
}

build_content <- function(current = NULL, new) {
  return(paste(current, new, sep = "\n"))
}

output_area <- function(i, output_directory) {
  area_name <- plot_info[i, "area_name"]
  
  area_content <- wrap_html_tag(area_name, "h2") 
  
  area_content %<>% build_content(
    paste0("<div><img src='maps/map-", area_name, ".png' ", 
           "width = '", round(overview_map_width / 2), "' ", 
           "data-original-width = '", round(overview_map_width / 2), "' ", 
           "height = '", round(overview_map_height / 2), "' ", 
           "data-original-height = '", round(overview_map_height / 2), "' ", 
           "class = 'overview-map-img'", 
           "/></div>")
  )
  
  image <- readPNG(paste0(output_directory, "/", area_name, ".png"), info = TRUE)
  
  area_content %<>% build_content(
    paste0("<div><img src='", area_name, ".png' ", 
           "width = '", round(attr(image, "dim")[2] / 2), "' ", 
           "data-original-width = '", round(attr(image, "dim")[2] / 2), "' ", 
           "height = '", round(attr(image, "dim")[1] / 2), "' ", 
           "data-original-height = '", round(attr(image, "dim")[1] / 2), "' ", 
           "class = 'town-img'", 
           "/></div>")
  )
  
  area_content %<>% wrap_html_tag("div", "class = 'town-div'")
  
  return(area_content)
}

generate_gallery <- function(output_directory) {
  # Title
  content <- wrap_html_tag("Small towns of Aotearoa", "h1")
  content %<>% build_content(
    wrap_html_tag(
      paste("<p>Made by <a href='http://schiff.co.nz'>Aaron Schiff</a> with data from LINZ and Statistics New Zealand.</p>", 
            "<p>All images are licensed <a href='https://creativecommons.org/licenses/by/4.0/'>CC BY 4.0</a>.</p>", 
            sep = "\n"), 
      "div", 
      "id = 'info'"
    )
  )
  
  # Loop through images
  for (i in 1:nrow(plot_info)) {
    content %<>% build_content(output_area(i, output_directory))
  }
  
  # End
  output_html(content, paste0(output_directory, "/index.html"))
  file.copy(from = "style.css", 
            to = paste0(output_directory, "/style.css"), 
            overwrite = TRUE)
}
generate_gallery("outputs-edited-cropped")




