############################################################
# EXTRACT SPECIES AROUND A POINT USING GBIF (STEP BY STEP)
############################################################

# Load required packages
library(sf)        # spatial objects
library(dplyr)     # data manipulation
library(rgbif)     # GBIF API

# ----------------------------------------------------------
# 1) Define the point coordinates
# ----------------------------------------------------------
# Example: a point in Neuchâtel (Switzerland)
lon <- 6.93
lat <- 46.99

# ----------------------------------------------------------
# 2) Define the buffer size (in meters)
# ----------------------------------------------------------
# buffer_m represents HALF the side of the square
# buffer_m = 500 → square of 1000 m x 1000 m
buffer_m <- 500

# ----------------------------------------------------------
# 3) Convert the point into a spatial object
# ----------------------------------------------------------
point_sf <- st_as_sf(
  data.frame(lon = lon, lat = lat),
  coords = c("lon", "lat"),
  crs = 4326  # WGS84 coordinate system
)

# ----------------------------------------------------------
# 4) Create a local projection in meters
# ----------------------------------------------------------
# This allows us to work with distances in meters
local_crs <- paste0(
  "+proj=aeqd +lat_0=", lat,
  " +lon_0=", lon,
  " +datum=WGS84 +units=m +no_defs"
)

# Transform the point into this local projection
point_local <- st_transform(point_sf, crs = local_crs)

# ----------------------------------------------------------
# 5) Extract projected coordinates
# ----------------------------------------------------------
coords <- st_coordinates(point_local)

x <- coords[1, 1]
y <- coords[1, 2]

# ----------------------------------------------------------
# 6) Build a square around the point
# ----------------------------------------------------------
# We manually define the 4 corners of the square
square_coords <- matrix(
  c(
    x - buffer_m, y - buffer_m,
    x + buffer_m, y - buffer_m,
    x + buffer_m, y + buffer_m,
    x - buffer_m, y + buffer_m,
    x - buffer_m, y - buffer_m  # close polygon
  ),
  ncol = 2,
  byrow = TRUE
)

# Create the polygon object
square <- st_polygon(list(square_coords)) |>
  st_sfc(crs = local_crs)

# Transform back to geographic coordinates (WGS84)
square_wgs84 <- st_transform(square, 4326)

# ----------------------------------------------------------
# 7) Convert the polygon to WKT format
# ----------------------------------------------------------
# GBIF requires geometry as a WKT string
wkt <- st_as_text(square_wgs84)

wkt

# ----------------------------------------------------------
# 8) Query GBIF using the polygon
# ----------------------------------------------------------
res_gbif <- occ_search(
  geometry = wkt,
  hasCoordinate = TRUE,
  limit = 2000
)

# ----------------------------------------------------------
# 9) Extract observations table
# ----------------------------------------------------------
obs_gbif <- res_gbif$data

head(obs_gbif)

# ----------------------------------------------------------
# 10) Clean and extract species names
# ----------------------------------------------------------
species_list <- obs_gbif %>%
  mutate(
    # Use 'species' if available, otherwise 'scientificName'
    species_final = coalesce(species, scientificName)
  ) %>%
  filter(!is.na(species_final), species_final != "") %>%
  distinct(species_final) %>%   # remove duplicates
  arrange(species_final)

species_list

# ----------------------------------------------------------
# 11) Convert to a simple vector
# ----------------------------------------------------------
liste_especes <- species_list$species_final

liste_especes

# ----------------------------------------------------------
# 12) Count number of species
# ----------------------------------------------------------
length(liste_especes)
