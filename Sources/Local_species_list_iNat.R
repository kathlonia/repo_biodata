############################################################
# EXTRACT SPECIES AROUND A POINT USING iNaturalist
############################################################

library(sf)
library(dplyr)
library(rinat)

# ----------------------------------------------------------
# 1) Define the point
# ----------------------------------------------------------
lon <- 6.93
lat <- 46.99

# ----------------------------------------------------------
# 2) Define buffer size
# ----------------------------------------------------------
buffer_m <- 500

# ----------------------------------------------------------
# 3) Create spatial point
# ----------------------------------------------------------
point_sf <- st_as_sf(
  data.frame(lon = lon, lat = lat),
  coords = c("lon", "lat"),
  crs = 4326
)

# ----------------------------------------------------------
# 4) Local projection (meters) # because we have created the point, so there is no projection for the moment
# ----------------------------------------------------------
local_crs <- paste0(
  "+proj=aeqd +lat_0=", lat,
  " +lon_0=", lon,
  " +datum=WGS84 +units=m +no_defs"
)

point_local <- st_transform(point_sf, crs = local_crs)

# ----------------------------------------------------------
# 5) Extract coordinates
# ----------------------------------------------------------
coords <- st_coordinates(point_local)

x <- coords[1, 1]
y <- coords[1, 2]

# ----------------------------------------------------------
# 6) Build square polygon
# ----------------------------------------------------------
square_coords <- matrix(
  c(
    x - buffer_m, y - buffer_m,
    x + buffer_m, y - buffer_m,
    x + buffer_m, y + buffer_m,
    x - buffer_m, y + buffer_m,
    x - buffer_m, y - buffer_m
  ),
  ncol = 2,
  byrow = TRUE
)

square <- st_polygon(list(square_coords)) |>
  st_sfc(crs = local_crs)

# Back to geographic coordinates
square_wgs84 <- st_transform(square, 4326)

# ----------------------------------------------------------
# 7) Extract bounding box
# ----------------------------------------------------------
bb <- st_bbox(square_wgs84)

# Format expected by rinat:
# c(lat_min, lon_min, lat_max, lon_max)
bounds <- c(bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"])

bounds

# ----------------------------------------------------------
# 8) Query iNaturalist
# ----------------------------------------------------------
obs_inat <- get_inat_obs(
  bounds = bounds,
  quality = "research",  # keep high-quality observations
  maxresults = 1000
)

head(obs_inat)

# ----------------------------------------------------------
# 9) Extract species list
# ----------------------------------------------------------
species_list_inat <- obs_inat %>%
  filter(!is.na(scientific_name), scientific_name != "") %>%
  distinct(scientific_name) %>%
  arrange(scientific_name)

species_list_inat

# ----------------------------------------------------------
# 10) Convert to vector
# ----------------------------------------------------------
liste_especes_inat <- species_list_inat$scientific_name

liste_especes_inat

# ----------------------------------------------------------
# 11) Count species
# ----------------------------------------------------------
length(liste_especes_inat)
