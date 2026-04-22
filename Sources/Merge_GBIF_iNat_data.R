#### Script to merge GBIF and iNaturalist occurences for two species of Chyroptera ######

# Disable spherical geometry for simpler spatial operations
library(rgbif)         # get the informations from GBIF
library(rnaturalearth) # country maps
library(ggplot2)       # graphics
library(rinat)         # access to iNaturalist data
library(raster)        # spatial extent management
library(sf)            # modern spatial objects

sf_use_s2(FALSE)

# Species of interest (first try with one species)
myspecies_1 <- "Rhinolophus ferrumequinum"

# Maximum number of GBIF records to download
gbif_limit <- 4000

# Time filtering period
date_start <- as.Date("2021-01-01")
date_end   <- as.Date("2025-12-31")

# Simplified geographic extent for France
xmin <- 6
xmax <- 11
ymin <- 46
ymax <- 48

###############################################################################
# 3) BASE MAP: FRANCE
###############################################################################

# Download the outline of France
France <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "France"
)

# Simple visualization of the map
windows()
ggplot(data = France) +
  geom_sf(fill = "grey95", color = "black") +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()

###############################################################################
# 4) DOWNLOAD GBIF DATA
###############################################################################

# Download occurrences with coordinates
gbif_raw <- occ_data(
  scientificName = myspecies_1,
  hasCoordinate = TRUE,
  limit = gbif_limit
)

# Extract the main data table
gbif_occ <- gbif_raw$data

# Quick inspection
#head(gbif_occ)
#names(gbif_occ)

# Vérifier la structure du dataframe
#str(gbif_occ) #dnaSequenceID is a list, we have to remove it 
library(dplyr)
gbif_France <- gbif_occ %>%
  select(-dnaSequenceID) %>%
  filter(country == "France")

# Check number of records
#nrow(gbif_France)

# Quick base plot for checking
windows()
plot(
  gbif_France$decimalLongitude,
  gbif_France$decimalLatitude,
  pch = 16,
  col = "darkgreen",
  xlab = "Longitude",
  ylab = "Latitude",
  main = "GBIF occurrences in Europe"
)

# Map showing GBIF occurrences only
windows()
ggplot(data = France) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = gbif_France,
    aes(x = decimalLongitude, y = decimalLatitude),
    size = 3,
    shape = 21,
    fill = "darkgreen",
    color = "black"
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()

###############################################################################
# 5) FORMAT GBIF DATA
###############################################################################

# Keep only the useful columns
# eventDate may contain date + time; as.Date() keeps only the date
data_gbif <- data.frame(
  species   = gbif_France$species,
  latitude  = gbif_France$decimalLatitude,
  longitude = gbif_France$decimalLongitude,
  date_obs  = as.Date(gbif_France$eventDate),
  source    = "gbif"
)

# Check structure
#head(data_gbif)
#str(data_gbif)

###############################################################################
# 6) DOWNLOAD iNaturalist DATA
###############################################################################

# Query iNaturalist for the same species in Europe
# place_id = "europe" usually works with rinat
inat_raw <- get_inat_obs(
  query = myspecies_1,
  place_id = "france"
)


# Inspect the structure
#head(inat_raw)
#names(inat_raw)

# Map showing iNaturalist occurrences only
windows()
ggplot(data = France) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = inat_raw,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = "darkred",
    color = "black"
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()

###############################################################################
# 7) FORMAT iNaturalist DATA
###############################################################################

# In most rinat versions the observation date is stored in observed_on
# Convert it to Date format
data_inat <- data.frame(
  species   = inat_raw$scientific_name,
  latitude  = inat_raw$latitude,
  longitude = inat_raw$longitude,
  date_obs  = as.Date(inat_raw$observed_on),
  source    = "inat"
)

# Check structure
#head(data_inat)
#str(data_inat)

###############################################################################
# 8) MERGE THE TWO DATABASES
###############################################################################

# IMPORTANT:
# Here we want to STACK GBIF and iNaturalist observations.
# Therefore we use bind_rows() instead of merge().
matrix_full_1 <- bind_rows(data_gbif, data_inat)

# Check results
#head(matrix_full_1)
#table(matrix_full_1$source, useNA = "ifany")
#summary(matrix_full_1$date_obs)

###############################################################################
# 9) TIME FILTERING BETWEEN TWO DATES
###############################################################################

# Keep only observations within the selected time interval
matrix_full_date_1 <- matrix_full_1 %>%
  filter(!is.na(date_obs)) %>%
  filter(date_obs >= date_start & date_obs <= date_end)

# Check results
#head(matrix_full_date_1)
#summary(matrix_full_date_1$date_obs)
#table(matrix_full_date_1$source)

###############################################################################
# 10) MAP OF COMBINED DATA
###############################################################################

windows()
ggplot(data = France) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = matrix_full_date_1,
    aes(x = longitude, y = latitude, fill = source),
    size = 3,
    shape = 21,
    color = "black",
    alpha = 0.8
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()

###############################################################################
# 11) DEFINE A SIMPLE SPATIAL EXTENT
###############################################################################

################################################################################
##### Crop the background using coordinates

library(sf)
library(raster)
sf_use_s2(FALSE)

# Define the spatial extent
extent(France)
ext_France_cut <- as(raster::extent(6, 11, 47, 48), "SpatialPolygons")

# Crop France map to the defined extent
bbox_cut <- st_bbox(c(xmin = 6, xmax = 11, ymin = 47, ymax = 48), crs = 4326)
France_crop <- st_crop(France, bbox_cut)

# Plot cropped map with occurrence points
windows()
ggplot(data = France) +
  geom_sf() +
  geom_point(
    data = matrix_full_date_1,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) + 
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()

################################################################################
################################################################################
##### Exclude points outside the specified spatial extent

# Convert occurrences to sf object
data_gbif_sf <- st_as_sf(matrix_full_1, coords = c("longitude", "latitude"), crs = 4326)

# Convert cropped France polygon to sf
France_sf <- st_as_sf(France)

# Identify points in france
intersects_result <- st_intersects(data_gbif_sf, France_sf)

# Garder uniquement les points qui intersectent au moins un polygone
keep <- lengths(intersects_result) > 0
cur_data_1 <- matrix_full_1[keep, ]

# Vérification
#nrow(cur_data_1)
#table(cur_data_1$source)

# Plot cropped France map with filtered points
windows()
ggplot(data = France_sf) +
  geom_sf() +
  geom_point(
    data = cur_data_1,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()

###############################################################################
# 14) OPTIONAL SAVE OF THE FINAL TABLE
###############################################################################

# Save filtered occurrence table
write.csv(
  cur_data_1,
  file = "Rhinolophus_ferrumequinum.csv",
  row.names = FALSE
)

# --------------------------------------------------------------------------------
# ------------- script for the second species: Plecotus auritus ------------------
# --------------------------------------------------------------------------------

#### Script to merge GBIF and iNaturalist occurences for two species of Chyroptera ######

library(rgbif)         # get the informations from GBIF
library(rnaturalearth) # country maps
library(ggplot2)       # graphics
library(rinat)         # access to iNaturalist data
library(raster)        # spatial extent management
library(sf)            # modern spatial objects

# Disable spherical geometry for simpler spatial operations
sf_use_s2(FALSE)

# Species of interest (first try with one species)
myspecies_2 <- "Barbastella barbastellus"

# Maximum number of GBIF records to download
gbif_limit <- 4000

# Time filtering period
date_start <- as.Date("2021-01-01")
date_end   <- as.Date("2025-12-31")

# Simplified geographic extent for Europe
xmin <- 6
xmax <- 11
ymin <- 46
ymax <- 48

###############################################################################
# 3) BASE MAP: France
###############################################################################

# Download the outline of France
France <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "France"
)

# Simple visualization of the map
windows()
ggplot(data = France) +
  geom_sf(fill = "grey95", color = "black") +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()

###############################################################################
# 4) DOWNLOAD GBIF DATA
###############################################################################

# Download occurrences with coordinates
gbif_raw <- occ_data(
  scientificName = myspecies_2,
  hasCoordinate = TRUE,
  limit = gbif_limit
)

# Extract the main data table
gbif_occ <- gbif_raw$data

# Quick inspection
#head(gbif_occ)
#names(gbif_occ)

# Vérifier la structure du dataframe
#str(gbif_occ) #dnaSequenceID is a list, we have to remove it 
library(dplyr)
gbif_France <- gbif_occ %>%
  select(-dnaSequenceID) %>%
  filter(country == "France")

# Check number of records
#nrow(gbif_France)

# Quick base plot for checking
windows()
plot(
  gbif_France$decimalLongitude,
  gbif_France$decimalLatitude,
  pch = 16,
  col = "darkgreen",
  xlab = "Longitude",
  ylab = "Latitude",
  main = "GBIF occurrences in France"
)

# Map showing GBIF occurrences only
windows()
ggplot(data = France) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = gbif_France,
    aes(x = decimalLongitude, y = decimalLatitude),
    size = 3,
    shape = 21,
    fill = "darkgreen",
    color = "black"
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()

###############################################################################
# 5) FORMAT GBIF DATA
###############################################################################

# Keep only the useful columns
# eventDate may contain date + time; as.Date() keeps only the date
data_gbif <- data.frame(
  species   = gbif_France$species,
  latitude  = gbif_France$decimalLatitude,
  longitude = gbif_France$decimalLongitude,
  date_obs  = as.Date(gbif_France$eventDate),
  source    = "gbif"
)

# Check structure
#head(data_gbif)
#str(data_gbif)

###############################################################################
# 6) DOWNLOAD iNaturalist DATA
###############################################################################

# Query iNaturalist for the same species in Europe
# place_id = "europe" usually works with rinat
inat_raw <- get_inat_obs(
  query = myspecies_2,
  place_id = "france"
)

# Inspect the structure
#head(inat_raw)
#names(inat_raw)

# Map showing iNaturalist occurrences only
windows()
ggplot(data = France) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = inat_raw,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = "darkred",
    color = "black"
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()

###############################################################################
# 7) FORMAT iNaturalist DATA
###############################################################################

# In most rinat versions the observation date is stored in observed_on
# Convert it to Date format
data_inat <- data.frame(
  species   = inat_raw$scientific_name,
  latitude  = inat_raw$latitude,
  longitude = inat_raw$longitude,
  date_obs  = as.Date(inat_raw$observed_on),
  source    = "inat"
)

# Check structure
#head(data_inat)
#str(data_inat)

###############################################################################
# 8) MERGE THE TWO DATABASES
###############################################################################

# IMPORTANT:
# Here we want to STACK GBIF and iNaturalist observations.
# Therefore we use bind_rows() instead of merge().
matrix_full_2 <- bind_rows(data_gbif, data_inat)

# Check results
#head(matrix_full_2)
#table(matrix_full_2$source, useNA = "ifany")
#summary(matrix_full_2$date_obs)

###############################################################################
# 9) TIME FILTERING BETWEEN TWO DATES
###############################################################################

# Keep only observations within the selected time interval
matrix_full_date_2 <- matrix_full_2 %>%
  filter(!is.na(date_obs)) %>%
  filter(date_obs >= date_start & date_obs <= date_end)

# Check results
#head(matrix_full_date_2)
#summary(matrix_full_date_2$date_obs)
#table(matrix_full_date_2$source)

###############################################################################
# 10) MAP OF COMBINED DATA
###############################################################################

windows()
ggplot(data = France) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = matrix_full_date_2,
    aes(x = longitude, y = latitude, fill = source),
    size = 3,
    shape = 21,
    color = "black",
    alpha = 0.8
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()

###############################################################################
# 11) DEFINE A SIMPLE SPATIAL EXTENT
###############################################################################

################################################################################
##### Crop the background using coordinates

library(sf)
library(dplyr)
sf_use_s2(FALSE)

# Define the spatial extent
extent(France)
ext_France_cut <- as(raster::extent(6, 11, 47, 48), "SpatialPolygons")

# Crop France map to the defined extent
bbox_cut <- st_bbox(c(xmin = 6, xmax = 11, ymin = 47, ymax = 48), crs = 4326)
France_crop <- st_crop(France, bbox_cut)

# Plot cropped map with occurrence points
windows()
ggplot(data = France) +
  geom_sf() +
  geom_point(
    data = matrix_full_date_2,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()

################################################################################
################################################################################
##### Exclude points outside the specified spatial extent

# Convert occurrences to sf object
data_gbif_sf <- st_as_sf(matrix_full_2, coords = c("longitude", "latitude"), crs = 4326)

# Convert cropped France polygon to sf
France_sf <- st_as_sf(France)

# Identify points in france
intersects_result <- st_intersects(data_gbif_sf, France_sf)

# Garder uniquement les points qui intersectent au moins un polygone
keep <- lengths(intersects_result) > 0
cur_data_2 <- matrix_full_2[keep, ]

# Vérification
#nrow(cur_data_2)
#table(cur_data_2$source)

# Plot cropped France map with filtered points
windows()
ggplot(data = France_sf) +
  geom_sf() +
  geom_point(
    data = cur_data_2,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()

###############################################################################
# 14) OPTIONAL SAVE OF THE FINAL TABLE
###############################################################################

# Save filtered occurrence table
write.csv(
  cur_data_2,
  file = "Barbastella_barbastellus.csv",
  row.names = FALSE
)

# I found barbastella barbastellus, with 355 data from GBIF and 92 from iNat
# and Rhinolophus ferrumquinum has 1580 data from GBIF and 92 from iNat


# --------------------------------------------------------------------
# ----------- Merge the data from the two species --------------------
# --------------------------------------------------------------------

library(dplyr)
all_species <- bind_rows(cur_data_1, cur_data_2)

# vérification
#table(all_species$species)
#table(all_species$source)

windows()
ggplot(data = France_sf) +
  geom_sf(fill = "grey95", color = "black") +
  
  geom_point(
    data = all_species,
    aes(
      x = longitude,
      y = latitude,
      color = species,
      shape = source
    ),
    size = 3,
    alpha = 0.8
  ) +
  
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  
  scale_color_manual(values = c(
    "Rhinolophus ferrumequinum" = "darkblue",
    "Barbastella barbastellus" = "darkred"
  )) +
  
  scale_shape_manual(values = c(
    "gbif" = 16,
    "inat" = 17
  )) +
  
  theme_classic() +
  
  labs(
    title = "Chiroptères in France (GBIF + iNaturalist)",
    color = "Species",
    shape = "Database"
  )
