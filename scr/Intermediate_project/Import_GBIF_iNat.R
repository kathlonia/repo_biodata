#### Script to merge GBIF and iNaturalist occurences for two species of Chyroptera ######

# Disable spherical geometry for simpler spatial operations
library(rgbif)         # get the informations from GBIF
library(rnaturalearth) # country maps
library(ggplot2)       # graphics
library(rinat)         # access to iNaturalist data
library(raster)        # spatial extent management
library(sf)            # modern spatial objects
library(dplyr)

sf_use_s2(FALSE) # spherical geometry switched off, to avoid issues later

# Species of interest number one: Rhinolophus ferrumequinum
myspecies_1 <- "Rhinolophus ferrumequinum"

# Maximum number of GBIF records to download
gbif_limit <- 4000

# Time filtering period
date_start <- as.Date("2020-01-01")
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

# create the folder for stocking the maps 
dir.create("data/merge_gbif_inat_maps", showWarnings = FALSE)

# Simple visualization of the map
p1 <-ggplot(data = France) +
  geom_sf(fill = "grey95", color = "black") +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()
print(p1)
ggsave(p1, filename = ".\\data\\merge_gbif_inat_maps\\p1.png", width = 10, height = 8)

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
# head(gbif_occ) 
# names(gbif_occ)

# check the structure of the data frame 
#str(gbif_occ) #dnaSequenceID is a list, we have to remove it 

gbif_France <- gbif_occ %>%
  dplyr::select(-dnaSequenceID) %>%
  filter(country == "France")

# Check number of records
# nrow(gbif_France) # 1633 occurences in France for GBIF

# Quick base plot for checking
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
p2 <- ggplot(data = France) +
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
print(p2)
ggsave(p2, filename = ".\\data\\merge_gbif_inat_maps\\p2.png", width = 10, height = 8)

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
# head(data_gbif)
# str(data_gbif)

###############################################################################
# 6) DOWNLOAD iNaturalist DATA
###############################################################################

# Query iNaturalist for the same species in Europe
inat_raw <- get_inat_obs(
  query = myspecies_1,
  place_id = "france"
)


# Inspect the structure
# head(inat_raw)
# names(inat_raw)

# Map showing iNaturalist occurrences only
p3 <- ggplot(data = France) +
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
print(p3)
ggsave(p3, filename = ".\\data\\merge_gbif_inat_maps\\p3.png", width = 10, height = 8)

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
matrix_full_1 <- dplyr::bind_rows(data_gbif, data_inat)

# Check results
#head(matrix_full_1)
#table(matrix_full_1$source, useNA = "ifany")
#summary(matrix_full_1$date_obs)

###############################################################################
# 9) TIME FILTERING BETWEEN TWO DATES
###############################################################################

# Keep only observations within the selected time interval
matrix_full_date_1 <- dplyr::filter(matrix_full_1, !is.na(date_obs)) %>%
  dplyr::filter(date_obs >= date_start & date_obs <= date_end)

# Check results
#head(matrix_full_date_1)
#summary(matrix_full_date_1$date_obs)
#table(matrix_full_date_1$source)

###############################################################################
# 10) MAP OF COMBINED DATA
###############################################################################

p4 <- ggplot(data = France) +
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
print(p4)
ggsave(p4, filename = ".\\data\\merge_gbif_inat_maps\\p4.png", width = 10, height = 8)

###############################################################################
# 11) DEFINE A SIMPLE SPATIAL EXTENT
###############################################################################

################################################################################
##### Crop the background using coordinates

library(sf)
library(raster)
sf_use_s2(FALSE) # again, switch off the spherical geometry

# Define the spatial extent
extent(France)
ext_France_cut <- as(raster::extent(6, 11, 47, 48), "SpatialPolygons")

# Crop France map to the defined extent
bbox_cut <- st_bbox(c(xmin = 6, xmax = 11, ymin = 47, ymax = 48), crs = 4326)
France_crop <- st_crop(France, bbox_cut)

# Plot cropped map with occurrence points
p5 <- ggplot(data = France) +
  geom_sf() +
  geom_point(
    data = matrix_full_date_1,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) + 
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()
print(p5)
ggsave(p5, filename = ".\\data\\merge_gbif_inat_maps\\p5.png", width = 10, height = 8)

################################################################################
################################################################################
##### Exclude points outside the specified spatial extent

# Convert occurrences to sf object
data_gbif_sf <- st_as_sf(matrix_full_1, coords = c("longitude", "latitude"), crs = 4326)

# Convert cropped France polygon to sf
France_sf <- st_as_sf(France)

# Identify points in france
intersects_result <- st_intersects(data_gbif_sf, France_sf)

# Keep only the points that intersect at least one polygone
keep <- lengths(intersects_result) > 0
cur_data_1 <- matrix_full_1[keep, ]

# Verification
#nrow(cur_data_1) # 1685 
#table(cur_data_1$source)

# Plot cropped France map with filtered points
p6 <- ggplot(data = France_sf) +
  geom_sf() +
  geom_point(
    data = cur_data_1,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()
print(p6)
ggsave(p6, filename = ".\\data\\merge_gbif_inat_maps\\p6.png", width = 10, height = 8)

###############################################################################
# 14) SAVE OF THE FINAL TABLE
###############################################################################

# Save filtered occurrence table
write.csv(
  cur_data_1,
  file = ".\\data\\merge_gbif_inat_maps\\Rhinolophus_ferrumequinum.csv",
  row.names = FALSE
)

# --------------------------------------------------------------------------------
# ------------- script for the second species: Plecotus auritus ------------------
# --------------------------------------------------------------------------------

# Disable spherical geometry for simpler spatial operations
sf_use_s2(FALSE)

# Species of interest (now the second one, Barbastella barbastellus)
myspecies_2 <- "Barbastella barbastellus"

# Maximum number of GBIF records to download
gbif_limit <- 4000

# Time filtering period
date_start <- as.Date("2020-01-01")
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

# Simple visualization of the map # no need since we've already done it beffor, for the first species 
# ggplot(data = France) +
  # geom_sf(fill = "grey95", color = "black") +
  # coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  # theme_classic()

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

# check the structure of the data frame
#str(gbif_occ)
gbif_France <- gbif_occ %>%
  select(-dnaSequenceID) %>%
  filter(country == "France")

# Check number of records
#nrow(gbif_France) 363

# Quick base plot for checking
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
p7 <- ggplot(data = France) +
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
print(p7)
ggsave(p7, filename = ".\\data\\merge_gbif_inat_maps\\p7.png", width = 10, height = 8)

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

# Query iNaturalist for the same species in France
inat_raw <- get_inat_obs(
  query = myspecies_2,
  place_id = "france"
)

# Inspect the structure
#head(inat_raw)
#names(inat_raw)

# Map showing iNaturalist occurrences only
p8 <- ggplot(data = France) +
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
print(p8)
ggsave(p8, filename = ".\\data\\merge_gbif_inat_maps\\p8.png", width = 10, height = 8)

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

p9 <- ggplot(data = France) +
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
print(p9)
ggsave(p9, filename = ".\\data\\merge_gbif_inat_maps\\p9.png", width = 10, height = 8)

###############################################################################
# 11) DEFINE A SIMPLE SPATIAL EXTENT
###############################################################################

################################################################################
##### Crop the background using coordinates

# Define the spatial extent
extent(France)
ext_France_cut <- as(raster::extent(6, 11, 47, 48), "SpatialPolygons")

# Plot cropped map with occurrence points
p10 <- ggplot(data = France) +
  geom_sf() +
  geom_point(
    data = matrix_full_date_2,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()
print(p10)
ggsave(p10, filename = ".\\data\\merge_gbif_inat_maps\\p10.png", width = 10, height = 8)

################################################################################
################################################################################
##### Exclude points outside the specified spatial extent

# Convert occurrences to sf object
data_gbif_sf <- st_as_sf(matrix_full_2, coords = c("longitude", "latitude"), crs = 4326)

# Identify points in france
intersects_result <- st_intersects(data_gbif_sf, France_sf)

# Garder uniquement les points qui intersectent au moins un polygone
keep <- lengths(intersects_result) > 0
cur_data_2 <- matrix_full_2[keep, ]

# Vérification
#nrow(cur_data_2)
#table(cur_data_2$source)

# Plot cropped France map with filtered points
p11 <- ggplot(data = France_sf) +
  geom_sf() +
  geom_point(
    data = cur_data_2,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
  coord_sf(xlim = c(-5, 10), ylim = c(42, 51)) +
  theme_classic()
print(p11)
ggsave(p11, filename = ".\\data\\merge_gbif_inat_maps\\p11.png", width = 10, height = 8)

###############################################################################
# 14) OPTIONAL SAVE OF THE FINAL TABLE
###############################################################################

# Save filtered occurrence table
write.csv(
  cur_data_2,
  file = ".\\data\\merge_gbif_inat_maps\\Barbastella_barbastellus.csv",
  row.names = FALSE
)

# --------------------------------------------------------------------
# ----------- Merge the data from the two species --------------------
# --------------------------------------------------------------------

library(dplyr)
all_species <- bind_rows(cur_data_1, cur_data_2)

# vérification
#table(all_species$species)
#table(all_species$source)

p12 <- ggplot(data = France_sf) +
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
    title = "Occurrence of two bat species in France between 2020 and 2025 from GBIF and iNaturalist",
    color = "Species",
    shape = "Database"
  )
print(p12)
ggsave(p12, filename = ".\\data\\merge_gbif_inat_maps\\p12.png", width = 10, height = 8)

# verification
#table(all_species$species) # 438 data for Barbastella barbastellus and 1674 data for Rhinolophus ferrumequinum
#table(all_species$source) # 1926 for GBIF and 186 for Inat 

# View(all_species)
# so here the final result is a table named all_species, who combines the occurences from GBIF and iNat, from 2020 to 2025, for the two species of bats.
# we have the columns species, latitude, longitude, date_obs and source (gbif or inat)
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# other comment: I chose these two species in particular because, although they are not directly related, 
# they have relatively different ecology. Barbastella barbastellus prefers bark or loose tree cavities and 
# can be found up to 7,000 meters in altitude, with a preference for mature deciduous forests. 
# Rhinolophus ferummequinum, on the other hand, inhabits temperate Mediterranean habitats (for example, deciduous and riparian forests, pastures), 
# as well as attics. My initial idea was to compare two species present in France but with different ecology in relation 
# to changes in light pollution, in order to see if their respective distribution ranges have been influenced by changes 
# in anthropogenic nighttime light. I tried to extract the VNL data from the american satellite for the light pollution in France, 
# but it was wayyyyy to heavy, and I couldn't crop it (the raster is for the world) (VNL_v22_npp-j01_2022_global_vcmslcfg_c202303062300.average.dat.tif.gz)
# So, having had great difficulty finding the raster of light pollution in France, 
## I decided to put this question on a more subsidiary level and to concentrate on the evolution of vegetation in and outside cities, in order to see if this and temperature influence the niches of the two species.
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------