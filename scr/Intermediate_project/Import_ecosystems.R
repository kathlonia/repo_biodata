###############################################################################
# ADDING ECOSYSTEM DATA TO SPECIES OCCURRENCE COORDINATES
# Example workflow:
# - read an ecosystem raster
# - crop it to France
# - extract ecosystem values at species occurrence points
# - join the raster values with metadata
# - visualize the result
###############################################################################
# now we want to add ecosystem data to our species occurence points; indeed, we want to get infos
# about the landcover, the climate ans possibly the elevation of the points where the species were observed.

#------------------------------------------------------------------------------
# 1) LOAD REQUIRED PACKAGES
#------------------------------------------------------------------------------

# raster: to read and manipulate raster files
# sf: to handle vector spatial data
# rnaturalearth: to download country boundaries
# ggplot2: to create graphs

library(raster)
library(sf)
library(rnaturalearth)
library(ggplot2)

#------------------------------------------------------------------------------
# 2) LOAD THE ECOSYSTEM RASTER
#------------------------------------------------------------------------------

# Define the path to the GeoTIFF file
file_path <- ".\\data\\WorldEcosystem.tif"

# Read the raster layer
# This raster contains ecosystem categories coded as numeric values
ecosystem_raster <- raster(file_path)

# Display basic information about the raster
print(ecosystem_raster)

# Optional: plot the full raster
# plot(ecosystem_raster, main = "Original Ecosystem Raster"), here, we don't need it, we just need the informations for the France boundaries 
#------------------------------------------------------------------------------
# 3) LOAD THE BOUNDARY OF FRANCE
#------------------------------------------------------------------------------

# Download the country boundary as an sf object
France <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "France"
)

# Plot the country boundary
plot(st_geometry(France), main = "Boundary of France")

#------------------------------------------------------------------------------
# 4) CROP AND MASK THE RASTER TO FRANCE
#------------------------------------------------------------------------------

# crop() keeps only the rectangular extent around France (metropolitan area, to avoid other france regions like Guadeloupe, Martinique... or so to come)
France_metro <- st_crop(France, xmin = -5, xmax = 10, ymin = 41, ymax = 52)
r2 <- crop(ecosystem_raster, extent(France_metro))

# mask() keeps only the pixels that fall inside the country boundary
ecosystem_france <- mask(r2, France_metro)

# create the folder for stocking the maps 
dir.create("data/ecosystems_maps", showWarnings = FALSE)

# Plot the cropped and masked raster and save it into the folder
png(".\\data\\ecosystems_maps\\p1.png", width = 10, height = 8, units = "in", res = 150)
plot(ecosystem_france, main = "Ecosystem Raster Restricted to France")
dev.off()

#------------------------------------------------------------------------------
# 5) CONVERT SPECIES COORDINATES INTO SPATIAL POINTS to fit with the sf object
#------------------------------------------------------------------------------

# matrix_full (=all_species in my case) is a data frame containing :
# - longitude
# - latitude
# - species
# - data_observation
# - source
# Example structure:
head(all_species)

# Convert the coordinate columns into spatial points
# The CRS used here is WGS84, which is the standard geographic coordinate system
spatial_points <- SpatialPoints(
  coords = all_species[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Add the occurrence points on top of the ecosystem map
# Sauvegarder p2 (la carte ecosystem)
png(".\\data\\ecosystems_maps\\p2.png", width = 10, height = 8, units = "in", res = 150)
plot(ecosystem_france, main = "Species Occurrences on Ecosystem Map")
plot(spatial_points, add = TRUE, pch = 16, cex = 1.2)
dev.off()

#------------------------------------------------------------------------------
# 6) EXTRACT ECOSYSTEM VALUES AT EACH OCCURRENCE POINT
#------------------------------------------------------------------------------

# extract() retrieves the raster value at the location of each point
# Each point receives the ecosystem code of the raster cell where it falls
eco_values <- raster::extract(ecosystem_france, spatial_points, df = FALSE)
length(eco_values)
           
# Check the extracted values
head(eco_values)

#------------------------------------------------------------------------------
# 7) ADD THE EXTRACTED ECOSYSTEM VALUES TO THE ORIGINAL DATA FRAME
#------------------------------------------------------------------------------

all_species_eco <- all_species
all_species_eco$eco_values <- eco_values
all_species_eco <- all_species_eco[!is.na(all_species_eco$eco_values), ]
nrow(all_species_eco)  

# Inspect the result
head(all_species_eco)

#------------------------------------------------------------------------------
# 8) LOAD THE ECOSYSTEM METADATA TABLE
#------------------------------------------------------------------------------

# This metadata table links the numeric raster code to descriptive ecosystem names
metadata_eco <- read.delim(".\\data\\WorldEcosystem.metadata.tsv")

# Inspect the metadata table
head(metadata_eco)

#------------------------------------------------------------------------------
# 9) MERGE THE EXTRACTED VALUES WITH THE METADATA
#------------------------------------------------------------------------------

# Merge the occurrence table with the metadata table
# each value (number) refers to a set of environmental metadata: temperature, moisture, landcover, landforms, climatic region, values of red band, green band and blue band, ecosystem and color in the raster
# we will link the metadata corresponding to each value to the all_species_eco table, and then we will keep only the columns that interest us for the follwing steps of the project
# by.x = "eco_values" means the ecosystem code in our occurrence table
# by.y = "Value" means the corresponding code column in the metadata table

all_species_eco_data <- merge(
  all_species_eco,
  metadata_eco,
  by.x = "eco_values",
  by.y = "Value"
)

# Inspect the enriched table
head(all_species_eco_data)

#------------------------------------------------------------------------------
# 10) VISUALIZE THE NUMBER OF OBSERVATIONS PER LANDCOVER CATEGORY AND SPECIES
#------------------------------------------------------------------------------

# Create a bar plot showing how many observations of each species
# are found in each landcover category, by proportion!
all_species_eco_data_prop_1 <- all_species_eco_data %>%
  group_by(species, Landcover) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(species) %>%
  mutate(prop = n / sum(n))

p3 <- ggplot(all_species_eco_data_prop_1, aes(x = Landcover, y = prop, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(
    "Rhinolophus ferrumequinum" = "#003366",
    "Barbastella barbastellus" = "#1B5E20"
  )) +
  labs(
    title = "Proportion of observations of each species by landcover",
    x = "Landcover category",
    y = "Proportion of observations"
  ) +
  theme_minimal()
print(p3)
ggsave(p3, filename = ".\\data\\ecosystems_maps\\p3.png", width = 10, height = 8)
# we can see that Rhinolophus is more present in cropland and in forest, but less in grassland, regarding Barbastella, 
# which is logical because Rhinolophus is less regarding about the forests than Barbastella 
#------------------------------------------------------------------------------
# 11) VISUALIZE THE NUMBER OF OBSERVATIONS PER TEMPERATURE AND SPECIES
#------------------------------------------------------------------------------

# Bar plot showing how many observations of each species
# are found for each temperature, by proportion
all_species_eco_data_prop_2 <- all_species_eco_data %>%
  group_by(species, Temperatur) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(species) %>%
  mutate(prop = n / sum(n))

p4 <- ggplot(all_species_eco_data_prop_2, aes(x = Temperatur, y = prop, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(
    "Rhinolophus ferrumequinum" = "#003366",
    "Barbastella barbastellus" = "#1B5E20"
  )) +
  labs(
    title = "Proportion of observations of each species by temperature",
    x = "Temperatur",
    y = "Proportion of observations"
  ) +
  theme_minimal()
print(p4)
ggsave(p4, filename = ".\\data\\ecosystems_maps\\p4.png", width = 10, height = 8)

# Hum this graph doesn't tell us much, except that we find usually more barbastella in cool temperate places and more rhinolophus in warm temperate places
# let's try a third graph, with the climatic_regions

# Bar plot showing how many observations of each species
# are found for each climatic region, by proportion
all_species_eco_data_prop_3 <- all_species_eco_data %>%
  group_by(species, Climate_Re) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(species) %>%
  mutate(prop = n / sum(n))

p5 <- ggplot(all_species_eco_data_prop_3, aes(x = Climate_Re, y = prop, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(
    "Rhinolophus ferrumequinum" = "#003366",
    "Barbastella barbastellus" = "#1B5E20"
  )) +
  labs(
    title = "Proportion of observations of each species by climatic region",
    x = "Climatic region",
    y = "Proportion of observations"
  ) +
  theme_minimal()
print(p5)
ggsave(p5, filename = ".\\data\\ecosystems_maps\\p5.png", width = 10, height = 8)

# Barbastella is exempte from the warm temperate dry region... 
# we could test in the future if the regions that become warmer and dryer in France hunt this species from there