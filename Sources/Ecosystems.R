###############################################################################
# ADDING ECOSYSTEM DATA TO SPECIES OCCURRENCE COORDINATES
# Example workflow:
# - read an ecosystem raster
# - crop it to France
# - extract ecosystem values at species occurrence points
# - join the raster values with metadata
# - visualize the result
###############################################################################

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
plot(ecosystem_raster, main = "Original Ecosystem Raster")

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

# crop() keeps only the rectangular extent around France
France_metro <- st_crop(France, xmin = -5, xmax = 10, ymin = 41, ymax = 52)
r2 <- crop(ecosystem_raster, extent(France_metro))

# mask() keeps only the pixels that fall inside the country boundary
ecosystem_france <- mask(r2, France_metro)

# Plot the cropped and masked raster
plot(ecosystem_france, main = "Ecosystem Raster Restricted to France")

#------------------------------------------------------------------------------
# 5) CONVERT SPECIES COORDINATES INTO SPATIAL POINTS
#------------------------------------------------------------------------------

# We assume that matrix_full is a data frame containing at least:
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
windows()
plot(ecosystem_france, main = "Species Occurrences on Ecosystem Map")
plot(spatial_points, add = TRUE, pch = 16, cex = 1.2)

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
# by.x = "eco_values" means the ecosystem code in our occurrence table
# by.y = "Value" means the corresponding code column in the metadata table
all_species_eco <- merge(
  all_species_eco,
  metadata_eco,
  by.x = "eco_values",
  by.y = "Value"
)

# Inspect the enriched table
head(all_species_eco)

#------------------------------------------------------------------------------
# 10) VISUALIZE THE NUMBER OF OBSERVATIONS PER CLIMATE CATEGORY AND SPECIES
#------------------------------------------------------------------------------

# Create a bar plot showing how many observations of each species
# are found in each climate category
windows()
p2 <- ggplot(all_species_eco, aes(x = Landcover, fill = species)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Count of Observations of Each Species by Climate",
    x = "Climate category",
    y = "Number of observations"
  ) +
  theme_minimal()

# Display the plot
print(p2)

View(all_species_eco)

