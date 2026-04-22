################################################################################
# EXTRACTING ELEVATION DATA IN FRANCE AND VISUALIZATION
################################################################################

# =========================
# 1. Load required packages
# =========================
library(sf)        # modern spatial data handling (simple features)
library(elevatr)   # download elevation data
library(raster)    # raster data manipulation (maps)
library(ggplot2)   # data visualization
install.packages("rnaturalearth")
library(rnaturalearth)  

# Disable s2 geometry engine (can avoid issues in some spatial operations)
sf_use_s2(FALSE)


# =========================
# 2. Load France boundaries
# =========================
# Retrieve country borders from Natural Earth

France <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "France"
)

France <- st_transform(France, 4326)

# Bounding box de la France métropolitaine
bbox_metro <- st_bbox(c(
  xmin = -6,
  xmax = 10,
  ymin = 41,
  ymax = 52
), crs = st_crs(4326))

France_metro <- st_crop(France, bbox_metro)

plot(France_metro)

# =========================
# 3. Download elevation data
# =========================
# z controls resolution (higher = more detail but slower)
elevation_france_metro <- get_elev_raster(France_metro, z = 8)

plot(elevation_france_metro)


# =========================
# 4. Prepare sampling points
# =========================
# We assume your dataset contains:
# - longitude
# - latitude

# Convert coordinates into a spatial object (SpatialPoints format)
spatial_points <- SpatialPoints(
  coords = all_species_eco[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)


# =========================
# 5. Extract elevation values
# =========================
# Extract raster values at each point location
elevation <- raster::extract(elevation_france_metro, spatial_points)

# =========================
# 6. Add elevation to the dataset
# =========================
matrix_full_eco_elev <- data.frame(
  all_species_eco,
  elevation = elevation
)
View(all_species_eco)

View(matrix_full_eco_elev) # there's the elevation column 

# =========================
# 7. Visualization: elevation distribution
# =========================
# Compare elevation distributions across climate categories

windows()
p3 <- ggplot(matrix_full_eco_elev, aes(x = elevation, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "Elevation Distribution by Climate",
    x = "Elevation (m)",
    y = "Density"
  ) +
  theme_minimal()

# Display the plot
print(p3)
