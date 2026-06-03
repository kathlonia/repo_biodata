################################################################################
# EXTRACTING ELEVATION DATA IN FRANCE AND VISUALIZATION
################################################################################

library(sf)        # modern spatial data handling (simple features)
library(elevatr)   # download elevation data
library(raster)    # raster data manipulation (maps)
library(ggplot2)   # data visualization
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

# Bounding box of metroplitan France
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
plot(elevation_france_metro) # but it seems not to be cropped!!

# =========================
# 4. Prepare sampling points
# =========================
# check that our dataset contains:
# - longitude
# - latitude
View(all_species_eco_data) #everything ok, we can continue

# Convert coordinates into a spatial object (SpatialPoints format)
spatial_points <- SpatialPoints(
  coords = all_species_eco_data[, c("longitude", "latitude")],
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
  all_species_eco_data,
  elevation = elevation
)
View(matrix_full_eco_elev) # there's the elevation column at the end!

# =========================
# 7. Visualization: elevation distribution
# =========================
# create the folder for stocking the maps 
dir.create("data/elevation_maps", showWarnings = FALSE)

# Compare elevation distributions across climate categories
p1 <- ggplot(matrix_full_eco_elev, aes(x = elevation, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "Elevation Distribution by Climate",
    x = "Elevation (m)",
    y = "Density"
  ) +
  theme_minimal()
print(p1)
ggsave(p1, filename = ".\\data\\elevation_maps\\p1.png", width = 10, height = 8)

# means that warm temperate dry regions are usually found not higher than around 100m of elevation,
# that warm temperate moist regions are found mostly around 100m also, but also until 800m of elevation,
# and that cool temperate moist regions are found until 2000m of elevation
# right now, for our species of bats, this graphe doesn't give us any supplementary informations
# but we could imagine for the future improve this graph in order to test the distirbution of our two species accross several elevations...

# Compare elevation distributions across climate categories
p2 <- ggplot(matrix_full_eco_elev, aes(x = elevation, fill = species )) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "Elevation Distribution by Species",
    x = "Elevation (m)",
    y = "Density"
  ) +
  theme_minimal()
print(p2)
ggsave(p2, filename = ".\\data\\elevation_maps\\p2.png", width = 10, height = 8)

# ---------------------------------------------------------------------------------------------------
# much more interesting! we can see that Rhinolophus ferrumequinum has a high density aroumf 250m,
# but the occurrences of decrease slowly when we get around 500m, and there is no Rhinolophus higher
# than 1500m, in contrast with Barbastella (we can find her until 2000m)
# ----------------------------------------------------------------------------------------------------
