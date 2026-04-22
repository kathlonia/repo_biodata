################################################################################
# SIMPLE MANUAL WORKFLOW:
# EXPLORE MODIS PRODUCTS, DOWNLOAD NDVI MANUALLY, READ IT IN R,
# EXTRACT VALUES AT POINT LOCATIONS, AND ADD THEM TO THE DATA TABLE
################################################################################

# ==============================================================================
# 1. Load required packages
# ==============================================================================

install.packages("remotes")
remotes::install_github("ropensci/MODIStsp")

install.packages("appeears")
library(luna)
library(MODIStsp)
library(appeears)
library(terra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)

# ==============================================================================
# 2. Explore available MODIS products
# ==============================================================================
# List all products available through AppEEARS
products <- rs_products()

# Display the first rows
head(products)


getProducts("^MOD|^MYD|^MCD")

#MOD = Terra satellite products

#MYD = Aqua satellite products

#MCD = Combined products (Terra + Aqua)

MODIStsp_get_prodlayers("M*D13Q1")

product <- "MOD09A1" #surface spectral reflectance of Terra
#product <- "MOD13Q1" # NDVI

productInfo(product)

# IMPORTANT:
# Look for the NDVI layer name in the printed list.
# This is the layer you will select manually in AppEEARS.


# ==============================================================================
# 4. Export the France polygon for manual upload in AppEEARS
# ==============================================================================
# This file can be uploaded directly in the AppEEARS web interface
# when creating an area request.

france_sf <- ne_countries(
  scale = "medium",
  country = "France",
  returnclass = "sf"
)

dir.create(".data", showWarnings = FALSE)

st_write(
  france_sf,
  ".data/france.geojson",
  delete_dsn = TRUE
)

plot(st_geometry(france_sf), col = "lightgray", main = "France")

# ------------------------------------------------------------------------------
# MANUAL STEP IN APP EEARS
# ------------------------------------------------------------------------------
# 1. Open the AppEEARS website
# 2. Create an AREA request
# 3. Upload the file: .data/france.geojson
# 4. Select product: MOD13Q1.061
# 5. Select layer: NDVI
# 6. Select the desired date range
# 7. Choose GeoTIFF as output format if available
# 8. Submit the task
# 9. Download the resulting NDVI raster manually
# 10. Save it in the folder: .data/appeears_manual_download
# ------------------------------------------------------------------------------


# ==============================================================================
# 5. Read the manually downloaded NDVI raster
# ==============================================================================
manual_path <- ".\\data\\appeears_manual_download"

# List all tif files in the folder
manual_tif <- list.files(
  manual_path,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

print(manual_tif)

# Read the first raster
ndvi_raster <- rast(manual_tif[1])

# Check raster information
print(ndvi_raster)

# Plot the raster
windows()
plot(ndvi_raster, main = "Manually downloaded NDVI raster")


# ==============================================================================
# 6. Clip the raster to the exact France border
# ==============================================================================
france_vect <- vect(france_sf)

# Reproject the France polygon to the raster CRS
france_vect <- project(france_vect, crs(ndvi_raster))

# Crop and mask
ndvi_france <- crop(ndvi_raster, france_vect)
ndvi_france <- mask(ndvi_france, france_vect)

# Plot the clipped raster
windows()
plot(ndvi_france, main = "NDVI raster clipped to France")
plot(france_vect, add = TRUE, border = "black", lwd = 1)


# ==============================================================================
# 7. Convert the sampling table to spatial points
# ==============================================================================
# We assume your data frame is called matrix_full_eco
# and contains longitude and latitude columns.

points_vect <- vect(
  matrix_full_eco_elev,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

# Reproject the points to the raster CRS
points_vect <- project(points_vect, crs(ndvi_france))

# Plot the points on top of the raster
plot(ndvi_france, main = "Sampling points over NDVI raster")
plot(points_vect, add = TRUE, col = "red", pch = 16)


# ==============================================================================
# 8. Extract NDVI values at point locations
# ==============================================================================
ndvi_values <- terra::extract(ndvi_france, points_vect)

# Check extracted values
head(ndvi_values)


# ==============================================================================
# 9. Add NDVI values to the original data frame
# ==============================================================================
# The first column returned by terra::extract() is usually the point ID
# and the second column contains the extracted raster value.

matrix_full_eco_elev$NDVI <- ndvi_values[, 2]

# Check the updated table
head(matrix_full_eco_elev)


# ==============================================================================
# 10. Simple control plot
# ==============================================================================

windows()


  ggplot(matrix_full_eco_elev, aes(x = NDVI, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +  # smoothed density curves
  labs(
    title = "NDVI Distribution by Climate",
    x = "NDVI",
    y = "Density"
  ) +
  theme_minimal()
