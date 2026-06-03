################################################################################
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
#products <- rs_products()

# Display the first rows
#head(products)
#getProducts("^MOD|^MYD|^MCD")

#MOD = Terra satellite products
#MYD = Aqua satellite products
#MCD = Combined products (Terra + Aqua)

#MODIStsp_get_prodlayers("M*D13Q1")

#product <- "MOD09A1" #surface spectral reflectance of Terra
#product <- "MOD13Q1" # NDVI

#productInfo(product)

# I've chosen to check two different informations: 
# 1) the vegetation index (NDVI) from the product MOD13Q1, to see if the evolution of the vegetation has an influence on the evolution of the occurences of the two species (knowing that specially Barbastellus barbastella frequents mature deciduous forest)
# 2) the concrete surface (Landcover type 1), from the product MOD09A1, to see if the evolution of concrete surface has an influence on the occurence of the two species 
# I downlaoded one file for the NDVI for the month of MAY 2020, and one file for the NDVI for the month of MAY 2025
# then I downlaoded one file for the concrete surface for the month of MAY 2020, and one file for the concrete surface for the month of MAY 2024 (MAY 2025 wasn't available yet)

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
# 3. Order all the files
# ==============================================================================
# Créer les sous-dossiers
dir.create("data/appeears_manual_download/NDVI_may2020", showWarnings = FALSE)
dir.create("data/appeears_manual_download/NDVI_may2025", showWarnings = FALSE)
dir.create("data/appeears_manual_download/LandCover_2020", showWarnings = FALSE)
dir.create("data/appeears_manual_download/LandCover_2024", showWarnings = FALSE)

file.rename(
  from = list.files("data/appeears_manual_download", 
                    pattern = "MOD13Q1.*2020.*\\.tif$", 
                    full.names = TRUE),
  to   = file.path("data/appeears_manual_download/NDVI_may2020",
                    list.files("data/appeears_manual_download", 
                               pattern = "MOD13Q1.*2020.*\\.tif$"))
)

file.rename(
  from = list.files("data/appeears_manual_download", 
                    pattern = "MOD13Q1.*2025.*\\.tif$", 
                    full.names = TRUE),
  to   = file.path("data/appeears_manual_download/NDVI_may2025",
                    list.files("data/appeears_manual_download", 
                               pattern = "MOD13Q1.*2025.*\\.tif$"))
)

file.rename(
  from = list.files("data/appeears_manual_download", 
                    pattern = "MCD12Q1.*2020.*\\.tif$", 
                    full.names = TRUE),
  to   = file.path("data/appeears_manual_download/LandCover_2020",
                    list.files("data/appeears_manual_download", 
                               pattern = "MCD12Q1.*2020.*\\.tif$"))
)

file.rename(
  from = list.files("data/appeears_manual_download", 
                    pattern = "MCD12Q1.*2024.*\\.tif$", 
                    full.names = TRUE),
  to   = file.path("data/appeears_manual_download/LandCover_2024",
                    list.files("data/appeears_manual_download", 
                               pattern = "MCD12Q1.*2024.*\\.tif$"))
)

# ==============================================================================
# 4. COMPARE NDVI BETWEEN 2020 AND 2025 
# ==============================================================================

# --------------------------------------------------------------------------------------------------------------------------------------------------
# select only the occurences in May for the two species (this has more sens to look at the NDVI in May, since it's spring and the bats are active)
# --------------------------------------------------------------------------------------------------------------------------------------------------

library(lubridate)
matrix_may <- matrix_full_eco_elev_climate %>%
  filter(month(date_obs) == 05)

table(matrix_may$species)
# 18 points left for Barbastella barbastellus and 77 points left for Rhinolophus ferrumequinum, not much.... not really relevant but okay for this project  

# remove the VI_quality files
ndvi_2020_files <- list.files(
  "data/appeears_manual_download/NDVI_may2020",
  pattern = "NDVI.*\\.tif$",
  full.names = TRUE
)

ndvi_2025_files <- list.files(
  "data/appeears_manual_download/NDVI_may2025",
  pattern = "NDVI.*\\.tif$",
  full.names = TRUE
)

# compute the mean
ndvi_2020_mean <- mean(rast(ndvi_2020_files), na.rm = TRUE)
ndvi_2025_mean <- mean(rast(ndvi_2025_files), na.rm = TRUE)

# -----------------------------------------------------------------------------
# Crop on the France territory 
# -----------------------------------------------------------------------------
france_vect <- vect(France_sf)
france_vect_proj <- project(france_vect, crs(ndvi_2020_mean))

ndvi_2020_france <- mask(crop(ndvi_2020_mean, france_vect_proj), france_vect_proj)
ndvi_2025_france <- mask(crop(ndvi_2025_mean, france_vect_proj), france_vect_proj)

# ----------------------------------------------------------------------------------------------------
# Compute the delta NDVI => the difference of vegetation indice between 2020 and 2025 (2025 - 2020)
# ----------------------------------------------------------------------------------------------------
delta_ndvi <- ndvi_2025_france - ndvi_2020_france

# create the folder for stocking the maps 
dir.create("data/satellite_maps", showWarnings = FALSE)

png(".\\data\\satellite_maps\\p1.png", width = 10, height = 8, units = "in", res = 150)
plot(delta_ndvi, main = "Delta NDVI (mai 2025 - mai 2020)",
     col = colorRampPalette(c("red", "white", "darkgreen"))(100))
plot(france_vect_proj, add = TRUE, border = "black", lwd = 1)
dev.off()

# -----------------------------------------------------------------------------
# convert occurences dots from my matrice to spatial object sf
# -----------------------------------------------------------------------------
points_vect <- vect(
  matrix_may,
  geom = c("longitude", "latitude"),
  crs  = "EPSG:4326"
) %>% project(crs(ndvi_2020_france))

# -----------------------------------------------------------------------------
# 5) Extract the NDVI 2020 + 20205 + delta for each occurence dot 
# -----------------------------------------------------------------------------
matrix_may$NDVI_2020  <- terra::extract(ndvi_2020_france, points_vect)[, 2]
matrix_may$NDVI_2025  <- terra::extract(ndvi_2025_france, points_vect)[, 2]
matrix_may$delta_NDVI <- terra::extract(delta_ndvi,       points_vect)[, 2]

# check the raw values extracted
summary(matrix_may$NDVI_2020)
summary(matrix_may$NDVI_2025)
View(matrix_may)
# then no need to divide by 1000, appEARS already did it! 

# -----------------------------------------------------------------------------
# Plot : NDVI 2020 vs NDVI 2025, per species 
# -----------------------------------------------------------------------------
p2 <- ggplot(matrix_may,
       aes(x = NDVI_2020, y = NDVI_2025, color = species)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(
    "Barbastella barbastellus"  = "darkred",
    "Rhinolophus ferrumequinum" = "steelblue"
  )) +
  labs(
    title = "NDVI may 2020 vs may 2025 where the bat species were found",
    x     = "NDVI may 2020",
    y     = "NDVI may 2025",
    color = "Species"
  ) +
  theme_classic()
print(p2)
ggsave(p2, filename = ".\\data\\satellite_maps\\p2.png", width = 10, height = 8)

# each dot on the dashed line has the same NDVI on May 2020 than on May 2025, and 
# each dot above the line represents higher NDVI on May 2025 than on May 2020. 
# Globally, ther are more point below the line, meaning than the area where the bats 
# were found on May 2025 were less green than the area where these bats were seen on May 2020.
# but the bats move a lot, so it's not really relevant, it's just interesting to see how to 
# create and read this graph. (especially since there are just 18 occurrences for Barbastellus !)

# -----------------------------------------------------------------------------
# 7) Plot : distribution of the NDVI delta per species
# -----------------------------------------------------------------------------
p3 <- ggplot(matrix_may, aes(x = delta_NDVI, fill = species)) +
  geom_density(alpha = 0.5, adjust = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c(
    "Barbastella barbastellus"  = "darkred",
    "Rhinolophus ferrumequinum" = "steelblue"
  )) +
  labs(
    title = "Evolution of NDVI (May 2025 - May 2020) by species",
    x     = "Delta NDVI (+ = greening, - = browning)",
    y     = "Density",
    fill  = "Species"
  ) +
  theme_classic()
print(p3)
ggsave(p3, filename = ".\\data\\satellite_maps\\p3.png", width = 10, height = 8)

# The highest density of Rhinolophus found on 2025 was on area with a lower NDVI than on 2020
# same for the Barbastella; however, there are some specimen of Rhinolophus that are found with an 
# higher NDVI, there's still hope! 
# another explanation could be => the bats migrate towards cities because the forests are disappearing...

# ==============================================================================
# 5. COMPARE LandCover urban surface between may 2020 and may 2024 
# ==============================================================================
# -----------------------------------------------------------------------------
# Read LandCover raster 
# -----------------------------------------------------------------------------
lc_2020 <- rast(list.files("data/appeears_manual_download/LandCover_2020",
                             pattern = "\\.tif$", full.names = TRUE)[1])
lc_2024 <- rast(list.files("data/appeears_manual_download/LandCover_2024",
                             pattern = "\\.tif$", full.names = TRUE)[1])

# keep only the urban pixels (value 13)
urban_2020 <- lc_2020 == 13
urban_2024 <- lc_2024 == 13

# -----------------------------------------------------------------------------
# Check which landcover categories are present in the two landcover rasters
# -----------------------------------------------------------------------------
lc_points <- vect(matrix_full_eco_elev_climate,
                  geom = c("longitude", "latitude"),
                  crs  = "EPSG:4326") %>%
  project(crs(lc_2020))

matrix_full_eco_elev_climate$lc_value_2020 <- terra::extract(lc_2020, lc_points)[, 2]
matrix_full_eco_elev_climate$lc_value_2024 <- terra::extract(lc_2024, lc_points)[, 2]

#table(matrix_full_eco_elev_climate$lc_value_2020)
#table(matrix_full_eco_elev_climate$lc_value_2024)

# Clip to France
lc_france_proj <- project(france_vect, crs(lc_2020))
urban_2020_france <- mask(crop(urban_2020, lc_france_proj), lc_france_proj)
urban_2024_france <- mask(crop(urban_2024, lc_france_proj), lc_france_proj)

# Extract values at occurrence points
lc_points <- vect(matrix_full_eco_elev_climate,
                   geom = c("longitude", "latitude"),
                   crs  = "EPSG:4326") %>%
  project(crs(lc_2020))

matrix_full_eco_elev_climate$urban_2020 <- as.integer(terra::extract(urban_2020_france, lc_points)[, 2] == 13)
matrix_full_eco_elev_climate$urban_2024 <- as.integer(terra::extract(urban_2024_france, lc_points)[, 2] == 13)

# -------------------------------------------------------------------------------------------
# Plot : proportion of urban sites per species, 2020 VS 2024 (because the LC is from 2024)
# --------------------------------------------------------------------------------------------
#urban_summary <- matrix_full_eco_elev_climate %>%
#  group_by(species) %>%
#  summarise(
#    prop_urban_2020 = mean(urban_2020 == 1, na.rm = TRUE),
#    prop_urban_2024 = mean(urban_2024 == 1, na.rm = TRUE)
#  ) %>%
#  pivot_longer(
#    cols      = c(prop_urban_2020, prop_urban_2024),
#   names_to  = "year",
#    values_to = "proportion_urban"
#  ) %>%
#  mutate(year = ifelse(year == "prop_urban_2020", "2020", "2024"))

#p4 <- ggplot(urban_summary, aes(x = year, y = proportion_urban, fill = species)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  scale_fill_manual(values = c(
#    "Rhinolophus ferrumequinum" = "#003366",
#   "Barbastella barbastellus" = "#1B5E20"
#  )) +
#  labs(
#    title = "Proportion of urban sites per species, 2020 VS 2024",
#    x     = "Year",
#    y     = "Proportion of urban sites",
#    fill  = "Species"
#  ) +
#  theme_classic()
#print(p4)
#ggsave(p4, filename = ".\\data\\satellite_maps\\p4.png", width = 10, height = 8)

#View(matrix_full_eco_elev_climate)
### there are 0 urban points!!! 
# meaning that all the species found were not found in urban areas, but rather in different
# types of forest (lc_values 1 to 9) or in prairies (lc_value 10). This is a bit surprising because in our graph 
# data\ecosystems_maps\p3.png, we cold see that there were some observations on settlement! but maybe they were found around the cities 
# and not directly into them, so the pixels is rather grassland, or forest... depend on the size of the pixel I guess! 
# another solution then is to see the pixels around each species dot, in order to see if the landscape around them has changed and 
# if it influences the niches ? We will focus on the forests, pixels 1 to 9

# =====================================================================================================================
# 6: compare the evolution of the forest area around the occurences of the two species (all the year, not only in May)
# =====================================================================================================================

# buffer zone of 1km around each dot 
points_buffer <- buffer(lc_points, width = 1000)

# ----- 2020 -----
lc_buffer_2020 <- terra::extract(lc_2020, points_buffer)
forest_prop_2020 <- lc_buffer_2020 %>%
  group_by(ID) %>%
  summarise(prop_forest_2020 = mean(MCD12Q1.061_LC_Type1_20200101T000000_aid0001 %in% 1:9, na.rm = TRUE))

matrix_full_eco_elev_climate$prop_forest_2020 <- forest_prop_2020$prop_forest_2020

# ----- 2024 -----
lc_buffer_2024 <- terra::extract(lc_2024, points_buffer)
forest_prop_2024 <- lc_buffer_2024 %>%
  group_by(ID) %>%
  summarise(prop_forest_2024 = mean(MCD12Q1.061_LC_Type1_doy2024001000000_aid0001 %in% 1:9, na.rm = TRUE))

matrix_full_eco_elev_climate$prop_forest_2024 <- forest_prop_2024$prop_forest_2024

# ----- Delta -----
matrix_full_eco_elev_climate <- matrix_full_eco_elev_climate %>%
  mutate(delta_forest = prop_forest_2024 - prop_forest_2020)

# check 
summary(matrix_full_eco_elev_climate$prop_forest_2020)
summary(matrix_full_eco_elev_climate$prop_forest_2024)
summary(matrix_full_eco_elev_climate$delta_forest)
View(matrix_full_eco_elev_climate)

# great, we have mostly no difference, but we also have some augmentation of the presence of forest and some diminutions
# now we could get a plot or a map out of these, to illustrate the evolution of the forest around the occurences of the two species

# =============================================================
# 7: Interactive spatial map of the forest delta
# =============================================================
library(plotly)
library(broom)  

France_df <- st_coordinates(France_sf) %>%
  as.data.frame() %>%
  rename(long = X, lat = Y) %>%
  mutate(group = paste(L1, L2, sep = "_"))  

p_for_plotly <- ggplot() +
  geom_polygon(                            
    data  = France_df,
    aes(x = long, y = lat, group = group),
    fill  = "grey95",
    color = "black"
  ) +
  geom_point(
    data = matrix_full_eco_elev_climate,
    aes(x     = longitude,
        y     = latitude,
        color = delta_forest,
        text  = paste("Species:", species,
                      "<br>Delta forest:", round(delta_forest, 3),
                      "<br>Forest 2020:", round(prop_forest_2020, 3),
                      "<br>Forest 2024:", round(prop_forest_2024, 3))),
    size  = 2,
    alpha = 0.7
  ) +
  scale_color_gradient2(
    low      = "red",
    mid      = "white",
    high     = "darkgreen",
    midpoint = 0,
    name     = "Delta forest"
  ) +
  facet_wrap(~ species) +
  coord_cartesian(xlim = c(-5, 10), ylim = c(42, 51)) + 
  labs(
    title = "Spatial evolution of forest proportion around occurrences (2020 → 2024)",
    x     = "Longitude",
    y     = "Latitude"
  ) +
  theme_classic()

plotly::ggplotly(p_for_plotly, tooltip = "text")

# htmlwidgets::saveWidget(
# ggplotly(p_map, tooltip = "text"),
# file = "data/map_interactive.html")
# browseURL("data/map_interactive.html")

# =============================================================
# 8: Boxplot to compare 2020 vs 2024 per species
# =============================================================

forest_long <- matrix_full_eco_elev_climate %>%
  select(species, prop_forest_2020, prop_forest_2024) %>%
  pivot_longer(
    cols      = c(prop_forest_2020, prop_forest_2024),
    names_to  = "year",
    values_to = "prop_forest"
  ) %>%
  mutate(year = ifelse(year == "prop_forest_2020", "2020", "2024"))

p5 <- ggplot(forest_long, aes(x = year, y = prop_forest, fill = species)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  scale_fill_manual(values = c(
    "Barbastella barbastellus"  = "darkred",
    "Rhinolophus ferrumequinum" = "steelblue"
  )) +
  labs(
    title = "Forest proportion in 1km buffer around occurrences",
    x     = "Year",
    y     = "Forest proportion",
    fill  = "Species"
  ) +
  theme_classic()

print(p5)
ggsave(p5, filename = ".\\data\\satellite_maps\\p5.png", width = 10, height = 8)

# the proportion of forest seems to be slightly higer for Rhinolophus ferrumequinum 
# but not for Barbastellus barbastella; both species haven't dramatically migrated or changed
# environements from 2020 to 2024, or the environement around this two species hasn't dramatically
# changed in this period. Good news!

# for the follows step of the project, I will export my matrix full as a csv,
# because we will not add any other informations to it, we will just perform analysis
# and plots with it.

write.csv(matrix_full_eco_elev_climate, "data/matrix_full.csv", row.names = FALSE)
