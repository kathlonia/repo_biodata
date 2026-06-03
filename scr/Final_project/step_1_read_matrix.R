library(dplyr)
library(lubridate)

# charge the matrix
matrix_full <- read.csv ("data\\matrix_full.csv", sep = ",", header =TRUE, stringsAsFactors = FALSE)
View(matrix_full)

# ==================================
# Ecological question
# ==================================
# Given that both bat species use deciduous forests during the spring, but that Barbastella barbastellus 
# is more tolerant of cold and high altitudes, I would like to compare the environmental niche of these 
# two species and understand which variable most discriminates between them. 
# I predict that it will be the elevation (Barbastella can go up to 7000 m!)

# Variables I will use:
# - Temperature
# - Elevation
# - NDVI 2020 and NDVI 2025
# - Land Cover 

# since I have extracted the informations for the NDVI for 2020 and 2025, 
# I will filter my observations to keep only those that were observed in the spring of 2020 and 2025
matrix_full_filtered <- matrix_full %>%
  filter(year(date_obs) %in% c(2020, 2025))
View(matrix_full_filtered)

table(matrix_full_filtered$species)
# we have 90 observations of Barbastella barbastellus and 102 observations of Rhinolophus ferrumequinum