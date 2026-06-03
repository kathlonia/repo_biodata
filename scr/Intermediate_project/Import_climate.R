###############################################################################
# ADD CLIMATE DATA TO MY SPECIES COORDINATE TABLE
###############################################################################
# =========================
# 1) PACKAGES
# =========================

library(Rchelsa)
library(terra)
library(tidyr)
library(dplyr)
library(ggplot2)

# Add occurrence_id to matrix_full_eco_elev
matrix_full_eco_elev <- matrix_full_eco_elev %>%
  mutate(occurrence_id = row_number())

# I had to find another way to create my column occurrence_id because I had an error message "duplicate lines"
# each time that I wanted to run the original code, creating a data frame...
# I took the row number and create another column based on it

# Create coords_df
coords_df <- matrix_full_eco_elev %>%
  select(longitude, latitude, occurrence_id)

# head(coords_df) # everything is ok, each line has his own occurrence_id number, no duplicates
# sum(is.na(coords_df$occurrence_id)) # no empty number, no NA, check is ok

# =========================
# 2) EXTRACT MONTHLY Tmax FOR 2020
# =========================
# CHELSA variable naming:
# - tas    = near-surface air temperature
# - tasmin = minimum near-surface air temperature
# - tasmax = maximum near-surface air temperature
# - pr     = precipitation
#
# Temperature values are often returned in Kelvin.
# Conversion to Celsius: °C = K - 273.15

install.packages("remotes")
remotes::install_github("inSileco/rchelsa")

tmax_r <- getChelsa(
  var       = "tasmax",
  coords    = coords_df %>% select(longitude, latitude),
  startdate = as.Date("2020-01-01"),
  enddate   = as.Date("2021-01-01"),
  dataset   = "chelsa-monthly"
)
# add the colum tmax_r to the data frame 
names(tmax_r) <- c("time", coords_df$occurrence_id)
#head(names(tmax_r))

# Compute the annual mean per occurence 
tmax_df <- tmax_r %>%
  select(-time) %>%
  as.data.frame() %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "occurrence_id",
    values_to = "tasmax_mean_k"
  ) %>%
  mutate(
    occurrence_id = as.integer(occurrence_id),
    tmax_mean_c   = (tasmax_mean_k) - 273.15
  ) %>%
  select(-tasmax_mean_k)

#View(tmax_df)

# =========================
# 3) EXTRACT MONTHLY PRECIPITATION FOR 2020
# =========================
prec_r <- getChelsa(
  var       = "pr",
  coords    = coords_df %>% select(longitude, latitude),
  startdate = as.Date("2020-01-01"),
  enddate   = as.Date("2021-01-01"),
  dataset   = "chelsa-monthly"
)

# add the colum prec_r to the data frame 
names(prec_r) <- c("time", coords_df$occurrence_id)
#head(names(prec_r))

# Compute the annual mean of precipitation per occurrence
prec_df <- prec_r %>%
  select(-time) %>%
  as.data.frame() %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%  
  pivot_longer(
    cols      = everything(),
    names_to  = "occurrence_id",
    values_to = "prec_annual_mm"                                
  ) %>%
  mutate(
    occurrence_id  = as.integer(occurrence_id)
  )

#View(prec_df)

# =========================
# 4) JOIN THE NEW CLIMATE VARIABLES
#    TO THE ORIGINAL DATASET
# =========================
matrix_full_eco_elev_climate <- matrix_full_eco_elev %>%
  left_join(tmax_df, by = "occurrence_id") %>%
  left_join(prec_df, by = "occurrence_id")

View(matrix_full_eco_elev_climate)
# now per species occurence point, we have two new informations: maximum temperature in 2021 and annual precipitation in 2021

# =========================
# 5) CHECK THE RESULT
# =========================
#dim(matrix_full_eco_elev)           # original dimensions
#dim(matrix_full_eco_elev_climate)   # enriched dimensions
#names(matrix_full_eco_elev_climate) # column names after enrichment

# =========================
# 6) PLOT THE DISTRIBUTION OF ANNUAL MEAN Tmax PER SPECIES
# =========================
# create the folder for stocking the maps 
dir.create("data/climate_maps", showWarnings = FALSE)

p1 <- ggplot(matrix_full_eco_elev_climate, aes(x = tmax_mean_c, fill = species)) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Barbastella barbastellus and Rhinolophus ferrumequinum : annual mean Tmax (2021)",
    x = "Annual mean Tmax (°C)",
    y = "Density"
  )
print(p1)
ggsave(p1, filename = ".\\data\\climate_maps\\p1.png", width = 10, height = 8)

# ------------------------------------------------------------------------------------------------------------
# again, we can see that Barbastella barbastellus prefers cooler temperatures than Rhinolophus ferrumequinum
# ------------------------------------------------------------------------------------------------------------

# =========================
# 7) PLOT THE DISTRIBUTION OF ANNUAL MEAN PRECIPITATION PER SPECIES
# =========================
p2 <- ggplot(matrix_full_eco_elev_climate, aes(x = prec_annual_mm, fill = species)) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Barbastella barbastellus and Rhinolophus ferrumequinum : annual mean precipitation (2021)",
    x = "Annual mean precipitation",
    y = "Density"
  )
print(p2)
ggsave(p2, filename = ".\\data\\climate_maps\\p2.png", width = 10, height = 8)
# ------------------------------------------------------------------------------------------------------------
# both species prefer low precipitation (around 70mm per year)
# ------------------------------------------------------------------------------------------------------------

# 8)  CURRENT CLIMATE VS FUTURE CLIMATE
#     SIMPLIFIED EXAMPLE WITH NOVEMBER
# =========================
# why this months in particulary? Bats must begin their hibernation in mid-november, but
# we can predict that the climate in november will be warmer in the future, which could delay 
# the hibernation of bats and thus have consequences on their survival (e.g. if they are active but there is no food, 
# or if they are active but there is a cold snap, etc.)

# ------------------------------------------------------------
# 8A) CURRENT CLIMATE: november temperature
#      climatology over 1981-2010
# ------------------------------------------------------------

tas_cur_november <- getChelsa(
  var     = "tas",
  coords  = coords_df %>% select(longitude, latitude),
  date    = c(11, 1981, 2010),   # November climatology
  dataset = "chelsa-climatologies"
)

# add the column tas_cur_november to the actual matrix 
names(tas_cur_november) <- c("time", coords_df$occurrence_id)
#head(names(tas_cur_november))

# Compute the annual mean per occurence 
tas_cur_november_df <- tas_cur_november %>%
  select(-time) %>%
  as.data.frame() %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "occurrence_id",
    values_to = "tas_cur_november_c"
  ) %>%
  mutate(
    occurrence_id = as.integer(occurrence_id),
    tas_cur_november_c   = (tas_cur_november_c) - 273.15)

#View(tas_cur_november_df)

# ------------------------------------------------------------
# 8B) FUTURE CLIMATE: November temperature in 2050 under SSP126
# ------------------------------------------------------------

tas_fut_november <- getChelsa(
  var     = "tas",
  coords  = coords_df %>% select(longitude, latitude),
  date    = as.Date("2050-11-01"),
  dataset = "chelsa-climatologies",
  ssp     = "ssp126",
  forcing = "MPI-ESM1-2-HR"
)

# add the column tas_fut_november to the actual matrix 
names(tas_fut_november) <- c("time", coords_df$occurrence_id)
#head(names(tas_fut_november))

tas_fut_november_df <- tas_fut_november %>%
  select(-time) %>%
  as.data.frame() %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "occurrence_id",
    values_to = "tas_fut_november_c"
  ) %>%
  mutate(
    occurrence_id = as.integer(occurrence_id),
    tas_fut_november_c   = (tas_fut_november_c) - 273.15)

#View(tas_fut_november_df)

# ------------------------------------------------------------
# 8C) ADD CURRENT AND FUTURE NOVEMBER TEMPERATURE
#      TO THE ORIGINAL TABLE
# ------------------------------------------------------------

matrix_full_eco_elev_climate <- matrix_full_eco_elev %>%
  left_join(tmax_df, by = "occurrence_id") %>%
  left_join(prec_df, by = "occurrence_id") %>%
  left_join(tas_cur_november_df, by = "occurrence_id") %>%
  left_join(tas_fut_november_df, by = "occurrence_id") %>%
  mutate(
    delta_tas_november_c = tas_fut_november_c - tas_cur_november_c
  )
View(matrix_full_eco_elev_climate)
# oops, we have three times duplicates for each occurence! let's keep only one number per occurrence

matrix_full_eco_elev_climate <- matrix_full_eco_elev_climate %>%
  distinct(occurrence_id, .keep_all = TRUE)
View(matrix_full_eco_elev_climate) # great, we have again 2'086 occurrences

# =========================
# 9) PLOT CURRENT VS FUTURE TEMPERATURE
# =========================
p3 <- ggplot(matrix_full_eco_elev_climate, aes(x = tas_cur_november_c, y = tas_fut_november_c, fill = species)) +
  geom_point(size = 4, alpha = 0.7, shape = 21, color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c(                  
    "Rhinolophus ferrumequinum" = "#FF9800",
    "Barbastella barbastellus" = "#66BB6A"
  )) +
  theme_classic() +
  labs(
    title = "Distribution of Barbastella barbastellus and Rhinolophus ferrumequinum : current vs future November temperature",
    x = "Current November temperature (°C)",
    y = "Future November temperature in 2050 (°C)"
  )
print(p3)
ggsave(p3, filename = ".\\data\\climate_maps\\p3.png", width = 10, height = 8)

## what do we see with this graph? Overall, Barbastella barbastellus occupies cooler sites (further to the left of the x-axis),
# which is consistent with its ecology (nests in forests, can be found up to 7000m) and the first graph 
# (annual mean temperature maximal). It is less thermophilic than Rhinolophus ferrumequinum,
# which generally occupies slightly warmer sites (more common in cities). Therefore, we can predict that since
# all the sites where we found the species between 2020 and 2025 will experience a temperature increase (ranging from 1° to 1.4°C),
# Barbastella barbastellus may be more affected, will therefore have to migrate to higher altitudes, and may compete with other bats,
# such as Plecotus macrobullaris, which already hunts in the Alps and Pyrenees.
# In general, if both species remain in their niches in 2050, their hibernation is very likely to be delayed until later than mid-November.
# This could cause problems if insects become scarce or if a cold snap occurs before they enter their winter roosts.