############################################################
# MACHINE LEARNING INTRODUCTION
# Random Forest for species discrimination
# Probability mapping on an environmental grid
#
# Teaching objective:
# 1. Load species occurrence data
# 2. Train a Random Forest model => split the dataset into train dataset and test dataset (50% de chaque espèce doit apparaître dans le train dataset, pour ne pas générer de biais dans l'algorithme qui s'entraînerait plus sur une espèce que sur une autre)
# 3. Evaluate the model
# 4. Identify important environmental variables
# 5. Predict species probabilities on a prediction grid
# 6. Map the predicted probability for one selected species
############################################################


############################################################
# 0) Packages
############################################################
# Used to train the Random Forest model.
#install.packages("randomForest", "caret")
library("randomForest") 
library("caret")
library("dplyr")
library("ggplot2")
library("viridis")

############################################################
# 1) Import occurrence data
############################################################


# Number of occurrences per species
table(matrix_full$species)

# Barbastella barbastellus Rhinolophus ferrumequinum 
#                     448                      1534 


############################################################
# 2) Import environmental prediction grid
############################################################

# I generated with the AI Claude a fake environmental grid of France 
grid_pred <- read.csv(
  "data/fake_grid_france.csv"
)

head(grid_pred)
str(grid_pred)

############################################################
# 3) First map of occurrence points
############################################################

# This first plot simply shows where the occurrence points are located.


ggplot(matrix_full, aes(x = longitude, y = latitude, color = species)) +
  geom_point(size = 2, alpha = 0.7) +
  coord_equal() +
  theme_classic() +
  labs(
    title = "Occurrence points of the bat species",
    x = "Longitude",
    y = "Latitude",
    color = "Species"
  )

############################################################
# 4) Prepare occurrence data for machine learning
############################################################

# We create a clean table for the Random Forest model.
#
# The response variable is:
# - species
#
# The predictor variables are:
# - elevation
# - prec_annual_mm
# - tmax_mean_c
# - Red, Green, Blue
# - eco_values
# - prop_forest_2024
# - Landcover
# - Landforms

ml_matrix_full <- matrix_full %>%
  select(
    species,
    longitude,
    latitude,
    elevation,
    prec_annual_mm,
    tmax_mean_c,
    Red,
    Green,
    Blue,
    eco_values,
    Temperatur,
    Moisture,
    Landcover,
    Landforms,
    Climate_Re,
    W_Ecosystm
  )


# Remove missing values.
# Random Forest cannot use rows with NA values.
ml_matrix_full <- na.omit(ml_matrix_full)

# Convert the response variable to a factor.
# This tells R that species is a categorical variable.
ml_matrix_full$species <- as.factor(ml_matrix_full$species)

# Convert categorical predictors to factors.
# Random Forest can use factors as categorical predictors.
ml_matrix_full$Temperatur <- as.factor(ml_matrix_full$Temperatur)
ml_matrix_full$Moisture   <- as.factor(ml_matrix_full$Moisture)
ml_matrix_full$Landcover  <- as.factor(ml_matrix_full$Landcover)
ml_matrix_full$Landforms  <- as.factor(ml_matrix_full$Landforms)
ml_matrix_full$Climate_Re <- as.factor(ml_matrix_full$Climate_Re)
ml_matrix_full$W_Ecosystm <- as.factor(ml_matrix_full$W_Ecosystm)

# Check the final structure
str(ml_matrix_full)

# Check the number of samples per species
table(ml_matrix_full$species)
# Barbastella barbastellus Rhinolophus ferrumequinum 
#                      448                      1534 

############################################################
# 5) Train / test split
############################################################

# We split the data into:
# - 70% training data
# - 30% testing data
#
# The training data are used to build the model.
# The testing data are used to evaluate the model on unseen data.

set.seed(123)
train_index <- createDataPartition(
  y = ml_matrix_full$species,
  p = 0.7,
  list = FALSE
)

train_matrix_full <- ml_matrix_full[train_index, ]
test_matrix_full  <- ml_matrix_full[-train_index, ]

# Check that both species are present in both datasets
table(train_matrix_full$species)
table(test_matrix_full$species)


############################################################
# 6) Train the Random Forest model
############################################################

# The formula species ~ . means:
# predict species using all other columns as predictors.
#
# ntree = 500 means that the forest contains 500 trees.
# importance = TRUE allows us to calculate variable importance.

rf_species <- randomForest(
  species ~ .,
  data = train_matrix_full,
  ntree = 500,
  importance = TRUE
)

print(rf_species)
# there is much more chance to classify one barabstella as one rhinolophus than the opposite, 
# and that can come from the fact that in our dataset we have much more data for rhinolophus and they have a very similar niche 

############################################################
# 7) Prediction on test data
############################################################

# We now ask the model to predict the species
# of the test dataset.

pred_species <- predict(
  rf_species,
  newdata = test_matrix_full
)

head(pred_species)

############################################################
# 8) Model evaluation
############################################################

# The confusion matrix compares:
# - predicted species
# - observed species
#
# It gives an estimate of model performance.

confusionMatrix(
  data = pred_species,
  reference = test_matrix_full$species
)
# althought the model shows a 85% of accuracy, it is not good
# indeed, since the data are already unbalanced in our matrix (448 data for Barbastella and 1534 for Rhinolophus),
# the model predict more often the rhinolophus species. 

############################################################
# 9) Feature importance
############################################################

# Random Forest can estimate which variables are most useful
# for discriminating the species.

windows()
importance(rf_species)

# Basic Random Forest importance plot
varImpPlot(rf_species)

# Create a cleaner ggplot version
importance_matrix <- importance(rf_species) %>%
  as.data.frame()

importance_matrix$feature <- rownames(importance_matrix)

importance_matrix <- importance_matrix %>%
  arrange(desc(MeanDecreaseGini))

p1 <- ggplot(
  importance_matrix,
  aes(
    x = reorder(feature, MeanDecreaseGini),
    y = MeanDecreaseGini
  )
) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Most important features to discriminate the species",
    x = "Feature",
    y = "Mean decrease in Gini"
  )
print(p1)
ggsave(p1, filename = "data/results_plots/confusion_matrix.png", width = 10, height = 6, dpi = 300)
# the variables that the model uses the most to discriminate the species are : latitude, maximum annual mean temperature, 
# mean annual precipitation, world ecosystem, longitude and elevation 

############################################################
# 10) Prepare the prediction grid
############################################################

# The prediction grid must contain the same predictor columns
# as the training data.
#
# It must NOT contain the response variable species,
# because this is what we want to predict.

grid_ml <- grid_pred %>%
  select(
    longitude,
    latitude,
    elevation,
    prec_annual_mm,
    tmax_mean_c,
    Red,
    Green,
    Blue,
    eco_values,
    Temperatur,
    Moisture,
    Landcover,
    Landforms,
    Climate_Re,
    W_Ecosystm
  )

# Convert categorical grid variables to factors.
#
# Important:
# The factor levels must be exactly the same as in the training data.
# Otherwise, R may not be able to use the Random Forest model.

grid_ml$Temperatur <- factor(
  grid_ml$Temperatur,
  levels = levels(train_matrix_full$Temperatur)
)

grid_ml$Moisture <- factor(
  grid_ml$Moisture,
  levels = levels(train_matrix_full$Moisture)
)

grid_ml$Landcover <- factor(
  grid_ml$Landcover,
  levels = levels(train_matrix_full$Landcover)
)

grid_ml$Landforms <- factor(
  grid_ml$Landforms,
  levels = levels(train_matrix_full$Landforms)
)

grid_ml$Climate_Re <- factor(
  grid_ml$Climate_Re,
  levels = levels(train_matrix_full$Climate_Re)
)

grid_ml$W_Ecosystm <- factor(
  grid_ml$W_Ecosystm,
  levels = levels(train_matrix_full$W_Ecosystm)
)

# Remove rows with missing values.

# Missing values may appear if some categories in the grid
# are not present in the training data.

grid_ml <- na.omit(grid_ml)

str(grid_ml)


############################################################
# 11) Predict species probabilities on the grid
############################################################

# type = "prob" asks the model to return probabilities
# instead of only the most likely class.
#
# The output contains one column per species.

grid_prob <- predict(
  rf_species,
  newdata = grid_ml,
  type = "prob"
)

head(grid_prob)

# Combine coordinates, predictors and probabilities in one table.

grid_map <- cbind(grid_ml, grid_prob)

head(grid_map)


############################################################
# 12) Map probability for one selected species
############################################################

# First, list all species available in the model.

species_names <- levels(train_matrix_full$species)
species_names

# ----------------------------------------------------------
# IMPORTANT 
# ----------------------------------------------------------
# To change the species displayed on the probability map,
# modify only the line below.
#
# Example:
# target_species <- "Cardamine bellidifolia"
#
# or:
# target_species <- "Cardamine resedifolia"
#
# The name must be written exactly as it appears in:
# species_names
# ----------------------------------------------------------

target_species <- "Rhinolophus ferrumequinum" 


ggplot(grid_map, aes(x = longitude, y = latitude)) +
  geom_tile(aes(fill = .data[[target_species]])) + #  geom_tile creates a colored tile for each grid point, colored according to the predicted probability of the target species.
  geom_point(
    data = matrix_full,
    aes(x = longitude, y = latitude),
    inherit.aes = FALSE,
    color = "black",
    size = 0.8,
    alpha = 0.5
  ) +
  scale_fill_viridis_c(limits = c(0, 1)) +
  coord_equal() +
  theme_classic() +
  labs(
    title = paste("Predicted probability map for", target_species),
    subtitle = "Prediction on an environmental grid over France",
    x = "Longitude",
    y = "Latitude",
    fill = "Probability"
  )


############################################################
# Create the probability map with occurrence points colored by species
############################################################

library(sf)
library(rnaturalearth)

france <- ne_countries(scale = "medium", country = "France", returnclass = "sf")

# box around the france borders
bbox <- st_as_sfc(st_bbox(c(
  xmin = -6, xmax = 10,
  ymin = 41, ymax = 52
), crs = 4326))

# remove france from the bbox => polygone "everything without france"
masque_exterieur <- st_difference(bbox, st_union(france))


# ── create one map per species, and then assemble both with patchwork ─────────────────────────────
library(patchwork)
make_map <- function(species_col, species_name, point_color) {
  
  ggplot(grid_map, aes(x = longitude, y = latitude)) +
    
    geom_tile(aes(fill = .data[[species_col]])) +
    
    geom_sf(
      data = masque_exterieur,
      fill = "white", color = NA,
      inherit.aes = FALSE
    ) +
    
    geom_sf(
      data = france,
      fill = NA, color = "black", linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    
    geom_point(
      data = matrix_full %>% filter(species == species_name),
      aes(x = longitude, y = latitude),
      inherit.aes = FALSE,
      color = "black", size = 2.5, alpha = 0.9
    ) +
    
    geom_point(
      data = matrix_full %>% filter(species == species_name),
      aes(x = longitude, y = latitude),
      inherit.aes = FALSE,
      color = point_color, size = 1.5, alpha = 0.9
    ) +
    
    scale_fill_viridis_c(limits = c(0, 1)) +
    coord_sf(xlim = c(-5, 9.5), ylim = c(41, 51.5), expand = FALSE) +
    theme_classic() +
    labs(
      title = species_name,
      x = "Longitude", y = "Latitude",
      fill = "Probability"
    )
}

# ── Create the two maps ─────────────────────────────────────────────────
map_rhino <- make_map(
  species_col  = "Rhinolophus ferrumequinum",
  species_name = "Rhinolophus ferrumequinum",
  point_color  = "#3b6884"
)

map_barba <- make_map(
  species_col  = "Barbastella barbastellus",
  species_name = "Barbastella barbastellus",
  point_color  = "#8c0f19"
)

graphA <- map_rhino + map_barba +
  plot_annotation(
    title    = "Predicted probability maps — Random Forest",
    subtitle = "Prediction on the environmental grid over France"
  ) +
  plot_layout(guides = "collect")

windows()
print(graphA)

# this maps shows us that the prediction probability for the locations of the species are way much higher
# for Rhinolophus ferrumequinum than for Barbastella barbastellus, and this is probably due to the unbalanced 
# data !! As the confusion matrix shows us, the algorithm has only 63% of sensitivity and 14% of detection for barbastella.
# we could maybe delete this biais by including the sample size in the calcule, or take the same number of observations
# for both species. 