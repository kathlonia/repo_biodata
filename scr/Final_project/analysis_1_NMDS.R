# I want to do first a NMDS, to visualize the two different niches
library(vegan)
library(ggforce)
library(dplyr)
library(lubridate)
library(ggrepel)

# 1) select if the year 2024 has enough data to use prop_forest_2024
table(year(matrix_full$date_obs)) # 114 data for 2024
matrix_full %>%
  filter(year(date_obs) == 2024) %>%
  count(species) # 54 for barbastella and 60 for rhinolophus => ok!

# 2) create the matrix for NMDS
NMDS_numeric <- matrix_full %>%
  filter(year(date_obs) == 2024) %>%
  dplyr::select(species, tmax_mean_c, elevation, prec_annual_mm, prop_forest_2024) %>%
  na.omit()
#View(NMDS_numeric)
#head(NMDS_numeric)

# 2) separate the matrix: one with the numeric values and one with the species values (factor)
NMDS_num <- NMDS_numeric %>%
  dplyr::select(tmax_mean_c, elevation, prec_annual_mm, prop_forest_2024)

NMDS_meta <- NMDS_numeric %>%
  dplyr::select(species)

# 3) compute the NMDS
nmds_result <- metaMDS(
  NMDS_num,
  distance = "euclidean",
  k = 2,
  trymax = 20
)

nmds_result$stress # good result, < 0.05

# 4) project the numeric variables 
env_fit <- envfit(nmds_result, NMDS_num, permutations = 999)
arrows_df <- as.data.frame(scores(env_fit, display = "vectors"))
arrows_df$variable <- rownames(arrows_df)

# 5) project the categorical variables
env_cat <- NMDS_meta %>%
  dplyr::select(species) %>%
  mutate(across(everything(), as.factor))

fit_cat <- envfit(nmds_result, env_cat, permutations = 999)

# 6) Extract the scores
nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_scores$species <- NMDS_meta$species

# 7) plot the NMDS with ggplot2
scale_factor <- 0.1  

arrows_df_scaled <- arrows_df %>%
  mutate(
    NMDS1 = NMDS1 * scale_factor,
    NMDS2 = NMDS2 * scale_factor
  )

# data frame with manual coordinates positions for each variable label, to avoir overlaps
arrows_labels <- arrows_df_scaled %>%
  mutate(
    label_x = case_when(
      variable == "prop_forest_2024" ~ NMDS1 * 1.3 - 0.06,  
      variable == "elevation"        ~ NMDS1 * 1.3 + 0.02,
      variable == "tmax_mean_c"      ~ NMDS1 * 1.3,
      variable == "prec_annual_mm"   ~ NMDS1 * 1.3,
      TRUE ~ NMDS1 * 1.3
    ),
    label_y = case_when(
      variable == "prop_forest_2024" ~ NMDS2 * 1.3,
      TRUE ~ NMDS2 * 1.3
    )
  )

dir.create("data/results_plots", recursive = TRUE)

p1 <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = species)) +
  geom_point(alpha = 0.7, size = 3) +
  stat_ellipse(aes(fill = species), geom = "polygon",   # ellipses de confiance
               alpha = 0.15, linetype = "dashed") +
  geom_segment(
    data = arrows_df_scaled,
    aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
    color = "black",
    arrow = arrow(length = unit(0.3, "cm")),
    inherit.aes = FALSE
  ) +
 geom_text(
    data = arrows_labels,
    aes(x = label_x, y = label_y, label = variable),
    color = "black", size = 3, fontface = "bold.italic",
    inherit.aes = FALSE
  ) +
  annotate("text", x = Inf, y = Inf,
           label = paste("Stress =", round(nmds_result$stress, 3)),
           hjust = 1.1, vjust = 1.5, size = 3.5, color = "grey40") +
  scale_color_manual(values = c(
    "Barbastella barbastellus"  = "darkred",
    "Rhinolophus ferrumequinum" = "steelblue"
  )) +
  scale_fill_manual(values = c(
    "Barbastella barbastellus"  = "darkred",
    "Rhinolophus ferrumequinum" = "steelblue"
  )) +
  labs(title = "NMDS - Environmental niche of two bat species",
       x = "NMDS1", y = "NMDS2") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(face = "bold")                           
  )
print(p1)
ggsave(p1, filename = "data/results_plots/NMDS.png", width = 8, height = 6, dpi = 300)

# graph interpretation:
# there is a lot of dots for the two species that are pointing the arrow prop_forest_2024 
# => which confirms that these two bat species are mostly forestery species  
# Barbastellus has more dot in high elevation than Rhinolophus
# Rhinolophus has more dots in mean maximal temperature
# but overall the two niches seem to overlap a lot, which indicates us that other variables could be used to discriminate them more