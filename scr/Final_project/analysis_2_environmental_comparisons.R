# ============================================================================================
# -------------------------------------- NUMERIC VARIABLES -----------------------------------
# ============================================================================================

# ============================================================================================
# to compare our environmental variables, we will first do a heatmap, to see the correlation 
# ============================================================================================

install.packages(c("fmsb", "cowplot", "gridGraphics", "reshape", "patchwork"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggnewscale)
library(fmsb)
library(cowplot)
library(gridGraphics)
library(reshape)
library(ggplot2)

# 1A) calculate the correlation matrix
View(matrix_full)
matrix_full_heatmap <- matrix_full %>%
    dplyr::select(elevation, tmax_mean_c, prec_annual_mm, prop_forest_2024)
#View(matrix_full_heatmap)
cor_matrix <- cor(matrix_full_heatmap)

# 2A) convert into a long format for ggplot
cor_long <- melt(cor_matrix)
colnames(cor_long) <- c("Var1", "Var2", "correlation")
View(cor_long)

# 3A) plot the correlation matrix into a heatmap
heatmap <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = correlation)) +
  geom_tile(color= "black") +
  geom_text(aes(label = round(correlation, 2)), color = "black", size = 4) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  coord_fixed() +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20)) +
  labs(title = "Correlation heatmap of environmental variables",
       x = "", y = "") +
  theme(
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(hjust = 1)
  )

print(heatmap)
ggsave(heatmap, filename = "data/results_plots/heatmap.png", width = 6, height = 5, dpi = 300)

# interpretation of the heatmap:
# there is a positive correlation between elevation and prop of forest (0.39): 
# -- that can be a normal result, since in low altitude there is maybe more croplands than in high altitude, where the forest are predominant
# there is a positive correlation, a bit less strong (0.28), between elevation and annual precipitation :
# -- the higher we go, the more it rains... ok
# there is a strong negative correlation between elevation and maximal temperature (-0.67):
# -- that is perfectly normal and expected: the higher we go, the colder it is 
# there is a negative correlation between annual precipitation and maximal temperature in mean (-0.28):
# --- the more it rains, the less it is hot... ok
# there is a negative correlation between proportion of forest and maximal temperature in mean (-0.16):
# --- most of the forests where the species is found are (according to the theory) decidous forests, and not mediteraneans forest: they can not support strong temperature
# there is a positive correlation between proportion of forest and annual precipitation (0.27): 
# --- this kind of forests are also senstive to rain and they prefer area with more rains
# ("Annually, temperate deciduous forests experience approximately 750 to 1,500 millimeters of precipitation" source: "Temperate Deciduous Forest: Mission: Biomes". earthobservatory.nasa.gov. 2023-10-24. Retrieved 2023-10-24.)

# ==================================================================
# our second graph will be a violin plot combined with a boxplot
# ==================================================================

# the boxplot indicates us the mean and the quartiles, but not how the data are distributed into this quartile
# a violin plot could then be interesting to discriminate our two niches, to see where the pics of observations are located in the four variables 

# 1B) prepare the data for boxplot and violin plot
matrix_full_boxplot <- matrix_full %>%
    dplyr::select(species, elevation, tmax_mean_c, prec_annual_mm, prop_forest_2024)
    
# 2B) convert into a long format for ggplot
boxplot_long <- melt(matrix_full_boxplot, id.vars = "species", 
                  variable.name = "variable", 
                  value.name = "value")
View(boxplot_long)

# 3B) plot the matrice into a violin plot and add a boxplot 
graphC <- ggplot(boxplot_long,
       aes(x = species,
           y = value,
           fill = species)) +

  geom_violin(alpha = 0.5) +

  geom_jitter(
    width = 0.1,
    alpha = 0.15,
    size = 0.5
  ) +

  geom_boxplot(
    width = 0.15,
    color = "black",
    outlier.shape = NA
  ) +

  facet_wrap(
  ~ variable,
  scales = "free_y",
  labeller = as_labeller(c(
    elevation = "Elevation (m)",
    tmax_mean_c = "Mean maximum temperature (°C)",
    prec_annual_mm = "Annual precipitation (mm)",
    prop_forest_2024 = "Forest cover proportion (%)"
  ))) +
  scale_fill_manual(
  values = c(
    "Barbastella barbastellus" = "#8f2201",
    "Rhinolophus ferrumequinum" = "#3e6bbe")) +
  labs(
    title = "Distribution of environmental variables per species"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    strip.text = element_text(
    size = 14,
    face = "bold"
    ),
    strip.background = element_rect(
    fill = "grey90",
    colour = "black"
),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
print(graphC)
ggsave(graphC, filename = "data/results_plots/graph_C.png", width = 10, height = 6, dpi = 300)

# =====================================================================================================
# our third and last plot will be a radar plot, to synthetize the niche profil for the two species 
# ======================================================================================================
# 1C) prepare the matrix for the radar plot
matrix_full_radar <- matrix_full %>%
  dplyr::select(species, elevation, tmax_mean_c, prec_annual_mm, prop_forest_2024)

radar_data <- matrix_full_radar %>%
  group_by(species) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  tibble::column_to_rownames("species")

print(radar_data)

colnames(radar_data) <- c("Elevation (m)", 
                           "Max temp (°C)", 
                           "Precipitation (mm)", 
                           "Forest cover (%)")

# 2C) normalize the data using min-max normalization to make the axes comparable
radar_norm <- as.data.frame(
  scale(radar_data)  # z-score : (x - mean) / sd
)

lim <- max(abs(radar_norm))
radar_ready <- rbind(
  rep(lim, ncol(radar_norm)),
  rep(-lim, ncol(radar_norm)),
  radar_norm
)

areas <- c(rgb(1, 0, 0, 0.15),
           rgb(0, 0, 1, 0.15)) 

cols <- c(rgb(0.8, 0, 0, 1),
          rgb(0, 0, 0.8, 1))

par(mar = c(6, 2, 4, 2))

# 3C) Plot the radar chart
windows()
radarchart(radar_ready,
           cglty = 1,
           cglcol = "gray80",
           pcol = cols,
           plwd = 2,
           plty = 1,
           pfcol = areas,
           vlcex = 0.9,
           vlabels = c("Elevation (m)", 
                       "Max temp (°C)", 
                       "Precipitation (mm)", 
                       "Forest cover (%)"))

title(main = "Environmental niche profile of two bat species",
      cex.main = 1.2, font.main = 2, line = 1)

legend("topright",
       legend = rownames(radar_data),
       bty = "n", pch = 20, col = cols,
       text.col = "grey25", pt.cex = 2)

# 4C) add the raw mean values, to help interpret the radar plot

means_table <- round(radar_data, 2)
col_headers <- c("Species", "Elevation (m)", "Max temp (°C)", "Precip. (mm)", "Forest cover (%)")

x_start <- -1.55
y_start <- -1.28        
col_widths <- c(0.72, 0.52, 0.50, 0.50, 0.58)
x_positions <- cumsum(c(x_start, col_widths))

par(xpd = NA)


for (i in seq_along(col_headers)) {
  text(x_positions[i], y_start, col_headers[i],
       font = 2, cex = 0.72, adj = 0, col = "grey20")
}
         
segments(x_positions[1], y_start - 0.06, 
         x_positions[length(x_positions) - 1], y_start - 0.06,
         col = "grey60", lwd = 0.8)

species_names <- rownames(means_table)
species_cols  <- c(rgb(0.8, 0, 0, 1), rgb(0, 0, 0.8, 1))

for (i in seq_along(species_names)) {
  y_row <- y_start - 0.13 * (i + 0.5)
  row_vals <- c(species_names[i], as.character(means_table[i, ]))
  
  for (j in seq_along(row_vals)) {
    text(x_positions[j], y_row, row_vals[j],
         cex = 0.70, adj = 0,
         col = if (j == 1) species_cols[i] else "grey30",
         font = if (j == 1) 3 else 1)
  }
}

par(xpd = FALSE)  
graphB <- recordPlot()

# Since the data were on different scales, they were normalized from 0 to 1, which leads to a small difference 
# in data being greatly overrepresented on the plot. Therefore, I included the average raw values ​​of each environmental 
# data point for each species, so that it becomes clear that the profiles are not as different as they appear. 
# The main difference that distinguishes niches between species remains altitude!

# ============================================================================================
# ------------------------------------ CATEGORICAL VARIABLES ---------------------------------
# ============================================================================================

# ============================================
# barplots with landcover and landform
# =============================================

# 1D) create the matrix for the barplot
landscape_long <- matrix_full %>%
  select(species, Landforms, Landcover) %>%
  pivot_longer(
    cols = c(Landforms, Landcover),
    names_to = "variable",
    values_to = "category"
  )


# 2D) plot the observations into two different barplot

p_landform <- ggplot(
  subset(landscape_long, variable == "Landforms"),
  aes(x = species, fill = category)
) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c(
      "Mountains"   = "#8c510a",
      "Tablelands" = "#bf812d",
      "Hills"      = "#dfc27d",
      "Plains"     = "#f6e8c3"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
  labels = c(
    "Barbastella barbastellus" = "B. barbastellus",
    "Rhinolophus ferrumequinum" = "R. ferrumequinum"
  )
) +
  labs(fill = "Landform") +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(
    size = 12,
    face = "bold"
  )
)

p_landcover <- ggplot(
  subset(landscape_long, variable == "Landcover"),
  aes(x = species, fill = category)
) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c(
      "Forest"     = "#00441b",
      "Shrubland"  = "#238b45",
      "Cropland"   = "#74c476",
      "Grassland"  = "#a1d99b",
      "Settlement" = "#bdbdbd"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(
  labels = c(
    "Barbastella barbastellus" = "B. barbastellus",
    "Rhinolophus ferrumequinum" = "R. ferrumequinum"
  )
) +
  labs(fill = "Land cover") +
    theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(
    size = 12,
    face = "bold"
  )
)

graphD <- p_landform + p_landcover +
  plot_annotation(
    title = "Landscape composition associated with each species",
    theme = theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5
      )
    )
  )
ggsave(graphD, filename = "data/results_plots/graph_D.png", width = 10, height = 6, dpi = 300)

# Barbastella was less found in hills and more found in tablelands than Rhinolophus
# Barbastella appeared more in grassland and less in forest
# Generally speaking, and as we have already seen, the two species frequent more or less the same landscapes.
# They are almost never found in cities, but rather in open landscapes (grasslands, croplands) and in forests.
# Barbastella is found slightly more often in the mountains (more tablelands), and Rhinolophus seems to hunt more in forests (~35% versus ~20%).