# ============================================================
#  COMBINE THE PLOTS WITH COWPLOT
#
#  ggdraw() creates an empty 1 × 1 canvas using relative units.
#  draw_plot(plot, x, y, width, height) places each plot on this canvas.
#
#  x, y    = bottom-left corner of the plot (0 = left/bottom edge, 1 = right/top edge)
#  width   = plot width as a proportion of the full figure
#  height  = plot height as a proportion of the full figure
#
#  Layout:
#    ┌──────────────────────────────────────────┐  y = 0.60 → 1.00
#    │           graphA  (prediction map)                  │
#    ├────────────────────┬─────────────────────┤  y = 0.25 → 0.60
#    │   graphB  (radar)  │  graphC  (boxplot)  │
#    ├────────────────────┴─────────────────────┤  y = 0.00 → 0.25
#    │           graphD  (barplot)              │
#    └──────────────────────────────────────────┘
# ============================================================

# first, select the graphs that I want to add to my final panel
# graphA: predicted probability for the two species => we have in one map the real distribution of the species, and the prediction
# graphB: radar plot => overall view of the environmental profil 
# graphC: boxplot and violin plot => distribution of the species according to numeric variables (elevation, precipitation, temperature and forest cover proportion)
# graphD: barplot => distribution of the species according to categorical variables (Landcver + Landtypes)


# I will place again the scripts for the four graphs, to have them here 

############################################################
#                     graph A
############################################################
# graph A and C are not directly produced with ggplot, so we have to convert them

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
    subtitle = "Prediction on the environmental grid over France",
    theme = theme(
     plot.title = element_text(
     size = 16,
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5
      )
    )
  ) +
  plot_layout(guides = "collect")

windows()
print(graphA)
ggsave(graphA, filename = "data/results_plots/graph_A.png", width = 10, height = 6, dpi = 300)

library(patchwork)
graphA_grob <- patchworkGrob(graphA)

############################################################
#                     graph B
############################################################

library(gridGraphics)
library(grid)

graphB_grob <- as_grob(~ {
  par(
  mar = c(2,2,3,2),
  xpd = FALSE
)
  
  radarchart(radar_ready,                   
             cglty = 1,
             cglcol = "gray80",
             pcol = cols,                    
             plwd = 2,
             plty = 1,
             pfcol = areas,                  
             vlcex = 0.95,
             caxislabels = seq(0,1,0.25),
             vlabels = c("Elevation", 
                         "Temperature", 
                         "Precipitation", 
                         "Forest cover"))
  
  title(main = "Environmental niche profile of two bat species",
        cex.main = 1.4, font.main = 2, line = 1)

par(xpd = NA)

legend(
  x = -1.5,
  y = -1,
  legend = c(
    "Barbastella barbastellus",
    "Rhinolophus ferrumequinum"
  ),
  bty = "n",
  pch = 20,
  col = cols,
  cex = 0.8
)

par(xpd = FALSE)
})

windows()
grid.newpage()
grid.draw(graphB_grob)

############################################################
#                     graph C
############################################################
# 1B) prepare the data for boxplot and violin plot
matrix_full_boxplot <- matrix_full %>%
    dplyr::select(species, elevation, tmax_mean_c, prec_annual_mm, prop_forest_2024)
    
# 2B) convert into a long format for ggplot
boxplot_long <- melt(matrix_full_boxplot, id.vars = "species", 
                  variable.name = "variable", 
                  value.name = "value")
#View(boxplot_long)

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

############################################################
#                     graph D
############################################################
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
library(patchwork)

graphD <- p_landform + p_landcover +
  plot_annotation(
    title = "Landscape composition associated with each species",
    theme = theme(
      plot.title = element_text(
        size = 16,
        face = "bold",
        hjust = 0.5
      )
    )
  )
ggsave(graphD, filename = "data/results_plots/graph_D.png", width = 10, height = 6, dpi = 300)

# ---- Final figure ----
# x, y   = bottom-left corner of the plot (0 = left/bottom edge, 1 = right/top edge)
# width  = plot width as a proportion of the full figure
# height = plot height as a proportion of the full figure

library(cowplot)
figure_finale <- ggdraw() +

  draw_plot(graphA_grob, x = 0.00, y = 0.68, width = 0.70, height = 0.32) +
  draw_plot(graphB_grob, x = 0.69, y = 0.68, width = 0.31, height = 0.32) +
  draw_plot(graphC, x = 0.00, y = 0.34, width = 1.00, height = 0.34) +
  draw_plot(graphD, x = 0.00, y = 0.00, width = 1.00, height = 0.34) +


  draw_line(x = c(0,1), y = c(0.68,0.68), colour = "grey70", linewidth = 0.5) +
  draw_line(x = c(0,1), y = c(0.34,0.34), colour = "grey70", linewidth = 0.5) +
  draw_line(x = c(0.70,0.70), y = c(0.68,1), colour = "grey70", linewidth = 0.5) +

  draw_label("A", x = 0.01, y = 0.995, fontface = "bold", size = 16) +
  draw_label("B", x = 0.73, y = 0.995, fontface = "bold", size = 16) +
  draw_label("C", x = 0.01, y = 0.675, fontface = "bold", size = 16) +
  draw_label("D", x = 0.01, y = 0.335, fontface = "bold", size = 16)

windows(width = 18, height = 22)
print(figure_finale)

# ============================================================
#  EXPORT
# ============================================================

ggsave("data/Figures_bats_species_France.png", figure_finale,
       width = 24, height = 20, dpi = 400, bg = "white")

ggsave("data/Figures_bats_species_France.pdf", figure_finale,
       width = 18, height = 20, dpi = 300, bg = "white")
