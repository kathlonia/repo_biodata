# ============================================================
# LEARNING GGPLOT2 step by step - BDA 2026
# ============================================================
#
# Goal of this script:
# - learn how to build figures progressively with ggplot2
# - understand the logic of aesthetics, geometries, themes, facets, scales
# - produce increasingly polished plots
# - discover simple PCA visualization
# - finish with a few interactive plots using ggplotly
#
# Important teaching idea:
# ggplot2 works by LAYERS.
# You usually start with:
#   ggplot(data = ..., aes(...))
# and then add visual layers with +
#
# In this script, we use a biodiversity-inspired toy dataset.
# It is artificial, but larger and more realistic than a tiny example.
#
# ============================================================
# 0) PACKAGES
# ============================================================

# Install packages if needed (run once only)
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr")
install.packages("plotly")

library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

# To make the random dataset reproducible:
set.seed(123)


# ============================================================
# 1) CREATE A LARGER TOY BIODIVERSITY DATASET
# ============================================================
# Instead of creating only a few rows by hand, we now generate
# a larger dataset inspired by plant biodiversity sampling.
#
# Variables:
# - site: sampling site
# - habitat: Forest / Meadow / Wetland
# - species: plant species name
# - replicate: repeated quadrat / sampling replicate
# - abundance: number of individuals observed
# - richness: local species richness
# - elevation_m: elevation in meters
# - soil_moisture: percentage of soil moisture
# - flower_cover: percentage of flowering cover
# - pollinator_visits: number of pollinator visits observed
#
# Each row = one observation of one species in one replicate at one site.

sites <- data.frame(
  site = c("Site_A","Site_B","Site_C","Site_D","Site_E","Site_F","Site_G","Site_H","Site_I"),
  habitat = c("Forest","Forest","Forest",
              "Meadow","Meadow","Meadow",
              "Wetland","Wetland","Wetland"),
  elevation_m = c(920, 870, 790,
                  620, 560, 510,
                  460, 430, 390),
  soil_moisture = c(48, 43, 39,
                    30, 26, 34,
                    78, 72, 81),
  flower_cover = c(22, 28, 18,
                   62, 58, 67,
                   20, 16, 24)
)

species_info <- data.frame(
  species = c(
    "Achillea millefolium",
    "Bellis perennis",
    "Plantago lanceolata",
    "Trifolium pratense",
    "Leucanthemum vulgare",
    "Ranunculus acris",
    "Lotus corniculatus",
    "Carex flacca"
  ),
  preferred_habitat = c(
    "Meadow", "Meadow", "Meadow", "Meadow",
    "Meadow", "Wetland", "Meadow", "Wetland"
  ),
  base_abundance = c(16, 12, 19, 18, 14, 10, 15, 11)
)

# Create all combinations of site x species x replicate
# We use more replicates than before so we have more points.
biodiv <- expand.grid(
  site = sites$site,
  species = species_info$species,
  replicate = 1:8,
  stringsAsFactors = FALSE
) %>%
  left_join(sites, by = "site") %>%
  left_join(species_info, by = "species")

# Generate abundance with some ecological structure:
# - species do better in their preferred habitat
# - meadow tends to have higher flower cover and pollinators
# - wetland tends to have higher soil moisture
# - we add random variation to keep it realistic for teaching
biodiv <- biodiv %>%
  mutate(
    habitat_bonus = ifelse(habitat == preferred_habitat, 8, 0),
    habitat_effect = case_when(
      habitat == "Forest"  ~ -2,
      habitat == "Meadow"  ~  5,
      habitat == "Wetland" ~ -1
    ),
    random_noise = rnorm(n(), mean = 0, sd = 4),

    abundance = round(pmax(
      0,
      base_abundance + habitat_bonus + habitat_effect + random_noise
    )),

    richness = round(pmax(
      5,
      case_when(
        habitat == "Forest"  ~ rnorm(n(), mean = 18, sd = 2),
        habitat == "Meadow"  ~ rnorm(n(), mean = 28, sd = 3),
        habitat == "Wetland" ~ rnorm(n(), mean = 16, sd = 2)
      )
    )),

    soil_moisture = pmin(100, pmax(
      5,
      soil_moisture + rnorm(n(), mean = 0, sd = 4)
    )),

    flower_cover = pmin(100, pmax(
      0,
      flower_cover + rnorm(n(), mean = 0, sd = 7)
    )),

    pollinator_visits = round(pmax(
      0,
      5 + 0.55 * flower_cover + 0.35 * abundance + rnorm(n(), mean = 0, sd = 5)
    ))
  ) %>%
  select(
    site, habitat, species, replicate, abundance, richness,
    elevation_m, soil_moisture, flower_cover, pollinator_visits
  )

# Look at the dataset
head(biodiv)
str(biodiv)
summary(biodiv)

# Number of rows
nrow(biodiv)


# ============================================================
# 2) A SECOND DATASET SUMMARIZED BY SITE
# ============================================================
# ggplot2 is often used on either:
# - raw data
# - summarized data
#
# Here we build a site-level summary table.

site_summary <- biodiv %>%
  group_by(site, habitat) %>%
  summarise(
    total_abundance = sum(abundance),
    mean_richness = mean(richness),
    elevation_m = mean(elevation_m),
    soil_moisture = mean(soil_moisture),
    flower_cover = mean(flower_cover),
    pollinator_visits = mean(pollinator_visits),
    .groups = "drop"
  )

site_summary


# ============================================================
# 3) THE BASIC LOGIC OF GGPLOT2
# ============================================================
# General structure:
#
# ggplot(data = my_data, aes(x = variable1, y = variable2)) +
#   geom_something()
#
# Important ideas:
# - data = the table you use
# - aes() = aesthetic mappings (x, y, color, size, shape, fill, etc.)
# - geom_*() = the type of object drawn on the plot
#
# Common geoms:
# - geom_point()     -> scatter plot
# - geom_line()      -> line plot
# - geom_col()       -> bar plot with values already calculated
# - geom_boxplot()   -> boxplot
# - geom_violin()    -> violin plot
# - geom_smooth()    -> regression / trend line


# ============================================================
# 4) FIRST VERY SIMPLE PLOT
# ============================================================
# Scatter plot: species abundance as a function of elevation

windows()
p1 <- ggplot(data = biodiv, aes(x = elevation_m, y = abundance)) +
  geom_point()

p1


# ============================================================
# 5) ADD COLOR
# ============================================================

windows()
p2 <- ggplot(data = biodiv, aes(x = elevation_m, y = abundance, color = habitat)) +
  geom_point(size = 2.5, alpha = 0.8)

p2
# here the color "habitat" is not a set of colors chosen, it's automatically chosen by ggplot, we can
# then choose the colors 

# Example of fixed color:
ggplot(data = biodiv, aes(x = elevation_m, y = abundance)) +
  geom_point(size = 2.5, color = "darkgreen")


# ============================================================
# 6) ADD SHAPE
# ============================================================

windows()
p3 <- ggplot(data = biodiv, aes(x = elevation_m, y = abundance,
                                color = habitat, shape = habitat)) +
  geom_point(size = 2.5, alpha = 0.8)

p3


# ============================================================
# 7) ADD TITLES AND LABELS
# ============================================================

windows()
p4 <- ggplot(data = biodiv, aes(x = elevation_m, y = abundance, color = habitat)) +
  geom_point(size = 2.5, alpha = 0.8) +
  labs(
    title = "Plant abundance along an elevation gradient",
    subtitle = "Larger toy biodiversity dataset",
    x = "Elevation (m)",
    y = "Observed abundance",
    color = "Habitat"
  )

p4


# ============================================================
# 8) IMPROVE THE THEME
# ============================================================

windows()
p5 <- ggplot(data = biodiv, aes(x = elevation_m, y = abundance, color = habitat)) +
  geom_point(size = 2.5, alpha = 0.8) +
  labs(
    title = "Plant abundance along an elevation gradient",
    subtitle = "Larger toy biodiversity dataset",
    x = "Elevation (m)",
    y = "Observed abundance"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey30"),
    legend.position = "right"
  )

p5


# ============================================================
# 9) CUSTOM COLORS
# ============================================================

windows()
p6 <- ggplot(data = biodiv, aes(x = elevation_m, y = abundance, color = habitat)) +
  geom_point(size = 3, alpha = 0.85) +
  scale_color_manual(values = c(
    "Forest" = "forestgreen",
    "Meadow" = "goldenrod3",
    "Wetland" = "steelblue3"
  )) +
  labs(
    title = "Plant abundance by habitat",
    x = "Elevation (m)",
    y = "Observed abundance"
  ) +
  theme_classic(base_size = 13)

p6


# ============================================================
# 10) BAR PLOT WITH SUMMARIZED DATA
# ============================================================

windows()
p7 <- ggplot(data = site_summary, aes(x = site, y = total_abundance, fill = habitat)) +
  geom_col() +
  labs(
    title = "Total plant abundance per site",
    x = "Site",
    y = "Total abundance"
  ) +
  theme_bw(base_size = 13)

p7

# here we add geom_col to get columns and not just points!

# ============================================================
# 11) BETTER BAR PLOT
# ============================================================

windows()
p8 <- ggplot(data = site_summary, aes(x = site, y = total_abundance, fill = habitat)) +
  geom_col(width = 0.7, color = "black") +
  scale_fill_manual(values = c(
    "Forest" = "darkolivegreen3",
    "Meadow" = "khaki3",
    "Wetland" = "cadetblue3"
  )) +
  labs(
    title = "Total abundance per site",
    subtitle = "Summarized biodiversity data",
    x = "Sampling site",
    y = "Total abundance",
    fill = "Habitat"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p8


# ============================================================
# 12) BOXPLOT
# ============================================================

windows()
p9 <- ggplot(data = biodiv, aes(x = habitat, y = abundance, fill = habitat)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Distribution of abundance among habitats",
    x = "Habitat",
    y = "Abundance"
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

p9

# a boxplot provide not engouh informations for the species distribution... BAD for the project
# ============================================================
# 13) BOXPLOT + JITTERED POINTS
# ============================================================

windows()
p10 <- ggplot(data = biodiv, aes(x = habitat, y = abundance, fill = habitat)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 1.8, alpha = 0.7) +
  labs(
    title = "Abundance by habitat",
    subtitle = "Boxplot + raw observations",
    x = "Habitat",
    y = "Abundance"
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "none")

p10
# jitter increases the transparency of the data collected, about the distribution
# we can also make subgroups in the jitter, color differently... stack the informations

# ============================================================
# 14) DENSITY PLOT
# ============================================================
# A density plot is a smooth version of a histogram.
# It helps visualize the shape of a distribution.

windows()
p11 <- ggplot(data = biodiv, aes(x = abundance, fill = habitat)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Density distribution of abundance",
    subtitle = "Comparison among habitats",
    x = "Abundance",
    y = "Density",
    fill = "Habitat"
  ) +
  theme_bw(base_size = 13)

p11


# ============================================================
# 15) DENSITY PLOT WITH LINES ONLY
# ============================================================

windows()
p12 <- ggplot(data = biodiv, aes(x = abundance, color = habitat)) +
  geom_density(linewidth = 1.1) +
  labs(
    title = "Density curves of abundance",
    x = "Abundance",
    y = "Density",
    color = "Habitat"
  ) +
  theme_classic(base_size = 13)

p12


# ============================================================
# 16) VIOLIN PLOT + BOXPLOT
# ============================================================

windows()
p13 <- ggplot(data = biodiv, aes(x = habitat, y = abundance, fill = habitat)) +
  geom_violin(alpha = 0.5, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8) +
  geom_jitter(width = 0.08, size = 1.4, alpha = 0.6) +
  labs(
    title = "Distribution of abundance among habitats",
    subtitle = "Violin plot + boxplot + raw points",
    x = "Habitat",
    y = "Abundance"
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

p13

# but it's a bit too complicated to interpret => we have to find a compromise between raw data and vulgarisation of them

# ============================================================
# 17) SCATTER PLOT WITH POINT SIZE
# ============================================================

windows()
p14 <- ggplot(data = site_summary,
              aes(x = flower_cover, y = pollinator_visits,
                  size = mean_richness, color = habitat)) +
  geom_point(alpha = 0.85) +
  labs(
    title = "Pollinator visits increase with flower cover",
    subtitle = "Point size represents mean richness",
    x = "Flower cover (%)",
    y = "Pollinator visits",
    size = "Mean richness",
    color = "Habitat"
  ) +
  theme_minimal(base_size = 13)

p14
# the size is easy to read, better than the color to interpret the variation

# ============================================================
# 18) ADD A REGRESSION / TREND LINE
# ============================================================

windows()
p15 <- ggplot(data = site_summary,
              aes(x = flower_cover, y = pollinator_visits)) +
  geom_point(size = 3, color = "darkorange3") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Relationship between flower cover and pollinator visits",
    x = "Flower cover (%)",
    y = "Pollinator visits"
  ) +
  theme_bw(base_size = 13)

p15


# ============================================================
# 19) COLOR THE TREND BY GROUP
# ============================================================

windows()
p16 <- ggplot(data = site_summary,
              aes(x = flower_cover, y = pollinator_visits, color = habitat)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Flower cover vs pollinator visits by habitat",
    x = "Flower cover (%)",
    y = "Pollinator visits"
  ) +
  theme_classic(base_size = 13)

p16


# ============================================================
# 20) FACETING
# ============================================================

windows()
p17 <- ggplot(data = biodiv, aes(x = elevation_m, y = abundance)) +
  geom_point(size = 2, color = "darkgreen", alpha = 0.75) +
  facet_wrap(~ habitat) +
  labs(
    title = "Abundance along elevation in each habitat",
    x = "Elevation (m)",
    y = "Abundance"
  ) +
  theme_bw(base_size = 13)

p17


# ============================================================
# 21) FACETING WITH SPECIES
# ============================================================

windows()
p18 <- ggplot(data = biodiv, aes(x = species, y = abundance, fill = species)) +
  geom_boxplot() +
  facet_wrap(~ habitat, scales = "free_y") +  # for trend comparison, important de rajouter les axes!
  labs(
    title = "Species abundance distribution in each habitat",
    x = "Species",
    y = "Abundance"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

p18


# ============================================================
# 22) REORDER CATEGORIES
# ============================================================

windows()
p19 <- ggplot(data = site_summary,
              aes(x = reorder(site, total_abundance), y = total_abundance, fill = habitat)) +
  geom_col(color = "black") +
  coord_flip() +
  labs(
    title = "Sites ordered by total abundance",
    x = "Site",
    y = "Total abundance"
  ) +
  theme_bw(base_size = 13)

p19


# ============================================================
# 23) BUILDING A NICER PUBLICATION-STYLE FIGURE
# ============================================================

windows()
p20 <- ggplot(data = site_summary,
              aes(x = flower_cover, y = pollinator_visits,
                  color = habitat, size = mean_richness)) +
  geom_point(alpha = 0.9) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE,
              color = "grey20", linewidth = 0.8) +
  scale_color_manual(values = c(
    "Forest" = "forestgreen",
    "Meadow" = "goldenrod2",
    "Wetland" = "deepskyblue3"
  )) +
  labs(
    title = "A polished ggplot2 figure",
    subtitle = "Flower cover, pollinator activity, habitat and richness",
    x = "Flower cover (%)",
    y = "Pollinator visits",
    color = "Habitat",
    size = "Mean richness"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey30"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

p20


# ============================================================
# 24) INTRODUCTION TO PCA
# ============================================================
# PCA = Principal Component Analysis
#
# PCA is useful when we have several numerical variables and want to:
# - reduce dimensionality
# - visualize multivariate patterns
# - see whether groups separate in multivariate space
#
# Here, we use the raw biodiversity table instead of the site summary.
# This gives us many more points, which makes ellipses more meaningful.

pca_data <- biodiv %>%
  select(
    abundance,
    richness,
    elevation_m,
    soil_moisture,
    flower_cover,
    pollinator_visits
  )

# Run PCA
pca_res <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Explore the PCA result
summary(pca_res)

# Scores = coordinates of observations in PCA space
pca_scores <- as.data.frame(pca_res$x)

# Add metadata back
pca_scores <- bind_cols(
  biodiv %>% select(site, habitat, species, replicate),
  pca_scores
)

head(pca_scores)

# Percentage of variance explained
var_explained <- (pca_res$sdev^2 / sum(pca_res$sdev^2)) * 100
pc1_lab <- paste0("PC1 (", round(var_explained[1], 1), "%)")
pc2_lab <- paste0("PC2 (", round(var_explained[2], 1), "%)")


# ============================================================
# 25) BASIC PCA PLOT
# ============================================================

windows()
p21 <- ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(size = 2.2, alpha = 0.7) +
  labs(
    title = "Basic PCA plot",
    subtitle = "Each point is one observation",
    x = pc1_lab,
    y = pc2_lab
  ) +
  theme_bw(base_size = 13)

p21


# ============================================================
# 26) PCA WITH COLORS
# ============================================================

windows()
p22 <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = habitat)) +
  geom_point(size = 2.4, alpha = 0.75) +
  scale_color_manual(values = c(
    "Forest" = "forestgreen",
    "Meadow" = "goldenrod3",
    "Wetland" = "steelblue3"
  )) +
  labs(
    title = "PCA colored by habitat",
    subtitle = "Raw observations projected in multivariate space",
    x = pc1_lab,
    y = pc2_lab,
    color = "Habitat"
  ) +
  theme_bw(base_size = 13)

p22


# ============================================================
# 27) PCA WITH ELLIPSES
# ============================================================

windows()
p23 <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = habitat, fill = habitat)) +
  stat_ellipse(geom = "polygon", alpha = 0.18, color = NA) +
  stat_ellipse(linewidth = 1) +
  geom_point(size = 2.2, alpha = 0.7) +
  scale_color_manual(values = c(
    "Forest" = "forestgreen",
    "Meadow" = "goldenrod3",
    "Wetland" = "steelblue3"
  )) +
  scale_fill_manual(values = c(
    "Forest" = "forestgreen",
    "Meadow" = "goldenrod3",
    "Wetland" = "steelblue3"
  )) +
  labs(
    title = "PCA with habitat ellipses",
    subtitle = "Group structure in multivariate space",
    x = pc1_lab,
    y = pc2_lab,
    color = "Habitat",
    fill = "Habitat"
  ) +
  theme_bw(base_size = 13)

p23


# ============================================================
# 28) SAVING A FIGURE
# ============================================================
# ggsave() saves the LAST plot shown, or a plot object if specified.
#
# Example:
# ggsave("my_figure.png", plot = p23, width = 8, height = 5, dpi = 300)
#
# Useful formats:
# - PNG for slides and screens
# - PDF for vector quality
# - TIFF sometimes for journals


# ============================================================
# 29) COMMON BEGINNER MISTAKES
# ============================================================
# 1. Forgetting the + at the end of a line
# 2. Writing aes() outside ggplot() or geom() when not appropriate
# 3. Using geom_bar() when you should use geom_col()
# 4. Confusing fixed color with mapped color
# 5. Using a summarized table for one plot and raw data for another without noticing
# 6. Forgetting to label axes and legends
#
# Compare these two examples:

# Fixed color for all points
# ggplot(biodiv, aes(elevation_m, abundance)) +
#   geom_point(color = "red")

# Color mapped to a variable
# ggplot(biodiv, aes(elevation_m, abundance, color = habitat)) +
#   geom_point()


# ============================================================
# 30) INTRODUCTION TO GGPLOTLY
# ============================================================
# plotly::ggplotly() converts a ggplot into an interactive plot.
# This means you can:
# - hover over points
# - zoom in/out
# - move around the figure
# - hide/show groups by clicking the legend
#
# Not every ggplot feature converts perfectly, but for many plots it works well.


# ============================================================
# 31) FIRST INTERACTIVE PLOT
# ============================================================

windows()
p24 <- ggplot(data = site_summary,
              aes(x = flower_cover, y = pollinator_visits,
                  color = habitat,
                  text = paste(
                    "Site:", site,
                    "<br>Habitat:", habitat,
                    "<br>Mean richness:", round(mean_richness, 1),
                    "<br>Elevation:", round(elevation_m, 1), "m"
                  ))) +
  geom_point(size = 4, alpha = 0.85) +
  labs(
    title = "Interactive biodiversity scatter plot",
    x = "Flower cover (%)",
    y = "Pollinator visits"
  ) +
  theme_bw(base_size = 13)

p24

interactive_p24 <- ggplotly(p24, tooltip = "text")
interactive_p24


# ============================================================
# 32) SECOND INTERACTIVE PLOT
# ============================================================

windows()
p25 <- ggplot(data = site_summary,
              aes(x = site, y = total_abundance, fill = habitat,
                  text = paste(
                    "Site:", site,
                    "<br>Habitat:", habitat,
                    "<br>Total abundance:", total_abundance,
                    "<br>Mean richness:", round(mean_richness, 1)
                  ))) +
  geom_col(color = "black") +
  labs(
    title = "Interactive bar plot",
    x = "Site",
    y = "Total abundance"
  ) +
  theme_minimal(base_size = 13)

p25

interactive_p25 <- ggplotly(p25, tooltip = "text")
interactive_p25


# ============================================================
# 33) INTERACTIVE PCA
# ============================================================
# PCA plots also work very well in plotly.

windows()
p26 <- ggplot(
  pca_scores,
  aes(
    x = PC1,
    y = PC2,
    color = habitat,
    text = paste(
      "Site:", site,
      "<br>Species:", species,
      "<br>Replicate:", replicate,
      "<br>PC1:", round(PC1, 2),
      "<br>PC2:", round(PC2, 2)
    )
  )
) +
  geom_point(size = 2.5, alpha = 0.75) +
  labs(
    title = "Interactive PCA plot",
    x = pc1_lab,
    y = pc2_lab
  ) +
  theme_bw(base_size = 13)

p26

interactive_p26 <- ggplotly(p26, tooltip = "text")
interactive_p26


# ============================================================
# KEY TAKE-HOME MESSAGES
# ============================================================
# - ggplot2 builds plots layer by layer
# - aes() maps variables to visual properties
# - geom_*() defines what is drawn
# - themes improve readability and style
# - summarized data and raw data are both useful, depending on the question
# - a good figure should be readable, informative, and visually clean
# - ggplotly can make some ggplot figures interactive very easily
# - ellipses help summarize group structure 
#
# Once you are comfortable with this script, a very good next step is to learn:
# - facet_grid()
# - position = "dodge" / "stack"
# - scale_*_manual()
# - annotations with annotate() or geom_text()
# ============================================================