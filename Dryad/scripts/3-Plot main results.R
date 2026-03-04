#### PLOT FIGURES OF THE PAPER ####

#Load packages
library(terra)
library(geobr)
library(tidyterra)
library(ggspatial)
library(scales)
library(patchwork)
library(cowplot)
library(ggthemes)
library(tidyverse)
library(showtext)
library(data.table)

# Import helper functions
source("scripts//plot_results_helpers2.R")

#### FIGURE 1 - STUDY AREA ####

#Import spatial data
sc <- read_state(code_state = "SC") %>% vect() #Santa catarina map
bb_sc <- ext(sc) #Extent of Santa Catarina (to zoom in map)
sa <- vect("Vectors/South_America.gpkg") #South America
br <- read_state() %>% vect() #Brazil
bb_br <- ext(br) #Extent of Brazil

# Import inventary
inv <- fread("data/Inventario_SC/PAM_SC_04jul24.csv")

# Import vegetation from mapbiomas
# Downloaded from: https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_2024.tif
# Already masked to SC
sc_veg <- rast("Rasters/Mapbiomas_SC_2024.tif")
plot(sc_veg)

#Main map Brazil
g_map <- ggplot() +
  geom_sf(data = sa, fill = "grey77", size = 0.1, colour = "white") +
  geom_sf(data = br, fill = "grey89", size = 0.1, colour = "grey40") +
  geom_spatraster(data = sc_veg) +
  tidyterra::scale_fill_terrain_c() +
  geom_sf(data = br, fill = NA, size = 0.1, colour = "grey40") +
  coord_sf(xlim = c(bb_br[1] - 0.5, xmax=bb_br[2] - 8),
           ylim = c(bb_br[3] - 0.5, ymax=bb_br[4]),
           expand = T) +
  annotate("rect", xmin= bb_sc[1], xmax=bb_sc[2],
           ymin=bb_sc[3], ymax=bb_sc[4],
           alpha=0, fill="black", color = "black", linewidth = 0.5) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = 'aliceblue', colour = NA),
        panel.border = element_rect(colour = "black", size = 2, fill = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text = element_text(size = 7)) +
  metR::scale_x_longitude(ticks = 6) + metR::scale_y_latitude(ticks = 6) +
  annotation_scale(location = "bl", text_cex = 0.4, height = unit(0.15, "cm"))
g_map

# Zoomed map
g_zoom <-  ggplot() +
  geom_sf(data = sa, fill = "grey77", size = 0.1, colour = "white") +
  geom_sf(data = br, fill = "grey89", size = 0.1, colour = "grey40") +
  geom_spatraster(data = sc_veg) +
  tidyterra::scale_fill_terrain_c(name = "Percentage of natural vegetation cover",
                                  labels = function(x) paste0(x, "%")) +
  geom_point(data = inv, aes(x = x, y = y), size = 0.75) +
  geom_sf(data = br, fill = NA, size = 0.1, colour = "grey40") +
  coord_sf(xlim = c(bb_sc[1] - 0.5, xmax=bb_sc[2] + 0.2),
           ylim = c(bb_sc[3] - 0.5, ymax=bb_sc[4] + 0.5),
           expand = T) +
  theme(legend.position = "bottom",
        legend.title.align = 0.5,
        panel.background = element_rect(fill = 'aliceblue', colour = NA),
        panel.border = element_rect(colour = "black", size = 2, fill = NA),
        legend.margin = margin(t = -10, r = 0, b = 0, l = 0, unit = "pt"),
        legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0, unit = "pt"),
        legend.spacing.y = unit(0, "cm")) +
  metR::scale_x_longitude(ticks = 1) + metR::scale_y_latitude(ticks = 1) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5, # Centraliza o título
    barwidth = 15)) +
  annotation_scale(location = "br")
g_zoom

# Add tags
g_map <- g_map +
  labs(tag = "(A)") +
  theme(
    plot.tag.position = c(0.92, 0.95),
    plot.tag = element_text(face = "bold", size = 16)
  )

g_zoom <- g_zoom +
  labs(tag = "(B)") +
  theme(
    plot.tag.position = c(0.95, 0.97),
    plot.tag = element_text(face = "bold", size = 20)
  )
g_zoom

# Merge maps
g_sc <- g_zoom +
  inset_element(
    g_map,
    left = -0.115,
    bottom = -0.045,
    right = 0.5,
    top = 0.535
  )
g_sc
# Save
dir.create("Figures")
ggsave("Figures/Figure_1.png", g_sc,
       units = "in", width = 7, height = 6,
       dpi = 600, scale = 1.1)

#### FIGURE 3 - CORRELATION BETWEEN OBSERVED AND PREDICTED RICHNESS ####
#### Import results
df <- fread("data/ModelsxInventary/Results.gz")

#Convert type of model and resolution to factor
unique(df$Type) %>% dput()
df$Type <- factor(x = df$Type,
                  levels = c("sesam_only", "only_threshold_min",
                             "only_threshold10", "thresholmin_and_sesam",
                             "threshold10_and_sesam"),
                  labels = c("Ranking", "Minimum threshold", "10% threshold",
                             "Minimum thr. and Ranking", "10% thr. and Ranking"))
#Resolution
unique(df$Resolution)
df$Resolution <- factor(x = df$Resolution,
                        levels = c("2.5arc_min", "5arc_min", "10arc_min", "30arc_min",
                                   "60arc_min"),
                        labels = c("2.5 arc-min", "5 arc-min", "10 arc-min", "30 arc-min",
                                   "60 arc-min"))

#Subset data
df_both <- df %>% filter(Type %in% c("10% threshold", "10% thr. and Ranking"))

# Rename assembly methods
df_both$Type <- factor(df_both$Type,
                       levels = c("10% threshold", "10% thr. and Ranking"),
                       labels = c("Only Stacked", "Stacked and Ranked"))

# Plot
g_cor <- ggplot(data = df_both, aes(x = Rich_obs, y = Rich_pred, group = Type,
                                    fill = Type)) +
  geom_point(alpha = 0.25, aes(col = Type)) +
  geom_smooth(method = "lm", aes(col = Type)) +
  scale_color_manual(values = c("#CC79A7", "#009E73"), name = "Assembly") +
  scale_fill_manual(values = c("#CC79A7", "#009E73"), name = "Assembly") +
  geom_abline(intercept = 0, slope = 1, colour = "red",
              linetype = "dashed", size = 1.2) +
  xlab("Observed richness") +
  ylab("Predicted richness") +
  scale_y_continuous(limits = c(0, 620), breaks = seq(0, 620, by = 100)) +
  #expand_limits(x = 0, y=0) +
  ggpubr::theme_pubclean() +
  theme(axis.text=element_text(size=12), #Change size of numbers in axis
        axis.title=element_text(size=14,face="bold"),   #Change size of text in axis
        legend.position = "bottom",
        strip.text = element_text(size = 15, face = "bold", color = "black")) +
  facet_wrap(.~Resolution, scales = "free", nrow = 2)
g_cor
ggsave("Figures/Figure_3.png", g_cor,
       units = "in", width = 7, height = 5,
       dpi = 600, scale = 1.5)

#### FIGURE 4 - DISSIMILARITY MAPS ####

#### Import results
df <- fread("data/ModelsxInventary/Results.gz")

# Convert type of model and resolution to factor
unique(df$Type) %>% dput()
df$Type <- factor(x = df$Type,
                  levels = c("sesam_only", "only_threshold_min",
                             "only_threshold10", "thresholmin_and_sesam",
                             "threshold10_and_sesam"),
                  labels = c("Ranking", "Minimum threshold", "10% threshold",
                             "Minimum thr. and Ranking", "10% thr. and Ranking"))
#Resolution
unique(df$Resolution)
df$Resolution <- factor(x = df$Resolution,
                        levels = c("2.5arc_min", "5arc_min", "10arc_min", "30arc_min",
                                   "60arc_min"),
                        labels = c("2.5 arc-min", "5 arc-min", "10 arc-min", "30 arc-min",
                                   "60 arc-min"))

#Subset data
df_both <- df %>% filter(Type %in% c("10% threshold", "10% thr. and Ranking"))

# Rename assembly methods
df_both$Type <- factor(df_both$Type,
                       levels = c("10% threshold", "10% thr. and Ranking"),
                       labels = c("Only Stacked", "Stacked and Ranked"))

# Import raster base
base <- rast("Rasters/base.tiff")

# Rasterize data
dr <- get_rasters(data = df_both, to_rast <- c("BetaDiv_jac"))
dr_stacked <- rast(dr$`Only Stacked`)
dr_ranked <- rast(dr$`Stacked and Ranked`)
# Only ranked
g_stacked <- lapply(names(dr_stacked), function(x){
  r <- dr_stacked[[x]]

  # #Get limits
  # limits_r <- minmax(r)
  # # Round
  # limits_r <- round(limits_r, digits = 2)
  #
  #
  # #Get breaks
  # breaks_r <- seq(limits_r[1], limits_r[2], length.out = 6)
  # #round
  # breaks_r <- round(breaks_r, digits = 2)
  # Set limits manually
  if(x %in% c("2.5 arc-min", "5 arc-min", "10 arc-min")){
    limits_r <- c(0.7, 1)
    breaks_r <- c(0.7, 0.8, 0.9, 1)
  }

  if(x %in% c("30 arc-min")){
    limits_r <- c(0.5, 1)
    breaks_r <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1)
  }

  if(x %in% c("60 arc-min")){
    limits_r <- c(0.3, 1)
    breaks_r <- c(0.3, 0.5, 0.7, 0.9, 1)
  }
  # Plot
  ggplot() +
    geom_sf(data = sa, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = br, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = sc, fill = "snow1", colour = "black") +
    geom_spatraster(data = r, na.rm = TRUE) +
    geom_sf(data = sc, fill = NA, colour = "black") +
    scale_fill_whitebox_b(palette = "muted", name = "Dissimilarity",
                          limits = limits_r, breaks = breaks_r) +
    coord_sf(xlim = c(bb_sc[1], xmax=bb_sc[2]),
             ylim = c(bb_sc[3], ymax=bb_sc[4]),
             expand = T) +
    #ylab(type) +
    # metR::scale_x_longitude(ticks = 2) +
    # metR::scale_y_latitude(ticks = 2) +
    ggpubr::theme_pubclean() +
    #ggtitle(gsub("\n", " ", type_i)) +
    theme(panel.background = element_rect(fill = 'aliceblue'),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.34, 0.15), legend.direction = "horizontal",
          legend.key.height =unit(0.2, "cm"),
          legend.key.width = unit(0.9, "cm"),
          legend.background = element_rect(fill = "transparent", size = 0.5,
                                           colour = "transparent"),
          # legend.background = element_rect(fill = "white", size = 0.5, alpha = 0,
          #                                  colour = "black"),
          legend.title.position = "top",
          #legend.margin = margin(0, 0, 0, -0.32, "cm"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "bold"),
          plot.margin = margin(t = 0,  # Top margin
                               r = 0,  # Right margin
                               b = 0,  # Bottom margin
                               l = -0.5),
          #axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    facet_grid(paste(x) ~ ., switch = "y")
})
# Stacked and ranked
g_ranked <- lapply(names(dr_ranked), function(x){
  r <- dr_ranked[[x]]

  # #Get limits
  # limits_r <- minmax(r)
  # # Round
  # limits_r <- round(limits_r, digits = 2)
  #
  # #Get breaks
  # breaks_r <- seq(limits_r[1], limits_r[2], length.out = 6)
  # #round
  # breaks_r <- round(breaks_r, digits = 2)

  # Set limits and breaks manually
  if(x %in% c("2.5 arc-min", "5 arc-min", "10 arc-min")){
    limits_r <- c(0.7, 1)
    breaks_r <- c(0.7, 0.8, 0.9, 1)
  }

  if(x %in% c("30 arc-min")){
    limits_r <- c(0.5, 1)
    breaks_r <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1)
  }

  if(x %in% c("60 arc-min")){
    limits_r <- c(0.3, 1)
    breaks_r <- c(0.3, 0.5, 0.7, 0.9, 1)
  }


  # Plot
  ggplot() +
    geom_sf(data = sa, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = br, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = sc, fill = "snow1", colour = "black") +
    geom_spatraster(data = r, na.rm = TRUE) +
    geom_sf(data = sc, fill = NA, colour = "black") +
    scale_fill_whitebox_b(palette = "muted", name = "Dissimilarity",
                          limits = limits_r, breaks = breaks_r) +
    coord_sf(xlim = c(bb_sc[1], xmax=bb_sc[2]),
             ylim = c(bb_sc[3], ymax=bb_sc[4]),
             expand = T) +
    #ylab(type) +
    # metR::scale_x_longitude(ticks = 2) +
    # metR::scale_y_latitude(ticks = 2) +
    ggpubr::theme_pubclean() +
    #ggtitle(gsub("\n", " ", type_i)) +
    theme(panel.background = element_rect(fill = 'aliceblue'),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.34, 0.15),
          legend.direction = "horizontal",
          legend.key.height =unit(0.2, "cm"),
          legend.key.width = unit(0.9, "cm"),
          legend.background = element_rect(fill = "transparent", size = 0.5,
                                           colour = "transparent"),
          # legend.background = element_rect(fill = "white", size = 0.5, alpha = 0,
          #                                  colour = "black"),
          legend.title.position = "top",
          #legend.margin = margin(0, 0, 0, -0.32, "cm"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "bold"),
          plot.margin = margin(t = 0,  # Top margin
                               r = 0,  # Right margin
                               b = 0,  # Bottom margin
                               l = -0.5),
          #axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
})
# Arrange plots
g_sr <- ((g_stacked[[1]] + facet_grid("2.5 arc-min" ~ "Only Stacked",
                                      switch = "y")) +
           (g_ranked[[1]] + facet_grid(. ~ "Stacked and Ranked"))) /
  (g_stacked[[2]] + g_ranked[[2]]) /
  (g_stacked[[3]] + g_ranked[[3]]) /
  (g_stacked[[4]] + g_ranked[[4]]) /
  (g_stacked[[5]] + g_ranked[[5]]) &
  theme(strip.text = element_text(size = 18, face = "bold"))

ggsave("Figures/Figure_4.png", g_sr,
       units = "in", width = 6.5, height = 10,
       dpi = 600, scale = 1.25)



#### FIGURE 5 - DISSIMILARITY ####
#Dissimilarity
df.beta <- df %>% group_by(Type, Resolution) %>%
  do(w = wilcox.test(.$BetaDiv_sor, conf.int = TRUE, conf.level = 0.95)) %>%
  summarise(Type, Resolution, Value = w$estimate,
            low.ci = w$conf.int[1], upper.ci = w$conf.int[2],
            Metric = "Dissimilarity")
#Turnover
df.turn <- df %>% group_by(Type, Resolution) %>%
  do(w = wilcox.test(.$Turnover_sor, conf.int = TRUE, conf.level = 0.95)) %>%
  summarise(Type, Resolution, Value = w$estimate,
            low.ci = w$conf.int[1], upper.ci = w$conf.int[2],
            Metric = "Turnover")
#Nestedness
df.nest <- df %>% group_by(Type, Resolution) %>%
  do(w = wilcox.test(.$Nestedness_sor, conf.int = TRUE, conf.level = 0.95)) %>%
  summarise(Type, Resolution, Value = w$estimate,
            low.ci = w$conf.int[1], upper.ci = w$conf.int[2],
            Metric = "Nestedness")
# Merge data
df.diff <- rbind(df.beta, df.turn, df.nest)

# Select binarizations
df_both2 <- df.diff %>% filter(Type %in% c("10% threshold", "10% thr. and Ranking"))

# Rename assemnbly methods
df_both2$Type <- factor(df_both2$Type,
                       levels = c("10% threshold", "10% thr. and Ranking"),
                       labels = c("Only Stacked", "Stacked and Ranked"))


# Plot
g_beta <- ggplot(df_both2, aes(x = Resolution, y = Value,
                           group = interaction(Metric, Type),
                           colour = Metric, linetype = Type)) +
  geom_line(size = 1.15) +
  geom_point(size = 2, position = position_dodge(width = 0)) +
  geom_errorbar(aes(ymin = low.ci, ymax = upper.ci), width = .2,
                position = position_dodge(0)) +
  scale_colour_manual(values = c("#999999", "#0072B2", "#D55E00"),
                      name = "Metric") +
  scale_linetype_manual(values=c("Only Stacked"= 1,
                                 "Stacked and Ranked"= 3),
                        name = "Assembly") +
  xlab("Resolution") + ylab("Value (pseudomedian)") +
  ylim(0, 1) +
  ggpubr::theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(legend.position = "right")
g_beta
ggsave("Figures/Figure_4.png", g_beta,
       units = "in", width = 7, height = 5,
       dpi = 600, scale = 1.1)

#### FIGURE 6 - BAYESIAN MODELS ####
# Import data
d <- read.csv("data/Bayesian_Models_Results.csv")
str(d)

# Remove intercept and spatial eingevectors
d <- d %>% filter(!Parameter %in%
                    c("b_nslatitudinal_eingevectordfEQ31",
                      "b_nslatitudinal_eingevectordfEQ32",
                      "b_nslatitudinal_eingevectordfEQ33",
                      "b_nslongitudinal_eingevectordfEQ31",
                      "b_nslongitudinal_eingevectordfEQ32",
                      "b_nslongitudinal_eingevectordfEQ33",
                      "b_Intercept"))

#Order and name of predictors
p <- c("b_bio2", "b_bio6", "b_bio7","b_bio15", "b_bio17","b_elevation",
       "b_Forest_coverage_2007", "b_Forest_patch_area",
       "b_sampling_effort")
n <- c( "bio2", "bio6", "bio7", "bio15", "bio17","Elevation",
        "Forest coverage", "Patch area","Density of occurence")

#Get responses
d <- d %>%
  separate(mod, into = c("model", "dataset", "scale", "filter",
                         "threshold", "type", "div_metric", "distance", "method"), sep = "_")

# Get only results of models binarizes with 10% threshold and ranking
t10 <- d %>% filter(threshold %in% c("threshold10", "and")) %>%
  mutate(div_metric = ifelse(type=="sesam", div_metric, type)) %>%
  mutate(threshold = recode(threshold,
                            "and" = "Stacked and Ranked",
                            "threshold10" = "Only Stacked")) %>%
  mutate(div_metric= recode(div_metric, "BetaDiv" = "Dissimilarity",
                            "Rich" = "Overprediction"))

# Order parameters and scales
t10$Parameter <- factor(t10$Parameter, levels = p, labels = n)
t10$scale <- factor(t10$scale, levels = c("2.5a", "5a", "10a", "30a", "60a"),
                    labels <- c("2.5 arc-min", "5 arc-min", "10 arc-min",
                                "30 arc-min", "60 arc-min"))

# Plot
g_model <- ggplot(t10, aes(x = Parameter, y= scale, fill = Large)) +
  geom_tile(color = "white", alpha=0.8) +
  scale_fill_gradient2_tableau("Orange-Blue Diverging", name = "Effect size")+
  theme_few() +
  labs(x = "", y = "") +
  facet_grid(rows = vars(threshold), cols = vars(div_metric), switch = "y") +
  geom_text(aes(label = ifelse(CI_low > 0, "↑", ifelse(CI_high < 0,"↓"," "))),
            size=10,color = "white") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 20,face="bold"),
        strip.placement = "outside",
        axis.text = element_text(size=16, face="bold"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=25, margin = margin(b = 20))) +
  guides(fill = guide_colorbar(barheight = unit(10, "cm"),
                               barwidth = unit(1, "cm")))

g_model

ggsave("Figures/Figure_5.png", g_model,
       units = "in", width = 7, height = 6.5,
       dpi = 600, scale = 1.8)
