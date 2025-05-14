#### Plot variables ####
library(ggplot2)
library(ggpubr)
library(dplyr)
library(data.table)
library(pbapply)
library(stringr)
library(pals)
library(reshape2)
library(geobr)
library(RColorBrewer)
library(corrplot)
library(tidyverse)
library(ggnewscale)
library(terra)
library(tidyterra)
library(ggrepel)
library(patchwork)

#Import helpers to plot
source("Analysis_Scripts/plot_results_helpers.R")
source("Analysis_Scripts/plot_results_helpers2.R")

#### Import commplete data ####
df <- fread("Results/ModelsxInventary/Results.gz")
#write.csv(df, "Results/ModelsxInventary/Results.csv", row.names = FALSE)
#Convert type of model and resolution to factor
unique(df$Type) %>% dput()
df$Type <- factor(x = df$Type,
                  levels = c("sesam_only", "only_threshold_min",
                             "only_threshold10", "thresholmin_and_sesam",
                             "threshold10_and_sesam"),
                  labels = c("SESAM", "Minimum threshold", "10% threshold",
                             "Minimum thr. and SESAM", "10% thr. and SESAM"))
#Resolution
unique(df$Resolution)
df$Resolution <- factor(x = df$Resolution,
                        levels = c("2.5arc_min", "5arc_min", "10arc_min", "30arc_min",
                                   "60arc_min"),
                        labels = c("2.5arc-min", "5arc-min", "10arc-min", "30arc-min",
                                   "60arc-min"))

#Get some model
m <- readRDS("levdata_Bayes_models_script_results_2025/all_levdata_models_2025.rds")

# #Extract data from models of interest
# d2.5 <- list("10% threshold" = m[["mod_levdata_2.5a_only_threshold10_BetaDiv_jac_splines"]]$data,
#              "10% thr. and SESAM" = m[["mod_levdata_2.5a_threshold10_and_sesam_BetaDiv_jac_splines"]]$data)
# d5 <- list("10% threshold" = m[["mod_levdata_5a_only_threshold10_BetaDiv_jac_splines"]]$data,
#              "10% thr. and SESAM" = m[["mod_levdata_5a_threshold10_and_sesam_BetaDiv_jac_splines"]]$data)
# d10 <- list("10% threshold" = m[["mod_levdata_10a_only_threshold10_BetaDiv_jac_splines"]]$data,
#              "10% thr. and SESAM" = m[["mod_levdata_10a_threshold10_and_sesam_BetaDiv_jac_splines"]]$data)
# d30 <- list("10% threshold" = m[["mod_levdata_30a_only_threshold10_BetaDiv_jac_splines"]]$data,
#              "10% thr. and SESAM" = m[["mod_levdata_30a_threshold10_and_sesam_BetaDiv_jac_splines"]]$data)
# d60 <- list("10% threshold" = m[["mod_levdata_60a_only_threshold10_BetaDiv_jac_splines"]]$data,
#             "10% thr. and SESAM" = m[["mod_levdata_60a_threshold10_and_sesam_BetaDiv_jac_splines"]]$data)

#Extract data from models of interest
d2.5 <- list("10% threshold" = m[["mod_levdata_2.5a_only_threshold10_BetaDiv_jac_splines"]]$data)
d5 <- list("10% threshold" = m[["mod_levdata_5a_only_threshold10_BetaDiv_jac_splines"]]$data)
d10 <- list("10% threshold" = m[["mod_levdata_10a_only_threshold10_BetaDiv_jac_splines"]]$data)
d30 <- list("10% threshold" = m[["mod_levdata_30a_only_threshold10_BetaDiv_jac_splines"]]$data)
d60 <- list("10% threshold" = m[["mod_levdata_60a_only_threshold10_BetaDiv_jac_splines"]]$data)

d <- list(d2.5, d5, d10, d30, d60)
names(d) <- c("2.5arc-min", "5arc-min", "10arc-min", "30arc-min", "60arc-min")

#Looping to extract data in a single dataframe
d_final <- lapply(names(d), function(r){
  d_r <- d[[r]]
  z <- lapply(names(d_r), function(i){
    r_i <- d_r[[i]]
    r_i$`ns(longitudinal_eingevector, df = 3)` <- NULL
    r_i$`ns(latitudinal_eingevector, df = 3)` <- NULL
    r_i <- r_i %>% mutate(Resolution = r, Type = i, .before = 1)

    #Add eingevectors
    einge_long1 <-  d_r[[i]]$`ns(longitudinal_eingevector, df = 3)`[,1]
    einge_long2 <-  d_r[[i]]$`ns(longitudinal_eingevector, df = 3)`[,2]
    einge_long3 <-  d_r[[i]]$`ns(longitudinal_eingevector, df = 3)`[,3]
    einge_lat1 <-  d_r[[i]]$`ns(latitudinal_eingevector, df = 3)`[,1]
    einge_lat2 <-  d_r[[i]]$`ns(latitudinal_eingevector, df = 3)`[,2]
    einge_lat3 <-  d_r[[i]]$`ns(latitudinal_eingevector, df = 3)`[,3]

    r_i <- r_i %>%
      mutate(longitudinal_eingevector_1 = einge_long1,
             longitudinal_eingevector_2 = einge_long2,
             longitudinal_eingevector_3 = einge_long3,
             latitudinal_eingevector_1 = einge_lat1,
             latitudinal_eingevector_2 = einge_lat2,
             latitudinal_eingevector_3 = einge_lat3)

    #Get xy from data
    df_i <- df %>% filter(Type == i, Resolution == r)
    r_i <- r_i %>% mutate(Longitude = df_i$Longitude, Latitude = df_i$Latitude,
                          .before = 1)
    r_i
  })
  z <- rbindlist(z)
}) %>% rbindlist()

#Rasterize data
rd <- get_rasters(d_final, to_rast = c("Forest_patch_area",
                                       "Forest_coverage_2007", "bio2", "bio6",
                                       "bio7", "bio15", "bio17", "elevation",
                                       "sampling_effort",
                                       "longitudinal_eingevector_1",
                                       "longitudinal_eingevector_2",
                                       "longitudinal_eingevector_3",
                                       "latitudinal_eingevector_1",
                                       "latitudinal_eingevector_2",
                                       "latitudinal_eingevector_3"))
#Plot
p2.5 <- lapply(names(rd[[1]]$`2.5arc-min`), function(i){
  ggplot() +
    geom_sf(data = sa, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = br, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = sc, fill = "snow1", colour = "black") +
    geom_spatraster(data = rd[[1]]$`2.5arc-min`[[i]], na.rm = TRUE) +
    geom_sf(data = sc, fill = NA, colour = "black") +
    #scale_fill_whitebox_b(palette = "muted", name = "") +
    scale_fill_terrain_b(name = "") +
                          #limits = limits_r,
                          #breaks = 6) +
    coord_sf(xlim = c(bb_sc[1], xmax=bb_sc[2]),
             ylim = c(bb_sc[3], ymax=bb_sc[4]),
             expand = T) +
    #ylab(type) +
    # metR::scale_x_longitude(ticks = 2) +
    # metR::scale_y_latitude(ticks = 2) +
    ggpubr::theme_pubclean() +
    ggtitle(i) +
    theme(panel.background = element_rect(fill = 'aliceblue'),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.32, 0.15), legend.direction = "horizontal",
          legend.key.height =unit(0.2, "cm"),
          legend.key.width = unit(1, "cm"),
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

#Arrange plot
# pp2.5 <- ggarrange(plotlist = p2.5, ncol = 3, nrow = 5)
# pp2.5
pp2.5 <- patchwork::wrap_plots(p2.5, ncol = 3, nrow = 5) +
  plot_annotation("2.5arc-min",
                  theme = theme(plot.title = element_text(size = 20,
                                                          face = "bold",
                                                          hjust = 0.5)))
#Save
ggsave(filename = "Results/Images/Model_variables/2.5arcmin.png", pp2.5, dpi = 600,
       units = "px", width = 2500,
       height = 3200, scale = 2.5)

#Plot
p5 <- lapply(names(rd[[1]]$`5arc-min`), function(i){
  ggplot() +
    geom_sf(data = sa, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = br, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = sc, fill = "snow1", colour = "black") +
    geom_spatraster(data = rd[[1]]$`5arc-min`[[i]], na.rm = TRUE) +
    geom_sf(data = sc, fill = NA, colour = "black") +
    #scale_fill_whitebox_b(palette = "muted", name = "") +
    scale_fill_terrain_b(name = "") +
    #limits = limits_r,
    #breaks = 6) +
    coord_sf(xlim = c(bb_sc[1], xmax=bb_sc[2]),
             ylim = c(bb_sc[3], ymax=bb_sc[4]),
             expand = T) +
    #ylab(type) +
    # metR::scale_x_longitude(ticks = 2) +
    # metR::scale_y_latitude(ticks = 2) +
    ggpubr::theme_pubclean() +
    ggtitle(i) +
    theme(panel.background = element_rect(fill = 'aliceblue'),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.32, 0.15), legend.direction = "horizontal",
          legend.key.height =unit(0.2, "cm"),
          legend.key.width = unit(1, "cm"),
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

#Arrange plot
# pp5 <- ggarrange(plotlist = p5, ncol = 3, nrow = 5)
# pp5
pp5 <- patchwork::wrap_plots(p5, ncol = 3, nrow = 5) +
  plot_annotation("5arc-min",
                  theme = theme(plot.title = element_text(size = 20,
                                                          face = "bold",
                                                          hjust = 0.5)))
#Save
ggsave(filename = "Results/Images/Model_variables/5arcmin.png", pp5, dpi = 600,
       units = "px", width = 2500,
       height = 3200, scale = 2.5)

#Plot
p10 <- lapply(names(rd[[1]]$`10arc-min`), function(i){
  ggplot() +
    geom_sf(data = sa, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = br, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = sc, fill = "snow1", colour = "black") +
    geom_spatraster(data = rd[[1]]$`10arc-min`[[i]], na.rm = TRUE) +
    geom_sf(data = sc, fill = NA, colour = "black") +
    #scale_fill_whitebox_b(palette = "muted", name = "") +
    scale_fill_terrain_b(name = "") +
    #limits = limits_r,
    #breaks = 6) +
    coord_sf(xlim = c(bb_sc[1], xmax=bb_sc[2]),
             ylim = c(bb_sc[3], ymax=bb_sc[4]),
             expand = T) +
    #ylab(type) +
    # metR::scale_x_longitude(ticks = 2) +
    # metR::scale_y_latitude(ticks = 2) +
    ggpubr::theme_pubclean() +
    ggtitle(i) +
    theme(panel.background = element_rect(fill = 'aliceblue'),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.32, 0.15), legend.direction = "horizontal",
          legend.key.height =unit(0.2, "cm"),
          legend.key.width = unit(1, "cm"),
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

#Arrange plot
# pp10 <- ggarrange(plotlist = p10, ncol = 3, nrow = 5)
# pp10
pp10 <- patchwork::wrap_plots(p10, ncol = 3, nrow = 5) +
  plot_annotation("10arc-min",
                  theme = theme(plot.title = element_text(size = 20,
                                                          face = "bold",
                                                          hjust = 0.5)))
#Save
ggsave(filename = "Results/Images/Model_variables/10arcmin.png", pp10, dpi = 600,
       units = "px", width = 2500,
       height = 3200, scale = 2.5)

#Plot
p30 <- lapply(names(rd[[1]]$`30arc-min`), function(i){
  ggplot() +
    geom_sf(data = sa, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = br, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = sc, fill = "snow1", colour = "black") +
    geom_spatraster(data = rd[[1]]$`30arc-min`[[i]], na.rm = TRUE) +
    geom_sf(data = sc, fill = NA, colour = "black") +
    #scale_fill_whitebox_b(palette = "muted", name = "") +
    scale_fill_terrain_b(name = "") +
    #limits = limits_r,
    #breaks = 6) +
    coord_sf(xlim = c(bb_sc[1], xmax=bb_sc[2]),
             ylim = c(bb_sc[3], ymax=bb_sc[4]),
             expand = T) +
    #ylab(type) +
    # metR::scale_x_longitude(ticks = 2) +
    # metR::scale_y_latitude(ticks = 2) +
    ggpubr::theme_pubclean() +
    ggtitle(i) +
    theme(panel.background = element_rect(fill = 'aliceblue'),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.32, 0.15), legend.direction = "horizontal",
          legend.key.height =unit(0.2, "cm"),
          legend.key.width = unit(1, "cm"),
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

#Arrange plot
# pp30 <- ggarrange(plotlist = p30, ncol = 3, nrow = 5)
# pp30
pp30 <- patchwork::wrap_plots(p30, ncol = 3, nrow = 5) +
  plot_annotation("30arc-min",
                  theme = theme(plot.title = element_text(size = 20,
                                                          face = "bold",
                                                          hjust = 0.5)))
#Save
ggsave(filename = "Results/Images/Model_variables/30arcmin.png", pp30, dpi = 600,
       units = "px", width = 2500,
       height = 3200, scale = 2.5)

#Plot
p60 <- lapply(names(rd[[1]]$`60arc-min`), function(i){
  ggplot() +
    geom_sf(data = sa, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = br, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = sc, fill = "snow1", colour = "black") +
    geom_spatraster(data = rd[[1]]$`60arc-min`[[i]], na.rm = TRUE) +
    geom_sf(data = sc, fill = NA, colour = "black") +
    #scale_fill_whitebox_b(palette = "muted", name = "") +
    scale_fill_terrain_b(name = "") +
    #limits = limits_r,
    #breaks = 6) +
    coord_sf(xlim = c(bb_sc[1], xmax=bb_sc[2]),
             ylim = c(bb_sc[3], ymax=bb_sc[4]),
             expand = T) +
    #ylab(type) +
    # metR::scale_x_longitude(ticks = 2) +
    # metR::scale_y_latitude(ticks = 2) +
    ggpubr::theme_pubclean() +
    ggtitle(i) +
    theme(panel.background = element_rect(fill = 'aliceblue'),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.32, 0.15), legend.direction = "horizontal",
          legend.key.height =unit(0.2, "cm"),
          legend.key.width = unit(1, "cm"),
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

#Arrange plot
# pp60 <- ggarrange(plotlist = p60, ncol = 3, nrow = 5)
# pp60
pp60 <- patchwork::wrap_plots(p60, ncol = 3, nrow = 5) +
  plot_annotation("60arc-min",
                  theme = theme(plot.title = element_text(size = 20,
                                                          face = "bold",
                                                          hjust = 0.5)))
#Save
ggsave(filename = "Results/Images/Model_variables/60arcmin.png", pp60, dpi = 600,
       units = "px", width = 2500,
       height = 3200, scale = 2.5)
