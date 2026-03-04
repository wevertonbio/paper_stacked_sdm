#### Plot results ####
#Data: 20 February 2026
#Carregar pacotes
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
source("scripts/plot_results_helpers.R")
source("scripts//plot_results_helpers2.R")

# Create folder to save figures
dir.create("data/Images", recursive = TRUE)

#### Import summary of results ####
s <- fread("data/ModelsxInventary/Summary_Results.gz")

#Convert type of model and resolution to factor
unique(s$Type) %>% dput()
s$Type <- factor(x = s$Type,
                  levels = c("sesam_only", "only_threshold_min",
                             "only_threshold10", "thresholmin_and_sesam",
                             "threshold10_and_sesam"),
                  labels = c("Ranking", "Minimum threshold", "10% threshold",
                             "Minimum thr. and Ranking", "10% thr. and Ranking"))

#Resolution
unique(s$Resolution)
s$Resolution <- factor(x = s$Resolution,
                        levels = c("2.5arc_min", "5arc_min", "10arc_min", "30arc_min",
                                   "60arc_min"),
                        labels = c("2.5arc-min", "5arc-min", "10arc-min", "30arc-min",
                                   "60arc-min"))

####H0 BETTER MODELS TO PREDICT RICHNESS AND COMPOSITION####
my_col <- as.character(pals::brewer.set1(5))
g0 <- ggplot(data = s, aes(x = 1 - BetaDiv_sor, y = Rich_cor, color = Type)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = my_col, name = "Binarization") +
  #geom_label_repel(aes(label = Type)) +
  guides(color = guide_legend(override.aes = aes(label = ""),
                              nrow = 3,
                              title.position = "top", title.hjust = 0.5)) +
  xlab("Similarity (1 - Dissimilarity)") +
  ylab("Richness correlation\n(Observed x predicted)") +
  ggpubr::theme_pubclean() +
  theme(legend.position = "bottom") +
  facet_wrap(.~ Resolution, scales = "free_x")
g0
ggsave(filename = "data/Images/H0-Similarity x Richness.png", g0,
       dpi = 600, units = "px", width = 2700,
       height = 2000, scale = 2)

#### Import complete data ####
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
                        labels = c("2.5arc-min", "5arc-min", "10arc-min", "30arc-min",
                                   "60arc-min"))

#### H1 - MAPS AND CORRELATION PLOT ####
#Species richness from stacked SDMs present a positive correlation with estimates from field inventories, however, with a tendency to overpredict the number of species.
####Import spatial data####
sc <- read_state(code_state = "SC") %>% vect() #Santa catarina map
bb_sc <- ext(sc) #Extent of Santa Catarina (to zoom in map)
sa <- vect("Vectors/South_America.gpkg") #South America
br <- read_country() %>% vect() #Brazil
base <- rast("Rasters/base.tiff") #Raster base
#Rasterize values to plot
dr <- get_rasters(data = df, to_rast <- c("Rich_obs", "Rich_pred",  "Rich_ratio",
                                          "BetaDiv_jac", "Turnover_jac", "Nestedness_jac",
                                          "BetaDiv_sor", "Turnover_sor", "Nestedness_sor"))

#Get plot of results - Put this in supl. material
dr_res <- plot_maps_h1(data = dr,
                       output_dir = "data/Images/H1_maps/") #Folder to save


#Plot 10% threshold and 10% threshold plus sesam
#10% threshold
data10 <- dr[["10% threshold"]]

####Observed richness####
i_obs10 <- lapply(data10, function(x) x$Rich_obs) %>% rast()
#Plot
g_obs10 <- lapply(i_obs10, gg_map, title_legend = "Richness")
names(g_obs10) <- names(i_obs10)
#Arrange plot
g_obs10 <- arrange_obs(g_obs10, type = NULL)

####Predicted richness####
i_pred10 <- lapply(data10, function(x) x$Rich_pred) %>% rast()
#Plot
g_pred10 <- lapply(i_pred10, gg_map, title_legend = "Richness")
names(g_pred10) <- names(i_pred10)
#Arrange plot
g_pred10 <- arrange_pred_over(g_pred10, facet = "S Predicted",
                              type = "10% threshold")

####Overprediction####
i_over10 <- lapply(data10, function(x) x$Rich_ratio) %>% rast()
#Plot
g_over10 <- lapply(i_over10, gg_map, title_legend = "SPred/SObs")
names(g_over10) <- names(i_over10)
#Arrange plot
g_over10 <- arrange_pred_over(g_over10, facet = "Overprediction",
                              type = "10% threshold")

####Predicted vs Observed correlation
#Subset data
df10 <- df %>% filter(Type == "10% threshold")
#split by res
df10 <- split(df10, df10$Resolution)
g_cor10 <- lapply(df10, gg_cor, type = "10% threshold")
names(g_over10) <- names(df10)
#Arrange plot
g_cor10 <- arrange_cor(g_cor10, facet = "SObs~SPred")

#10% threshold and sesam
data10sesam <- dr[["10% thr. and Ranking"]]

####Predicted richness####
i_pred10sesam <- lapply(data10sesam, function(x) x$Rich_pred) %>% rast()
#Plot
g_pred10sesam <- lapply(i_pred10sesam, gg_map, title_legend = "Richness")
names(g_pred10sesam) <- names(i_pred10sesam)
#Arrange plot
g_pred10sesam <- arrange_pred_over(g_pred10sesam, facet = "S Predicted",
                              type = "10% thr. and Ranking")

####Overprediction####
i_over10sesam <- lapply(data10sesam, function(x) x$Rich_ratio) %>% rast()
#Plot
g_over10sesam <- lapply(i_over10sesam, gg_map, title_legend = "SPred/SObs")
names(g_over10sesam) <- names(i_over10sesam)
#Arrange plot
g_over10sesam <- arrange_pred_over(g_over10sesam, facet = "Overprediction",
                              type = "10% thr. and Ranking")

####Predicted vs Observed correlation
#Subset data
df10sesam <- df %>% filter(Type == "10% thr. and Ranking")
#split by res
df10sesam <- split(df10sesam, df10sesam$Resolution)
g_cor10sesam <- lapply(df10sesam, gg_cor, type = "10% thr. and Ranking")
names(g_over10sesam) <- names(df10sesam)
#Arrange plot
g_cor10sesam <- arrange_cor(g_cor10sesam, facet = "SObs~SPred")


#Arrange all plots#
all_p <- g_obs10 / g_pred10 / g_pred10sesam / g_over10 / g_over10sesam / g_cor10 / g_cor10sesam
  # plot_annotation(title = gsub("\n", " ", "10% threshold"),
  #                 theme = theme(plot.title = element_text(size = 28,
  #                                                         face = "bold",
  #                                                         hjust = 0.5)))
#Save
ggsave(filename = "data/Images/H1_maps/10thr and ranking.png",  all_p, dpi = 600,
       units = "px", width = 2600,
       height = 3100, scale = 6)

####Option 2 - Do not plot S predicted (only overprediction) and merge correlation in a single plot ####
#Invert facet: resolution in y and plots in X

####Observed richness####
data <- dr[[1]]
i_obs <- lapply(data, function(x) x$Rich_obs) %>% rast()
#Set limits according to minimum and maximum
minmax(i_obs)
l <- list("2.5arc-min" = c(0, 100),
         "5arc-min" = c(0, 100),
         "10arc-min" = c(0, 180),
         "30arc-min" = c(0, 300),
         "60arc-min" = c(0, 420))


#Plot
g_obs <- lapply(i_obs, gg_map, title_legend = "Richness", l = l)
names(g_obs) <- names(i_obs)
#Arrange plot
#g_obs <- arrange_obs2(d = g_obs, type = NULL)

#Plot 10% threshold and 10% threshold plus sesam
#10% threshold overprediction
data10 <- dr[["10% threshold"]]
####Overprediction####
i_over10 <- lapply(data10, function(x) x$Rich_ratio) %>% rast()
#Get limits
minmax(i_over10)
minmax(i_over10sesam) #Run below to get this object
l <- list("2.5arc-min" = c(2, 45),
          "5arc-min" = c(2, 45),
          "10arc-min" = c(2, 30),
          "30arc-min" = c(1, 26),
          "60arc-min" = c(1, 16))


#Plot
g_over10 <- lapply(i_over10, gg_map, title_legend = "SPred/SObs", l = l)
names(g_over10) <- names(i_over10)

#10% + SESAM overprediction
data10sesam <- dr[["10% thr. and Ranking"]]
i_over10sesam <- lapply(data10sesam, function(x) x$Rich_ratio) %>% rast()
#Plot
g_over10sesam <- lapply(i_over10sesam, gg_map, title_legend = "SPred/SObs", l = l)
names(g_over10sesam) <- names(i_over10sesam)

#Richnesse correlation
#Subset data
df_both<- df %>% filter(Type %in% c("10% threshold", "10% thr. and Ranking"))

#Split by res
df_both <- split(df_both, df_both$Resolution)
g_cor_both <- lapply(df_both, gg_cor2)
names(g_cor_both)

#Arrange plots
all_plots <- arrange_plots3(g_obs, g_over10, g_over10sesam, g_cor_both)
#
# ggsave(filename = "data/Images/H1_maps/10thr and ranking_v3.png", all_plots, dpi = 600,
#        units = "px", width = 3200,
#        height = 2500, scale = 3.1)

ggsave(filename = "data/Images/H1_maps/Figure 2.png", all_plots, dpi = 600,
       units = "px", width = 3400,
       height = 2000, scale = 3.1)


####H2####
#Stacked SDMs are capable to predict species presence in local communities, however, have a limited ability to predict absences

#Create two columns
#False negatives/Number of present species: lower values, best to predict presences
#False positives/Number of present species: lower values, best to predict absences

df$FN_RichObs <- df$FN/df$Rich_pred
df$FP_RichObs <- df$FP/df$Rich_pred
#Create column with metrics
df2 <- df %>% gather(Metric, Value, FN_RichObs, FP_RichObs)

#Wilcoxon test to test difference between true negatives and false positives
wt <- df2 %>% group_by(Type, Resolution) %>%
  do(w = wilcox.test(Value ~ Metric, data=., paired=T)) %>%
  summarise(Type,
            Resolution,
            W = w$statistic,
            p.value = w$p.value)

#Ver como reportar teste de Wilcoxon: https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/
# a <- df.b2 %>%
# t <- wilcox.test(Value ~ Metric, a)
# a %>% wilcox_effsize(Value ~ Metric, paired = T)

#Plot boxplot
#Create folder to save
dir.create("data/Images/H2_boxplots/")
#By model
all_models <- unique(df2$Type)
g.h2 <- pblapply(all_models, function(i){
  d_i <- df2 %>% filter(Type == i)
  #Remove \n and % of model name to save
  modelname <- gsub("\n", " ", i)
  modelname <- gsub("%", "", modelname)
  g2 <- ggplot(d_i, aes(x = Metric, y = Value)) +
    geom_boxplot() +
    geom_hline(yintercept=0, linetype="dashed",col = "red") +
    xlab("") + ylab ("log(1 + Value / Total species on site)") + #Valores em log mpara melhor visualiza??o
    scale_x_discrete(labels = c("False Negatives", "False Positives")) +
    #stat_compare_means(paired = T) +
    facet_wrap(~Resolution, scales = "fixed") +
    ggpubr::theme_pubclean() +
    ggtitle(gsub("\n", " ", i)) +
    theme(plot.title = element_text(face = "bold",
                                    hjust = 0.5))
  #Save
  filename <- paste0("data/Images/H2_boxplots/", modelname, ".png")
  ggsave(filename = filename, g2,
         dpi = 600, width = 8, height = 6)
  return(g2)
})

#Facet plot
df_10_sesam <- df2 %>% filter(Type %in% c("10% threshold", "10% thr. and Ranking"))

g2f <- ggplot(df_10_sesam, aes(x = Metric, y = log(1 + Value))) +
  geom_boxplot() +
  geom_hline(yintercept=0, linetype="dashed",col = "red") +
  xlab("") + ylab ("log(1 + Value / Total species on site)") + #Valores em log mpara melhor visualiza??o
  scale_x_discrete(labels = c("False\nNegatives", "False\nPositives")) +
  #stat_compare_means(paired = T) +
  facet_wrap(~Resolution, scales = "fixed") +
  ggpubr::theme_pubclean() +
  #ggtitle(gsub("\n", " ", i)) +
  #theme(plot.title = element_text(face = "bold",
  #                               hjust = 0.5)) +
  #theme(axis.text.x = element_text(angle = 45)) +
  facet_grid(Type ~ Resolution)
g2f
#Save
filename <- paste0("data/Images/H2_boxplots/Boxplot_10thr_SESAM.png")
ggsave(filename = filename, g2f,
       dpi = 600, units = "px", width = 2900,
       height = 2200, scale = 1.7)

#### Option 2 - Maps of dissimilarity, turnover and nestdnedd ####
#Plot 10% threshold and 10% threshold plus sesam
#10% threshold overprediction
data10 <- dr[["10% threshold"]]

####Dissimilarity####
i_beta10 <- lapply(data10, function(x) x$BetaDiv_jac) %>% rast()
#Get limits
minmax(i_beta10)
minmax(i_beta10sesam) #Run below to get this object
l <- list("2.5arc-min" = c(0, 1),
          "5arc-min" = c(0, 1),
          "10arc-min" = c(0, 1),
          "30arc-min" = c(0, 1),
          "60arc-min" = c(0, 1))


#Plot
g_beta10 <- lapply(i_beta10, gg_map, title_legend = "Dissimilarity", l = l,
                   round = FALSE, breaks = 6)
names(g_beta10) <- names(i_beta10)

#10% + SESAM dissimilarity
data10sesam <- dr[["10% thr. and Ranking"]]
i_beta10sesam <- lapply(data10sesam, function(x) x$BetaDiv_jac) %>% rast()
#Plot
g_beta10sesam <- lapply(i_beta10sesam, gg_map, title_legend = "Dissimilarity", l = l,
                        round = FALSE, breaks = 6)
names(g_beta10sesam) <- names(i_beta10sesam)

#Nestedness
i_nest10 <- lapply(data10, function(x) x$Nestedness_jac) %>% rast()
#Get limits
minmax(i_nest10)
minmax(i_nest10sesam) #Run below to get this object
l <- list("2.5arc-min" = c(0, 1),
          "5arc-min" = c(0, 1),
          "10arc-min" = c(0, 1),
          "30arc-min" = c(0, 1),
          "60arc-min" = c(0, 1))


#Plot
g_nest10 <- lapply(i_nest10, gg_map, title_legend = "Nestedness", l = l,
                   round = FALSE, breaks = 6)
names(g_nest10) <- names(i_nest10)

#10% + SESAM dissimilarity
data10sesam <- dr[["10% thr. and Ranking"]]
i_nest10sesam <- lapply(data10sesam, function(x) x$Nestedness_jac) %>% rast()
#Plot
g_nest10sesam <- lapply(i_nest10sesam, gg_map, title_legend = "Nestedness", l = l,
                        round = FALSE, breaks = 6)
names(g_nest10sesam) <- names(i_nest10sesam)

#Turnover
i_turn10 <- lapply(data10, function(x) x$Turnover_jac) %>% rast()
#Get limits
minmax(i_turn10)
minmax(i_turn10sesam) #Run below to get this object
l <- list("2.5arc-min" = c(0, 1),
          "5arc-min" = c(0, 1),
          "10arc-min" = c(0, 1),
          "30arc-min" = c(0, 1),
          "60arc-min" = c(0, 1))


#Plot
g_turn10 <- lapply(i_turn10, gg_map, title_legend = "Turnover", l = l,
                   round = FALSE, breaks = 6)
names(g_turn10) <- names(i_turn10)

#10% + SESAM dissimilarity
data10sesam <- dr[["10% thr. and Ranking"]]
i_turn10sesam <- lapply(data10sesam, function(x) x$Turnover_jac) %>% rast()
#Plot
g_turn10sesam <- lapply(i_turn10sesam, gg_map, title_legend = "Turnover", l = l,
                        round = FALSE, breaks = 6)
names(g_turn10sesam) <- names(i_turn10sesam)



#Arrange plots
all_plots <- arrange_plots4(g_beta10, g_beta10sesam,
                            g_nest10, g_nest10sesam,
                            g_turn10, g_turn10sesam)

ggsave(filename = "data/Images/H2_boxplots//Figure 3.png", all_plots, dpi = 600,
       units = "px", width = 2900,
       height = 2300, scale = 3.6)


# #Plot density
# g.h2 <- ggplot(df.b2, aes(log(Value + 1), fill = Metric)) +
#   geom_density(alpha = 0.25) +
#   geom_hline(yintercept=0, linetype="dashed") +
#   #xlab("Metric") + ylab ("Value") +
#   facet_wrap(Resolution.f~., scales = "free") +
#   theme_bw()
# g.h2

####H3####
#The differences of community metrics between stacked SDMs and field inventories gradually reduce from local to macroscale.
#Create columns with values

#Create folder
dir.create("data/Images/H3_Dissimilarity/", showWarnings = FALSE)

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
#Unir dados
df.diff <- rbind(df.beta, df.turn, df.nest)


#Single plots
g.h3 <- pblapply(all_models, function(i){
  d_i <- df.diff %>% filter(Type == i)
  #Remove \n and % of model name to save
  modelname <- gsub("\n", " ", i)
  modelname <- gsub("%", "", modelname)
  #Plot
  g3 <- ggplot(d_i, aes(x=Resolution, y = Value, group = Metric,
                      colour = Metric)) +
    geom_line(size = 1.15) +
    geom_point(size = 2, position=position_dodge(width=0)) +
    geom_errorbar(aes(ymin=low.ci, ymax=upper.ci), width=.2,
                  position=position_dodge(0)) +
    scale_colour_manual(values = c("#999999", "#0072B2", "#D55E00"),
                        name = "Metric") +
    xlab("Resolution") + ylab("Value (pseudomedian)") +
    ylim(0,1) +
    ggpubr::theme_pubclean() +
    ggtitle(gsub("\n", " ", i)) +
    theme(plot.title = element_text(face = "bold",
                                    hjust = 0.5))
  #Save
  filename <- paste0("data/Images/H3_Dissimilarity/", modelname, ".png")
  ggsave(filename = filename, g3,
         dpi = 600, width = 8, height = 6)
  return(g3)
})

#Facet plot
df_both <- df.diff %>% filter(Type %in% c("10% threshold", "10% thr. and Ranking"))

#Rename levels
df_both$Type <- factor(df_both$Type,
                       levels = c("10% threshold", "10% thr. and Ranking"),
                       labels = c("Threshold-only", "Threshold-and-ranking"))

g3f <- ggplot(df_both, aes(x=Resolution, y = Value, group = Metric,
                      colour = Metric)) +
  geom_line(size = 1.15) +
  geom_point(size = 2, position=position_dodge(width=0)) +
  geom_errorbar(aes(ymin=low.ci, ymax=upper.ci), width=.2,
                position=position_dodge(0)) +
  scale_colour_manual(values = c("#999999", "#0072B2", "#D55E00"),
                      name = "Metric") +
  xlab("Resolution") + ylab("Value (pseudomedian)") +
  ylim(0,1) +
  ggpubr::theme_pubclean() +
  #ggtitle(gsub("\n", " ", i)) +
  #theme(plot.title = element_text(face = "bold",
   #                               hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(~Type, nrow = 1)
g3f
#Save
filename <- paste0("data/Images/H3_Dissimilarity/10_ranking_Models_v1.png")
ggsave(filename = filename, g3f,
       dpi = 600, units = "px", width = 2900,
       height = 1700, scale = 2)

#Grouped plot
g3g <- ggplot(df_both, aes(x = Resolution, y = Value,
                           group = interaction(Metric, Type),
                           colour = Metric, linetype = Type)) +
  geom_line(size = 1.15) +
  geom_point(size = 2, position = position_dodge(width = 0)) +
  geom_errorbar(aes(ymin = low.ci, ymax = upper.ci), width = .2,
                position = position_dodge(0)) +
  scale_colour_manual(values = c("#999999", "#0072B2", "#D55E00"),
                      name = "Metric") +
  scale_linetype_manual(values=c("Threshold-only"= 1,
                                 "Threshold-and-ranking"= 3),
                        name = "Assembly") +
  xlab("Resolution") + ylab("Value (pseudomedian)") +
  ylim(0, 1) +
  ggpubr::theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45))
g3g
#Save
filename <- paste0("data/Images/H3_Dissimilarity/Figure 4.png")
ggsave(filename = filename, g3g,
       dpi = 600, units = "px", width = 2000,
       height = 1500, scale = 2.7)



####H3####
#The dissimilarity (beta diversity) between stacked SDMs and local inventories is relatively high and mostly due to the higher contribution of the nestedness component.
#Density plot
#Criar coluna com valores
# colnames(df.b)
# df.b3 <- df.b %>% gather(Metric, Value, BetaDiv_sor, Turnover_sor, Nestedness_sor)
#
# #Plot density
# g.h3 <- ggplot(df.b3, aes(Value, fill = Metric)) +
#   geom_density(alpha = 0.45) +
#   geom_hline(yintercept=0, linetype="dashed") +
#   scale_fill_manual(values = c("#999999", "#F0E442", "#0072B2"),
#                     labels = c("Dissimilarity", "Nestedness", "Turnover")) +
#   xlab("Value") + ylab ("Density") +
#   facet_wrap(Resolution.f~., scales = "free_y") +
#   theme_bw()
# g.h3
# ggsave("Images_Hypothesis/H3-Metrics.png", g.h3,
#        dpi = 300, width = 8, height = 6)
#
# #Plot boxplot
# g.h3B <- ggplot(df.b3, aes(x = Metric, y = Value)) +
#   geom_boxplot() +
#   geom_hline(yintercept=0, linetype="dashed") +
#   scale_fill_manual(values = c("#999999", "#F0E442", "#0072B2"),
#                     labels = c("Dissimilarity", "Nestedness", "Turnover")) +
#   xlab("Value") + ylab ("Density") +
#   facet_wrap(Resolution.f~., scales = "free_x") +
#   theme_bw()
# g.h3B
# ggsave("Images_Hypothesis/H3-Metrics_Boxplot.png", g.h3B,
#        dpi = 300, width = 8, height = 6)
