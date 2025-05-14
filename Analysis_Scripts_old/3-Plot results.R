#### Plot results ####
#Data: 10 May 2024
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

#Import helpers to plot
source("Analysis_Scripts/plot_results_helpers.R")

#Criar pasta para salvar imagens
dir.create("Results/Images", recursive = TRUE)

#### Import summary of results ####
s <- fread("Results/ModelsxInventary/Summary_Results.gz")

#Create labels (letters)
s_labels <- data.frame(Type = unique(s$Type)) %>%
  mutate(l = letters[1:length(unique(Type))]) %>%
  mutate(to_label = paste0(l, ". ", Type))

#Join data
s <- left_join(s, s_labels)

#Convert type of model and resolution to factor
unique(s$to_label) %>% dput()
s$to_label <- factor(x = s$to_label,
                  levels = c("a. Ensemble_Only_Ranking",
                             "b. Ensemble_Only_Threshold10",
                             "c. Ensemble_Only_ThresholdMaxTSS",
                             "d. Ensemble_Threshold10_and_Ranking",
                             "e. Ensemble_ThresholdMaxTSS_and_Ranking",
                             "f. Maxent_Only_Ranking",
                             "g. Maxent_Only_Threshold10",
                             "h. Maxent_Only_ThresholdMaxTSS",
                             "i. Maxent_Threshold10_and_Ranking",
                             "j. Maxent_ThresholdMaxTSS_and_Ranking"),
                  labels = c("a. Ensemble - Only Ranking",
                             "b. Ensemble - Only Threshold 10%",
                             "c. Ensemble - Only Threshold MaxTSS",
                             "d. Ensemble - Threshold 10% and Ranking",
                             "e. Ensemble - Threshold MaxTSS and Ranking",
                             "f. Maxent - Only Ranking",
                             "g. Maxent - Only Threshold 10%",
                             "h. Maxent - Only Threshold MaxTSS",
                             "i. Maxent - Threshold 10% and Ranking",
                             "j. Maxent - Threshold MaxTSS and\nRanking"))
#Resolution
unique(s$Resolution)
s$Resolution <- factor(x = s$Resolution,
                        levels = c("2.5arc_min", "5arc_min", "10arc_min", "30arc_min",
                                   "60arc_min"),
                        labels = c("2.5arc-min", "5arc-min", "10arc-min", "30arc-min",
                                   "60arc-min"))

####H0 BETTER MODELS TO PREDICT RICHNESS AND COMPOSITION####
my_col <- as.character(pals::brewer.set1(10))
g0 <- ggplot(data = s, aes(x = 1 - BetaDiv_sor, y = Rich_cor, color = to_label)) +
  geom_point() +
  scale_color_manual(values = my_col, name = "Models") +
  geom_label_repel(aes(label = l)) +
  guides(color = guide_legend(override.aes = aes(label = ""),
                              nrow = 3,
                              title.position = "top", title.hjust = 0.5)) +
  xlab("Similarity (1 - Dissimilarity)") +
  ylab("Richness correlation\n(Observed x predicted)") +
  ggpubr::theme_pubclean() +
  theme(legend.position = "bottom") +
  facet_wrap(.~ Resolution, scales = "free")
g0
ggsave(filename = "Results/Images/H0-Best_models.png", g0,
       dpi = 600, units = "px", width = 2700,
       height = 1700, scale = 2.7)

#### Import commplete data ####
df <- fread("Results/ModelsxInventary/Results.gz")
#Convert type of model and resolution to factor
unique(df$Type) %>% dput()
df$Type <- factor(x = df$Type,
                  levels = c("Ensemble_Only_Ranking",
                             "Ensemble_Only_Threshold10",
                             "Ensemble_Only_ThresholdMaxTSS",
                             "Ensemble_Threshold10_and_Ranking",
                             "Ensemble_ThresholdMaxTSS_and_Ranking",
                             "Maxent_Only_Ranking",
                             "Maxent_Only_Threshold10",
                             "Maxent_Only_ThresholdMaxTSS",
                             "Maxent_Threshold10_and_Ranking",
                             "Maxent_ThresholdMaxTSS_and_Ranking"),
                  labels = c("Ensemble\nOnly Ranking",
                             "Ensemble\nOnly Threshold 10%",
                             "Ensemble\nOnly Threshold maxTSS",
                             "Ensemble\nThreshold 10% and Ranking",
                             "Ensemble\nThreshold maxTSS and Ranking",
                             "Maxent\nOnly Ranking",
                             "Maxent\nOnly Threshold 10%",
                             "Maxent\nOnly Threshold maxTSS",
                             "Maxent\nThreshold 10% and Ranking",
                             "Maxent\nThreshold maxTSS and Ranking"))
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
sc <- read_state(code_state = "SC") %>% vect() #Santa catarina
bb_sc <- ext(sc) #Extent of Santa Catarina (to zoom in map)
sa <- vect("https://github.com/wevertonbio/spatial_files/raw/main/Data/South_America.gpkg") #South America
br <- read_country() %>% vect() #Brazil
base <- rast("Rasters/base.tiff") #Raster base
#Rasterize values to plot
dr <- get_rasters(data = df)
#Get plot os results
dr_res <- plot_maps_h1(data = dr,
                       output_dir = "Results/Images/H1_maps/") #Folder to save



####H2####
#Stacked SDMs are capable to predict species presence in local communities, however, have a limited ability to predict absences

#Create two columns
#False negatives/Number of present species: lower values, best to predict presences
#False positives/Number of present species: lower values, best to predict absences

df$FN_RichObs <- df$FN/df$Rich_obs
df$FP_RichObs <- df$FP/df$Rich_obs
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
dir.create("Results/Images/H2_boxplots/")
#By model
all_models <- unique(df2$Type)
g.h2 <- pblapply(all_models, function(i){
  d_i <- df2 %>% filter(Type == i)
  #Remove \n and % of model name to save
  modelname <- gsub("\n", " ", i)
  modelname <- gsub("%", "", modelname)
  g2 <- ggplot(d_i, aes(x = Metric, y = log(1 + Value))) +
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
  filename <- paste0("Results/Images/H2_boxplots/", modelname, ".png")
  ggsave(filename = filename, g2,
         dpi = 600, width = 8, height = 6)
  return(g2)
})

#Facet plot
g2f <- ggplot(df2, aes(x = Metric, y = log(1 + Value))) +
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
filename <- paste0("Results/Images/H2_boxplots/All_Models.png")
ggsave(filename = filename, g2f,
       dpi = 600, units = "px", width = 2900,
       height = 4000, scale = 2.5)

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
dir.create("Results/Images/H3_Dissimilarity/", showWarnings = FALSE)

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
  filename <- paste0("Results/Images/H3_Dissimilarity/", modelname, ".png")
  ggsave(filename = filename, g3,
         dpi = 600, width = 8, height = 6)
  return(g3)
})

#Facet plot
g3f <- ggplot(df.diff, aes(x=Resolution, y = Value, group = Metric,
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
  facet_wrap(~Type, nrow = 2)
g3f
#Save
filename <- paste0("Results/Images/H3_Dissimilarity/All_Models.png")
ggsave(filename = filename, g3f,
       dpi = 600, units = "px", width = 2900,
       height = 1700, scale = 3)




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
