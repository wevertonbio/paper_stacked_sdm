#### PLOT FIGURES IN APPENDIX S2 ####
# Figures for the other assembly methods not presented in the main paper

# Load packages
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

# Create folder to save figures
dir.create("Appendix_Figures//Appendix_S2", recursive = TRUE)


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

#### FIGURE S1 - BETTER MODELS TO PREDICT RICHNESS AND COMPOSITION####
my_col <- as.character(pals::brewer.set1(5))
g0 <- ggplot(data = s, aes(x = 1 - BetaDiv_sor, y = Rich_cor, color = Type)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = my_col, name = "Assembly method") +
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
ggsave(filename = "Appendix_Figures/Appendix_S2/Figure_s1.png", g0,
       dpi = 600, units = "px", width = 2700,
       height = 2000, scale = 2)

#### FIGURE S2 - CORRELATION BETWEEN OBSERVED AND PREDICTED RICHNESS ####

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
                        labels = c("2.5arc-min", "5arc-min", "10arc-min", "30arc-min",
                                   "60arc-min"))

#Subset data
df_both <- df %>% filter(!(Type %in% c("10% threshold", "10% thr. and Ranking")))
unique(df_both$Type)

# Plot
g_cor <- ggplot(data = df_both, aes(x = Rich_obs, y = Rich_pred, group = Type,
                                    fill = Type)) +
  geom_point(alpha = 0.25, aes(col = Type)) +
  geom_smooth(method = "lm", aes(col = Type)) +
  scale_color_manual(values = c("#E69F00", "#0072B2", "black"), name = "Assembly") +
  scale_fill_manual(values = c("#E69F00", "#0072B2", "black"), name = "Assembly") +
  geom_abline(intercept = 0, slope = 1, colour = "red",
              linetype = "dashed", size = 1.2) +
  xlab("Observed richness") +
  ylab("Predicted richness") +
  scale_y_continuous(limits = c(0, 620), breaks = seq(0, 620, by = 100)) +
  #expand_limits(x = 0, y=0) +
  ggpubr::theme_pubclean() +
  theme(axis.text=element_text(size=14), #Change size of numbers in axis
        axis.title=element_text(size=14,face="bold"))  + #Change size of text in axis
  facet_wrap(.~Resolution, scales = "free", nrow = 2)
g_cor
ggsave("Appendix_Figures/Appendix_S2/Figure_s2.png", g_cor,
       units = "in", width = 7, height = 5,
       dpi = 600, scale = 1.75)

#### FIGURE S3 - DISSIMILARITY ####
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
df_both2 <- df.diff %>% filter(!(Type %in% c("10% threshold", "10% thr. and Ranking")))

# Plot
g_beta <- ggplot(df.diff, aes(x = Resolution, y = Value,
                               group = interaction(Metric, Type),
                               colour = Metric)) +
  geom_line(size = 1.15) +
  geom_point(size = 2, position = position_dodge(width = 0)) +
  geom_errorbar(aes(ymin = low.ci, ymax = upper.ci), width = .2,
                position = position_dodge(0)) +
  scale_colour_manual(values = c("#999999", "#0072B2", "#D55E00"),
                      name = "Metric") +
  # scale_linetype_manual(values=c("Ranking"= 1,
  #                                "Minimum threshold"= 3,
  #                                "Minimum thr. and Ranking" = 2),
  #                       name = "Assembly") +
  xlab("Resolution") + ylab("Value (pseudomedian)") +
  ylim(0, 1) +
  ggpubr::theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(legend.position = "bottom") +
  facet_wrap(.~Type, nrow = 1)
g_beta
ggsave("Appendix_Figures/Appendix_S2/Figure_s3.png", g_beta,
       units = "in", width = 7, height = 3,
       dpi = 600, scale = 1.8)

