#Get model plots#
library(brms)
library(ggplot2)
library(dplyr)
library(qdapRegex)
library(data.table)

#Import models
m <- readRDS("levdata_Bayes_models_script_results_2025/all_levdata_models_2025.rds")

#Import r2 results
r2_res <- read.csv("levdata_Bayes_models_script_results_2025/levdata_bayes_models_R2_results.csv")

#Extract information to dataframe
d <- lapply(names(m), function(i){
  #print(i)
  #i = names(m)[2]

  #Get model
  m_i <- m[[i]]

  #Get r2
  r2 <- r2_res %>% filter(Model == i) %>% pull(Estimate)

  #Get resolution and model type
  new_name <- gsub("mod_levdata_|_splines", "", i)
  r <- ex_between(new_name, "^", "a_", fixed = FALSE)[[1]] #Resolution
  mt <- ex_between(new_name, "a_", "_Rich|_Beta", fixed = FALSE)[[1]] #binarization
  resp <- ex_between(new_name,
                     paste0(mt, "_"), "$", fixed = FALSE)[[1]] #Response

  # Extract fixed effects
  fixed_effects <- fixef(m_i, probs = c(0.025, 0.05, 0.11, 0.89, 0.95, 0.975))

  # Convert to data frame for plotting, excluding the intercept
  dt_fixed_effects <- data.frame(
    Predictor = rownames(fixed_effects),
    Estimate = fixed_effects[, "Estimate"],
    Q5 = fixed_effects[, "Q5"],
    Q95 = fixed_effects[, "Q95"],
    Q11 = fixed_effects[, "Q11"],
    Q89 = fixed_effects[, "Q89"]
  ) %>%
    filter(Predictor != "Intercept") %>%  # Exclude the intercept
    mutate(direction = ifelse(Q11 > 0, "Positive",
                              ifelse(Q89 < 0, "Negative", "Null"))) %>%
    arrange(Predictor) %>%
    #R2
    mutate(r2 = r2) %>%
    #Identify models
    mutate(Response = resp, Resolution = r, model_type = mt, .before = 1)

  #Remove rownames
  row.names(dt_fixed_effects) <- NULL
  dt_fixed_effects

})

#Bind list
d <- rbindlist(d)

#Save list
write.csv(d, "Results/Bayes_results.csv", row.names = FALSE)

#### Plot ####

#Import data
d <- read.csv("Results/Bayes_results.csv")

#### Plot panel by response - Only non-spatial predictors ####
#Order and name of predictors
p <- c("sampling_effort", "Forest_coverage_2007", "Forest_patch_area",
       "bio15", "bio17", "bio2", "bio6", "bio7", "elevation")
n <- c("Sampling effort", "Forest coverage", "Patch area",
       "bio15", "bio17", "bio2", "bio6", "bio7", "Elevation")
#Get responses
d <- d %>% mutate(Response = ifelse(Response == "BetaDiv_jac",
                                    "Dissimilarity", "Overprediction"))

r <- unique(d$Response)

#Plot
g <- lapply(r, function(i){
  print(i)
  #i = r[1]

  d_i <- d %>% filter(Response == i)

  #Remove eingevectors
  d_i <- d_i %>% filter(!grepl("einge", Predictor))

  #Order factors
  d_i$Predictor <- factor(d_i$Predictor, levels = p, labels = n)
  d_i$Resolution <- factor(d_i$Resolution,
                           levels = c(2.5, 5, 10, 30, 60),
                           labels <- c("2.5arc-min", "5arc-min", "10arc-min",
                                       "30arc-min", "60arc-min"))
  d_i$model_type <- factor(d_i$model_type,
                           levels = c("sesam_only", "only_threshold_min",
                                      "only_threshold10", "thresholmin_and_sesam",
                                      "threshold10_and_sesam"),
                           labels = c("SESAM", "Minimum threshold", "10% threshold",
                                      "Min. thr. and SESAM", "10% thr. and SESAM"))
  # Criar um dataset auxiliar com os valores de R² para cada facet
  r2_labels <- d_i %>%
    group_by(model_type, Resolution) %>%
    summarise(R2_text = paste0("R² = ", round(unique(r2), 2)), .groups = "drop",
              y = max(Estimate))

  #Get min y by resolution
  r2_labels <- r2_labels %>% group_by(Resolution) %>% mutate(y_min = max(y))

  #Plot
  ggplot(d_i, aes(x = Predictor, y = Estimate, color = direction)) +
    geom_pointrange(aes(ymin = Q5, ymax = Q95),
                    fatten = 1, linewidth = 1.5, alpha = 0.4) +
    geom_pointrange(aes(ymin = Q11, ymax = Q89),
                    fatten = 2, linewidth = 3.5, alpha = 0.7) +
    geom_hline(yintercept = 0, linewidth = 1, colour = "darkgray", linetype = "dashed") +
    scale_colour_manual(values = c("Positive" = "#D55E00",
                                   "Negative" = "#0072B2",
                                   "Null" = "darkgrey")) +
    #Add r2
    coord_flip() +
    labs(x = NULL,
         y = "Predictor Effect (Estimate)") +
    ggtitle(i) +
    #theme_new() +
    ggpubr::theme_pubclean() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(vjust = 0.5, hjust = 0.7),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12, colour = "black"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    facet_grid(model_type ~ Resolution, scales = "free") +
    #   # # Adiciona os valores de R2 em cada facet
    geom_text(data = r2_labels, aes(x = "Elevation", y = Inf, label = R2_text),
              hjust = 1, vjust = 0.2, inherit.aes = FALSE, size = 3.2)
})
names(g) <- r
g
#Save
dir.create("Results/Images/H4_Models")
ggsave("Results/Images/H4_Models/Dissimilarity.png", g$Dissimilarity,
       dpi = 600, units = "px", width = 4800,
       height = 4000, scale = 1.7)
ggsave("Results/Images/H4_Models/Overprediction.png", g$Overprediction,
       dpi = 600, units = "px", width = 4800,
       height = 4000, scale = 1.7)

#Only threshold 10
#Plot
g <- lapply(r, function(i){
  print(i)
  #i = r[1]

  d_i <- d %>% filter(Response == i)

  #filter model_type
  d_i <- d_i %>% filter(model_type %in% c("only_threshold10",
                                          "threshold10_and_sesam"))

  #Remove eingevectors
  d_i <- d_i %>% filter(!grepl("einge", Predictor))

  #Order factors
  d_i$Predictor <- factor(d_i$Predictor, levels = p, labels = n)
  d_i$Resolution <- factor(d_i$Resolution,
                           levels = c(2.5, 5, 10, 30, 60),
                           labels <- c("2.5arc-min", "5arc-min", "10arc-min",
                                       "30arc-min", "60arc-min"))
  d_i$model_type <- factor(d_i$model_type,
                           levels = c("only_threshold10",
                                      "threshold10_and_sesam"),
                           labels = c("10% threshold","10% thr. and SESAM"))
  # Criar um dataset auxiliar com os valores de R² para cada facet
  r2_labels <- d_i %>%
    group_by(model_type, Resolution) %>%
    summarise(R2_text = paste0("R² = ", round(unique(r2), 2)), .groups = "drop",
              y = max(Estimate))

  #Get min y by resolution
  r2_labels <- r2_labels %>% group_by(Resolution) %>% mutate(y_min = max(y))

  #Plot
  ggplot(d_i, aes(x = Predictor, y = Estimate, color = direction)) +
    geom_pointrange(aes(ymin = Q5, ymax = Q95),
                    fatten = 1, linewidth = 1.5, alpha = 0.4) +
    geom_pointrange(aes(ymin = Q11, ymax = Q89),
                    fatten = 2, linewidth = 3.5, alpha = 0.7) +
    geom_hline(yintercept = 0, linewidth = 1, colour = "darkgray", linetype = "dashed") +
    scale_colour_manual(values = c("Positive" = "#D55E00",
                                   "Negative" = "#0072B2",
                                   "Null" = "darkgrey")) +
    #Add r2
    coord_flip() +
    labs(x = NULL,
         y = "Predictor Effect (Estimate)") +
    ggtitle(i) +
    #theme_new() +
    ggpubr::theme_pubclean() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(vjust = 0.5, hjust = 0.7),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12, colour = "black"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    facet_grid(model_type ~ Resolution, scales = "free") +
    #   # # Adiciona os valores de R2 em cada facet
    geom_text(data = r2_labels, aes(x = "Elevation", y = Inf, label = R2_text),
              hjust = 1, vjust = 0.2, inherit.aes = FALSE, size = 3.2)
})
names(g) <- r
g
#Save
dir.create("Results/Images/H4_Models")
ggsave("Results/Images/H4_Models/Dissimilarity_10.png", g$Dissimilarity,
       dpi = 600, units = "px", width = 4800,
       height = 4000, scale = 1.7)
ggsave("Results/Images/H4_Models/Overprediction_10.png", g$Overprediction,
       dpi = 600, units = "px", width = 4800,
       height = 4000, scale = 1.7)


#### Plot panel by response - Only spatial predictors ####
#Order and name of predictors
p_spatial <- c("nslatitudinal_eingevectordfEQ31",
               "nslatitudinal_eingevectordfEQ32", "nslatitudinal_eingevectordfEQ33",
               "nslongitudinal_eingevectordfEQ31", "nslongitudinal_eingevectordfEQ32",
               "nslongitudinal_eingevectordfEQ33")

n_spatial <- c("Lat_EQ31",
               "Lat_EQ32", "Lat_EQ33",
               "Long_EQ31", "Long_EQ32",
               "Long_EQ33")
#Plot
g_spatial <- lapply(r, function(i){
  print(i)
  #i = r[1]

  d_i <- d %>% filter(Response == i)

  #Remove eingevectors
  d_i <- d_i %>% filter(grepl("EQ", Predictor))

  #Order factors
  d_i$Predictor <- factor(d_i$Predictor, levels = p_spatial, labels = n_spatial)
  d_i$Resolution <- factor(d_i$Resolution,
                           levels = c(2.5, 5, 10, 30, 60),
                           labels <- c("2.5arc-min", "5arc-min", "10arc-min",
                                       "30arc-min", "60arc-min"))
  d_i$model_type <- factor(d_i$model_type,
                           levels = c("sesam_only", "only_threshold_min",
                                      "only_threshold10", "thresholmin_and_sesam",
                                      "threshold10_and_sesam"),
                           labels = c("SESAM", "Minimum threshold", "10% threshold",
                                      "Min. thr. and SESAM", "10% thr. and SESAM"))
  # Criar um dataset auxiliar com os valores de R² para cada facet
  r2_labels <- d_i %>%
    group_by(model_type, Resolution) %>%
    summarise(R2_text = paste0("R² = ", round(unique(r2), 2)), .groups = "drop",
              y = max(Estimate))

  #Get min y by resolution
  r2_labels <- r2_labels %>% group_by(Resolution) %>% mutate(y_min = max(y))

  #Plot
  ggplot(d_i, aes(x = Predictor, y = Estimate, color = direction)) +
    geom_pointrange(aes(ymin = Q5, ymax = Q95),
                    fatten = 1, linewidth = 1.5, alpha = 0.4) +
    geom_pointrange(aes(ymin = Q11, ymax = Q89),
                    fatten = 2, linewidth = 3.5, alpha = 0.7) +
    geom_hline(yintercept = 0, linewidth = 1, colour = "darkgray", linetype = "dashed") +
    scale_colour_manual(values = c("Positive" = "#D55E00",
                                   "Negative" = "#0072B2",
                                   "Null" = "darkgrey")) +
    #Add r2
    coord_flip() +
    labs(x = NULL,
         y = "Predictor Effect (Estimate)") +
    ggtitle(i) +
    #theme_new() +
    ggpubr::theme_pubclean() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(vjust = 0.5, hjust = 0.7),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12, colour = "black"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    facet_grid(model_type ~ Resolution, scales = "free") +
    #   # # Adiciona os valores de R2 em cada facet
    geom_text(data = r2_labels, aes(x = "Long_EQ33", y = Inf, label = R2_text),
              hjust = 1, vjust = 0.2, inherit.aes = FALSE, size = 3.2)
})
names(g_spatial) <- r
g_spatial
#Save
ggsave("Results/Images/H4_Models/Dissimilarity_only_spatial.png", g_spatial$Dissimilarity,
       dpi = 600, units = "px", width = 4800,
       height = 4000, scale = 1.4)
ggsave("Results/Images/H4_Models/Overprediction_only_spatial.png", g_spatial$Overprediction,
       dpi = 600, units = "px", width = 4800,
       height = 4000, scale = 1.4)
