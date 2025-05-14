#Extract some results to text#
library(dplyr)
library(data.table)

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

#Get overprediction by resolution and method
ov <- df %>% group_by(Type, Resolution) %>% summarise(mean = mean(Rich_ratio))

#Get correlation between predicted and observed
#Spit data
dt <- split(df, df$Type)
correlations <- lapply(names(dt), function(i){
  dt_i <- dt[[i]]
  #Split by resolution
  r <- split(dt_i, dt_i$Resolution)
  r_c <- sapply(names(r), function(x){
    rx <- r[[x]]
    cor(rx$Rich_obs, rx$Rich_pred, method = "spearman")
  }) %>% as.data.frame()
  res <- r_c %>% rename("Correlation" = ".") %>%
    mutate(Type = i, Resolution = names(r), .before = 1)
  row.names(res) <- NULL
  res$Correlation <- round(res$Correlation, 2)
  res
}) %>% rbindlist()
