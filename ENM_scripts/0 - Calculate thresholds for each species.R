#### Calculate thresholds for each species ####
#Minimum presence
#10% threshold
library(terra)
library(pbapply)
library(dplyr)

#Get all species
spp <- list.dirs("kuenm_models/", recursive = FALSE, full.names = FALSE)

#Get all fitted models
spp_fit <- list.files("kuenm_models/", pattern = "itt.*.RDS|itt.*.rds",
                      recursive = TRUE, full.names = TRUE)


m <- pblapply(spp_fit, function(i){
  sp <- gsub("kuenm_models/", "", dirname(i))
  print(sp)
  m_sp <- readRDS(i)
  #Get calibration_data on occurrences
  occ_sp <- m_sp$calibration_data[m_sp$calibration_data$pr_bg == 1,]
  #Predict best models to occurrence
  best_models <- m_sp$Models
  #Get models names
  nm <- names(best_models)

  #Predict models to occurrences - Per replicate
  p_occ <- lapply(nm, function(x){
    m_x <- best_models[[x]]
    #Remove full model if replicates exist
    if(any(grepl("Rep", names(m_x)))){
      m_x$Full_model <- NULL
    }
    #Predict by replicate
    p_r <- sapply(m_x, function(i){
      predict.glmnet_mx(object = i, newdata = occ_sp, type = "cloglog")
    })
    #Get median
    p_median <- apply(p_r, 1, median, na.rm=T)
    return(list(median = p_median))
  })
  names(p_occ) <- nm
  #Get consensus
  median_consensus <- apply(sapply(p_occ, function(x) x$median),
                            1, median, na.rm = TRUE)

  #Calculate threshold
  thr10 <- kuenm2:::calc_thr(occ_suitability = median_consensus, thr = 0.1)
  thr_min <- min(median_consensus[median_consensus > 0])
  df <- data.frame(species = sp, thr10 = thr10, thr_min = thr_min)
  return(df)
  })
mthr <- bind_rows(m)
#Save
write.csv(mthr, "Results/Thresholds.csv", row.names = FALSE)
