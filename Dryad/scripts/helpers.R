library(foreach)
library(doParallel)

#Version of ecospat.SESAM parallelized
ecospat.SESAM.parallel <- function(proba, sr, cores = 6) {
  projSR <- round(as.vector(sr[[1]]))
  new.prob.prr <- proba
  dataSSDM_p <- proba

  # Initialize parallel backend
  cl <- parallel::makeCluster(cores)

  #Set rogress bar
  n_tot <- nrow(proba)
  pb <- txtProgressBar(min = 0, max = n_tot, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  #Register cluster
  doSNOW::registerDoSNOW(cl)

  # Define the foreach loop with progress bar
  new.prob.prr <- foreach(i = 1:nrow(proba), .combine = rbind,
                          .options.snow = opts) %dopar% {
    SR <- projSR[i]
    if (SR > 0) {
      predcom <- dataSSDM_p[i, ]
      predcom_p <- as.numeric(dataSSDM_p[i, ])
      com <- order(predcom_p, decreasing = TRUE)
      pres <- com[1:SR]
      predcom[, pres] <- 1
      predcom[, -pres] <- 0
    } else {
      predcom <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(dataSSDM_p)))
      colnames(predcom) <- colnames(dataSSDM_p)
    }
    predcom
  }
  # Stop parallel backend
  parallel::stopCluster(cl)

  return(new.prob.prr)
}

#Implement the SESAM framework to predict community composition using a 'probability ranking' rule.
only_sesam <- function(pam){
  #Determine maximum species richness in each site (which is the sum of probabilities in each site)
  sr <- pam %>% dplyr::select(!c(x, y)) %>%
    rowSums(na.rm = TRUE) %>%
    as.data.frame()
  colnames(sr) <- "Richness"

  #Remove columns
  proba <- pam[, 3:ncol(pam)] %>% as.data.frame()

  #Implement the SESAM framework to predict community composition
  #using a 'probability ranking' rule.

  ppr <- ecospat.SESAM.parallel(proba = proba, sr = sr,
                                cores = 8)
  #Merge coordinate data
  cd <- pam %>% dplyr::select(x, y)
  com.rank <- bind_cols(cd, ppr)

  # #Check (change values to see)
  # vid <- 100
  # ppr.id %>% filter(ID == vid) %>% View()
  # com.rank %>% filter(ID == vid) %>% View()
  # cd %>% filter(ID == vid) %>% View()

  # #Merge with sites with 0 species
  # no.species <- pam %>%  #Use dataframe where probabilites at species records are set to 1
  #   rownames_to_column('ID') %>%
  #   filter((rownames(pam[, 3:ncol(pam)]) %in% sr.out))
  # #Merge
  # com.all <- rbind(com.rank, no.species)
  return(com.rank)
}

#Apply threshold
apply_thr <- function(species, metric, thr, pam) {
  for (i in species) {
    #Get threshold
    thr_i <- metric[[thr]][metric$species == i]
    pam[[i]][which(pam[[i]] < thr_i)] <- 0
  }
  return(pam)
}

#Binarize
bin <- function(df){
  df_new <- df
  # Loop sobre cada coluna, exceto as duas primeiras (x e y)
  for (col in names(df_new)[-c(1, 2)]) {
    df_new[[col]][df[[col]] > 0] <- 1
  }
  return(df_new)
}

#Match UAs: inventary x PAM
match_inv_pam <- function(inv, pam, r) {
  #From inventary, create new UA based on cells (merge some cells)
  new_inv <- extract(r, inv[, c("x", "y")], cells = TRUE) %>% pull(cell)
  new_inv <- inv %>% dplyr::mutate(ID = new_inv, .before = 1)
  #Concatenate UA and keep maximum value
  new_inv2 <- new_inv %>% group_by(ID) %>% mutate(UA = paste(UA, collapse=",")) %>%
    group_by(ID) %>%
    mutate(across(-c(UA, x, y), ~max(.))) %>%
    ungroup() %>%
    distinct(ID, .keep_all = TRUE)

  #Get ID and UAS in pam
  new_pam <- extract(r, pam[, c("x", "y")], cells = TRUE) %>% pull(cell)
  new_pam <- pam %>% dplyr::mutate(ID = new_pam, .before = 1)
  #Concatanate PAM to avoid duplicates
  new_pam2 <- new_pam %>% group_by(ID) %>%
    mutate(across(-c(x, y), ~max(.))) %>%
    ungroup() %>%
    distinct(ID, .keep_all = TRUE)
  #Get UAs
  new_pam2 <- left_join(new_inv2[, c("ID", "UA")], new_pam2)
  res <- list(inventary = new_inv2,
              pam = new_pam2)
  return(res)
}


calc_metrics <- function(data, name, r, resolution) {
  #Extract uas
  uas <- data$inventary$UA
  #Looping in UAS
  d.ua <- pblapply(uas, function(i){
        ua.id <- i
        inv_i <- data$inventary
        pam_i <- data$pam
        colnames(pam_i) <- gsub(" |-", ".", colnames(pam_i))
        #Extrair UA de inventário
        inv.id <- inv_i %>%
          filter(UA == ua.id) %>%
          dplyr::select(-x, -y, -UA, -ID) #Observado
        #Extrair UA de matriz PAM
        pam.id <- pam_i %>%
          filter(UA == ua.id) %>%
          dplyr::select(-x, -y, -UA, -ID) #Predito
        #Unir dados
        df.bp <- rbind(inv.id, pam.id) #Observado e predito
        obs.pred <- t(df.bp)  %>% as.data.frame %>% #Transpose
          dplyr::rename("obs" = V1, "pred" = V2)
        #Calcular riqueza observada e predita
        riq.obs <-rowSums(inv.id)
        riq.pred <- rowSums(pam.id)

        #Calcular True Positive (TP), True Negatives (TN), False Positive (FP) e False Negative (FN) considerando observado
        obs <- subset(obs.pred, obs == 1)
        no.obs <- subset(obs.pred, obs == 0)

        TP <- obs.pred %>% filter(obs == 1 & pred == 1) %>% nrow()
        TN <- obs.pred %>% filter(obs == 0 & pred == 0) %>% nrow()
        FP <- obs.pred %>% filter(obs == 0 & pred == 1) %>% nrow()
        FN <- obs.pred %>% filter(obs == 1 & pred == 0) %>% nrow()

        TPR <- TP/(TP + FN) #sensitivity
        TNR <- TN/(TN + FP) #specificity

        TSS <- TPR + TNR - 1

        #Sensitivity (True Positive Rate) e specificity (True Negative Rate)
        sensitivity_TPR <- TP/(TP + FN) #TPR
        specificity_TNR <- TN/(TN + FP) #TNR

        #Predicted-observades ratio
        richness_ratio <- riq.pred/riq.obs

        #Calcular beta-diversidade (com Sorensen)
        bp.s <- beta.pair(df.bp, index.family="sorensen")
        #Calcular beta-diversidade (com Jaccard)
        bp.j <- beta.pair(df.bp, index.family="jaccard")

        #Get values of variables
        ua.xy <- inv_i %>% filter(UA == ua.id) %>%
          dplyr::select(x, y) %>% as.data.frame()
        #Change resolution if necessary
        fact_res <- res(r)/res(variables)
        if(fact_res[1] > 1) {
        v <- terra::aggregate(variables,
                               fact = fact_res, fun = mean, na.rm = TRUE)
        } else { v <- variables}

        v.ua <- terra::extract(x = v, y = ua.xy) %>%
          dplyr::select(-ID)


        #Get size of forest patch
        all_uas <- strsplit(as.character(ua.id), ",") %>% unlist() %>% as.numeric()
        forest_area <- invdata %>% filter(UA %in% all_uas) %>%
          pull(`Área com floresta (m²)`)
        if(length(forest_area) == 0) {
          forest_area <- 4000
        }

        #Criar dataframe
        df <- data.frame("Longitude" = ua.xy$x,
                         "Latitude" = ua.xy$y,
                         "Type" = name,
                         "Resolution" = resolution,
                         "UA" = ua.id,
                         "Rich_obs" = riq.obs,
                         "Rich_pred" = riq.pred,
                         "RichDiff" = riq.pred - riq.obs,
                         "Rich_ratio" = richness_ratio,
                         "TP" = TP,
                         "TN" = TN,
                         "FP" = FP,
                         "FN" = FN,
                         "Sensitivity_TPR" = sensitivity_TPR,
                         "Specificity_TNR" = specificity_TNR,
                         "TSS" = TSS,
                         "Turnover_sor" = bp.s$beta.sim[1],
                         "Nestedness_sor" = bp.s$beta.sne[1],
                         "BetaDiv_sor" = bp.s$beta.sor[1],
                         "Turnover_jac" = bp.j$beta.jtu[1],
                         "Nestedness_jac" = bp.j$beta.jne[1],
                         "BetaDiv_jac" = bp.j$beta.jac[1],
                         "Forest_patch_area" = forest_area,
                         v.ua)
      return(df)})
  return(bind_rows(d.ua))
  }

