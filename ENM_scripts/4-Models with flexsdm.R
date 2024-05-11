#### ENSEMBLE DE MODELOS COM FLEXSDM####
#Veja mais em: https://sjevelazco.github.io/flexsdm/index.html

#Carregar pacotes
library(raster)
#library(ENMeval)
library(flexsdm)
library(dplyr)
library(pbapply)
library(geodist)
library(rgdal)
library(terra)
library(sf)
#library(mapview)
library(spData)
library(rgeos)
library(parallel)
library(dismo)
library(virtualspecies)
library(ks)
library(spatialEco)
library(ellipsenm)
library(tibble)
library(geobr)
library(tidyr)
library(kuenm)
library(data.table)
library(purrr)
#Definir diret?rio
#setwd("A:/OneDrive - ufpr.br/GitHub/Paper_Modelagem_flexsdm/")
#No linux
setwd("../home/weverton/Atlantic_Forest_Modelling")
getwd()
##Importar ocorrencias
pts <- read.csv("Ocorrencias.csv")
#Criar coluna indicando presen?a da esp?cie
pts$pr_ab <- 1

#Importar ocorr?ncias (sem filtros) para gerar pontos de background e de pseudo-aus?ncia
pts.all <- fread("G.Pontos_Filtrados_Neotropicos.csv")
pts.all <- pts.all %>% dplyr::select(species, x = decimalLongitude.new1,
                                     y = decimalLatitude.new1)
head(pts.all)
#####Load necessary objects####
data("world") #Continents
#Environmental layers
env <- rast(raster::stack(list.files("Rasters/M_extent/",full.names = TRUE)))
#See variables
names(env)
#Import sampling effort file
sum_records<- read.csv("Sampling_effort/Sampling.csv")
#Convert M variables to dataframe
#m.df <- as.data.frame(env, xy = TRUE, na.rm= TRUE)

####Run models in looping with lapply####
#Get species
spp <- unique(pts$species)
#spp <- c("Araucaria angustifolia", "Myrcia racemosa", "Persea venosa")
#Criar diret?rios para salvar modelos
dir.create("Models")
dir.create("Models/Maxent")
dir.create("Models/Ensemble")
dir.create("Models/Metrics")
dir.create("Models/Training_test_Points")

#Criar base para plotar mapa nos Neotropicos
neot <- env[[1]]
neot[!is.na(neot)] <- 0
#plot(neot)

#Ver esp?cies prontas
sp.ready <- list.files("Models/Ensemble")
sp.ready <- gsub("\\.tif", "", sp.ready)
sp.out <- setdiff(spp, sp.ready)
spp.miss <- data.frame("species" == sp.out)
write.csv(spp.miss, "Models/sppMissing.csv", row.names =F)

spp.miss <- read.csv("Models/sppMissing.csv")
spp <- unique(spp.miss$species)

#Criar fun??o para lidar com erros das fun??es (cria objeto vazio ao inv?s de interromper looping quando n?o consegue usar algum algoritmo)
tune_max2 <- possibly(.f = tune_max, otherwise = NULL)
tune_raf2 <- possibly(.f = tune_raf, otherwise = NULL)
tune_gbm2 <- possibly(.f = tune_gbm, otherwise = NULL)
tune_svm2 <- possibly(.f = tune_svm, otherwise = NULL)

#Dividir esp?cies
spp <- spp[1:1600]

pblapply(seq_along(spp), function(i) {
  tryCatch(
    {sp <- spp[i]
    print(sp)
    #Obter ocorr?ncias
    occ <- pts %>% filter(species == sp) #Pontos filtrados ambientalmente
    occ.all <- pts.all %>% filter(species == sp) #Pontos sem filtros

    ####Delimit calibration area (M)####
    #See more in: https://sjevelazco.github.io/flexsdm/reference/calib_area.html

    #Usando dist?ncia m?nima do ponto que est? mais longe * 1.5 (ideia baseada na fun??o msdm_posteriori(method = 'obr') do pacote flexsdm
    occ.pt <- terra::vect(as.matrix(occ.all[,2:3])) #Vectorizar pontos
    crs(occ.pt) <- "+init=epsg:4326" #Definir crs
    all.dist <- occ.pt %>% #Obter distancia entre todos os pontos
      terra::distance() %>% as.matrix() %>%
      data.frame()
    all.dist[all.dist == 0] <- NA #Distancia 0 = NA
    #Obter distancia m?nima entre todos os pontos
    distmin <- apply(all.dist, 1, function(x) {
      min(x, na.rm = TRUE)
    })
    set.dist <- (max(distmin)/1000)*2
    #Delimit M
    ca <- calib_area(data = occ.all, x ="x", y= "y", #ca = Calibration Area
                     method = c('bmcp', width= set.dist*1000),
                     crs = crs(env))
    #Cut raster to continent extent
    ca <- vect(gIntersection(as_Spatial(st_as_sf(ca)), as_Spatial(world)))
    #plot(ca)

    ####Dividir pontos em teste e treino####
    set.seed(42)
    env.ca <- mask(crop(env, ca), ca)
    occ_part <- occ %>%
      part_sblock(
        data = .,
        env_layer = env.ca,
        pr_ab = "pr_ab",
        x = "x",
        y = "y",
        n_part = 4,
        min_res_mult = 2,
        max_res_mult = 200,
        num_grids = 30,
        prop = 0.5,
        min_occ = round(nrow(occ)/4,0) - round(nrow(occ)*0.2,0)
      )
    occ_pf <- occ_part$part
    #table(occ_pf$.part)

    # Transform best block partition to a raster layer with same resolution and extent than
    # predictor variables
    block_layer <- get_block(env_layer = env, best_grid = occ_part$grid)

    ###Sample background points based on bias file and distant from occurrence points

    #Cut samplig efforts file
    rec.r <- raster::rasterize(x = sum_records[,1:2],
                               y = raster(env[[1]]),
                               field = sum_records[,3],
                               background = 0)
    rec.r <- mask(crop(rec.r, as(ca, "Spatial")), as(ca, "Spatial"))
    rec.df <- as.data.frame(rec.r, xy = TRUE, na.rm= T)
    # Extract coordinates.
    coords <- cbind(rec.df[,1], rec.df[,2])

    # Create a scale.
    scale <- length(rec.df[,3]) / sum(rec.df[,3])
    rec.df[,4] <- rec.df[,3] * scale

    # Do a 2d kernel density estimation.
    target_density <- kde(coords, w=rec.df[,4])

    # Create raster.
    target_raster <- raster(target_density)


    # Clip data to the same resolution/extent.
    target_raster <- resample(target_raster, raster(env[[1]]), method='bilinear')

    # Normalize bias file between 0 and 1.
    target_raster <- target_raster - minValue(target_raster)
    bias <- raster.transformation(target_raster, trans="norm")
    bias <- mask(crop(rast(bias), ca), ca)
    #plot(bias)

    #Para pseudo-absences n?o pegar pontos pr?ximos aos pontos de ocorr?ncia
    b <- buffer_area(occ.all, longitude = "x", latitude = "y", buffer_distance = 15) %>%
      vect()
    pa.bias <- mask(bias, b, inverse = T) #Background para extrair pseudo-aus?ncias
    #plot(pa.bias)

    # Spatial blocks where species occurs
    #Obter n?mero de c?lulas em cada bloco
    n_cells <- block_layer %>% values() %>% table()

    # Sample background points throughout study area with biased method, allocating 100X the number of presences a background
      bg <- lapply(1:4, function(i) {
      set.seed(42)
      sample_background(
        data = occ_pf,
        x = "x",
        y = "y",
        n = n_cells[i] * 0.05, #Obter amostra de 5% de cada bloco
        method = "biased",
        rlayer = block_layer,
        maskval = i,
        calibarea = ca,
        rbias = bias #Para background, usar todo background
      )
    }) %>%
      bind_rows()
    bg <- sdm_extract(data = bg, x = "x", y = "y", env_layer = block_layer, filter_na = TRUE)

    #Extract variables to background
    bg <- bg %>%
      sdm_extract(
        data = .,
        x = "x",
        y = "y",
        env_layer = env,
        filter_na = TRUE
      )

    #Criar pseudo-aus?ncias (targe + distance from occurrence points)
    # Sample a number of pseudo-absences equal to the presence in each partition
    set.seed(42)
    psa <- lapply(1:4, function(i) {
      sample_background(
        data = occ_pf,
        x = "x",
        y = "y",
        n = sum(occ_pf$.part == i),
        method = "biased",
        rlayer = block_layer,
        maskval = i,
        calibarea = ca,
        rbias = pa.bias
      )
    }) %>%
      bind_rows()
    psa <- sdm_extract(data = psa, x = "x", y = "y", env_layer = block_layer)

    # Bind a presences and pseudo-absences
    occ_pa <- bind_rows(occ_pf, psa)
    #Extract variables
    occ_pa <- occ_pa %>%
      sdm_extract(
        data = .,
        x = "x",
        y = "y",
        env_layer = env,
        filter_na = TRUE
      )


    ####Tune Maxent####
    max_t <- tune_max2(
      data = occ_pa,
      response = "pr_ab",
      predictors = names(env),
      background = bg,
      partition = ".part",
      grid = expand.grid(
        regmult = c(0.1, seq(0.5, 3, 0.5)),
        classes = c("l", "lq", "lqh", "lqhp")
      ),
      thr = c("max_sens_spec", 'sensitivity', sens='0.9'),
      metric = "TSS",
      clamp = TRUE,
      pred_type = "cloglog",
      n_cores = 6
    )
    #max_t$hyper_performance %>% View()

    ####Tune Random Forest####
    rf_t <-
      tune_raf2(
        data = occ_pa,
        response = "pr_ab",
        predictors = names(env),
        partition = ".part",
        grid = expand.grid(mtry = seq(1, 9, 1)),
        thr = 'max_sens_spec',
        metric = "TSS",
        n_cores = 6
      )
    #rf_t$hyper_performance %>% View()


    ####Tune  Generalized Boosted Regression####
    gbm_t <-
      tune_gbm2(
        data = occ_pa,
        response = "pr_ab",
        predictors = names(env),
        partition = ".part",
        grid = expand.grid(
          n.trees = c(20, 50, 100),
          shrinkage = c(0.1, 0.5, 1),
          n.minobsinnode = c(1, 3, 5, 7, 9)
        ),
        thr = "max_sens_spec",
        metric = "TSS",
        n_cores = 6
      )
    #gbm_t$performance %>% View()

    ####Tune Suport vector machine####
    svm_t <-
      tune_svm2(
        data = occ_pa,
        response = "pr_ab",
        predictors = names(env),
        partition = ".part",
        grid = expand.grid(
          C = c(2, 4, 8, 16, 20),
          sigma = c(0.01, 0.1, 0.2, 0.3, 0.4)
        ),
        thr = "max_sens_spec",
        metric = "TSS",
        n_cores = 6
      )
    #svm_t$performance %>% View()


    ####Ensemble models####
    m.list <- list(max_t, rf_t, svm_t, gbm_t) %>% discard(is.null)

    mensemble <- fit_ensemble(
      models = m.list,
      ens_method = "meanw",
      thr = c('max_sens_spec', 'sensitivity', sens = 0.9),
      thr_model = "max_sens_spec",
      metric = "TSS"
    )

    #Predizer modelos para maxent
    pr_max <- sdm_predict(
      models = max_t,
      pred = env,
      thr = NULL,
      con_thr = TRUE,
      predict_area = ca
    )

    #Predizer modelos para ensemble
    pr_ens <- sdm_predict(
      models = mensemble,
      pred = env,
      thr = NULL,
      con_thr = TRUE,
      predict_area = ca
    )

    #Criar modelos finais (nos neotropicos)
    max_neot <- mosaic(pr_max$max, neot, fun = "max")
    ens_neot <- mosaic(pr_ens$meanw, neot, fun = "max")
    #Salvar rasters de modelos
    writeRaster(max_neot, paste0("Models/Maxent/", sp, ".tif"))
    writeRaster(ens_neot, paste0("Models/Ensemble/", sp, ".tif"))

    #Salvar m?tricas
    #Maxent
    thr10 <- max_t$data_ens %>% filter(pr_ab == 1) %>% dplyr::select(pred) %>%
      pull() %>% quantile(., 0.1) #Obter threshold de 10%
    max_met_maxTSS <- max_t$performance %>% mutate("species" = sp) %>%
      dplyr::select(species, everything())
    max_met_Spec10 <- max_t$hyper_performance %>%
      filter(regmult == max_met_maxTSS$regmult,
             classes == max_met_maxTSS$classes,
             threshold == "sensitivity") %>% mutate("species" = sp,
                                                    "thr_value" = thr10,
                                                    "n_presences" = nrow(occ),
                                                    "n_absences" = nrow(occ))
    max_met <- rbindlist(list(max_met_maxTSS, max_met_Spec10), fill = T)

    #Ensemble
    ens_met <- mensemble$performance %>% mutate("species" = sp) %>%
      dplyr::select(species, everything())
    #Merge metrics
    met <- rbindlist(list(max_met, ens_met), fill = T)
    met$model[which(met$model =="max")] <- "Maxent"
    met$model[which(met$model =="meanw")] <- "Ensemble"
    #Salvar
    write.csv(met, paste0("Models/Metrics/", sp, ".csv"))
    #Salvar pontos de treino e de teste
    write.csv(occ_pa[,1:4], paste0("Models/Training_test_Points/", sp, ".csv"))

    gc()}, #Free unused ram memory
    error = function(msg){ #End of trycatch
      message(paste("Error for:", sp))
      return(NA)
    })
})

####Adicionar m?trica de threshold de 10%####
#Importar esp?cies
#Ver esp?cies prontas
sp.ready <- list.files("Models/Ensemble")
spp <- gsub("\\.tif", "", sp.ready)

pblapply(seq_along(spp), function(i) {
sp <- spp[i]
#Importar pontos de ocorr?ncia
occ <- read.csv(paste0("Models/Training_test_Points/", sp, ".csv")) %>%
  dplyr::select(x, y, pr_ab) %>% filter(pr_ab == 1)
m <- rast(paste0("Models/Maxent/",sp, ".tif"))
occ.m <- sdm_extract(occ, x = "x", y = "y", m)
thr10 <- quantile(occ.m$max, 0.1) %>% as.numeric()
#Recriar dataframe com m?tricas
met <- read.csv(paste0("Models/Metrics/", sp, ".csv"))
new.met <- met[which(met$model == "Maxent"),]
new.met$threshold <-  "sensitivity"
new.met$thr_value <- thr10

final.met <- rbind(met, new.met)
write.csv(final.met, paste0("Models/Metrics/", sp, ".csv")) })

####Save metrics in single dataframe####
#Set directory
my_dir <- "C:/Users/wever/OneDrive - ufpr.br/GitHub/Paper_Modelagem_flexsdm/Models"
#Import and merge metrics
met <- list.files(path = file.path(my_dir, "Metrics"), pattern = ".csv",
                  full.names = TRUE)
met <- pblapply(met, fread)
met <- bind_rows(met)
#Remove columns
met$V1 <- NULL
met$X <- NULL
#Save in Results
dir.create(file.path(my_dir, "Results"))
fwrite(met, file = file.path(my_dir, "Results", "Metrics.gz"), compress = "gzip", row.names = FALSE)

####Assemble community####
#Get species
spp <- unique(met$species)
#List files - Maxent
maxent_list <- list.files(path = file.path(my_dir, "Maxent"), pattern = ".tif",
                     full.names = TRUE)
#Rasterize
maxent_res <- rast(maxent_list)
#Rename models with species
names(maxent_res) <- maxent_list %>% basename() %>% fs::path_ext_remove()

#Import map of Santa Catarina (and set 15km of buffer)
sc <- read_state(code_state = "SC") %>% vect() %>% buffer(width = 15*1000)
#Cut maps to SC
maxent_res <- crop(maxent_res, sc, mask = TRUE)
#Plot to check
maxent_res[[1:10]] %>% terra::plot()
#Convert to dataframe
max_df <- as.data.frame(maxent_res, xy = TRUE)
#Save
fwrite(max_df, file = file.path(my_dir, "Results", "Maxent_PAM.gz"),
       compress = "gzip", row.names = FALSE)

#List files - ensemble
ensemble_list <- list.files(path = file.path(my_dir, "Ensemble"), pattern = ".tif",
                          full.names = TRUE)
#Rasterize
ensemble_res <- rast(ensemble_list)
#Rename models with species
names(ensemble_res) <- ensemble_list %>% basename() %>% fs::path_ext_remove()

#Cut maps to SC
ensemble_res <- crop(ensemble_res, sc, mask = TRUE)
#Plot to check
ensemble_res[[1:10]] %>% terra::plot()
#Convert to dataframe
ens_df <- as.data.frame(ensemble_res, xy = TRUE)
#Save
fwrite(ens_df, file = file.path(my_dir, "Results", "Ensemble_PAM.gz"),
       compress = "gzip", row.names = FALSE)
