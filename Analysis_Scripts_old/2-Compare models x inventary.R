####Beta-diversidade: Modelos x invent?rio####
#TSS calculates following: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5288248/#:~:text=The%20true%20skill%20statistics%20is,1997%3B%20Table%201.).

# #Raster base
# dir.create("Rasters")
# r <- rast("C:/Users/wever/OneDrive - ufpr.br/GitHub/Paper_Modelagem_flexsdm/Models/Maxent/Abarema langsdorffii.tif")
# r <- crop(r, sc, mask = T)
# plot(r)
#
# r[!is.na(r)] <- 1:length(r[!is.na(r)])
# plot(r)
# writeRaster(r, filename = "Rasters/base.tiff")

#Carregar pacotes
library(betapart)
library(dplyr)
library(terra)
library(geobr)
library(pbapply)
library(data.table)
library(terra)
library(pROC)
library(mapview)

#Create directory to save
dir.create("Results/ModelsxInventary")

#Get functions
source("Analysis_Scripts/helpers.R")

####Importar dados necess?rios####
variables <- rast("Rasters/Variables.tiff")

#Invent?rio
inv <- read.csv("Results/Inventario_SC/PAM_SC_15ago22.csv")

#Import info on forest patch area
invdata <- fread("Results/Inventario_SC/Tamanho Unidades Amostrais IFFSC.csv",
                 encoding = "Latin-1")

#Get raster base
r <- rast("Rasters/base.tiff")

#Get list of PAMS
pam_list <- list.files(path = "Results/PAM/", full.names = TRUE)
#Read
pams <- pblapply(pam_list, fread)
#Names
names(pams) <- pam_list %>% fs::path_ext_remove() %>% basename()

####Looping 2.5 arc-min ####
res_2.5 <- lapply(names(pams), function(x) {
  message(paste0("Running ", x))
  pams_x <- pams[[x]]
  #Match pam and inventary UAs
  pam_inv <- match_inv_pam(inv = inv, pam = pams_x, r = r)
  #Get metrics
  m <- calc_metrics(data = pam_inv, name = x, r = r, resolution = "2.5arc_min")
  return(m)
}) %>% bind_rows()

####Looping 5 arc-min ####
fact5 <- round(0.0833333/res(r),0)
r5 <- aggregate(r, fact = fact5)

res_5 <- lapply(names(pams), function(x) {
  message(paste0("Running ", x))
  pams_x <- pams[[x]]
  #Match pam and inventary UAs
  pam_inv <- match_inv_pam(inv = inv, pam = pams_x, r = r5)
  #Get metrics
  m <- calc_metrics(data = pam_inv, name = x, r = r5, resolution = "5arc_min")
  }) %>% bind_rows()

####Looping 10 arc-min ####
fact10 <- round(0.166667/res(r), 0)
r10 <- aggregate(r, fact = fact10)

res_10 <- lapply(names(pams), function(x) {
  message(paste0("Running ", x))
  pams_x <- pams[[x]]
  #Match pam and inventary UAs
  pam_inv <- match_inv_pam(inv = inv, pam = pams_x, r = r10)
  #Get metrics
  m <- calc_metrics(data = pam_inv, name = x, r = r10, resolution = "10arc_min")
}) %>% bind_rows()

####Looping 30 arc-min ####
fact30 <- round(0.5/res(r), 0)
r30 <- aggregate(r, fact = fact30)

res_30 <- lapply(names(pams), function(x) {
  message(paste0("Running ", x))
  pams_x <- pams[[x]]
  #Match pam and inventary UAs
  pam_inv <- match_inv_pam(inv = inv, pam = pams_x, r = r30)
  #Get metrics
  m <- calc_metrics(data = pam_inv, name = x, r = r30, resolution = "30arc_min")
}) %>% bind_rows()

####Looping 60 arc-min ####
fact60 <- round(1/res(r), 0)
r60 <- aggregate(r, fact = fact60)

res_60 <- lapply(names(pams), function(x) {
  message(paste0("Running ", x))
  pams_x <- pams[[x]]
  #Match pam and inventary UAs
  pam_inv <- match_inv_pam(inv = inv, pam = pams_x, r = r60)
  #Get metrics
  m <- calc_metrics(data = pam_inv, name = x, r = r60, resolution = "60arc_min")
}) %>% bind_rows()

#Merge all results
all_res <- bind_rows(res_2.5, res_5, res_10, res_30, res_60)
#Save
fwrite(all_res, "Results/ModelsxInventary/Results.gz", row.names = FALSE,
       compress = "gzip")
fwrite(all_res, "Results/ModelsxInventary/Results.csv", row.names = FALSE)

#### Which model is better to predict richness and composition? ####
library(dplyr)
library(data.table)
library(pbapply)

#Import results
res <- fread("Results/ModelsxInventary/Results.gz") %>% as.data.frame()
#Get unique combination of model and resolution
mr <- res %>% dplyr::select(Type, Resolution) %>% distinct()

#For each combination, get:
#Correlation between observed and predicted richness
#Average of dissimilarity metrics:
dm <- c("TSS", "Turnover_sor",
        "Nestedness_sor", "BetaDiv_sor", "Turnover_jac", "Nestedness_jac",
        "BetaDiv_jac")

s <- pblapply(1:nrow(mr), function(i){
  m_i <- mr$Type[i]
  r_i <- mr$Resolution[i]
  #Subset model and resolution from results
  res_i <- res %>% filter(Type == m_i, Resolution == r_i)
  #Correlation between observed and predicted richness
  rich_cor <- cor(res_i$Rich_obs, res_i$Rich_pred)
  #Mean of metrics od dissimilarity
  mb <- apply(res_i[dm], 2, mean, na.rm = TRUE) %>% as.data.frame() %>% t()
  #Save final results
  res_final <- data.frame("Type" = m_i,
                          "Resolution" = r_i,
                          Rich_cor = rich_cor,
                          mb)
  return(res_final)
}) %>% bind_rows()
#Save results
fwrite(s, file = "Results/ModelsxInventary/Summary_Results.gz",
       compress = "gzip", row.names = FALSE)
