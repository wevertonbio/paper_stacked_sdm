# #### Subset models from my kuenm models ####
# library(dplyr)
# library(pbapply)
#
# #All models#
# allm <- list.dirs("c:/Users/wever/Desktop/kumodels/kuenm_models/",
#                   full.names = FALSE, recursive = FALSE)
# head(allm)
#
# #Get species from inventary
# inv <- read.csv("Results/Inventario_SC/PAM_SC_15ago22.csv")
# #Update colnames
# colnames(inv) <- gsub("\\.", "_", colnames(inv))
# colnames(inv)[which(colnames(inv) == "Erythrina_crista_galli")] <- "Erythrina_crista-galli"
# colnames(inv)[which(colnames(inv) == "Solanum_sanctae_catharinae")] <- "Solanum_sanctae-catharinae"
# colnames(inv)[which(colnames(inv) == "Aiouea_hirsuta")] <- "Cinnamomum_hirsutum"
# #Save
# write.csv(inv, "Results/Inventario_SC/PAM_SC_04jul24.csv", row.names = FALSE)
#
#
# spp_inv <- inv %>% dplyr::select(-x, -y, -UA) %>% colnames()
#
#
# #Species out
# spp_out <- setdiff(spp_inv, allm)
# spp_out #Must be 0
#
# #Get folders
# spp_folders <- paste0("C:/Users/wever/Desktop/kumodels/kuenm_models/", spp_inv)
# #Copy folders
# file.copy(from = spp_folders, to = "kuenm_models/", recursive = TRUE,
#           overwrite = TRUE)

#### Assembly models in a Presence-Absence Matrix in SC ####
library(geobr)
library(terra)
library(data.table)

#Check species
spp <- list.dirs("kuenm_models/", recursive = FALSE, full.names = FALSE)
#Inventary species
inv <- fread("Results/Inventario_SC/PAM_SC_04jul24.csv")
spp_inv <- inv %>% dplyr::select(-x, -y, -UA) %>% colnames()
setdiff(spp_inv, spp)

#Get all rasters
r <- list.files(path = "kuenm_models/", pattern = "Median.tif",
                full.names = F, recursive = TRUE)
spp_r <- dirname(r)
setdiff(spp, spp_r)

#Read rasters and cut using SC mask
#Import map of Santa Catarina (and set 15km of buffer)
sc <- read_state(code_state = "SC") %>% vect() %>% buffer(width = 15*1000)
plot(sc)

r_spp <- list.files(path = "kuenm_models/", pattern = "Median.tif",
                full.names = TRUE, recursive = TRUE)
r2 <- pblapply(r_spp, function(x){
  crop(rast(x), sc, mask = TRUE)
})
r3 <- rast(r2)
names(r3) <- gsub("kuenm_models/", "", dirname(r_spp))
#Convert to dataframe
pam <- as.data.frame(r3, xy = TRUE)
#See species without occurrence in SC
pam[,colSums(pam, na.rm = TRUE) == 0] %>% View()
#Save
dir.create("Results")
fwrite(pam, "Results/PAM_continuous.gz", row.names = FALSE, compress = "gzip")
