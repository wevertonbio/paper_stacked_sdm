####ASSEMBLE COMMUNITY USING MAXENT MODELS AND SESAM FRAMEWORK####

#Load packages
library(pbapply)
library(dplyr)
library(mapview)
library(terra)
library(ecospat)
library(geobr)
library(data.table)
library(foreach)
library(doParallel)
library(tibble)
#Import helpers
source("Analysis_Scripts/helpers.R")

#Create folder to save binarized communities
dir.create("Results/PAM/")

#Import metric results
res <- fread("Results/Metrics.gz")
spp <- unique(res$species) #Get species

####ONLY SESAM - MAXENT####
#See more in: https://www.rdocumentation.org/packages/ecospat/versions/3.2.1/topics/ecospat.SESAM.prr
pam_maxent <- fread("Results/Maxent_PAM.gz")

##Assemble community
sesam_maxent <- only_sesam(pam = pam_maxent)
#Save
fwrite(sesam_maxent, "Results/PAM/Maxent_Only_Ranking.gz", compress = "gzip",
       row.names = FALSE)
# #Rasterize some species to see presence
# sp <- "Araucaria angustifolia"
# r.base <- raster("Models/Maxent/Araucaria angustifolia.tif")
# sp.plot <- com.all %>% dplyr::select(x, y, sp) %>%
#   st_as_sf(coords = c("x", "y"), crs = 4326) %>%
#   rasterize(r.base)
# crs(sp.plot) <- "+init=epsg:4326"
# mapview(sp.plot[[2]])

###ONLY SESAM - ENSEMBLE####
pam_ensemble <- fread("Results/Ensemble_PAM.gz")
##Assemble community
sesam_ensemble <- only_sesam(pam = pam_ensemble)
#Save
fwrite(sesam_ensemble, "Results/PAM/Ensemble_Only_Ranking.gz", compress = "gzip",
       row.names = FALSE)

####USING THRESHOLDS: 10% and Maximum Sensitivity plus Specificity (maxTSS)- MAXENT####
#### Threshold of 10%
maxent_10 <- apply_thr(species = spp, metric = res, thr = "sensitivity",
                       pam = pam_maxent, m = "Maxent")
#Binarize
maxent_10_bin <- bin(maxent_10)
#Save results
fwrite(maxent_10_bin, "Results/PAM/Maxent_Only_Threshold10.gz", compress = "gzip",
       row.names = FALSE)

####Max TSS
maxent_maxtss <- apply_thr(species = spp, metric = res, thr = "max_sens_spec",
                       pam = pam_maxent, m = "Maxent")
#Binarize
maxent_maxtss_bin <- bin(maxent_maxtss)
#Save results
fwrite(maxent_maxtss_bin, "Results/PAM/Maxent_Only_ThresholdMaxTSS.gz", compress = "gzip",
       row.names = FALSE)

##Combining threshold and SESAM
#10%
maxent_10_sesam <- only_sesam(maxent_10)
fwrite(maxent_10_sesam, "Results/PAM/Maxent_Threshold10_and_Ranking.gz", compress = "gzip",
       row.names = FALSE)
#Max TSS
maxent_maxtss_sesam <- only_sesam(maxent_maxtss)
fwrite(maxent_maxtss_sesam, "Results/PAM/Maxent_ThresholdMaxTSS_and_Ranking.gz", compress = "gzip",
       row.names = FALSE)

####USING THRESHOLDS: 10% and Maximum Sensitivity plus Specificity (maxTSS)- ENSEMBLE####
#### Threshold of 10%
ensemble_10 <- apply_thr(species = spp, metric = res, thr = "sensitivity",
                       pam = pam_ensemble, m = "Ensemble")
#Binarize
ensemble_10_bin <- bin(ensemble_10)
#Save results
fwrite(ensemble_10_bin, "Results/PAM/Ensemble_Only_Threshold10.gz", compress = "gzip",
       row.names = FALSE)

####Max TSS
ensemble_maxtss <- apply_thr(species = spp, metric = res, thr = "max_sens_spec",
                           pam = pam_ensemble, m = "Ensemble")
#Binarize
ensemble_maxtss_bin <- bin(ensemble_maxtss)
#Save results
fwrite(ensemble_maxtss_bin, "Results/PAM/Ensemble_Only_ThresholdMaxTSS.gz", compress = "gzip",
       row.names = FALSE)

##Combining threshold and SESAM
#10%
ensemble_10_sesam <- only_sesam(ensemble_10)
fwrite(ensemble_10_sesam, "Results/PAM/Ensemble_Threshold10_and_Ranking.gz", compress = "gzip",
       row.names = FALSE)
#Max TSS
ensemble_maxtss_sesam <- only_sesam(ensemble_maxtss)
fwrite(ensemble_maxtss_sesam, "Results/PAM/Ensemble_ThresholdMaxTSS_and_Ranking.gz", compress = "gzip",
       row.names = FALSE)



