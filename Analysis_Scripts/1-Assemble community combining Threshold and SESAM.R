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

#Import thresholds
res <- fread("Results/Thresholds.csv")
spp <- unique(res$species) #Get species

#Import continuous PAM
pam <- fread("Results/PAM_continuous.gz")

#### ONLY SESAM ####
#See more in: https://www.rdocumentation.org/packages/ecospat/versions/3.2.1/topics/ecospat.SESAM.prr
##Assemble community
sesam_only <- only_sesam(pam = pam)
#Save
fwrite(sesam_only, "Results/PAM/sesam_only.gz", compress = "gzip",
       row.names = FALSE)

# #Rasterize some species to see presence
# sp <- "Araucaria_angustifolia"
# r.base <- rast("kuenm_models/Abarema_langsdorffii/Current_Median.tiff")
# sp.plot <- sesam_only %>% dplyr::select(x, y, sp) %>%
#   vect(geom = c(x = "x", y = "y"), crs = "+init=epsg:4326")
# sp.plot2 <- rasterize(sp.plot, r.base, sp)
# mapview(sp.plot2)


#### USING THRESHOLDS: 10% ####
pam_10 <- apply_thr(species = spp, metric = res, thr = "thr10",
                   pam = pam)
#Binarize
pam_10_bin <- bin(pam_10)
#Save results
fwrite(pam_10_bin, "Results/PAM/only_threshold10.gz", compress = "gzip",
       row.names = FALSE)

#### USING minimum training presence ####
pam_min <- apply_thr(species = spp, metric = res, thr = "thr_min",
                     pam = pam)
#Binarize
pam_min_bin <- bin(pam_min)
#Save results
fwrite(pam_min_bin, "Results/PAM/only_threshold_min.gz", compress = "gzip",
       row.names = FALSE)

##Combining threshold and SESAM
#10%
pam_10_sesam <- only_sesam(pam_10)
fwrite(pam_10_sesam, "Results/PAM/threshold10_and_sesam.gz", compress = "gzip",
       row.names = FALSE)
#Minimum occurrence
pam_min_sesam <- only_sesam(pam_min)
fwrite(pam_min_sesam, "Results/PAM/thresholmin_and_sesam.gz", compress = "gzip",
       row.names = FALSE)

