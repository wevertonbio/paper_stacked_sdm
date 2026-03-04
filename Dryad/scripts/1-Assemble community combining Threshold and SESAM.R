#### ASSEMBLE COMMUNITY USING MAXENT MODELS AND SESAM FRAMEWORK####

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

#Import helper functions
source("scripts/helpers.R")

#Create folder to save binarized communities
dir.create("data/PAM/")

#Import thresholds
res <- fread("data/Thresholds.csv")
spp <- unique(res$species) #Get species

#Import continuous PAM
pam <- fread("data/PAM_continuous.gz")

#### ONLY SESAM ####
#See more in: https://www.rdocumentation.org/packages/ecospat/versions/3.2.1/topics/ecospat.SESAM.prr
##Assemble community
sesam_only <- only_sesam(pam = pam)
# Replace NA with 0, if necessary
if(any(is.na(sesam_only))){
  sesam_only <- as.data.frame(sesam_only)
  sesam_only[is.na(sesam_only)] <- 0
}

#Save
fwrite(sesam_only, "data/PAM/sesam_only.gz", compress = "gzip",
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

# Replace NA with 0, if necessary
if(any(is.na(pam_10_bin))){
  pam_10_bin <- as.data.frame(pam_10_bin)
  pam_10_bin[is.na(pam_10_bin)] <- 0
}

#Save results
fwrite(pam_10_bin, "data/PAM/only_threshold10.gz", compress = "gzip",
       row.names = FALSE)

#### USING minimum training presence ####
pam_min <- apply_thr(species = spp, metric = res, thr = "thr_min",
                     pam = pam)
#Binarize
pam_min_bin <- bin(pam_min)

# Replace NA with 0, if necessary
if(any(is.na(pam_min_bin))){
  pam_min_bin <- as.data.frame(pam_min_bin)
  pam_min_bin[is.na(pam_min_bin)] <- 0
}

#Save results
fwrite(pam_min_bin, "data/PAM/only_threshold_min.gz", compress = "gzip",
       row.names = FALSE)

##Combining threshold and SESAM
#10%
pam_10_sesam <- only_sesam(pam_10)

# Replace NA with 0, if necessary
if(any(is.na(pam_10_sesam))){
  pam_10_sesam <- as.data.frame(pam_10_sesam)
  pam_10_sesam[is.na(pam_10_sesam)] <- 0
}

fwrite(pam_10_sesam, "data/PAM/threshold10_and_sesam.gz", compress = "gzip",
       row.names = FALSE)

#Minimum occurrence
pam_min_sesam <- only_sesam(pam_min)

# Replace NA with 0, if necessary
if(any(is.na(pam_min_sesam))){
  pam_min_sesam <- as.data.frame(pam_min_sesam)
  pam_min_sesam[is.na(pam_min_sesam)] <- 0
}

fwrite(pam_min_sesam, "data/PAM/thresholmin_and_sesam.gz", compress = "gzip",
       row.names = FALSE)

