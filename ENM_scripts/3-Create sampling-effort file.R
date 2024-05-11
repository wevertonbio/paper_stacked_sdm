####CREATE BIAS FILE TO EXTRACT BACKGROUND POINTS - TARGET GROUP####
#See more in: https://onlinelibrary.wiley.com/doi/pdfdirect/10.1111/ddi.13442
#See code in: https://github.com/Syrph/Sampling_bias

#Load packages
library(raster)
library(mapview)
library(ks)
library(spatialEco)

setwd("A:/OneDrive - ufpr.br/GitHub/Paper_Modelagem/2-Modelos/")
#Import occurrences
pf <- read.csv("../1-Filtragem_de_Pontos/Check_Points/I.PontosNeotropicos.csv")

#Import raster base
r <- raster("Rasters/M_extent/PC1.tif")

# Aggregate by coordinates
sum_records <- as.data.frame(dplyr::count(pf, x, y))
dir.create("Sampling_effort")
write.csv(sum_records,"Sampling_effort/Sampling.csv", row.names = FALSE)

# # Extract coordinates.
# coords <- cbind(sum_records[,1], sum_records[,2])
# 
# # Create a scale.
# scale <- length(sum_records[,3]) / sum(sum_records[,3])
# sum_records[,4] <- sum_records[,3] * scale
# 
# # Do a 2d kernel density estimation.
# target_density <- kde(coords, w=sum_records[,4])
# 
# # Create raster.
# target_raster <- raster(target_density)
# 
# # # Define in OSGB.
# # crs(target_raster) <- '+init=EPSG:27700'
# 
# # Clip data to the same resolution/extent.
# target_raster <- resample(target_raster, r, method='bilinear')
# 
# # Mask bias file.
# for (x in 1:nlayers(r)){
#   target_raster <- mask(target_raster, r[[x]])
# }
# 
# # Normalize bias file between 0 and 1.
# target_raster <- target_raster - minValue(target_raster)
# target_raster <- raster.transformation(target_raster, trans="norm")
# 
# #Plotar mapa
# mapview(target_raster)
# 
# # Create a file pathway and export the raster
# dir.create("BiasFile")
# writeRaster(target_raster, "BiasFile/TargetGroup_biasfile.tif", overwrite=TRUE)
# 
