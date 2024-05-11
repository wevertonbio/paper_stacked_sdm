####Generate predictor variables####
library(terra)
library(dplyr)
library(geobr)
library(pbapply)
library(data.table)
library(bigmds)

#Import map of Santa Catarina (and set 15km of buffer)
sc <- read_state(code_state = "SC") %>% vect() %>% buffer(width = 15*1000)

####Forest in 2007####
mb <- rast("C:/Users/wever/OneDrive - ufpr.br/GitHub/Paper_Modelagem_flexsdm/MapBiomas/brasil_coverage_2007.tif")
#Cut to SC
mb <- crop(mb, sc, mask = TRUE)
plot(mb)
#Binarize model: 1 - forest, 0 - non-forest
#See values for forest here: https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2023/08/Legenda-Colecao-8-LEGEND-CODE.pdf
#values(mb) %>% unique()
mb[mb %in% c(1, 3, 4, 5, 6, 49)] <- 1
mb[mb != 1] <- 0
plot(mb)

#Resample data to 2.5 arc-min
#Raster base
r <- rast("Rasters/base.tiff")
mb_res <- terra::resample(x = mb, y = r, method = "average")
plot(mb_res)
res(mb_res)
names(mb_res) <- "Forest_coverage_2007"


#Get Bioclimatic variables (Bio02, Bio06, Bio 15, Bio17, and elevation)
vars <- list.files("C:/Users/wever/OneDrive - ufpr.br/GitHub/Paper_Modelagem_flexsdm/Current_Neotropic/",
                   full.names = TRUE)
#select vars
vars <- vars[c(12, 16, 17, 7, 9, 29)]
vars
vars <- rast(vars)
names(vars) <- c("bio2", "bio6", "bio7", "bio15", "bio17", "elevation")
#Cut variables
vars <- crop(vars, sc, mask = TRUE)
plot(vars)


#Sampling effort
occ <- list.files("C:/Users/wever/OneDrive - ufpr.br/GitHub/Paper_Modelagem_flexsdm/Species_Occurrence/",
                 full.names = TRUE)
occ <- pblapply(occ, fread)
occ <- bind_rows(occ)
#Extract and summarize number of occurrences by cell
by_cell <- terra::extract(r, occ[, c("x","y")], cells = TRUE, xy = TRUE) %>%
  na.omit()
d <- by_cell %>% group_by(cell) %>% summarise(n = n())
#Merge
d <- right_join(by_cell[, c("x","y", "cell")], d)
# Extract coordinates.
coords <- d %>% dplyr::select(x, y)

# Create a scale.
scale <- length(d$n) / sum(d$n)
effort <- d$n * scale

# Do a 2d kernel density estimation.
target_density <- ks::kde(coords, w=effort)

# Create raster.
effort_r <- raster::raster(target_density) %>% rast()
plot(effort_r)
#Crop to Atlantic Forest
se <- crop(effort_r, sc, mask = TRUE)
plot(se)
#Resample
se_r <- resample(se, r, method = "bilinear")
names(se_r) <- "sampling_effort"
plot(se_r)


#####SPATIAL EINGEVECTORS####
#Isstall older version of bigmds package
#remotes::install_version("bigmds", version = "2.0.1")
library(bigmds)
#Get coordinates
coords <- as.data.frame(r, xy = TRUE) %>% dplyr::select(x, y) %>% as.matrix()

## calculating eigenvectors via principal coordinate analysis (K = numero de puntos - 1)
pcoa2 <- fast_mds(coords, l = 200, s_points = 5 * 10, r = 10, n_cores = 1)


eigs2 <- pcoa2$eig # all eigenvalues
pos_eigs2 <- 1:length(eigs2[eigs2 > 0]) # position of positive eigenvalues

eigens2 <- pcoa2$points[, pos_eigs2[1:100]] # getting only eigenvectors with positive eigenvalues


## selecting eigenvectors that should be considered in glms ********TWO EIGENVECTORS WERE SELECTED**********
### plots to select eigenvectors
plot(1:10, eigs2[1:10] / max(eigs2), type = "l", col = "black", las = 1, xlab = "", ylab = "") # all eigenvalues
abline(h = 0, col = "grey75", lwd = 1, lty = 2)
title(xlab = "Rank of eigenvalues", ylab = "", cex.lab = 1.2, line = 3)
title(xlab = "", ylab = "Eigenvalues (normalized)", cex.lab = 1.2, line = 3.3)
abline(v = 2, col = "red", lwd = 1, lty = 2)

#Get eingevectors
E1_2 <- eigens2[, 1]
E2_2 <- eigens2[, 2]
E3_2 <- eigens2[, 3]
#Rasterize eingevectors
resolution <- res(r)
extension <- terra::ext(r)
r_base <- rast(resolution = resolution, extent = extension)
r_res_1 <- rasterize(coords,
                     r_base,
                     values = E1_2)
plot(r_res_1) #Longitudinal variation
r_res_2 <- rasterize(coords,
                     r_base,
                     values = E2_2)
plot(r_res_2) #Latitudinal variation
r_res_3 <- rasterize(coords,
                     r_base,
                     values = E3_2)
plot(r_res_3) #??????variation
#Save variable sin object
spatial_variation <- c(r_res_1, r_res_2)
names(spatial_variation) <- c("longitudinal_eingevector",
                              "latitudinal_eingevector")
plot(spatial_variation)

####Join data####
p <- c(mb_res, vars, se_r, spatial_variation)
names(p)
#Save
writeRaster(p, "Rasters/Variables.tiff")



