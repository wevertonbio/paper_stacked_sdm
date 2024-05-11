####PREPARE DATA TO FLEXSDM MODELS####
#See more in: https://sjevelazco.github.io/flexsdm/reference/occfilt_env.html

#Load packages####
# devtools::install_github('sjevelazco/flexsdm')
library(flexsdm)
library(dplyr)
library(raster)
library(ellipsenm)
library(RStoolbox)
library(vegan)
library(terra)
library(geobr)
library(pbapply)
library(tidyverse)

setwd("A:/OneDrive - ufpr.br/GitHub/Paper_Modelagem/2-Modelos/")

#Import data
pf <- read.csv("../1-Filtragem_de_Pontos/Check_Points/I.PontosNeotropicos.csv")

#Arrumarc colunas
df <- pf[,c("species", "x", "y")]
#Create ID
df$ID <- row.names(df)

#Selecionar apenas algumas espécies
# sp.int <- c("Araucaria angustifolia", "Myrcia hatschbachii",
#             "Ocotea paranaensis") 
# df <- subset(df, df$species %in% sp.int)

####Environmental filtering####
#Prepare environmental rasters (M Extent)
var <- list.files(path = "Rasters/M_extent/", pattern = ".tif", full.names = T) %>% 
  rast()
#Select variables of interes
names(var)
var.int <- c("PC1", "PC2", "PC3")
var <- var[[var.int]]

#Environmental filtering
dir.create("Species_Occurrence")
setwd("Species_Occurrence/")
species.list <- split(df, df$species)

#Ver espécies e criar backup
names(species.list)
sp.list <- species.list
names(sp.list)

# #Ver espécies que não deram certo (rodar no final)
# sp.in <- list.files() %>% gsub(".csv", "", .)
# sp.out <- subset(unique(df$species), !(unique(df$species) %in% sp.in))
# sp.list <- subset(df, df$species %in% sp.out) %>% split(., .$species)

#### Filtro ambiental com método de Moran ####
#See more in: https://github.com/sjevelazco/Ligustrum_lucidum_modeling/blob/main/R/02_Clean_coordinate_Env_filtering.R
#Set number of bins to be tested
bins <- c(100, 90, 80, 70, 60, 50, 40, 30, 20, 10, 5, 2)

pblapply(seq_along(sp.list), function(i) {
  tryCatch({ #Avoid errors
    pts <- sp.list[[i]] %>% mutate(ID = row_number())
    #Looping bins with lapply
    filtered <- lapply(seq_along(bins), function(i){
      message('Filtering ', bins[i], ' bins')
      occfilt_env(
        data = pts,
        x = "x",
        y = "y",
        id = "ID",
        env_layer = var,
        nbins = bins[i]
      )
    })
    
    # How many records were left at the end of each filtering?
    #sapply(filtered, nrow)
    #Names
    names(filtered) <- bins
    
    #Subset original dataframes
    filtered2 <- filtered
    for(i in 1:length(filtered)){
      filtered2[[i]] <- pts %>% 
        dplyr::filter(x%in%filtered[[i]]$x & y%in%filtered[[i]]$y)
    }
    head(filtered2)
    
    # Autocorrelation analysis and number of records for each bin
    imoran <- list()
    for(i in 1:length(filtered2)){
      message("Spatial autocorrelation dataset ", i)
      coord <- filtered2[[i]] %>% dplyr::select(x, y)
      data <- data.frame(terra::extract(var, coord))
      distm <- dist(coord)
      distm <- as.matrix(distm)
      distm <- 1/distm
      diag(distm) <- 0
      distm[is.infinite(distm)] <- 0 #Add this line only with points with get error
      try(imoran[[i]] <-
            apply(data, 2, function(x)
              ape::Moran.I(x, distm, na.rm = T)[c(1, 4)] %>% unlist) %>% data.frame() %>% as_tibble()
      )
      try(imoran[[i]] <- imoran[[i]][1,])
    }
    
    names(imoran) <- bins
    imorandf <- dplyr::bind_rows(imoran, .id='bins')
    imorandf <- imorandf %>% dplyr::mutate(
      mean_bin=apply(imorandf[, c('PC1', 'PC2', 'PC3')], 1, mean))
    imorandf$nrecords <- sapply(filtered2, nrow)
    imorandf
    
    # Final filter
    finalfilter <- imorandf %>% 
      dplyr::filter(mean_bin<=quantile(mean_bin)[2]) %>% # Bins set with lower spatial autocorrelation
      dplyr::filter(nrecords==max(nrecords)) %>% # Bins set with higher number of records 
      dplyr::sample_n(1) %>% # Select a random value if more than one is selected
      pull(bins)
    finalfilter
    
    # Best filtered occurrence set 
    occ.env.Moran <- filtered2[[finalfilter]] %>% dplyr::select(species, x, y)
    
    #Save occurrences
    write.csv(occ.env.Moran, paste0(unique(pts$species),
                                    ".csv"),
              row.names = F)},
                                error=function(e) NULL) #Avoid errors
    })

setwd("../")

#Import data
l.csv <- list.files(path = "Species_Occurrence/", full.names = TRUE)
l.spp <- pblapply(seq_along(l.csv), function(i) {
  read.csv(l.csv[[i]])
})
spp <- l.spp %>% map_df(bind_rows)
#Filter data: select only species with at least 8 occurrence points
n.spp <- data.frame(table(spp$species))
n.spp <- subset(n.spp, n.spp$Freq >= 8)
spps <- subset(spp, spp$species %in% n.spp$Var1)
length(unique(spps$species))
write.csv(spps, "Ocorrencias.csv", row.names = FALSE)

#Metadata: species removidas por terem menos de 8 ocorrencias
sp.out <- subset(names(species.list),
                 !(names(species.list) %in% unique(spps$species)))
writeLines(c("ESPÉCIES ELIMINADAS POR TEREM MENOS DE 8 REGISTROS",
             sp.out),
           "EspeciesComMenosde8Registros.txt")
