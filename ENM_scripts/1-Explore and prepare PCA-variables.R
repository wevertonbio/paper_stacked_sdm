####EXPLORE AND PREPARE ENVIRONMENTAL VARIABLES####
#Use pca variables, not raw variables

#Load packages
library(raster)
library(RStoolbox)
library(rgdal)
library(mapview)
library(vegan)
library(ggplot2)
library(ggrepel)
library(corrplot)
library(FactoMineR)
library(rgeos)
library(ellipsenm)
library(sf)
library(terra)
library(geobr)

#Set folder
my.dir <- "A:/OneDrive - ufpr.br/GitHub/Paper_Modelagem_Final/"
setwd(my.dir)

####Import environmental variables####
var <- list.files(path = "Current_Neotropics/", pattern = ".tif",
                  full.names = TRUE) %>% rast()
#Renomear variáveis
names(var) <- c("Clay", "Altitude", "Nitrogen", "Org_Carbon",
                "pH", "Slope", "Ruggedness", "TopoIndex", 
                "Bio01", "Bio10", "Bio11", "Bio12", "Bio13", "Bio14",
                "Bio15", "Bio16","Bio17", "Bio18", "Bio19", "Bio02",
                "Bio03", "Bio04", "Bio05", "Bio06","Bio07","Bio08", "Bio09")
#Reordenar e selecionar variáveis
var <- var[[c("Bio01", "Bio02", "Bio03", "Bio04", "Bio05", "Bio06",
              "Bio07","Bio08", "Bio09","Bio10", "Bio11", "Bio12", "Bio13",
              "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19",
               "Clay", "Nitrogen","Org_Carbon","pH",
               "Slope", "TopoIndex")]]


####Cortar variáveis para máximo M extent ####
#Import data
pf <- read.csv("1-Filtragem_de_Pontos/Check_Points/I.PontosNeotropicos.csv")
pf.m <- convex_area(data = pf,
                    longitude = "x", latitude = "y",
                    split = F,
                    buffer_distance = 750)
df <- st_as_sf(pf, coords = c("x", "y"), crs = 4326)
mapview(list(pf.m, df["species"]))
#Cut variables to m extent
pf.m <- pf.m %>% vect()
var.m <- mask(crop(var, pf.m), pf.m)
plot(var.m[[2]])
####PCA####
pca.var <- rasterPCA(var.m, spca = TRUE) #It takes a while
summary(pca.var$model)
bstick(pca.var$model) - eigenvals(pca.var$model)
#Ver mapas
mapview(pca.var$map[[1:9]], alpha.regions = 0.99)

####Save Variables (M extent)####
dir.create("Rasters")
dir.create("Rasters/M_extent")
#Save M Extent
pbapply::pblapply(seq_along(1:9), function(i) {
  writeRaster(pca.var$map[[i]], 
              filename = paste0("Rasters/M_extent/",
                names(pca.var$map[[i]]),
              ".tif"))
  })

# ####Cut and Save to Santa Catarina Extent####
# #Get Santa Catarina extent
# sc <- read_state(code_state = "SC")
# var.sc <- mask(crop(pca.var$map[[1:9]], sc), sc)
# plot(var.sc[[1]])
# #Create directory
# dir.create("Rasters/Santa_Catarina")
# #Save M Extent
# pbapply::pblapply(seq_along(1:9), function(i) {
#   writeRaster(var.sc[[i]], 
#               filename = paste0("Rasters/Santa_Catarina/",
#                                 names(var.sc[[i]]),
#                                 ".tif"))
# })

#Save contributions
as.data.frame.summary.princomp <- function(x, ...) {
  vars <- x$sdev^2
  vars <- vars/sum(vars)
  type.convert(
    as.data.frame(
      rbind(`Standard deviation` = x$sdev, `Proportion of Variance` = vars, 
            `Cumulative Proportion` = cumsum(vars))
    )
  )
}
res <- as.data.frame(summary(pca.var$model))
write.csv2(res, "PCAVarianceExplained.csv")

#Save loadings
scores = data.frame(pca.var$model$loadings[,1:9]) #To put the scores in a dataframe
write.csv2(scores, "PCAScores.csv")

####Gerar gráficos de PCA####
ggdata <- data.frame(scores, "Color") #It creates a new column to fill in with colors
#Here, we are going to color the temperature variables with orange, precipitation variables with blue, soil variables with green and topographic variables with pink
rownames(scores)
ggdata$X.Color. <- c(rep("#D55E00", 11), #Orange
                          rep("#0072B2", 8), #Blue
                          rep("#009E73", 4), #Green
                          rep("#CC79A7", 2) #Pink
                          ) #You can change de colors as you wish
#Plot PC1 and 2 
g<- ggplot(data = ggdata, aes(x = Comp.8, y = Comp.9)) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") + #To create the background of plot
  geom_point(shape=16, alpha = 1 , colour=ggdata$X.Color., size=2) + theme_bw() + #To set the colors (based on column previously created) and the size of points
  geom_label_repel(label = rownames(scores),
                   colour = ggdata$X.Color., size = 3, vjust=1.5) + #To create labels
  ggtitle("PCA of bioclimatic variables", subtitle = "Loadings of PC8 and PC9") +
  xlab("PC8 (2%)") + ylab("PC9 (2%)") + #To create a tittle and x and y labels (change the values according to your results)
  theme(plot.title = element_text(size = 10, face = "bold", hjust=0.5)) +
  theme(plot.subtitle = element_text(size = 8, face = "bold", hjust=0.5)) #To set up the title and subtitle
g
#Save images
#Create folder to save images
dir.create("Images")
ggsave('Images/PCA8-9.png', g, units="in", dpi=600, width = 6, height = 5)

