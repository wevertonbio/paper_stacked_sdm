get_rasters <- function(data){
  #Variables do rasterize
  to_rast <- c("Rich_obs", "Rich_pred",  "Rich_ratio")
  #Split data by model
  df_m <- split(data, data$Type)
  #Get resolutions
  all_res <- unique(data$Resolution)

  #Looping throughout models
  by_model <- pblapply(df_m, function(i){
    #Split data by resolution
    by_res <- lapply(all_res, function(x){
      df_x <- i %>% filter(Resolution == x)
      #Rasterize
      m_rast <- lapply(to_rast, function(z){
        #Get new raster base according to resolution
        if(x == "2.5arc-min") {f = round(0.0416667/res(base)[1],0)}
        if(x == "5arc-min") {f = round(0.0833333/res(base)[1],0)}
        if(x == "10arc-min") {f = round(0.166667/res(base)[1],0)}
        if(x == "30arc-min") {f = round(0.5/res(base)[1],0)}
        if(x == "60arc-min") {f = round(1/res(base)[1],0)}
        #Resample raster base, if necessary
        if(f > 1){
          new_base <- terra::aggregate(base, fact = f)
        } else {
          new_base <- base
        }
        df_rast <- terra::rasterize(as.matrix(df_x[,c("Longitude", "Latitude")]),
                                    y = new_base, value = df_x[[z]], background = NA)
        #Resample
        if(f > 1){
          df_rast2 <- terra::resample(x = df_rast, y = base, method = "max")} else {
            df_rast2 <- df_rast}
        #Crop raster
        df_rast2 <- crop(df_rast2, sc, mask = TRUE)
        names(df_rast2) <- z
        return(df_rast2)
      }) %>% rast()
      return(m_rast)
    })
  names(by_res) <- all_res
  return(by_res)
  })
} #End of function

####Plot maps####
gg_map <- function(r, title_legend = "Richness"){
  ggplot() +
    geom_sf(data = sa, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = br, fill = "white", size = 0.1, colour = "grey40") +
    geom_sf(data = sc, fill = "snow1", colour = "black") +
    geom_spatraster(data = r, na.rm = TRUE) +
    geom_sf(data = sc, fill = NA, colour = "black") +
    scale_fill_whitebox_b(palette = "muted", name = title_legend) +
    coord_sf(xlim = c(bb_sc[1], xmax=bb_sc[2]),
             ylim = c(bb_sc[3], ymax=bb_sc[4]),
             expand = T) +
    # metR::scale_x_longitude(ticks = 2) +
    # metR::scale_y_latitude(ticks = 2) +
    ggpubr::theme_pubclean() +
    #ggtitle(gsub("\n", " ", type_i)) +
    theme(panel.background = element_rect(fill = 'aliceblue'),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.1, 0.3),
          legend.key.height =unit(0.9, "cm"),
          legend.background = element_rect(fill = "white", size = 0.5,
                                           colour = "black"),
          #legend.margin = margin(0, 0, 0, -0.32, "cm"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "bold"),
          plot.margin = margin(t = 0,  # Top margin
                               r = 0,  # Right margin
                               b = 0,  # Bottom margin
                               l = -0.5),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
}

gg_cor <- function(data){
  ggplot(data, aes(x = Rich_obs, y = Rich_pred)) +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm", col = "black") +
    geom_abline(intercept = 0, slope = 1, colour = "red",
                linetype = "dashed", size = 1.2) +
    xlab("Observed richness") + ylab("Predicted richness") +
    ggpubr::theme_pubclean() +
    theme(axis.text=element_text(size=14), #Change size of numbers in axis
          axis.title=element_text(size=20,face="bold")) #Change size of text in axis
}

####Arrange plots####
arrange_obs <- function(d){
  d[[1]] + facet_grid("S Observed" ~ paste(names(d[1])), switch = "y") +
    d[[2]] + facet_grid(.~ paste(names(d[2]))) +
    d[[3]] + facet_grid(.~ paste(names(d[3]))) +
    d[[4]] + facet_grid(.~ paste(names(d[4]))) +
    d[[5]] + facet_grid(.~ paste(names(d[5]))) +
    plot_layout(nrow = 1) &
    theme(strip.text = element_text(size = 18, face = "bold"))
}

arrange_pred_over <- function(d, facet){
  d[[1]] + facet_grid(paste0(facet) ~., switch = "y") +
    d[[2]] +
    d[[3]] +
    d[[4]] +
    d[[5]] +
    plot_layout(nrow = 1) &
    theme(strip.text = element_text(size = 18, face = "bold"))
}

arrange_cor <- function(d, facet){
  d[[1]] + facet_grid(paste0(facet) ~., switch = "y") +
    d[[2]] + theme(axis.title.y = element_blank()) +
    d[[3]] + theme(axis.title.y = element_blank()) +
    d[[4]] + theme(axis.title.y = element_blank()) +
    d[[5]] + theme(axis.title.y = element_blank()) +
    plot_layout(nrow = 1) &
    theme(strip.text = element_text(size = 18, face = "bold"))
}

#Plot and save maps
plot_maps_h1 <- function(data, output_dir = "Results/Images/H1_maps/") {
  #Create directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  #Get maps by model
  by_model <- pblapply(names(data), function(i){
    data_i <- data[[i]]
    ####Observed richness####
    i_obs <- lapply(data_i, function(x) x$Rich_obs) %>% rast()
    #Plot
    g_obs <- lapply(i_obs, gg_map, title_legend = "Richness")
    names(g_obs) <- names(i_obs)
    #Arrange plot
    g_obs2 <- arrange_obs(g_obs)

    ####Predicted richness####
    i_pred <- lapply(data_i, function(x) x$Rich_pred) %>% rast()
    #Plot
    g_pred <- lapply(i_pred, gg_map, title_legend = "Richness")
    names(g_pred) <- names(i_pred)
    #Arrange plot
    g_pred2 <- arrange_pred_over(g_pred, facet = "S Predicted")

    ####Overprediction####
    i_over <- lapply(data_i, function(x) x$Rich_ratio) %>% rast()
    #Plot
    g_over <- lapply(i_over, gg_map, title_legend = "SPred/SObs")
    names(g_over) <- names(i_over)
    #Arrange plot
    g_over2 <- arrange_pred_over(g_over, facet = "Overprediction")

    ####Predicted vs Observed correlation
    #Subset data
    df_i <- df %>% filter(Type == i)
    #split by res
    df_i <- split(df_i, df_i$Resolution)
    g_cor <- lapply(df_i, gg_cor)
    names(g_over) <- names(df_i)
    #Arrange plot
    g_cor2 <- arrange_cor(g_cor, facet = "SObs~SPred")

    #Arrange all plots#
    all_p <- g_obs2 / g_pred2 / g_over2 / g_cor2 +
      plot_annotation(title = gsub("\n", " ", i),
                      theme = theme(plot.title = element_text(size = 28,
                                                              face = "bold",
                                                              hjust = 0.5)))
    #Save
      #Filename
    i_type <- gsub("\n", " ", i)
    i_type <- gsub("\\%", "", i_type)
    filename <- file.path(output_dir, paste0(i_type, ".png"))
    ggsave(filename = filename,  all_p, dpi = 600, units = "px", width = 2900,
           height = 1700, scale = 5.9)
    return(all_p)
    # ggsave("Results/Images/Teste.png", tt, dpi = 600, units = "px", width = 3000,
    #        height = 1700, scale = 5.5)
    })
  names(by_model) <- names(data)
  return(by_model)
  }

