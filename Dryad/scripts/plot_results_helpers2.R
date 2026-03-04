# ####Arrange plots####
# arrange_obs2 <- function(d, type = NULL){
#   d_1 <- d[[1]] + ylab(type) + facet_grid(paste(names(d[1])) ~ "S Observed", switch = "y")
#   d_2 <- d[[2]] + facet_grid(paste(names(d[2])) ~ ., switch = "y")
#   d_3 <- d[[3]] + facet_grid(paste(names(d[3])) ~ ., switch = "y")
#   d_4 <- d[[4]] + facet_grid(paste(names(d[4])) ~ ., switch = "y")
#   d_5 <- d[[5]] + facet_grid(paste(names(d[5])) ~ ., switch = "y")
#   #PLOT
#   d_1 / d_2 / d_3 / d_4 / d_5 +
#     plot_layout(nrow = 5) &
#     theme(strip.text = element_text(size = 18, face = "bold"),
#           axis.title=element_text(size=20,face="bold"))
#
# }
#
# arrange_pred_over2 <- function(d, facet, type = NULL){
#   d_6 <- d[[1]] + ggtitle(type) + facet_grid(paste(names(d[1])) ~ "Observed richness", switch = "y")
#   d_7 <- d[[2]] + facet_grid(paste(names(d[2])) ~ ., switch = "y")
#   d_8 <- d[[3]] + facet_grid(paste(names(d[3])) ~ ., switch = "y")
#   d_9 <- d[[4]] + facet_grid(paste(names(d[4])) ~ ., switch = "y")
#   d_10 <- d[[5]] + facet_grid(paste(names(d[5])) ~ ., switch = "y")
#   #PLOT
#   z <- d_1 + d_2 + d_3 + d_4 + d_5 + d_6 + d_7 + d_8 + d_9 + d_10 + plot_layout(nrow = 5, ncol = 2)
#   z
#
#   plot_layout(nrow = 5) &
#     theme(strip.text = element_text(size = 18, face = "bold"),
#           axis.title=element_text(size=20,face="bold"))
# }


gg_cor2 <- function(data, type = NULL){
  ggplot(data, aes(x = Rich_obs, y = Rich_pred, group = Type, fill = Type)) +
    geom_point(alpha = 0.25, aes(col = Type)) +
    geom_smooth(method = "lm", aes(col = Type)) +
    scale_color_manual(values = c("#CC79A7", "#009E73")) +
    scale_fill_manual(values = c("#CC79A7", "#009E73")) +
    geom_abline(intercept = 0, slope = 1, colour = "red",
                linetype = "dashed", size = 1.2) +
    xlab("Observed richness") +
    ylab(paste0("Predicted richness\n", type)) +
    scale_y_continuous(limits = c(0, 620), breaks = seq(0, 620, by = 100)) +
    #expand_limits(x = 0, y=0) +
    ggpubr::theme_pubclean() +
    theme(axis.text=element_text(size=14), #Change size of numbers in axis
          axis.title=element_text(size=14,face="bold")) #Change size of text in axis
}

arrange_plots <- function(g_obs, g_over10, g_over10sesam, g_cor_both){
  #PLot like this:
  # a b c d
  # e f g h
  # i j k l
  #m n o p
  #2.5arcmin
  a <- g_obs$`2.5arc-min` +
    facet_grid("2.5arc-min" ~ "Observed Richness", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  b <- g_over10$`2.5arc-min` +
    facet_grid(. ~ "Overprediction (10% Threshold)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  c <- g_over10sesam$`2.5arc-min` +
    facet_grid(. ~ "Overprediction (10% thr. and Ranking)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  d <- g_cor_both$`2.5arc-min` + facet_grid(. ~ "Observed ~ Predicted", switch = "y") +
    scale_y_continuous(position = "right") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17),
          axis.title.x = element_blank())

  #5arc-min
  e <- g_obs$`5arc-min` + facet_grid("5arc-min" ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  f <- g_over10$`5arc-min` + facet_grid(. ~ ., switch = "y")
  g <- g_over10sesam$`5arc-min` +
    facet_grid(. ~ ., switch = "y")
  h <- g_cor_both$`5arc-min` + facet_grid(. ~ ., switch = "y") +
    scale_y_continuous(position = "right") + theme(legend.position = "none",
                                                   axis.title.x = element_blank())

  #10arc-min
  i <- g_obs$`10arc-min` + facet_grid("10arc-min" ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  j <- g_over10$`10arc-min` + facet_grid(. ~ ., switch = "y")
  k <- g_over10sesam$`10arc-min` +
    facet_grid(. ~ ., switch = "y")
  l <- g_cor_both$`10arc-min` + facet_grid(. ~ ., switch = "y") +
    scale_y_continuous(position = "right") + theme(legend.position = "none",
                                                   axis.title.x = element_blank())

  #30arcmin
  m <- g_obs$`30arc-min` + facet_grid("30arc-min" ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  n <- g_over10$`30arc-min` + facet_grid(. ~ ., switch = "y")
  o <- g_over10sesam$`30arc-min` +
    facet_grid(. ~ ., switch = "y")
  p <- g_cor_both$`30arc-min` + facet_grid(. ~ ., switch = "y") +
    scale_y_continuous(position = "right") +
    theme(legend.position = "none")

  #Arrange all lines
  first_line <- a | b | c | d
  second_line <- e | f | g | h
  third_line <- i | j | k | l
  fourth_line <- m | n | o | p
  all_lines <- first_line / second_line / third_line / fourth_line
  all_lines
}

arrange_plots2 <- function(g_beta10, g_beta10sesam,
                           g_nest10, g_nest10sesam,
                           g_turn10, g_turn10sesam){
  #PLot like this:
  # a b c d e f
  # g h i j k l
  # m n o p q r
  # s t u v x z

  #2.5arcmin
  a <- g_beta10$`2.5arc-min` +
    facet_grid("2.5arc-min" ~ "Dissimilarity (10% Threshold)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  b <- g_beta10sesam$`2.5arc-min` +
    facet_grid(. ~ "Dissimilarity (10% thr. and Ranking)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  c <- g_nest10$`2.5arc-min` +
    facet_grid(. ~ "Nestedness (10% Threshold)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  d <- g_nest10sesam$`2.5arc-min` +
    facet_grid(. ~ "Nestedness (10% thr. and Ranking)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  e <- g_turn10$`2.5arc-min` +
    facet_grid(. ~ "Turnover (10% Threshold)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  f <- g_turn10sesam$`2.5arc-min` +
    facet_grid(. ~ "Turnover (10% thr. and Ranking)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))


  #5arcmin
  g <- g_beta10$`5arc-min` +
    facet_grid("5arc-min" ~ "Dissimilarity (10% Threshold)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  h <- g_beta10sesam$`5arc-min` +
    facet_grid(. ~ "Dissimilarity (10% thr. and Ranking)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  i <- g_nest10$`5arc-min` +
    facet_grid(. ~ "Nestedness (10% Threshold)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  j <- g_nest10sesam$`5arc-min` +
    facet_grid(. ~ "Nestedness (10% thr. and Ranking)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  k <- g_turn10$`5arc-min` +
    facet_grid(. ~ "Turnover (10% Threshold)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  l <- g_turn10sesam$`5arc-min` +
    facet_grid(. ~ "Turnover (10% thr. and Ranking)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))

  #10arcmin
  m <- g_beta10$`10arc-min` +
    facet_grid("10arc-min" ~ "Dissimilarity (10% Threshold)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  n <- g_beta10sesam$`10arc-min` +
    facet_grid(. ~ "Dissimilarity (10% thr. and Ranking)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  o <- g_nest10$`10arc-min` +
    facet_grid(. ~ "Nestedness (10% Threshold)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  p <- g_nest10sesam$`10arc-min` +
    facet_grid(. ~ "Nestedness (10% thr. and Ranking)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  q <- g_turn10$`10arc-min` +
    facet_grid(. ~ "Turnover (10% Threshold)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  r <- g_turn10sesam$`10arc-min` +
    facet_grid(. ~ "Turnover (10% thr. and Ranking)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))

  #30arcmin
  s <- g_beta10$`30arc-min` +
    facet_grid("30arc-min" ~ "Dissimilarity (10% Threshold)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  t <- g_beta10sesam$`30arc-min` +
    facet_grid(. ~ "Dissimilarity (10% thr. and Ranking)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  u <- g_nest10$`30arc-min` +
    facet_grid(. ~ "Nestedness (10% Threshold)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  v <- g_nest10sesam$`30arc-min` +
    facet_grid(. ~ "Nestedness (10% thr. and Ranking)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  x <- g_turn10$`30arc-min` +
    facet_grid(. ~ "Turnover (10% Threshold)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))
  z <- g_turn10sesam$`30arc-min` +
    facet_grid(. ~ "Turnover (10% thr. and Ranking)", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 17))



  #Arrange all lines
  first_line <- a | b | c | d | e | f
  second_line <- g | h | i | j | k | l
  third_line <- m | n | o | p | q | r
  fourth_line <- s | t | u | v | x | z
  all_lines <- first_line / second_line / third_line / fourth_line
  all_lines
}

arrange_plots3 <- function(g_obs, g_over10, g_over10sesam, g_cor_both){
  #PLot like this:
  # a b c d
  # e f g h
  # i j k l
  #m n o p
  #Observed
  a <- g_obs$`2.5arc-min` +
    facet_grid("A\nObserved\nRichness" ~ "2.5arc-min", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  b <- g_obs$`5arc-min` +
    facet_grid(. ~ "5arc-min", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  c <- g_obs$`10arc-min` +
    facet_grid(. ~ "10arc-min", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  d <- g_obs$`30arc-min` +
    facet_grid(. ~ "30arc-min", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  e <- g_obs$`60arc-min` +
    facet_grid(. ~ "60arc-min", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))


  #Overprediction (10%)
  f <- g_over10$`2.5arc-min` + facet_grid("B\nOverprediction\n(Threshold-only)" ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  g <- g_over10$`5arc-min` + facet_grid(. ~ ., switch = "y")
  h <- g_over10$`10arc-min` + facet_grid(. ~ ., switch = "y")
  i <- g_over10$`30arc-min` + facet_grid(. ~ ., switch = "y")
  j <- g_over10$`60arc-min` + facet_grid(. ~ ., switch = "y")

  #Overprediction (10% and ranking)
  k <- g_over10sesam$`2.5arc-min` + facet_grid("C\nOverprediction\n(Threshold-and-ranking)" ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  l <- g_over10sesam$`5arc-min` + facet_grid(. ~ ., switch = "y")
  m <- g_over10sesam$`10arc-min` + facet_grid(. ~ ., switch = "y")
  n <- g_over10sesam$`30arc-min` + facet_grid(. ~ ., switch = "y")
  o <- g_over10sesam$`60arc-min` + facet_grid(. ~ ., switch = "y")


  #Predicted ~ observed
  p <- g_cor_both$`2.5arc-min` + facet_grid("D\nPredicted~Observed" ~ ., switch = "y") +
    theme(legend.position = "none", axis.title.y = element_text(vjust = -3),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  q <- g_cor_both$`5arc-min` + facet_grid(. ~ ., switch = "y") +
    theme(legend.position = "none",
          axis.text.y = element_blank(), axis.title.y = element_blank(),
          axis.ticks = element_blank())
  r <- g_cor_both$`10arc-min` + facet_grid(. ~ ., switch = "y") +
    theme(legend.position = "none",
          axis.text.y = element_blank(), axis.title.y = element_blank(),
          axis.ticks = element_blank())
  s <- g_cor_both$`30arc-min` + facet_grid(. ~ ., switch = "y") +
    theme(legend.position = "none",
          axis.text.y = element_blank(), axis.title.y = element_blank(),
          axis.ticks = element_blank())
  t <- g_cor_both$`60arc-min` + facet_grid(. ~ ., switch = "y") +
    theme(legend.position = c(0.75, 0.18),
          legend.title = element_blank(),
          legend.background = element_blank(),
          axis.text.y = element_blank(), axis.title.y = element_blank(),
          axis.ticks = element_blank())


  #Arrange all lines
  first_line <- a | b | c | d | e
  #first_line <- first_line + plot_annotation(tag_levels = "a")
  second_line <- f | g | h | i | j
  third_line <- k | l | m | n | o
  fourth_line <- p | q | r | s | t
  fourth_line <- fourth_line + plot_layout(axis_titles = "collect",
                                           nrow = 1)
  all_lines <- first_line / second_line / third_line / fourth_line
  all_lines
}

arrange_plots4 <- function(g_beta10, g_beta10sesam,
                           g_nest10, g_nest10sesam,
                           g_turn10, g_turn10sesam){
  #PLot like this:
  # a b c d e f
  # g h i j k l
  # m n o p q r
  # s t u v x z

  #Dissimilarity (10% Threshold)
  a <- g_beta10$`2.5arc-min` +
  facet_grid("A\nDissimilarity\n(Threshold-only)" ~ "2.5arc-min", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))

  b <- g_beta10$`5arc-min` +
    facet_grid(. ~ "5arc-min", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  c <- g_beta10$`10arc-min` +
    facet_grid(. ~ "10arc-min", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  d <- g_beta10$`30arc-min` +
    facet_grid(. ~ "30arc-min", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  e <- g_beta10$`60arc-min` +
    facet_grid(. ~ "60arc-min", switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))


  #Dissimilarity (10% thr. and Ranking)
  f <- g_beta10sesam$`2.5arc-min` +
    facet_grid("B\nDissimilarity\n(Threshold-and-ranking)" ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  g <- g_beta10sesam$`5arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  h <- g_beta10sesam$`10arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  i <- g_beta10sesam$`30arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  j <- g_beta10sesam$`60arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))

  #Nestedness (10% Threshold)
  k <- g_nest10$`2.5arc-min` +
    facet_grid("C\nNestedness\n(Threshold-only)" ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  l <- g_nest10sesam$`5arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  m <- g_nest10sesam$`10arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  n <- g_nest10sesam$`30arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  o <- g_nest10sesam$`60arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))

  #Nestedness (10% thr. and Ranking)
  p <- g_nest10sesam$`2.5arc-min` +
    facet_grid("D\nNestedness\n(Threshold-and-ranking)" ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  q <- g_nest10sesam$`5arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  r <- g_nest10sesam$`10arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  s <- g_nest10sesam$`30arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  t <- g_nest10sesam$`60arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))

  #Turnover (10% Threshold)
  u <- g_turn10$`2.5arc-min` +
    facet_grid("E\nTurnover\n(Threshold-only)" ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  v <- g_turn10$`5arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  x <- g_turn10$`10arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  z <- g_turn10$`30arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  y <- g_turn10$`60arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))

  #Turnover (10% thr. and Ranking)
  w <- g_turn10sesam$`2.5arc-min` +
    facet_grid("F\nTurnover\n(Threshold-and-ranking)" ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  a2 <- g_turn10sesam$`5arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  b2 <- g_turn10sesam$`10arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  c2 <- g_turn10sesam$`30arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  d2 <- g_turn10sesam$`60arc-min` +
    facet_grid(. ~ ., switch = "y") +
    theme(strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))


  #Arrange all lines
  first_line <- a | b | c | d | e
  second_line <- f| g | h | i | j
  third_line <- k | l | m | n | o
  fourth_line <- p | q | r | s | t
  fifth_line <- u | v | x | z | y
  sixth_line <- w | a2 | b2 | c2 | d2
  all_lines <- first_line / second_line / third_line / fourth_line / fifth_line / sixth_line
  all_lines
}


