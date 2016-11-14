## Figures

## Multiplot function ----------------------------------------------------------

grid_arrange_shared_legend <- function(...) {
  library(grid)
  library(gridExtra)
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
  # from Hadley github for ggplot2
}

## Time series ---------------------------------------------------------------------
plot_temp_time_series <- function (temp){
  ggplot(temp, aes(x = as.Date(temp$Date), y = temp_avg)) + 
    geom_smooth(method = 'loess', se=T) + 
    geom_point(size=3) + 
    theme_bw() +
    scale_x_date(name="Date") + 
    scale_y_continuous(name=expression (Average ~ Water ~ Temperature ~ (degree ~ C)))+ 
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12), 
          legend.text=element_text(color="black", size=12), 
          legend.title=element_text(color="black", size = 12), 
          legend.position="bottom")  
}

## Kernel density -----------------------------------------------------------------

# ggplot version
plot_kernel_density_reef_complexity <- function (env_data, text_size){
  ggplot(env_data, aes(x=DRR))+
    theme_classic()+
    geom_density(aes(group=Reef_type, colour = Reef_type, fill = Reef_type), kernel = "gaussian", alpha=0.5)+
    #geom_density(aes(x=DRR))+ # all reefs
    labs (x = "Digital reef rugosity (DRR)", y = "Gaussian kernel density") +
    scale_fill_manual(name = "Reef Type", 
                      values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    guides(colour=FALSE)+ # removes legend for color but OK because have legend for fill coded in line above
    theme(axis.text=element_text(size=text_size, colour="black"), 
          axis.title.x=element_text(size=text_size, colour="black", vjust=-.5), 
          axis.title.y=element_text(size=text_size, colour="black", vjust=1.3),
          legend.position = c(.9,.9),
          legend.text = element_text(size=text_size),
          legend.title = element_text(size = text_size),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
}

plot_kern_dens_reefs_complex <- function(env_data){ 
  plot (density(env_data$DRR), ylim = c(0,3.0),lwd=3,col="black", xlab = "Structural Complexity (DRR)",ylab = "Gausian Kernel Density",main = "",cex.axis=1.2, cex.lab=1.2,cex=1.2)
  polygon(density(env_data$DRR[env_data$Reef_type == "Natural"]), col=rgb(0, 0, 1, 0.5), border="NA", lwd=3)
  polygon(density(env_data$DRR[env_data$Reef_type == "Artificial"]),col=rgb(1, 0, 0, 0.5), border="NA", lwd=3)
  legend("topright", legend=c("All", "Artificial", "Natural"),col=c("black", NA, NA),pt.bg=c(rgb(0, 1, 0, 0.6), rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)), pch=c(NA, 22, 22), lty=c(1, NA, NA), lwd=c(3, 10, 10), cex=1.5)
  lines(density(env_data$DRR), col="black", lwd=3)
}

plot_kern_dens_reefs_relief <- function(env_data){ 
  plot (density(env_data$ver_rel), ylim = c(0,0.6),lwd=3,col="black", xlab = "Vertical Relief (m)",ylab = "Gausian Kernel Density",main = "",cex.axis=1.2, cex.lab=1.2,cex=1.2)
  polygon(density(env_data$ver_rel[env_data$Reef_type == "Natural"]), col=rgb(0, 0, 1, 0.5), border="NA", lwd=3)
  polygon(density(env_data$ver_rel[env_data$Reef_type == "Artificial"]),col=rgb(1, 0, 0, 0.5), border="NA", lwd=3)
  legend("topright", legend=c("All", "Artificial", "Natural"),col=c("black", NA, NA),pt.bg=c(rgb(0, 1, 0, 0.6), rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)), pch=c(NA, 22, 22), lty=c(1, NA, NA), lwd=c(3, 10, 10), cex=1.5)
  lines(density(env_data$ver_rel), col="black", lwd=3)
}

## Sediment vs abundance for all reef types ---------------------------------------------

plot_abund_rich_sed <- function(comm_mets, env_data, xlim, ylim_abund, brk_abund, ylim_rich, brk_rich, text_size){
  
  abund_sed <- ggplot() + 
    geom_point(data = comm_mets, aes(x = env_data$sed_avg, y = comm_mets$abund, shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    #geom_smooth(method='loess', se=T, size = 1.5) +
    #stat_smooth(method = "lm", formula = y ~ x, size = 1) + # linear regression
    stat_smooth(data = comm_mets, aes(x=env_data$sed_avg, y = comm_mets$abund), 
                method = "lm", formula = y ~ x + I(x^2), size = 1, colour = "black") + # sed and squared sed terms
    labs (x = "Sediment depth (cm)", y = "Abundance") +
    theme_classic() + 
    geom_text(aes(x = 0, y = 25000), label = "a", size = 4)+
    theme (axis.text = element_text(size = text_size, colour = "black"), 
           axis.title.x = element_text(size= text_size, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size= text_size, colour="black", vjust = 1.0),
           legend.position = "none",
           legend.text = element_text(size= text_size),
           legend.title = element_text(size = text_size),
           axis.line.x = element_line(color="black", size = 0.5),
           axis.line.y = element_line(color="black", size = 0.5)) +
    scale_y_continuous  (limits=c(-350, ylim_abund), breaks = seq(0,ylim_abund, by = brk_abund)) +
    scale_x_continuous (limits=c(0, 22)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))
  #theme(plot.margin = unit(c(-1, 0.5, 0.5, 0.5), "lines"))
  
  rich_sed <- ggplot() + 
    geom_point(data = comm_mets, aes(x = env_data$sed_avg, y = comm_mets$S, shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    #geom_smooth(method='loess', se=T, size = 1.5) +
    stat_smooth(data = comm_mets, aes(x=env_data$sed_avg, y = comm_mets$S), method = "lm", formula = y ~ x, size = 1, colour = "black") + # linear regression
    #stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, colour = "black") + # DRR and squared DRR terms
    labs (x = "Sediment depth (cm)", y = "Richness") +
    theme_classic() + 
    geom_text(aes(x = 0, y = 50), label = "b", size = 4)+
    theme (axis.text = element_text(size = text_size, colour = "black"), 
           axis.title.x = element_text(size=text_size, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=text_size, colour="black", vjust = 1.0),
           legend.position = "none",
           legend.text = element_text(size=text_size),
           legend.title = element_text(size = text_size),
           axis.line.x = element_line(color="black", size = 0.5),
           axis.line.y = element_line(color="black", size = 0.5)) +
    scale_y_continuous  (limits=c(-1, ylim_rich), breaks = seq(0,ylim_rich, by = brk_rich)) +
    scale_x_continuous (limits=c(0, 22)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))
  #theme(plot.margin = unit(c(0.5, 0.5, -1, 0.5), "lines"))
  
  library(gtable)
  gp1 <- ggplot_gtable(ggplot_build(abund_sed))
  gp2 <- ggplot_gtable(ggplot_build(rich_sed))
  
  maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
  gp1$widths[2:3] <- maxWidth
  gp2$widths[2:3] <- maxWidth

  grid.arrange(gp1, gp2)
  
  # code to align grids found here: http://stackoverflow.com/questions/15016995/how-to-align-multiple-ggplot2-plots-and-add-shadows-over-all-of-them
  
  #library(cowplot)
  #plot_grid(abund_drr, abund_sed, rich_drr, rich_sed, ncol=2,align="v")
  
}   

## Rugosity vs. abundance and richness, as well as sediment depth for all reef types ---------


plot_abund_rich_rugosity_sed <- function(comm_mets, env_data, xlim, ylim_abund, brk_abund, ylim_rich, brk_rich, text_size){
  
  abund_drr <- ggplot() + 
    geom_point(data = comm_mets, aes(x = env_data$DRR, y = comm_mets$abund, shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    #geom_smooth(method='loess', se=T, size = 1.5) +
    #stat_smooth(method = "lm", formula = y ~ x, size = 1) + # linear regression
    stat_smooth(data = comm_mets, aes(x=env_data$DRR, y = comm_mets$abund), method = "lm", formula = y ~ x + I(x^2), size = 1, colour = "black") + # DRR and squared DRR terms
    labs (x = "Digital reef rugosity (DRR)", y = "Abundance") +
    theme_classic() + 
    theme (axis.text = element_text(size = text_size, colour = "black"), 
           axis.title.x = element_text(size= text_size, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size= text_size, colour="black", vjust = 1.0),
           legend.position = "none",
           legend.text = element_text(size= text_size),
           legend.title = element_text(size = text_size)) +
    scale_y_continuous  (limits=c(-1, ylim_abund), breaks = seq(0,ylim_abund, by = brk_abund)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))
    #theme(plot.margin = unit(c(-1, 0.5, 0.5, 0.5), "lines"))

  abund_sed <- ggplot() + 
    geom_point(data = comm_mets, aes(x = env_data$sed_avg, y = comm_mets$abund, shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    #geom_smooth(method='loess', se=T, size = 1.5) +
    #stat_smooth(method = "lm", formula = y ~ x, size = 1) + # linear regression
    stat_smooth(data = comm_mets, aes(x=env_data$sed_avg, y = comm_mets$abund), 
                method = "lm", formula = y ~ x + I(x^2), size = 1, colour = "black") + # sed and squared sed terms
    labs (x = "Sediment depth (cm)", y = "Abundance") +
    theme_classic() + 
    theme (axis.text = element_text(size = text_size, colour = "black"), 
           axis.title.x = element_text(size= text_size, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size= text_size, colour="black", vjust = 1.0),
           legend.position = "none",
           legend.text = element_text(size= text_size),
           legend.title = element_text(size = text_size)) +
    scale_y_continuous  (limits=c(-350, ylim_abund), breaks = seq(0,ylim_abund, by = brk_abund)) +
    scale_x_continuous (limits=c(0, 22)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))
    #theme(plot.margin = unit(c(-1, 0.5, 0.5, 0.5), "lines"))
  
  rich_drr <- ggplot() + 
    geom_point(data = comm_mets, aes(x = env_data$DRR, y = comm_mets$S, shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    #geom_smooth(method='loess', se=T, size = 1.5) +
    stat_smooth(data = comm_mets, aes(x=env_data$DRR, y = comm_mets$S), method = "lm", formula = y ~ x, size = 1, colour = "black") + # linear regression
    #stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, colour = "black") + # DRR and squared DRR terms
    labs (x = "Digital reef rugosity (DRR)", y = "Richness") +
    theme_classic() + 
    theme (axis.text = element_text(size = text_size, colour = "black"), 
           axis.title.x = element_text(size=text_size, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=text_size, colour="black", vjust = 1.0),
           legend.position = "none",
           legend.text = element_text(size=text_size),
           legend.title = element_text(size = text_size),
           axis.line.x = element_line(color="black", size = 0.5),
           axis.line.y = element_line(color="black", size = 0.5)) +
    scale_y_continuous  (limits=c(-1, ylim_rich), breaks = seq(0,ylim_rich, by = brk_rich)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))
    #theme(plot.margin = unit(c(0.5, 0.5, -1, 0.5), "lines"))
  
  rich_sed <- ggplot() + 
    geom_point(data = comm_mets, aes(x = env_data$sed_avg, y = comm_mets$S, shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    #geom_smooth(method='loess', se=T, size = 1.5) +
    stat_smooth(data = comm_mets, aes(x=env_data$sed_avg, y = comm_mets$S), method = "lm", formula = y ~ x, size = 1, colour = "black") + # linear regression
    #stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, colour = "black") + # DRR and squared DRR terms
    labs (x = "Sediment depth (cm)", y = "Richness") +
    theme_classic() + 
    theme (axis.text = element_text(size = text_size, colour = "black"), 
           axis.title.x = element_text(size=text_size, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=text_size, colour="black", vjust = 1.0),
           legend.position = "none",
           legend.text = element_text(size=text_size),
           legend.title = element_text(size = text_size),
           axis.line.x = element_line(color="black", size = 0.5),
           axis.line.y = element_line(color="black", size = 0.5)) +
    scale_y_continuous  (limits=c(-1, ylim_rich), breaks = seq(0,ylim_rich, by = brk_rich)) +
    scale_x_continuous (limits=c(0, 22)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))
    #theme(plot.margin = unit(c(0.5, 0.5, -1, 0.5), "lines"))
  
  library(gtable)
  gp1 <- ggplot_gtable(ggplot_build(abund_drr))
  gp2 <- ggplot_gtable(ggplot_build(abund_sed))
  gp3 <- ggplot_gtable(ggplot_build(rich_drr))
  gp4 <- ggplot_gtable(ggplot_build(rich_sed))
  
  maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3], gp3$widths[2:3], gp4$widths[2:3])
  gp1$widths[2:3] <- maxWidth
  gp2$widths[2:3] <- maxWidth
  gp3$widths[2:3] <- maxWidth
  gp4$widths[2:3] <- maxWidth
  grid.arrange(gp1, gp2, gp3, gp4)
  
  # code to align grids found here: http://stackoverflow.com/questions/15016995/how-to-align-multiple-ggplot2-plots-and-add-shadows-over-all-of-them

  #library(cowplot)
  #plot_grid(abund_drr, abund_sed, rich_drr, rich_sed, ncol=2,align="v")

}   



## Rugosity vs. abundance and richness for all reef types -----------

plot_abund_rich_rugosity <- function(comm_mets, env_data, xlim, ylim_abund, brk_abund, ylim_rich, brk_rich, text_size){
  
  abund <- ggplot() + 
    geom_point(data = comm_mets, aes(x = env_data$DRR, y = comm_mets$abund, shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    #geom_smooth(method='loess', se=T, size = 1.5) +
    #stat_smooth(method = "lm", formula = y ~ x, size = 1) + # linear regression
    stat_smooth(data = comm_mets, aes(x=env_data$DRR, y = comm_mets$abund), method = "lm", formula = y ~ x + I(x^2), size = 1, colour = "black") + # DRR and squared DRR terms
    labs (x = "", y = "Abundance") +
    theme_classic() + 
    theme (axis.text = element_text(size = text_size, colour = "black"), 
           axis.title.x = element_text(size= text_size, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size= text_size, colour="black", vjust = 1.0),
           legend.position = "none",
           legend.text = element_text(size= text_size),
           legend.title = element_text(size = text_size)) +
    scale_y_continuous  (limits=c(-1, ylim_abund), breaks = seq(0,ylim_abund, by = brk_abund)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))+
    theme(plot.margin = unit(c(-1, 0.5, 0.5, 0.5), "lines"))
  
  rich <- ggplot() + 
    geom_point(data = comm_mets, aes(x = env_data$DRR, y = comm_mets$S, shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    #geom_smooth(method='loess', se=T, size = 1.5) +
    stat_smooth(data = comm_mets, aes(x=env_data$DRR, y = comm_mets$S), method = "lm", formula = y ~ x, size = 1, colour = "black") + # linear regression
    #stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, colour = "black") + # DRR and squared DRR terms
    labs (x = "Digital reef rugosity (DRR)", y = "Richness") +
    theme_classic() + 
    theme (axis.text = element_text(size = text_size, colour = "black"), 
           axis.title.x = element_text(size=text_size, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=text_size, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=text_size),
           legend.title = element_text(size = text_size)) +
    scale_y_continuous  (limits=c(-1, ylim_rich), breaks = seq(0,ylim_rich, by = brk_rich)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))+
    theme(plot.margin = unit(c(0.5, 0.5, -1, 0.5), "lines"))
  library(gtable)
  gp1 <- ggplot_gtable(ggplot_build(abund))
  gp2 <- ggplot_gtable(ggplot_build(rich))
  maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
  gp1$widths[2:3] <- maxWidth
  gp2$widths[2:3] <- maxWidth
  grid.arrange(gp1, gp2)
  
  # code to align grids found here: http://stackoverflow.com/questions/15016995/how-to-align-multiple-ggplot2-plots-and-add-shadows-over-all-of-them
  
  #grid_arrange_shared_legend(gp2, gp1, ncol = 1)  
}   

## Heterogeneity vs. abundance for all fish and all sizes for different reef types---------------


plot_rugosity_abund_sizes_art <- function(comm_mets.art, comm_sm.art, comm_md.art, comm_lg.art, comm_ap.art, text_size){
  ggplot()+
    theme_classic()+
    stat_smooth(data = comm_mets.art, 
                aes(x=comm_mets.art$DRR, y = comm_mets.art$abund, colour =  "All fish"), 
                method = lm, formula = y ~ x + I(x^2), se=T, lwd = 1.5) +
    stat_smooth(data = comm_sm.art, 
                aes(x=comm_sm.art$DRR, y = comm_sm.art$abund, colour =  "Small (1-10 cm)"), 
                method = lm, formula = y ~ x + I(x^2), se=T, lwd = 1.5) +
    stat_smooth(data = comm_md.art, 
                aes(x=comm_md.art$DRR, y = comm_md.art$abund, colour =  "Medium (11-29 cm)"), 
                method = lm, formula = y ~ x + I(x^2), se=T, lwd = 1.5) +
    stat_smooth(data = comm_lg.art, 
                aes(x=comm_lg.art$DRR, y = comm_lg.art$abund, colour = "Large (30-49 cm)"), 
                method = lm, formula = y ~ x + I(x^2), se=T, lwd = 1.5) +    
    stat_smooth(data = comm_ap.art, 
                aes(x=comm_ap.art$DRR, y = comm_ap.art$abund, colour = "Apex (>50 cm)"), 
                method = lm, formula = y ~ x, se=T, lwd = 1.5, linetype="dashed") + 
    scale_colour_manual("", 
                        values = c("All fish" = "black", "Small (1-10 cm)" = "blue", "Medium (11-29 cm)" = "green", "Large (30-49 cm)" = "red", "Apex (>50 cm)" = "orange"))+
    #scale_linetype_manual(name = "Reef Type", values = c("Artificial" = 1, "Natural" = 2), guide = FALSE)+
    labs (x = "Digital Reef Rugosity (DRR)", y = "Abundance") +
    scale_x_continuous (limits=c(0, 3.5)) + 
    scale_y_continuous  (limits=c(0, 2000),breaks = seq(0,2000, by = 500))+ 
    theme(axis.text=element_text(size=text_size, colour="black"), 
          axis.title.x=element_text(size=text_size, colour="black", vjust=-.5), 
          axis.title.y=element_text(size=text_size, colour="black", vjust=1.3),
          legend.position = c(0.2,.85),
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5)) 
}

plot_rugosity_abund_sizes_nat <- function(comm_mets.nat, comm_sm.nat, comm_md.nat, comm_lg.nat, comm_ap.nat, text_size){
  ggplot()+
    theme_classic()+
    stat_smooth(data = comm_mets.nat, 
                aes(x=comm_mets.nat$DRR, y = comm_mets.nat$abund, colour =  "All fish"), 
                #method = lm, formula = y ~ x, se=T, lwd = 1.5) +
                method = lm, formula = y ~ x + I(x^2), se=T, lwd = 1.5) +
    stat_smooth(data = comm_sm.nat, 
                aes(x=comm_sm.nat$DRR, y = comm_sm.nat$abund, colour =  "Small (1-10 cm)"), 
                method = lm, formula = y ~ x, se=T, lwd = 1.5, linetype="dashed") +
                #method = lm, formula = y ~ x + I(x^2), se=T, lwd = 1.5) +
    stat_smooth(data = comm_md.nat, 
                aes(x=comm_md.nat$DRR, y = comm_md.nat$abund, colour =  "Medium (11-29 cm)"), 
                method = lm, formula = y ~ x, se=T, lwd = 1.5, linetype="dashed") +
                #method = lm, formula = y ~ x + I(x^2), se=T, lwd = 1.5) +
    stat_smooth(data = comm_lg.nat, 
                aes(x=comm_lg.nat$DRR, y = comm_lg.nat$abund, colour = "Large (30-49 cm)"), 
                method = lm, formula = y ~ x, se=T, lwd = 1.5, linetype="dashed") +
                #method = lm, formula = y ~ x + I(x^2), se=T, lwd = 1.5) +
    stat_smooth(data = comm_ap.nat, 
                aes(x=comm_ap.nat$DRR, y = comm_ap.nat$abund, colour = "Apex (>50 cm)"), 
                #method = lm, formula = y ~ x, se=T, lwd = 1.5) + 
                method = lm, formula = y ~ I(x^2), se=T, lwd = 1.5) +
    scale_colour_manual("Size", 
                        values = c("All fish" = "black", "Small (1-10 cm)" = "blue", "Medium (11-29 cm)" = "green", "Large (30-49 cm)" = "red", "Apex (>50 cm)" = "orange"))+
    #scale_linetype_manual(name = "Reef Type", values = c("Artificial" = 1, "Natural" = 2), guide = FALSE)+
    labs (x = "Digital Reef Rugosity (DRR)", y = "Abundance") +
    scale_x_continuous (limits=c(0, 1)) + 
    scale_y_continuous  (limits=c(0, 2500),breaks = seq(0,2500, by = 500))+ 
    theme(axis.text=element_text(size=text_size, colour="black"), 
          axis.title.x=element_text(size=text_size, colour="black", vjust=-.5), 
          axis.title.y=element_text(size=text_size, colour="black", vjust=1.3),
          legend.position = c(0.2,.85),
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5)) 
}



## Niche width and species richness --------------------------------------


plot_comm_mets_niche_widths <- function(comm_mets, env_data, text_size){
  ggplot()+
    theme_classic()+
    
    # Niche 1
    #geom_smooth(data = comm_mets, 
    #           aes(x=env_data$DRR, y = comm_mets$S.niche1, color =  "Niche1", linetype = env_data$Reef_type), 
    #            method = loess, se=T, lwd = 1.5) +
    #geom_smooth(data = comm_mets, 
    #            aes(x=env_data$DRR, y = comm_mets$S.niche2, color =  "Niche2", linetype = env_data$Reef_type), 
    #            method = loess, se=T, lwd = 1.5) +
    #geom_smooth(data = comm_mets, 
    #            aes(x=env_data$DRR, y = comm_mets$S.niche3, color = "Niche3", linetype = env_data$Reef_type), 
    #            method = loess, se=T, lwd = 1.5) +    
    #geom_smooth(data = comm_mets, 
    #            aes(x=env_data$DRR, y = comm_mets$S.niche4, color = "Niche4", linetype = env_data$Reef_type), 
    #            method = loess, se=T, lwd = 1.5) + 
    stat_smooth(data = comm_mets, 
                aes(x=env_data$DRR, y = comm_mets$S.niche1, color =  "Niche1", linetype = env_data$Reef_type),
                method = "lm", formula = y ~ x + I(x^2), size = 1) + # DRR and squared DRR terms
    stat_smooth(data = comm_mets, 
                aes(x=env_data$DRR, y = comm_mets$S.niche2, color =  "Niche2", linetype = env_data$Reef_type), 
                method = "lm", formula = y ~ x + I(x^2), size = 1) + # DRR and squared DRR terms
    stat_smooth(data = comm_mets, 
                aes(x=env_data$DRR, y = comm_mets$S.niche3, color = "Niche3", linetype = env_data$Reef_type), 
                method = "lm", formula = y ~ x + I(x^2), size = 1) + # DRR and squared DRR terms
    stat_smooth(data = comm_mets, 
                aes(x=env_data$DRR, y = comm_mets$S.niche4, color = "Niche4", linetype = env_data$Reef_type), 
                method = "lm", formula = y ~ x + I(x^2), size = 1) + # DRR and squared DRR terms
    
    scale_colour_manual(name = "Niche width quartile", 
                        values = c("Niche1" = "blue", "Niche2" = "green", "Niche3" = "red", "Niche4" = "black"), 
                        labels=c("0-25%", "25-50%", "50-75%", "75-100%"))+
    scale_linetype_manual(name = "Reef Type", values = c("Artificial" = 1, "Natural" = 2), guide = FALSE)+
    labs (x = "Digital Reef Rugosity (DRR)", y = "Species Richness (S)") +
    scale_x_continuous (limits=c(0, 3.5)) + 
    scale_y_continuous  (limits=c(0, 20),breaks = seq(0,20, by = 5))+ 
    theme(axis.text=element_text(size=text_size, colour="black"), 
          axis.title.x=element_text(size=text_size, colour="black", vjust=-.5), 
          axis.title.y=element_text(size=text_size, colour="black", vjust=1.3),
          legend.position = c(0.15,.85),
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12)) 
}

## Climate zones ---------------------------------------------------------

plot_comm_mets_climate <- function(comm_mets, env_data, text_size){
  ggplot()+
    theme_classic()+
  # climate zones 
    #geom_smooth(data = comm_mets, 
    #            aes(x=env_data$DRR, y = comm_mets$S.Tropical, colour =  "Tropical", linetype = env_data$Reef_type), 
    #            method = loess, se=T, lwd = 1.5) +
    #geom_smooth(data = comm_mets, 
    #            aes(x=env_data$DRR, y = comm_mets$S.Subtropical, colour =  "Subtropical", linetype = env_data$Reef_type), 
    #            method = loess, se=T, lwd = 1.5) +
    #geom_smooth(data = comm_mets, 
    #            aes(x=env_data$DRR, y = comm_mets$S.Temperate, colour = "Temperate", linetype = env_data$Reef_type), 
    #            method = loess, se=T, lwd = 1.5) +
    stat_smooth(data = comm_mets, 
                aes(x=env_data$DRR, y = comm_mets$S.Tropical, colour =  "Tropical", linetype = env_data$Reef_type),
                method = "lm", formula = y ~ x + I(x^2), size = 1) + # DRR and squared DRR terms
    stat_smooth(data = comm_mets, 
                aes(x=env_data$DRR, y = comm_mets$S.Subtropical, colour =  "Subtropical", linetype = env_data$Reef_type),
                method = "lm", formula = y ~ x + I(x^2), size = 1) + # DRR and squared DRR terms
    stat_smooth(data = comm_mets, 
                aes(x=env_data$DRR, y = comm_mets$S.Temperate, colour = "Temperate", linetype = env_data$Reef_type),
                method = "lm", formula = y ~ x + I(x^2), size = 1) + # DRR and squared DRR terms
    
   # scale_colour_manual(name = "Climate zone", 
  #                      values = c("Tropical" = "blue", "Subtropical" = "green", "Temperate" = "red"), 
   #                     labels=c("Tropical", "Subtropical", "Temperate"))+
    scale_colour_manual(name = "Climate zone", 
                        values = c("Tropical" = "blue", "Subtropical" = "green", "Temperate" = "red"))+
    scale_linetype_manual(name = "Reef Type", values = c("Artificial" = 1, "Natural" = 2), guide = FALSE)+
    labs (x = "Digital Reef Rugosity (DRR)", y = "Species Richness (S)") +
    scale_x_continuous (limits=c(0, 3.5)) + 
    scale_y_continuous  (limits=c(0, 20),breaks = seq(0,20, by = 5))+ 
    theme(axis.text=element_text(size=text_size, colour="black"), 
          axis.title.x=element_text(size=text_size, colour="black", vjust=-.5), 
          axis.title.y=element_text(size=text_size, colour="black", vjust=1.3),
          legend.position = c(0.15,.85),
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12)) 
}



## Community metrics ------------------------------------------------------

plot_abund_rugosity <- function(comm_mets, env_data, xlim, ylim_abund, brk_abund){
  ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$abund, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
    geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=T, size = 1.5) +
    #stat_smooth(method = "lm", formula = y ~ x, size = 1) + # linear regression
    #stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) + # DRR and squared DRR terms
    labs (x = "Digital Reef Rugosity (DRR)", y = "Density") +
    theme_bw() + 
    theme (axis.text = element_text(size = 18, colour = "black"), 
           axis.title.x = element_text(size=18, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=18, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=18),
           legend.title = element_text(size = 18)) +
    scale_y_continuous  (limits=c(-1, ylim_abund), breaks = seq(0,ylim_abund, by = brk_abund)) +
    #scale_y_continuous  (limits=c(0, 15000), breaks = seq(0,15000, by = 3000)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))
}   

plot_rich_rugosity <- function(comm_mets, env_data, xlim, ylim_rich, brk_rich){
  ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$S, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
    geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=T, size = 1.5) +
    #stat_smooth(method = "lm", formula = y ~ x, size = 1) + # linear regression
    #stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) + # DRR and squared DRR terms
    labs (x = "Digital Reef Rugosity (DRR)", y = "Species richness") +
    theme_bw() + 
    theme (axis.text = element_text(size = 18, colour = "black"), 
           axis.title.x = element_text(size=18, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=18, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=18),
           legend.title = element_text(size = 18)) +
    scale_y_continuous  (limits=c(-1, ylim_rich), breaks = seq(0,ylim_rich, by = brk_rich)) +
    #scale_y_continuous  (limits=c(0, 15000), breaks = seq(0,15000, by = 3000)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))
}   


plot_comm_mets_reefs_rugosity <- function(comm_mets, env_data, xlim, ylim_rich, ylim_biom, ylim_abund, ylim_shan, brk_rich, brk_biom, brk_abund, brk_shan){
  abund <- ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$abund, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
    geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    #geom_smooth(method='loess', se=T, size = 1.5) +
    #stat_smooth(method = "lm", formula = y ~ x, size = 1) + # linear regression
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) + # DRR and squared DRR terms
    labs (x = " ", y = "Density (per 120 m^2)") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_y_continuous  (limits=c(-1, ylim_abund), breaks = seq(0,ylim_abund, by = brk_abund)) +
    #scale_y_continuous  (limits=c(0, 15000), breaks = seq(0,15000, by = 3000)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))   
  S <- ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$S, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
    geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    #geom_smooth(method='loess', se=T, size = 1.5) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) + # DRR and squared DRR terms
    #stat_smooth(method = "lm", formula = y ~ x, size = 1) + # linear regression
    labs (x = "Digital Reef Rugosity (DRR)", y = "Species Richness (S)") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_y_continuous  (limits=c(0, ylim_rich),breaks = seq(0,ylim_rich, by = brk_rich)) +
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
  shan <- ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$shan, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
    geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    #geom_smooth(method='loess', se=T, size = 1.5) +
    #stat_smooth(method = "lm", formula = y ~ x, size = 1) + # linear regression
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) + # DRR and squared DRR terms
    labs (x = "Digital Reef Rugosity (DRR)", y = "Shannon Diversity") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_y_continuous  (limits=c(0, ylim_shan),breaks = seq(0,ylim_shan, by = brk_shan)) +
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
  biom <- ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$biom, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
    geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=3, position=position_jitter(width=0.0,height=0.0)) +
    #geom_smooth(method='loess', se=T, size = 1.5) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) + # DRR and squared DRR terms
    #stat_smooth(method = "lm", formula = y ~ x, size = 1) + # linear regression
    labs (x = " ", y = "Biomass (kg)") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_y_continuous  (limits=c(0, ylim_biom),breaks = seq(0,ylim_biom, by = brk_biom)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))  
#   J <- ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$J, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
#     geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=4, position=position_jitter(width=0.0,height=0.0)) +
#     geom_smooth(method='loess', se=T) +
#     labs (x = "Digital Reef Rugosity (DRR)", y = "Pielou's Evenness (J)") +
#     theme_bw() + 
#     theme (axis.text = element_text(size = 12, colour = "black"), 
#            axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
#            axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
#            legend.position = "bottom",
#            legend.text = element_text(size=12),
#            legend.title = element_text(size = 12)) +
#     scale_x_continuous (limits=c(0, 3.5)) + 
#scale_colour_manual(name = "Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
#  scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
grid_arrange_shared_legend(abund, biom, S, shan, ncol = 2)
}

plot_comm_mets_depth_rugosity <- function(comm_mets, env_data, xlim, ylim_rich, ylim_biom, ylim_abund, ylim_shan, brk_rich, brk_biom, brk_abund, brk_shan){
  abund <- ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$abund, shape = env_data$Reef_type, colour = env_data$Depth_general)) + 
    #geom_point(aes(shape=env_data$Reef_type, colour = env_data$Depth_general),size=4, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=F) +
    labs (x = " ", y = "Density (per 120 m^2)") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_y_continuous  (limits=c(0, ylim_abund), breaks = seq(-1,ylim_abund, by = brk_abund)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Depth", values = c("Shallow" = "red", "Intermediate" = "blue", "Deep" = "black"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))   
  S <- ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$S, shape = env_data$Reef_type, colour = env_data$Depth_general)) + 
    #geom_point(aes(shape=env_data$Reef_type, colour = env_data$Depth_general),size=4, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=F) +
    labs (x = "Digital Reef Rugosity (DRR)", y = "Species Richness (S)") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_y_continuous  (limits=c(0, ylim_rich),breaks = seq(0,ylim_rich, by = brk_rich)) +
    scale_colour_manual(name = "Depth", values = c("Shallow" = "red", "Intermediate" = "blue", "Deep" = "black"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
  shan <- ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$shan, shape = env_data$Reef_type, colour = env_data$Depth_general)) + 
    #geom_point(aes(shape=env_data$Reef_type, colour = env_data$Depth_general),size=4, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=F) +
    labs (x = "Digital Reef Rugosity (DRR)", y = "Shannon Diversity") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_y_continuous  (limits=c(0, ylim_shan),breaks = seq(0,ylim_shan, by = brk_shan)) +
    scale_colour_manual(name = "Depth", values = c("Shallow" = "red", "Intermediate" = "blue", "Deep" = "black"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
  biom <- ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$biom, shape = env_data$Reef_type, colour = env_data$Depth_general)) + 
    #geom_point(aes(shape=env_data$Reef_type, colour = env_data$Depth_general),size=4, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=F) +
    labs (x = " ", y = "Biomass (kg)") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_y_continuous  (limits=c(0, ylim_biom),breaks = seq(0,ylim_biom, by = brk_biom)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Depth", values = c("Shallow" = "red", "Intermediate" = "blue", "Deep" = "black"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))  
  #   J <- ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$J, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
  #     geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=4, position=position_jitter(width=0.0,height=0.0)) +
  #     geom_smooth(method='loess', se=T) +
  #     labs (x = "Digital Reef Rugosity (DRR)", y = "Pielou's Evenness (J)") +
  #     theme_bw() + 
  #     theme (axis.text = element_text(size = 12, colour = "black"), 
  #            axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
  #            axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
  #            legend.position = "bottom",
  #            legend.text = element_text(size=12),
  #            legend.title = element_text(size = 12)) +
  #     scale_x_continuous (limits=c(0, 3.5)) + 
  #scale_colour_manual(name = "Reef Type", values = c("Artificial" = "red", "Natural" = "blue"))+
  #  scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
  grid_arrange_shared_legend(abund, biom, S, shan, ncol = 2)
}

plot_comm_mets_reefs_relief <- function(comm_mets, env_data, xlim, ylim_rich, ylim_biom, ylim_abund, ylim_shan, brk_rich, brk_biom, brk_abund, brk_shan){
  abund <- ggplot(comm_mets, aes( x = env_data$ver_rel, y = comm_mets$abund, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
    #geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=4, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=F) +
    labs (x = " ", y = "Density (per 120 m^2)") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_y_continuous  (limits=c(0, ylim_abund), breaks = seq(0,ylim_abund, by = brk_abund)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "red", "Natural" = "blue"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
  S <- ggplot(comm_mets, aes( x = env_data$ver_rel, y = comm_mets$S, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
    #geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=4, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=F) +
    labs (x = "Vertical Relief (m)", y = "Species Richness (S)") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_y_continuous  (limits=c(0, ylim_rich),breaks = seq(0,ylim_rich, by = brk_rich)) +
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "red", "Natural" = "blue"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
  shan <- ggplot(comm_mets, aes( x = env_data$ver_rel, y = comm_mets$shan, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
    #geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=4, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=F) +
    labs (x = "Vertical Relief (m)", y = "Shannon Diversity") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_y_continuous  (limits=c(0, ylim_shan),breaks = seq(0,ylim_shan, by = brk_shan)) +
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "red", "Natural" = "blue"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
  biom <- ggplot(comm_mets, aes( x = env_data$ver_rel, y = comm_mets$biom, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
    #geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=4, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=F) +
    labs (x = " ", y = "Biomass (kg)") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_y_continuous  (limits=c(0, ylim_biom),breaks = seq(0,ylim_biom, by = brk_biom)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Reef Type", values = c("Artificial" = "red", "Natural" = "blue"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
  #   J <- ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$J, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
  #     geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=4, position=position_jitter(width=0.0,height=0.0)) +
  #     geom_smooth(method='loess', se=T) +
  #     labs (x = "Digital Reef Rugosity (DRR)", y = "Pielou's Evenness (J)") +
  #     theme_bw() + 
  #     theme (axis.text = element_text(size = 12, colour = "black"), 
  #            axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
  #            axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
  #            legend.position = "bottom",
  #            legend.text = element_text(size=12),
  #            legend.title = element_text(size = 12)) +
  #     scale_x_continuous (limits=c(0, 3.5)) + 
  #scale_colour_manual(name = "Reef Type", values = c("Artificial" = "red", "Natural" = "blue"))+
  #  scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
  grid_arrange_shared_legend(abund, biom, S, shan, ncol = 2)
}

plot_comm_mets_depth_relief <- function(comm_mets, env_data, xlim, ylim_rich, ylim_biom, ylim_abund, ylim_shan, brk_rich, brk_biom, brk_abund, brk_shan){
  abund <- ggplot(comm_mets, aes( x = env_data$ver_rel, y = comm_mets$abund, shape = env_data$Reef_type, colour = env_data$Depth_general)) + 
    #geom_point(aes(shape=env_data$Reef_type, colour = env_data$Depth_general),size=4, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=F) +
    labs (x = " ", y = "Density (per 120 m^2)") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_y_continuous  (limits=c(0, ylim_abund), breaks = seq(0,ylim_abund, by = brk_abund)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Depth", values = c("Shallow" = "red", "Intermediate" = "blue", "Deep" = "black"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))   
  S <- ggplot(comm_mets, aes( x = env_data$ver_rel, y = comm_mets$S, shape = env_data$Reef_type, colour = env_data$Depth_general)) + 
    #geom_point(aes(shape=env_data$Reef_type, colour = env_data$Depth_general),size=4, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=F) +
    labs (x = "Vertical Relief (m)", y = "Species Richness (S)") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_y_continuous  (limits=c(0, ylim_rich),breaks = seq(0,ylim_rich, by = brk_rich)) +
    scale_colour_manual(name = "Depth", values = c("Shallow" = "red", "Intermediate" = "blue", "Deep" = "black"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
  shan <- ggplot(comm_mets, aes( x = env_data$ver_rel, y = comm_mets$shan, shape = env_data$Reef_type, colour = env_data$Depth_general)) + 
    #geom_point(aes(shape=env_data$Reef_type, colour = env_data$Depth_general),size=4, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=F) +
    labs (x = "Vertical Relief (m)", y = "Shannon Diversity") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_y_continuous  (limits=c(0, ylim_shan),breaks = seq(0,ylim_shan, by = brk_shan)) +
    scale_colour_manual(name = "Depth", values = c("Shallow" = "red", "Intermediate" = "blue", "Deep" = "black"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
  biom <- ggplot(comm_mets, aes( x = env_data$ver_rel, y = comm_mets$biom, shape = env_data$Reef_type, colour = env_data$Depth_general)) + 
    #geom_point(aes(shape=env_data$Reef_type, colour = env_data$Depth_general),size=4, position=position_jitter(width=0.0,height=0.0)) +
    geom_smooth(method='loess', se=F) +
    labs (x = " ", y = "Biomass (kg)") +
    theme_bw() + 
    theme (axis.text = element_text(size = 12, colour = "black"), 
           axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
           axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
           legend.position = "bottom",
           legend.text = element_text(size=12),
           legend.title = element_text(size = 12)) +
    scale_y_continuous  (limits=c(0, ylim_biom),breaks = seq(0,ylim_biom, by = brk_biom)) +
    scale_x_continuous (limits=c(0, xlim)) + 
    scale_colour_manual(name = "Depth", values = c("Shallow" = "red", "Intermediate" = "blue", "Deep" = "black"))+
    scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16))  
  #   J <- ggplot(comm_mets, aes( x = env_data$DRR, y = comm_mets$J, shape = env_data$Reef_type, colour = env_data$Reef_type)) + 
  #     geom_point(aes(shape=env_data$Reef_type, colour = env_data$Reef_type),size=4, position=position_jitter(width=0.0,height=0.0)) +
  #     geom_smooth(method='loess', se=T) +
  #     labs (x = "Digital Reef Rugosity (DRR)", y = "Pielou's Evenness (J)") +
  #     theme_bw() + 
  #     theme (axis.text = element_text(size = 12, colour = "black"), 
  #            axis.title.x = element_text(size=12, colour = "black", vjust = -0.5), 
  #            axis.title.y = element_text(size=12, colour="black", vjust = 1.0),
  #            legend.position = "bottom",
  #            legend.text = element_text(size=12),
  #            legend.title = element_text(size = 12)) +
  #     scale_x_continuous (limits=c(0, 3.5)) + 
  #scale_colour_manual(name = "Reef Type", values = c("Artificial" = "red", "Natural" = "blue"))+
  #  scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
  grid_arrange_shared_legend(abund, biom, S, shan, ncol = 2)
}


## Heterogeneity for fish of different size classes -------------------------------------

# abundance
plot_comm_mets_fish_sizes_abund <- function(comm_sm, comm_md, comm_lg, comm_ap, env_sizes, text_size){
  ggplot()+
    theme_classic()+
    geom_smooth(data = comm_sm, 
                aes(x=env_sizes$DRR, y = comm_sm$abund, colour =  "Small (1-10 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) +
    geom_smooth(data = comm_md, 
                aes(x=env_sizes$DRR, y = comm_md$abund, colour =  "Medium (11-29 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) +
    geom_smooth(data = comm_lg, 
                aes(x=env_sizes$DRR, y = comm_lg$abund, colour = "Large (30-49 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) +    
    geom_smooth(data = comm_ap, 
                aes(x=env_sizes$DRR, y = comm_ap$abund, colour = "Apex (>50 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) + 
    scale_colour_manual("Size", 
                        values = c("Small (1-10 cm)" = "blue", "Medium (11-29 cm)" = "green", "Large (30-49 cm)" = "red", "Apex (>50 cm)" = "black"))+
    scale_linetype_manual(name = "Reef Type", values = c("Artificial" = 1, "Natural" = 2), guide = FALSE)+
    labs (x = "Digital Reef Rugosity (DRR)", y = "Abundance") +
    scale_x_continuous (limits=c(0, 3.5)) + 
    scale_y_continuous  (limits=c(0, 15000),breaks = seq(0,15000, by = 5000))+ 
    theme(axis.text=element_text(size=text_size, colour="black"), 
          axis.title.x=element_text(size=text_size, colour="black", vjust=-.5), 
          axis.title.y=element_text(size=text_size, colour="black", vjust=1.3),
          legend.position = c(0.15,.85),
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12)) 
}

# abundance of just large and apex fish (different scales than above)
plot_comm_mets_fish_sizes_abund_lg_ap <- function(comm_sm, comm_md, comm_lg, comm_ap, env_sizes, text_size){
  ggplot()+
    theme_classic()+
    geom_smooth(data = comm_lg, 
                aes(x=env_sizes$DRR, y = comm_lg$abund, colour = "Large (30-49 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) +    
    geom_smooth(data = comm_ap, 
                aes(x=env_sizes$DRR, y = comm_ap$abund, colour = "Apex (>50 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) + 
    scale_colour_manual("Size", 
                        values = c("Large (30-49 cm)" = "red", "Apex (>50 cm)" = "black"))+
    scale_linetype_manual(name = "Reef Type", values = c("Artificial" = 1, "Natural" = 2), guide = FALSE)+
    labs (x = "Digital Reef Rugosity (DRR)", y = "Abundance") +
    scale_x_continuous (limits=c(0, 3.5)) + 
    scale_y_continuous  (limits=c(0, 100),breaks = seq(0,100, by = 20))+ 
    theme(axis.text=element_text(size=text_size, colour="black"), 
          axis.title.x=element_text(size=text_size, colour="black", vjust=-.5), 
          axis.title.y=element_text(size=text_size, colour="black", vjust=1.3),
          legend.position = c(0.15,.85),
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12)) 
}

# richness
plot_comm_mets_fish_sizes_rich <- function(comm_sm, comm_md, comm_lg, comm_ap, env_sizes, text_size){
  ggplot()+
    theme_classic()+
    
    # Small Fish
    geom_smooth(data = comm_sm, 
                aes(x=env_sizes$DRR, y = comm_sm$S, colour =  "Small (1-10 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) +
    geom_smooth(data = comm_md, 
                aes(x=env_sizes$DRR, y = comm_md$S, colour =  "Medium (11-29 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) +
    geom_smooth(data = comm_lg, 
                aes(x=env_sizes$DRR, y = comm_lg$S, colour = "Large (30-49 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) +    
    geom_smooth(data = comm_ap, 
                aes(x=env_sizes$DRR, y = comm_ap$S, colour = "Apex (>50 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) + 
    scale_colour_manual("Size", 
                        values = c("Small (1-10 cm)" = "blue", "Medium (11-29 cm)" = "green", "Large (30-49 cm)" = "red", "Apex (>50 cm)" = "black"))+
    scale_linetype_manual(name = "Reef Type", values = c("Artificial" = 1, "Natural" = 2), guide = FALSE)+
    labs (x = "Digital Reef Rugosity (DRR)", y = "Species Richness (S)") +
    scale_x_continuous (limits=c(0, 3.5)) + 
    scale_y_continuous  (limits=c(0, 25),breaks = seq(0,25, by = 5))+ 
    theme(axis.text=element_text(size=text_size, colour="black"), 
          axis.title.x=element_text(size=text_size, colour="black", vjust=-.5), 
          axis.title.y=element_text(size=text_size, colour="black", vjust=1.3),
          legend.position = c(0.15,.85),
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12)) 
}


# biomass
plot_comm_mets_fish_sizes_biom <- function(comm_sm, comm_md, comm_lg, comm_ap, env_sizes, text_size){
  ggplot()+
    theme_classic()+
    
    # Small Fish
    geom_smooth(data = comm_sm, 
                aes(x=env_sizes$DRR, y = comm_sm$biom, colour =  "Small (1-10 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) +
    geom_smooth(data = comm_md, 
                aes(x=env_sizes$DRR, y = comm_md$biom, colour =  "Medium (11-29 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) +
    geom_smooth(data = comm_lg, 
                aes(x=env_sizes$DRR, y = comm_lg$biom, colour = "Large (30-49 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) +    
    geom_smooth(data = comm_ap, 
                aes(x=env_sizes$DRR, y = comm_ap$biom, colour = "Apex (>50 cm)", linetype = env_sizes$Reef_type), 
                method = loess, se=T, lwd = 1.5) + 
    scale_colour_manual("Size", 
                        values = c("Small (1-10 cm)" = "blue", "Medium (11-29 cm)" = "green", "Large (30-49 cm)" = "red", "Apex (>50 cm)" = "black"))+
    scale_linetype_manual(name = "Reef Type", values = c("Artificial" = 1, "Natural" = 2), guide = FALSE)+
    labs (x = "Digital Reef Rugosity (DRR)", y = "Biomass") +
    scale_x_continuous (limits=c(0, 3.5)) + 
    scale_y_continuous  (limits=c(0, 500),breaks = seq(0,500, by = 100))+ 
    theme(axis.text=element_text(size=text_size, colour="black"), 
          axis.title.x=element_text(size=text_size, colour="black", vjust=-.5), 
          axis.title.y=element_text(size=text_size, colour="black", vjust=1.3),
          legend.position = c(0.15,.85),
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12)) 
}



## Bar plots -----------------------------------------------------------------------------

plot_bar_reef_type <- function (data_summary){
  ggplot (data_summary, aes(x=Reef_type, y=sums, fill=Reef_type)) + 
    theme_bw()+
    geom_bar(position=position_dodge(), stat="identity", colour="black") + 
    geom_errorbar(aes(ymin=sums-se, ymax=sums+se), width=.2, position=position_dodge(0.9)) + 
    scale_y_continuous(name="Mean large snapper-grouper abundance (per 120 m^2)", 
                     limits=c(0, 15), 
                     breaks=seq(0, 15,by=2)) + 
    theme(axis.text=element_text(size=16, colour="black"), 
        axis.title.x=element_text(size=16, colour="black", vjust=-.5), 
        axis.title.y=element_text(size=16, colour="black", vjust=1.3), 
        legend.position="none") + 
    scale_x_discrete(name="Reef Type")
}

plot_bar_depth <- function (data_summary){
  ggplot (data_summary, aes(x=Depth_general, y=sums, fill=Depth_general)) + 
    theme_bw()+
    geom_bar(position=position_dodge(), stat="identity", colour="black") + 
    geom_errorbar(aes(ymin=sums-se, ymax=sums+se), width=.2, position=position_dodge(0.9)) + 
    scale_y_continuous(name="Mean snapper-grouper fish density (per 120 m^2)", 
                       limits=c(0, 3000), 
                       breaks=seq(0,3000,by=500)) + 
    theme(axis.text=element_text(size=16, colour="black"), 
          axis.title.x=element_text(size=16, colour="black", vjust=-.5), 
          axis.title.y=element_text(size=16, colour="black", vjust=1.3), 
          legend.position="none") + 
    scale_x_discrete(name="Depth")
}

plot_bar_reef_type_depth <- function (data){
  ggplot (data, aes(x=Depth_general, y=sums, fill=Reef_type)) + 
    theme_bw() +
    geom_bar(position=position_dodge(), stat="identity", colour="black") + 
    geom_errorbar(aes(ymin=sums-se, ymax=sums+se), width=.2, position=position_dodge(0.9)) + 
    scale_y_continuous(name="Mean snapper-grouper abundance (per 120 m^2)", 
                       limits=c(0, 6000), 
                       breaks=seq(0,6000,by=1000)) + 
    theme(axis.text=element_text(size=16, colour="black"), 
          axis.title.x=element_text(size=16, colour="black", vjust=-.5), 
          axis.title.y=element_text(size=16, colour="black", vjust=1.3), 
          legend.position="bottom",
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12))+ 
    scale_x_discrete(name="Depth") + 
    scale_fill_manual(name="Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))     
}

## Bar plots by size class -------------------------------

plot_bar_fish_sizes_reef_type <- function (summary_combo){
  ggplot (summary_combo, aes(x=size, y=sums, fill=Reef_type)) + 
  theme_classic() +
  geom_bar(position=position_dodge(), stat="identity", colour="black") + 
  geom_errorbar(aes(ymin=sums-se, ymax=sums+se), width=.2, position=position_dodge(0.9)) + 
  scale_y_continuous(name="Snapper-grouper abundance (per 120 m^2)", 
                     limits=c(0, 3000), 
                     breaks=seq(0,3000,by=500)) + 
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title.x=element_text(size=16, colour="black", vjust=-.5), 
        axis.title.y=element_text(size=16, colour="black", vjust=1.3), 
        legend.position="bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size = 12))+ 
  scale_x_discrete(name="Fish Size Class") + 
  scale_fill_manual(name="Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))     
}

plot_bar_fish_sizes_reef_type_inset <- function (summary_combo){
  ggplot (summary_combo_short, aes(x=size, y=sums, fill=Reef_type)) + 
    theme_classic() +
    geom_bar(position=position_dodge(), stat="identity", colour="black") + 
    geom_errorbar(aes(ymin=sums-se, ymax=sums+se), width=.2, position=position_dodge(0.9)) + 
    scale_y_continuous( limits=c(0, 60), breaks=seq(0,60,by=20)) + 
    theme(axis.text=element_text(size=12, colour="black"), 
          axis.title.x=element_text(size=12, colour="black"), 
          axis.title.y=element_text(size=12, colour="black"),
          legend.position = "none",
          plot.margin = unit(rep(0,4), "lines")) +
    labs(x = NULL, y = NULL)  
}

plot_line_fish_sizes_reef_type_inset <- function(summary_combo_short){  
  ggplot(summary_combo_short, aes( x = size, y = sums, shape = Reef_type, colour = Reef_type, group = Reef_type)) +
    theme_bw()+
    geom_point(aes(shape=Reef_type, colour = Reef_type),size=5, position=position_jitter(width=0.0,height=0.0)) +
    geom_line(aes(colour = Reef_type), size = 1.5) +
    geom_errorbar(aes(ymin=sums-se, ymax=sums+se), width=.2, position=position_dodge(0.0)) + 
    scale_y_continuous(limits=c(0, 25), 
                       breaks=seq(0,25,by=5)) + 
    theme(axis.text=element_text(size=12, colour="black"), 
          axis.title.x=element_text(size=12, colour="black"), 
          axis.title.y=element_text(size=12, colour="black"), 
          legend.position="none",
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12),
          plot.margin = unit(rep(0,4), "lines")) +
    labs(x = NULL, y = NULL)  
}

plot_line_fish_sizes_reef_type <- function(summary_combo){  
ggplot(summary_combo, aes( x = size, y = sums, shape = Reef_type, colour = Reef_type, group = Reef_type)) +
  theme_bw()+
  geom_point(aes(shape=Reef_type, colour = Reef_type),size=5, position=position_jitter(width=0.0,height=0.0)) +
  stat_smooth(method='loess', se=T, size = 1.5) +
  geom_errorbar(aes(ymin=sums-se, ymax=sums+se), width=.2, position=position_dodge(0.0)) + 
  scale_y_continuous(name="Mean snapper-grouper abundance (per 120 m^2)", 
                     limits=c(0, 1500), 
                     breaks=seq(0,1500,by=200)) + 
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title.x=element_text(size=16, colour="black", vjust=-.5), 
        axis.title.y=element_text(size=16, colour="black", vjust=1.3), 
        legend.position="bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size = 12))+ 
  scale_x_discrete(name="Fish Size Class") + 
  scale_fill_manual(name="Reef Type", values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))  +
  scale_shape_manual(name="Reef Type", values = c("Artificial" = 17, "Natural" = 16)) 
}

plot_bar_fish_sizes_depth <- function (summary_combo){
  ggplot (summary_combo, aes(x=size, y=sums, fill=Depth_general)) + 
    theme_bw() +
    geom_bar(position=position_dodge(), stat="identity", colour="black") + 
    geom_errorbar(aes(ymin=sums-se, ymax=sums+se), width=.2, position=position_dodge(0.9)) + 
    scale_y_continuous(name="Mean snapper-grouper abundance (per 120 m^2)", 
                       limits=c(0, 1500), 
                       breaks=seq(0,1500,by=500)) + 
    theme(axis.text=element_text(size=16, colour="black"), 
          axis.title.x=element_text(size=16, colour="black", vjust=-.5), 
          axis.title.y=element_text(size=16, colour="black", vjust=1.3), 
          legend.position="bottom",
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12))+ 
    scale_x_discrete(name="Fish Size Class") + 
    scale_fill_manual(name="Depth", values = c("Shallow" = "indianred2", "Intermediate" = "darkturquoise", "Deep" = "slateblue3"))     
}

plot_bar_fish_sizes_depth_inset <- function (summary_combo){
  ggplot (summary_combo_short, aes(x=size, y=sums, fill=Depth_general)) + 
    theme_bw() +
    geom_bar(position=position_dodge(), stat="identity", colour="black") + 
    geom_errorbar(aes(ymin=sums-se, ymax=sums+se), width=.2, position=position_dodge(0.9)) + 
    scale_y_continuous( limits=c(0, 15), breaks=seq(0,15,by=5)) + 
    theme(axis.text=element_text(size=12, colour="black"), 
          axis.title.x=element_text(size=12, colour="black"), 
          axis.title.y=element_text(size=12, colour="black"),
          legend.position = "none",
          plot.margin = unit(rep(0,4), "lines")) +
    labs(x = NULL, y = NULL)+
    scale_fill_manual(name="Depth", values = c("Shallow" = "indianred2", "Intermediate" = "darkturquoise", "Deep" = "slateblue3"))     
}

plot_line_fish_sizes_depth_inset <- function(summary_combo_short){  
  ggplot(summary_combo_short, aes( x = size, y = sums, shape = Depth_general, colour = Depth_general, group = Depth_general)) +
    theme_bw()+
    geom_point(aes(shape=Depth_general, colour =Depth_general),size=5, position=position_jitter(width=0.0,height=0.0)) +
    geom_line(aes(colour = Depth_general), size = 1.5) +
    geom_errorbar(aes(ymin=sums-se, ymax=sums+se), width=.2, position=position_dodge(0.0)) + 
    scale_y_continuous(limits=c(0, 15), 
                       breaks=seq(0,15,by=5)) + 
    theme(axis.text=element_text(size=12, colour="black"), 
          axis.title.x=element_text(size=12, colour="black"), 
          axis.title.y=element_text(size=12, colour="black"), 
          legend.position="none",
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12),
          plot.margin = unit(rep(0,4), "lines")) +
    labs(x = NULL, y = NULL)  +
    scale_colour_manual(name="Depth", values = c("Shallow" = "indianred2", "Intermediate" = "darkturquoise", "Deep" = "slateblue3"))    
}

plot_line_fish_sizes_depth <- function(summary_combo){  
  ggplot(summary_combo, aes( x = size, y = sums, shape = Depth_general, colour = Depth_general, group = Depth_general)) +
    theme_bw()+
    geom_point(aes(shape=Depth_general, colour = Depth_general),size=5, position=position_jitter(width=0.0,height=0.0)) +
    stat_smooth(method='loess', se=T, size = 1.5) +
    geom_errorbar(aes(ymin=sums-se, ymax=sums+se), width=.2, position=position_dodge(0.0)) + 
    scale_y_continuous(name="Mean snapper-grouper abundance (per 120 m^2)", 
                       limits=c(0, 1500), 
                       breaks=seq(0,1500,by=500)) + 
    theme(axis.text=element_text(size=16, colour="black"), 
          axis.title.x=element_text(size=16, colour="black", vjust=-.5), 
          axis.title.y=element_text(size=16, colour="black", vjust=1.3), 
          legend.position="bottom",
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12))+ 
    scale_x_discrete(name="Fish Size Class") + 
    scale_colour_manual(name="Depth", values = c("Shallow" = "indianred2", "Intermediate" = "darkturquoise", "Deep" = "slateblue3"))+    
    scale_shape_manual(name="Depth", values = c("Shallow" = 17, "Intermediate" = 16, "Deep" = 15)) 
}

## NMDS plots ---------------------------------------------------------------------------

plot_nmds_transect_number <- function(species.nms, species.wa){
  NMDS = data.frame(MDS1 = species.nms[,1], MDS2 = species.nms[,2]) # data frame to work with
  yl<-paste("NMS 2 ", '(', as.character(round(r2.2, digits=2)*100), '%', ')', sep='') # y axis label
  xl<-paste("NMS 1 ", '(', as.character(round(r2.1, digits=2)*100), '%', ')', sep='') # x axis label
  title<-paste(" ", 'stress = ', as.character(round(stress, digits=2)), '', sep='') # title
  library(colorspace)
  ggplot()+
    coord_equal()+ # axes sizes equal
    theme_bw()+ # basic theme
    geom_point(data = NMDS, 
               aes(x = MDS1, y = MDS2, color = as.factor(species.nms$Transect_Number), shape=as.factor(species.nms$Transect_Number)), 
               size=5) + # adds sample points
    geom_text(data=species.wa,
              aes(x=species.wa[,2],y=species.wa[,3],label=species_code),
              alpha=0.9, # adds weighted averages for species
              position=position_jitter(width=0.1,height=0.2))+
    stat_ellipse(data = NMDS, 
                 aes(x=MDS1,y=MDS2,colour=as.factor(env_data$Transect_Number)),
                 level = 0.50, linetype=2, size =1) + # adds ellipses for 95% confidence intervals
    scale_y_continuous(name=yl) + # names y axis
    scale_x_continuous(name=xl) + # names x axis
    theme(axis.text=element_text(size=16), 
          axis.title=element_text(size=16), 
          legend.text=element_text(color="black", size=16), 
          legend.title=element_text(color="black", size = 16), 
          legend.position="bottom") + 
    #scale_colour_manual(name = "Reef Type", 
    #                    values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    #scale_shape_manual (name="Reef Type", 
    #                    values = c("Artificial" = 17, "Natural" = 16))+
    ggtitle(title)
  
}

plot_nmds_reef_type <- function(species.nms, species.wa){
  NMDS = data.frame(MDS1 = species.nms[,1], MDS2 = species.nms[,2]) # data frame to work with
  yl<-paste("NMS 2 ", '(', as.character(round(r2.2, digits=2)*100), '%', ')', sep='') # y axis label
  xl<-paste("NMS 1 ", '(', as.character(round(r2.1, digits=2)*100), '%', ')', sep='') # x axis label
  title<-paste(" ", 'stress = ', as.character(round(stress, digits=2)), '', sep='') # title
  library(colorspace)
  ggplot()+
    coord_equal()+ # axes sizes equal
    theme_bw()+ # basic theme
    geom_point(data = NMDS, 
               aes(x = MDS1, y = MDS2, color = species.nms$Reef_Type, shape=species.nms$Reef_Type), 
               size=5) + # adds sample points
    geom_text(data=species.wa,
              aes(x=species.wa[,2],y=species.wa[,3],label=species_code),
              alpha=0.9, # adds weighted averages for species
              position=position_jitter(width=0.1,height=0.2))+
    stat_ellipse(data = NMDS, 
                 aes(x=MDS1,y=MDS2,colour=env_data$Reef_Type),
                 level = 0.50, linetype=2, size =1) + # adds ellipses for 95% confidence intervals
    scale_y_continuous(name=yl) + # names y axis
    scale_x_continuous(name=xl) + # names x axis
    theme(axis.text=element_text(size=16), 
          axis.title=element_text(size=16), 
          legend.text=element_text(color="black", size=16), 
          legend.title=element_text(color="black", size = 16), 
          legend.position="bottom") + 
    scale_colour_manual(name = "Reef Type", 
                        values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual (name="Reef Type", 
                        values = c("Artificial" = 17, "Natural" = 16))+
    ggtitle(title)
  
}

plot_nmds_depth <- function(species.nms){
  NMDS = data.frame(MDS1 = species.nms[,1], MDS2 = species.nms[,2]) # data frame to work with
  yl<-paste("NMS 2 ", '(', as.character(round(r2.2, digits=2)*100), '%', ')', sep='') # y axis label
  xl<-paste("NMS 1 ", '(', as.character(round(r2.1, digits=2)*100), '%', ')', sep='') # x axis label
  title<-paste(" ", 'stress = ', as.character(round(stress, digits=2)), '', sep='') # title
  library(colorspace)
  ggplot()+
    coord_equal()+ # axes sizes equal
    theme_bw()+ # basic theme
    geom_point(data = NMDS, 
               aes(x = MDS1, y = MDS2, color = species.nms$Depth_general, shape=species.nms$Reef_type), 
               size=5) + # adds sample points
    #geom_text(data=species.wa,
    #          aes(x=species.wa[,2],y=species.wa[,3],label=species_code),
    #          alpha=0.9) + # adds weighted averages for species
    stat_ellipse(data = NMDS, 
                 aes(x=MDS1,y=MDS2,colour=species.nms$Depth_general),
                 level = 0.50, linetype=2, size =1) + # adds ellipses for 95% confidence intervals
    scale_y_continuous(name=yl) + # names y axis
    scale_x_continuous(name=xl) + # names x axis
    theme(axis.text=element_text(size=14), 
          axis.title=element_text(size=14), 
          legend.text=element_text(color="black", size=14), 
          legend.title=element_text(color="black", size = 14), 
          legend.position="bottom") + 
    scale_colour_manual(name = "Depth", 
                        values = c("Shallow" = "indianred2", "Intermediate" = "darkturquoise", "Deep" = "slateblue3"))+
    scale_shape_manual (name="Reef Type", 
                        values = c("Artificial" = 17, "Natural" = 16))+
    ggtitle(title)
}

plot_nmds_complexity_reef_type <- function(species.nms, species.wa){
  NMDS = data.frame(MDS1 = species.nms[,1], MDS2 = species.nms[,2]) # data frame to work with
  yl<-paste("NMS 2 ", '(', as.character(round(r2.2, digits=2)*100), '%', ')', sep='') # y axis label
  xl<-paste("NMS 1 ", '(', as.character(round(r2.1, digits=2)*100), '%', ')', sep='') # x axis label
  title<-paste("", '(stress = ', as.character(round(stress, digits=2)), ')', sep='') # title
  library(colorspace)
  ggplot()+
    coord_equal()+ # axes sizes equal
    theme_bw()+ # basic theme
    geom_point(data = NMDS, 
               aes(x = MDS1, y = MDS2, color = as.numeric(species.nms$DRR), shape=species.nms$Reef_type), 
               size=5) + # adds sample points
    #geom_text(data=species.wa,
    #          aes(x=species.wa[,2],y=species.wa[,3],label=species_code),
    #          alpha=0.9) + # adds weighted averages for species
    #stat_ellipse(data = NMDS, 
    #             aes(x=MDS1,y=MDS2,colour=env_data$Reef_type),
    #             level = 0.50, linetype=2, size =1) + # adds ellipses for 95% confidence intervals
    scale_y_continuous(name=yl) + # names y axis
    scale_x_continuous(name=xl) + # names x axis
    scale_colour_gradientn(name="Structural Complexity (DRR)", guide="colorbar", colours=(diverge_hcl(4))) +
    theme(axis.text=element_text(size=14), 
          axis.title=element_text(size=14), 
          legend.text=element_text(color="black", size=14), 
          legend.title=element_text(color="black", size = 14), 
          legend.position="bottom") + 
    #scale_colour_manual(name = "Reef Type", 
    #                    values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    scale_shape_manual (name="Reef Type", 
                        values = c("Artificial" = 17, "Natural" = 16))+
    ggtitle(title)
}


## PCA Plots ------------------------------------------------------------
plot_pca_reef_type <- function(){
  PCA = data.frame(PCA1 = env.pca.scores$Comp.1, PCA2 = env.pca.scores$Comp.2)
  library(ggplot2)
  library(gridExtra)
  vf<-envfit(env.pca.scores[,c(1:2)],env.data.pca)
  vect.vf <- as.data.frame(scores(vf, display = "vectors"))
  vect.vf <- cbind(vect.vf, Env = rownames(vect.vf))
  ggplot(PCA, aes(PCA1, PCA2))+
    coord_equal()+ # axes sizes equal
    theme_bw()+ # basic theme
    geom_point(aes(shape=env_data$Reef_Type, colour=env_data$Reef_Type), size=5)+
    geom_hline(aes(yintercept=0), linetype="dashed")+
    geom_vline(aes(xintercept=0), linetype="dashed")+
    #scale_y_continuous  (limits=c(-4.5, 4.5), breaks = seq(-4.5, 4.5, by = 1)) +
    #scale_x_continuous  (limits=c(-4.5, 4.5), breaks = seq(-4.5, 4.5, by = 1)) + 
    scale_shape_discrete(name="Reef Type", 
                         labels=c("Artificial", "Natural"))+ 
    scale_colour_manual(name = "Reef Type", 
                        values = c("Artificial" = "indianred2", "Natural" = "darkturquoise"))+
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12), 
          legend.text=element_text(color="black", size=12), 
          legend.position="bottom")+
    geom_segment(data = vect.vf,
                 aes(x = 0, xend = Comp.1, y = 0, yend = Comp.2), 
                 arrow = arrow(length = unit(0.25, "cm")), 
                 colour = "red") +
    geom_text(data = vect.vf, 
              aes(x = Comp.1 + 0.3, y = Comp.2 + 0.06, label = Env),
              size = 5)
}


plot_pca_transect_number <- function(){
  PCA = data.frame(PCA1 = env.pca.scores$Comp.1, PCA2 = env.pca.scores$Comp.2)
  library(ggplot2)
  library(gridExtra)
  vf<-envfit(env.pca.scores[,c(1:2)],env.data.pca)
  vect.vf <- as.data.frame(scores(vf, display = "vectors"))
  vect.vf <- cbind(vect.vf, Env = rownames(vect.vf))
  ggplot(PCA, aes(PCA1, PCA2))+
    coord_equal()+ # axes sizes equal
    theme_bw()+ # basic theme
    #env_data$Transect_Number <- as.character(env_data$Transect_Number)
    geom_point(aes(shape=env_data$Transect_Number, colour=env_data$Transect_Number), size=5)+
    geom_hline(aes(yintercept=0), linetype="dashed")+
    geom_vline(aes(xintercept=0), linetype="dashed")+
    #scale_y_continuous  (limits=c(-4.5, 4.5), breaks = seq(-4.5, 4.5, by = 1)) +
    #scale_x_continuous  (limits=c(-4.5, 4.5), breaks = seq(-4.5, 4.5, by = 1)) + 
    scale_shape_discrete(name="Distance from Structure", 
                         labels=c("0" = "Structure", "1" = "0-30 m", "2" = "30-60 m", "3"= "60-90 m"))+ 
    scale_colour_discrete(name = "Distance from Structure",
                          labels=c("0" = "Structure", "1" = "0-30 m", "2" = "30-60 m", "3"= "60-90 m"))+
    #scale_colour_manual(name = "Distance from Structure", 
         #               values = c("0" = "Structure"= "indianred2", "1" ="0-30 m" = "darkturquoise", "2" ="30-60 m" = "blueviolet", "3" ="60-90 m" = "olivedrab4"))+
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12), 
          legend.text=element_text(color="black", size=12), 
          legend.position="bottom")+
    geom_segment(data = vect.vf,
                 aes(x = 0, xend = Comp.1, y = 0, yend = Comp.2), 
                 arrow = arrow(length = unit(0.25, "cm")), 
                 colour = "red") +
    geom_text(data = vect.vf, 
              aes(x = Comp.1 + 0.3, y = Comp.2 + 0.06, label = Env),
              size = 5)
}

plot_pca_site <- function(){
  PCA = data.frame(PCA1 = env.pca.scores$Comp.1, PCA2 = env.pca.scores$Comp.2)
  library(ggplot2)
  library(gridExtra)
  vf<-envfit(env.pca.scores[,c(1:2)],env.data.pca)
  vect.vf <- as.data.frame(scores(vf, display = "vectors"))
  vect.vf <- cbind(vect.vf, Env = rownames(vect.vf))
  ggplot(PCA, aes(PCA1, PCA2))+
    coord_equal()+ # axes sizes equal
    theme_bw()+ # basic theme
       geom_point(aes(shape=env_data$Site, colour=env_data$Site), size=5)+
    geom_hline(aes(yintercept=0), linetype="dashed")+
    geom_vline(aes(xintercept=0), linetype="dashed")+
    #scale_y_continuous  (limits=c(-4.5, 4.5), breaks = seq(-4.5, 4.5, by = 1)) +
    #scale_x_continuous  (limits=c(-4.5, 4.5), breaks = seq(-4.5, 4.5, by = 1)) + 
    scale_shape_discrete(name="Site", 
                         labels=c("330", "345", "364", "370", "372", "378"))+ 
    scale_colour_manual(name = "Site", 
                        values = c("330" = "indianred2",  "345" = "darkturquoise", "364" = "blueviolet", "370" = "olivedrab4", "372" = "tomato1", "378" = "lightpink2"))+
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12), 
          legend.text=element_text(color="black", size=12), 
          legend.position="bottom")+
    geom_segment(data = vect.vf,
                 aes(x = 0, xend = Comp.1, y = 0, yend = Comp.2), 
                 arrow = arrow(length = unit(0.25, "cm")), 
                 colour = "red") +
    geom_text(data = vect.vf, 
              aes(x = Comp.1 + 0.3, y = Comp.2 + 0.06, label = Env),
              size = 5)
}
plot_pca_sampling_period <- function(){
  PCA = data.frame(PCA1 = env.pca.scores$Comp.1, PCA2 = env.pca.scores$Comp.2)
  library(ggplot2)
  library(gridExtra)
  vf<-envfit(env.pca.scores[,c(1:2)],env.data.pca)
  vect.vf <- as.data.frame(scores(vf, display = "vectors"))
  vect.vf <- cbind(vect.vf, Env = rownames(vect.vf))
  ggplot(PCA, aes(PCA1, PCA2))+
    coord_equal()+ # axes sizes equal
    theme_bw()+ # basic theme
    geom_point(aes(shape=env_data$Sampling_Period, colour=env_data$Sampling_Period), size=5)+
    geom_hline(aes(yintercept=0), linetype="dashed")+
    geom_vline(aes(xintercept=0), linetype="dashed")+
    #scale_y_continuous  (limits=c(-4.5, 4.5), breaks = seq(-4.5, 4.5, by = 1)) +
    #scale_x_continuous  (limits=c(-4.5, 4.5), breaks = seq(-4.5, 4.5, by = 1)) + 
    scale_shape_discrete(name="Season", 
                         labels=c("Fall", "Winter", "Spring"))+ 
    scale_colour_discrete(name="Season", 
                         labels=c("Fall", "Winter", "Spring"))+ 
    #scale_colour_manual(name = "Sampling Period", 
               #        values = c("Fall" = "indianred2",  "Winter" = "darkturquoise", "Spring" = "blueviolet"))+
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12), 
          legend.text=element_text(color="black", size=12), 
          legend.position="bottom")+
    geom_segment(data = vect.vf,
                 aes(x = 0, xend = Comp.1, y = 0, yend = Comp.2), 
                 arrow = arrow(length = unit(0.25, "cm")), 
                 colour = "red") +
    geom_text(data = vect.vf, 
              aes(x = Comp.1 + 0.3, y = Comp.2 + 0.06, label = Env),
              size = 5)
}




plot_pca_reef_type_depth <- function(){
  PCA = data.frame(PCA1 = env.pca.scores$Comp.1, PCA2 = env.pca.scores$Comp.2)
  library(ggplot2)
  library(gridExtra)
  vf<-envfit(env.pca.scores[,c(1:2)],env.data.pca)
  vect.vf <- as.data.frame(scores(vf, display = "vectors"))
  vect.vf <- cbind(vect.vf, Env = rownames(vect.vf))
  ggplot(PCA, aes(PCA1, PCA2))+
    coord_equal()+ # axes sizes equal
    theme_bw()+ # basic theme
    geom_point(aes(shape=env_data$Reef_type, colour=env_data$Depth_general), size=5)+
    geom_hline(aes(yintercept=0), linetype="dashed")+
    geom_vline(aes(xintercept=0), linetype="dashed")+
    #scale_y_continuous  (limits=c(-4.2, 4.2), breaks = seq(-4.0, 4.0, by = 1)) +
    #scale_x_continuous  (limits=c(-4.2, 4.2), breaks = seq(-4.0, 4.0, by = 1)) + 
    scale_shape_discrete(name="Reef Type", 
                         labels=c("Artificial", "Natural"))+ 
    scale_colour_manual(name = "Depth", 
                        values = c("Shallow" = "indianred2", "Intermediate" = "darkturquoise", "Deep" = "slateblue3"))+
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12), 
          legend.text=element_text(color="black", size=12), 
          legend.position="bottom")+
    geom_segment(data = vect.vf,
                 aes(x = 0, xend = Comp.1, y = 0, yend = Comp.2), 
                 arrow = arrow(length = unit(0.25, "cm")), 
                 colour = "red") +
    geom_text(data = vect.vf, 
              aes(x = Comp.1 + 0.3, y = Comp.2 + 0.06, label = Env),
              size = 5)
}

plot_pca_depth <- function(){
  PCA = data.frame(PCA1 = env.pca.scores$Comp.1, PCA2 = env.pca.scores$Comp.2)
  library(ggplot2)
  library(gridExtra)
  vf<-envfit(env.pca.scores[,c(1:2)],env.data.pca)
  vect.vf <- as.data.frame(scores(vf, display = "vectors"))
  vect.vf <- cbind(vect.vf, Env = rownames(vect.vf))
  ggplot(PCA, aes(PCA1, PCA2))+
    coord_equal()+ # axes sizes equal
    theme_bw()+ # basic theme
    geom_point(aes(shape=env_data$Depth_general, colour=env_data$Depth_general), size=5)+
    geom_hline(aes(yintercept=0), linetype="dashed")+
    geom_vline(aes(xintercept=0), linetype="dashed")+
    #scale_y_continuous  (limits=c(-4.2, 4.2), breaks = seq(-4.0, 4.0, by = 1)) +
    #scale_x_continuous  (limits=c(-4.2, 4.2), breaks = seq(-4.0, 4.0, by = 1)) + 
    scale_shape_discrete(name="Depth", 
                         labels=c("Shallow", "Intermediate", "Depth"))+ 
    scale_colour_manual(name = "Depth", 
                        values = c("Shallow" = "indianred2", "Intermediate" = "darkturquoise", "Deep" = "slateblue3"))+
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12), 
          legend.text=element_text(color="black", size=12), 
          legend.position="bottom")+
    geom_segment(data = vect.vf,
                 aes(x = 0, xend = Comp.1, y = 0, yend = Comp.2), 
                 arrow = arrow(length = unit(0.25, "cm")), 
                 colour = "red") +
    geom_text(data = vect.vf, 
              aes(x = Comp.1 + 0.3, y = Comp.2 + 0.06, label = Env),
              size = 5)
}

plot_pca_depth_sed <- function(){
PCA = data.frame(PCA1 = env.pca.scores$Comp.1, PCA2 = env.pca.scores$Comp.2)
library(ggplot2)
library(gridExtra)
vf<-envfit(env.pca.scores[,c(1:2)],env.data.pca)
vect.vf <- as.data.frame(scores(vf, display = "vectors"))
vect.vf <- cbind(vect.vf, Env = rownames(vect.vf))
ggplot(PCA, aes(PCA1, PCA2))+
  coord_equal()+ # axes sizes equal
  theme_bw()+ # basic theme
  geom_point(aes(shape=env_data$Depth_general, color=as.numeric(env_data$sed_stdv)), size=5)+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  geom_vline(aes(xintercept=0), linetype="dashed")+
  #scale_y_continuous  (limits=c(-4.2, 4.2), breaks = seq(-4.0, 4.0, by = 1)) +
  #scale_x_continuous  (limits=c(-4.2, 4.2), breaks = seq(-4.0, 4.0, by = 1)) + 
  scale_colour_gradientn(name="Sediment stdv (cm)", colours=(rainbow_hcl(5)))+
  scale_shape_discrete(name="Depth", 
                       labels=c("Shallow", "Intermediate", "Deep"))+ 
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        legend.text=element_text(color="black", size=12), 
        legend.position="bottom")+
  geom_segment(data = vect.vf,aes(x = 0, xend = Comp.1, y = 0, yend = Comp.2), 
               arrow = arrow(length = unit(0.25, "cm")), 
               colour = "red") +
  geom_text(data = vect.vf, aes(x = Comp.1 + 0.1, y = Comp.2 - 0.05, label = Env),size = 5)
}

plot_pca_depth_comp <- function(){
  PCA = data.frame(PCA1 = env.pca.scores$Comp.1, PCA2 = env.pca.scores$Comp.2)
  library(ggplot2)
  library(gridExtra)
  vf<-envfit(env.pca.scores[,c(1:2)],env.data.pca)
  vect.vf <- as.data.frame(scores(vf, display = "vectors"))
  vect.vf <- cbind(vect.vf, Env = rownames(vect.vf))
  ggplot(PCA, aes(PCA1, PCA2))+
    coord_equal()+ # axes sizes equal
    theme_bw()+ # basic theme
    geom_point(aes(shape=env_data$Depth, color=as.numeric(env_data$DRR)), size=5)+
    geom_hline(aes(yintercept=0), linetype="dashed")+
    geom_vline(aes(xintercept=0), linetype="dashed")+
    #scale_y_continuous  (limits=c(-4.2, 4.2), breaks = seq(-4.0, 4.0, by = 1)) +
    #scale_x_continuous  (limits=c(-4.2, 4.2), breaks = seq(-4.0, 4.0, by = 1)) + 
    scale_colour_gradientn(name="Structural Complexity (DRR)", colours=(rainbow_hcl(5)))+
    scale_shape_discrete(name="Depth", 
                         labels=c("Shallow", "Intermediate", "Deep"))+ 
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12), 
          legend.text=element_text(color="black", size=12), 
          legend.position="bottom")+
    geom_segment(data = vect.vf,aes(x = 0, xend = Comp.1, y = 0, yend = Comp.2), 
                 arrow = arrow(length = unit(0.25, "cm")), 
                 colour = "red") +
    geom_text(data = vect.vf, aes(x = Comp.1 + 0.1, y = Comp.2 - 0.05, label = Env),size = 5)
}

## DO NOT MODIFY --------------------------------------------------------
plot_nmds_MASTER_EXAMPLE <- function(species.nms, species.wa){
  NMDS = data.frame(MDS1 = species.nms[,1], MDS2 = species.nms[,2]) # data frame to work with
  yl<-paste("NMS 2 ", '(', as.character(round(r2.2, digits=2)*100), '%', ')', sep='') # y axis label
  xl<-paste("NMS 1 ", '(', as.character(round(r2.1, digits=2)*100), '%', ')', sep='') # x axis label
  library(colorspace)
ggplot()+
  #coord_equal()+
  theme_bw()+
  geom_polygon(data=hull.data.reefs,aes(x=NMS1,y=NMS2,fill=Reef_type,group=Reef_type),alpha=0.30)+
  geom_point(data = species.nms, aes(x = species.nms[,1], y = species.nms[,2], shape=species.nms$Reef_type), size=5) +
  geom_text(data=species.wa,aes(x=species.wa[,2],y=species.wa[,3],label=species_code),alpha=0.5) + 
  #geom_segment(data = vect.vf,aes(x = 0, xend = NMS1, y = 0, yend = NMS2), 
  #            arrow = arrow(length = unit(0.25, "cm")), 
  #            colour = "red") +
  #geom_text(data = vect.vf, aes(x = NMS1 + 0.3, y = NMS2 + 0.06, label = Env),size = 5)+
  #geom_path(data=df_ell, aes(x=NMS1, y=NMS2,colour=df_ell$group), size=1, linetype=2)+
  #annotate("text",x=NMDS.mean$MDS1,y=NMDS.mean$MDS2,label=NMDS.mean$group)+
  stat_ellipse(data = NMDS, aes(x=MDS1,y=MDS2,colour=env_data$Reef_type),level = 0.50, linetype=2, size =1) +
  scale_color_discrete (name = "95% CI ellipses")+
  #scale_colour_gradientn(name="Reef Type", guide="colorbar", colours=(diverge_hcl(4))) +
  scale_y_continuous(name=yl) + 
  scale_x_continuous(name=xl) + 
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        legend.text=element_text(color="black", size=12), 
        legend.title=element_text(color="black", size = 12), 
        legend.position="bottom") + 
  scale_shape_discrete(name="Reef Type", 
                       labels = c("Artificial", "Natural"))
}

plot_nms_color_shape <- function(species.nms, r2.2, r2.1, color_var, shape_var, color_name, shape_name, shape_name_lab1, shape_name_lab2){
  # Set up plot
  NMDS = data.frame(MDS1 = species.nms[,1], MDS2 = species.nms[,2], color=as.numeric(color_var))
  yl<-paste("NMS 2 ", '(', as.character(round(r2.2, digits=2)*100), '%', ')', sep='')
  xl<-paste("NMS 1 ", '(', as.character(round(r2.1, digits=2)*100), '%', ')', sep='')
  library(colorspace)
  # Plot it
  ggplot(NMDS, aes(x = MDS1, y = MDS2, color=as.numeric(color_var)))+
    geom_point(aes(shape=shape_var), size=5) +
    geom_text(data=species.wa,aes(x=species.wa[,2],y=species.wa[,3],label=species_code),alpha=0.5) + 
    scale_colour_gradientn(name=color_name, guide="colorbar", colours=(diverge_hcl(4))) +
    scale_y_continuous(name=yl) + 
    scale_x_continuous(name=xl) + 
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12), 
          legend.text=element_text(color="black", size=12), 
          legend.title=element_text(color="black", size = 12), 
          legend.position="bottom") + 
    scale_shape_discrete(name=shape_name, 
                         labels = c(shape_name_lab1, shape_name_lab2)) 
}


plot_nms_color <- function(species.nms, r2.2, r2.1, color_var, color_name){
  # Set up plot
  NMDS = data.frame(MDS1 = species.nms[,1], MDS2 = species.nms[,2], color=as.numeric(color_var))
  yl<-paste("NMS 2 ", '(', as.character(round(r2.2, digits=2)*100), '%', ')', sep='')
  xl<-paste("NMS 1 ", '(', as.character(round(r2.1, digits=2)*100), '%', ')', sep='')
  library(colorspace)
  # Plot it
  ggplot(NMDS, aes(x = MDS1, y = MDS2, color=as.numeric(color_var)))+
    geom_point(size=5)+
    scale_colour_gradientn(name=color_name, guide="colorbar", colours=(rainbow(4))) +
    scale_y_continuous(name=yl) + 
    scale_x_continuous(name=xl) + 
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12), 
          legend.text=element_text(color="black", size=12), 
          legend.title=element_text(color="black", size = 12), 
          legend.position="bottom") 
}





