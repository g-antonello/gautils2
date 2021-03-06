#' Fancy MDS Plots
#'
#' The function wraps the phyloseq functions "distance", "ordinate", and "plot_ordination",
#' also adding some fancier represantations, called "spiders", "hulls" and "ellipses".
#'
#' @param physeq A phyloseq object, **CAREFUL**: the function doesn't transform internally, so if you choose to calculate the distance matrix internally with "ordinate", make sure you transform the counts prior to function call (usually relative abundance is accepted)
#' @param dist Distance object or character vector saying the distance metric to apply
#' @param method One of the methods allowed by "ordinate" in the phyloseq object
#' @param axes The axes to be plotted, default are the first 2, which should discriminate samples better
#' @param color The variable to color the samples by. Only metadata variables are supported, to color taxa, use betaPlotTaxa
#' @param shape Should you want to further differentiate your points. the default is that shapes are chosen based on the color variable
#' @param visual_grouping This is the cool parameter of this function. the allowed ones are "spiders", "hulls" and "ellipses"
#' @param palette Similarly to ggpubr's "palette" parameters, the function allows for a palette name, (default is "standard", the ggplot2 default palette). All palettes in the ggsci package are allowed too
#'
#' @importFrom grDevices chull
#' @import tidyverse
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' data("enterotype")
#'
#' betaPlot(physeq = enterotype,
#' dist = "bray",
#' method = "PCoA",
#' axes = 1:2,
#' color = "SeqTech",
#' visual_grouping = "spiders",
#' palette = "jco")

phy_betaPlot <- function(physeq,
                     dist = "bray",
                     method = "PCoA",
                     axes = 1:2,
                     color,
                     shape = color,
                     visual_grouping = NULL,
                     palette = "standard"){
# generic preparatory phase, getting data ordinated

ord <- ordinate(physeq = physeq, method = method, distance = dist)

# palette settings
n_colors <- length(levels(as.factor(physeq@sam_data[[color]])))
if(n_colors > 6){print("there are more than 6 factors in your variable, not sure your plot will be understandable...")}

if(palette == "standard"){

  palette_for_all <- scales::hue_pal()(n_colors)

}else{
  palette_for_all <- eval(parse(text = paste0("ggsci::pal_", palette,"()","(",n_colors,")")))
}

# end of palette settings

# create a basic plot
basic_plot <- plot_ordination(physeq = physeq,
                              ordination = ord,
                              axes = axes,
                              color = color,
                              shape = color)+
  scale_color_manual(values = palette_for_all)


# retrieve data used to plot
basic_plot_data <-basic_plot$data

# see which axes were used, this will be used later on in the plotting
ax1 <- colnames(basic_plot_data)[1]
ax2 <- colnames(basic_plot_data)[2]

######## from now on I will plot case by case

if(visual_grouping == "spiders"){
  # calculate centroids with the mean
  centroids <- aggregate(as.matrix(basic_plot_data[,1:2])~eval(parse(text=color)), data = basic_plot_data, FUN = mean)
  colnames(centroids)[1] <- color

  final_plot <- basic_plot +
    geom_segment(data = merge(basic_plot_data, centroids, by = color, all.x = TRUE),
                 aes_string(x = paste0(ax1,".y"), y = paste0(ax2,".y"), xend = paste0(ax1,".x"), yend = paste0(ax2,".x")),
                 alpha = 0.5, show.legend = FALSE)+
    geom_point(data = centroids,
               size =2,
               shape = 21,
               stroke = 1,
               color = "white",
               aes_string(fill = color),inherit.aes = T)+
    scale_fill_manual(values = palette_for_all)

}

if(visual_grouping == "hulls"){
  final_plot <- basic_plot +
    ggforce::geom_mark_hull(aes(fill = eval(parse(text = color)), alpha = 0.3, show.legend = FALSE))

}

if(visual_grouping  == "ellipses"){
  final_plot <- basic_plot +
    stat_ellipse(aes_string(group = color, color = color), show.legend = FALSE)+
    scale_color_manual(values = palette_for_all)

}

if(is.null(visual_grouping)){
  return(basic_plot)
} else{

  x_pcnt <- final_plot$labels$x %>% strsplit(split = "? ") %>% sapply(function(l) l[[length(l)]])

  y_pcnt <- final_plot$labels$y %>% strsplit(split = "? ") %>% sapply(function(l) l[[length(l)]])

  x_final <- paste(method, axes[1], " ", x_pcnt)
  y_final <- paste(method, axes[2], " ", y_pcnt)

  final_plot$labels$x <- x_final
  final_plot$labels$y <- y_final

  return(final_plot)
}
}



