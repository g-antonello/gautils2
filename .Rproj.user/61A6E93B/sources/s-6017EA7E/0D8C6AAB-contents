#' Quick histogram of taxa
#'
#' Useful if you need a quick histogram or density plot of 1 or more taxa, which you can also color by another trait
#'
#' @param physeq a phyloseq object
#' @param taxa a character, the ID(s) of the taxa to plot
#' @param color_fill a character, the variable to color or fill the histogram or the density plot by
#' @param transform a character to transform the taxa with, any from "phy_transform" are allowed
#' @param alpha a double, transparency of the plot elements: between 0 (completely transparent) and 1 (full density)
#' @param geom character, either "histogram" or "density"
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#'
#' data("GlobalPatterns")
#'
#' # fix a bug in the ggplot2 code, which does not accept numeric variables as string names
#' taxa_names(GlobalPatterns) <- paste0("t",taxa_names(GlobalPatterns))
#'
#' histogram_taxa(physeq = GlobalPatterns, taxa = "t549656", color_fill = "SampleType", geom="density")
#' histogram_taxa(physeq = GlobalPatterns, taxa = "t549656", color_fill = "SampleType", geom="histogram")
#'
phy_plotTaxa_hist <- function(physeq,
                            taxa,
                            color_or_fill,
                            transform = "clr",
                            alpha = 1,
                            geom = "histogram"){

  if(lenght(taxa) == 1){
    # get at least one extra name if the length of the taxa vector is 1,
    # because otherwise the microbiome::transform function breaks
    extra_tax <- sample(taxa_names(physeq)[!(taxa_names(physeq) %in% taxa)],1)
  tmp <- subset_taxa(physeq, taxa_names(physeq) %in% c(taxa, extra_tax))
  }else{
    tmp <- subset_taxa(physeq, taxa_names(physeq) %in% taxa)
  }
  ################################################################
  # transform the taxa
  physeq_transf <- phy_transform(tmp, transform = transform)
  # join otu table with metadata
  data_merged <- phy_OtuMetaTable(physeq_transf)
  if(length(taxa) != 1){
  if (geom == "histogram"){
    histplots <- lapply(taxa, function(tx) ggplot(data = data_merged) +
      geom_histogram(aes_string(x = tx, fill = color_or_fill), alpha = alpha) +
      labs(title = paste0("taxa: ",tx),
           x = "",
           subtitle = paste0("transformation: ", transform))
    )
  }
  if (geom == "density"){
    histplots <- lapply(taxa, function(tx) ggplot(data = data_merged) +
      geom_density(aes_string(x = tx, color = color_or_fill), alpha = alpha) +
      labs(title = paste0("taxa: ",tx),
           x = "",
           subtitle = paste0("transformation: ", transform))
    )
  }

  return(ggpubr::ggarrange(plotlist = histplots))

  } else{
    if (geom == "histogram"){
      histplot <- ggplot(data = data_merged) +
                            geom_histogram(aes_string(x = taxa, fill = color_or_fill), alpha = alpha) +
                            labs(title = paste0("taxa: ",taxa),
                                 x = "",
                                 subtitle = paste0("transformation: ", transform))
    }
    if (geom == "density"){
      histplot <- lapply(taxa, function(tx) ggplot(data = data_merged) +
                            geom_density(aes_string(x = taxa, color = color_or_fill), alpha = alpha) +
                            labs(title = paste0("taxa: ",taxa),
                                 x = "",
                                 subtitle = paste0("transformation: ", transform))
      )
    }

    return(histplot)
  }
}
