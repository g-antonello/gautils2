#' Boxplot of a taxon
#'
#' @param physeq
#' @param x
#' @param y
#' @param fill
#' @param title
#' @param facet_grid
#' @param border_color
#'
#' @return
#' @export
#'
#' @examples
phy_barplotTaxa <-  function (physeq,
                        x = "Sample",
                        y = "Abundance",
                        fill = NULL,
                        title = NULL,
                        facet_grid = NULL,
                        border_color = NA){

  ##### plotting phase
  mdf <- psmelt(physeq)
  if(x == "sample"){
  p = ggplot(mdf, aes_string(x = x, y = y, fill = fill))
  p = p + geom_bar(stat = "identity", position = "stack",  color = border_color)
  p = p + theme(axis.text.x = element_text(angle = -90, hjust = 0))
  if (!is.null(facet_grid)) {
    p <- p + facet_grid(facet_grid)
  }
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  }

  if(x != "Sample"){
    # make mean of all samples in each group dictated by x, which has to be the name of a column of the metadata


  }

  return(p)
}
