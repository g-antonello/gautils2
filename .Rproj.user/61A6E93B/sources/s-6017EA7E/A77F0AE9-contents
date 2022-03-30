#' Transform microbiota counts
#'
#' Same as microbiome::transform(), but also adding the hyperbolic arcsine
#' @param physeq a phyloseq object
#' @param transform all options available in "microbiome::transform()", extending it with the "asinh" option
#'
#' @return
#' @export
#'
#' @examples
#' data(GlobalPatterns)
#' GP_Genus <- tax_glom(GlobalPatterns, "Genus")
#' tax <- "295395"
#' transf <- "clr"
#' transformed_GP <- transform_microbiota_ga(GP_Genus,transform = transf)
#'
#'
#' plot(density(microbiome::abundances(GP_Genus)[tax,]), main = paste("taxon n°", tax), col = "blue")
#' lines(density(microbiome::abundances(transformed_GP)[tax,]), col = "red")
#' legend("topright", legend = c("Untransformed", transf), col = c("blue", "red"), pch = 18)
#'

phy_transform <- function(physeq, transform){

  # transform otu counts
  if(transform %in% c("asinh", "gm_mean")){
    transformed_otu <- apply(microbiome::abundances(physeq), 1, transform)

  #rownames(t(transformed_otu)) <- taxa_names(physeq)
    physeq_transf <- physeq
    if(taxa_are_rows(physeq_transf)){
      physeq_transf@otu_table <- otu_table(t(transformed_otu), taxa_are_rows = TRUE)
    }else{
      physeq_transf@otu_table <- otu_table(transformed_otu, taxa_are_rows = FALSE)
    }

    # substitute otu table with newly transformed one
  }else{
    physeq_transf <- microbiome::transform(physeq, transform = transform)
  }

  return(physeq_transf)
}
