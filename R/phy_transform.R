#' Transform microbiota counts
#'
#' Same as microbiome::transform(), but also adding the hyperbolic arcsine
#' @param physeq a phyloseq object
#' @param transform all options available in "microbiome::transform()", extending it with the \code{arcsinh} ("asinh"), \code{geometric mean}("gm_mean") and \code{Rank Inverse Normal transf.}("irn")
#' @importFrom RNOmni RankNorm
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

  # if transform is not one of my additional transformations:
  if(!transform %in% c("asinh", "gm_mean", "irn")){
    physeq_transf <- microbiome::transform(physeq, transform = transform)
  }else{ # otherwise, if transform is in my transformations...

    if(transform == "irn"){
      tmp <- microbiome::transform(physeq, transform = "compositional")
      transformed_otu <- apply(microbiome::abundances(tmp), 1, RankNorm)
      rm(tmp)
    }

    if(transform %in% c("asinh", "gm_mean")){
      transformed_otu <- apply(microbiome::abundances(physeq), 1, transform)
    }

    physeq_transf <- physeq
    rm(physeq)
    # substitute otu table with newly transformed one
    if(taxa_are_rows(physeq_transf)){
      physeq_transf@otu_table <- otu_table(t(transformed_otu), taxa_are_rows = TRUE)
    }else{
      physeq_transf@otu_table <- otu_table(transformed_otu, taxa_are_rows = FALSE)
    }

  }

  return(physeq_transf)
  }
