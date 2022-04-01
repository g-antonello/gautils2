#' Create a phyloseq object from Metaphlan profiles
#'
#' Given the profiles' location, or a pre-loaded table, the function creates a taxonomic table and creates a phyloseq object. if metadata and phylogenetic tree are available, it will try to include those
#'
#' @param mpa Either a \code{character} vector containing the path to the text table to read, or the already-read object
#' @param metadata If in possession, add metadata to the phyloseq object. make sure the name and order of samples is correct for the mpa columns and metadata rows
#' @param phyloTree If in possession, ad phylogenetic tree of these taxa. make sure the tree is read with \code{ape::read.tree}
#' @param version the metaphlan version, by default, version 4 is taken (to date it is the latest - Apr 2022). Versions below 3 are not supported
#'
#' @importFrom phyloseq phyloseq
#' @importFrom phyloseq sam_data
#' @importFrom phyloseq otu_table
#' @importFrom magrittr %>%
#' @importFrom tidyr separate
#'
#' @return
#' @export
#'
#' @examples
#' data("metaphlanData")
#'
#' meta_physeq <- metaphlan_to_phyloseq <- function(metaphlanData,
#' metadata = NULL,
#' phyloTree = NULL,
#' version = 4)
#'

metaphlan_to_phyloseq <- function(mpa,
                                  metadata = NULL,
                                  phyloTree = NULL,
                                  version = 4){


  if(version == 4){
    if(is.character(mpa)){
      # load raw metaphlan data
      mpa <- data.table::fread(mpa) %>%
        as.data.frame()
    }

    if(version == 3){
      if(is.character(mpa)){
        # load raw metaphlan data
        mpa <- data.table::fread(mpa,skip = 1) %>%
          as.data.frame()
      }
    }

  rownames(mpa) <- paste("taxon", 1:nrow(mpa), sep = "")

  # subset taxonomic names, and keep the counts per samples only
  otu_ready <- mpa[,3:ncol(mpa)] %>%
    as.matrix()

  # create taxonomy table
  taxonomy_tab <- select(mpa, clade_name) %>%
    separate(col="clade_name", into = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "SGB"),sep = "\\|", fill = "right") %>%
    as.matrix()

    profiles <- phyloseq(otu_table(otu_ready, taxa_are_rows = TRUE),
                         tax_table(taxonomy_tab),
                         phy_tree(phyloTree, errorIfNULL = FALSE),
                         sample_data(metadata,errorIfNULL = FALSE)
    )

    return(profiles)
  }

}
