#' Read Metaphlan profiles
#'
#' Given the table location, the function
#'
#' @param path Either a \code{character} vector containing the path to the text table to read, or the already-read object.
#' @param version the metaphlan version, by default, version 4 is taken (to date it is the latest - Apr 2022)
#'
#' @importFrom phyloseq phyloseq
#' @importFrom phyloseq sam_data
#' @importFrom phyloseq otu_table
#' @importFrom data.table fread
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
      mpa <- fread(mpa_path) %>%
        as.data.frame()
    }

    if(version == 3){
      if(is.character(mpa)){
        # load raw metaphlan data
        mpa <- fread(mpa_path,skip = 1) %>%
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

    profiles <- phyloseq(otu_table(otu_ready),
                         tax_table(taxonomy_tab),
                         phy_tree(phyloTree),
                         sam_data(metadata))

    return(profiles)
  }

  ## if version == 3? older versions are not too important
}
