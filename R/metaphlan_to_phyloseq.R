#' Read Metaphlan profiles
#'
#' Given the table location, the function
#'
#' @param path a \code{character} vector containing the path to the text table to read
#' @param version the metaphlan version, by default, version 4 is taken (to date it is the latest - Apr 2022)
#'
#' @importFrom phyloseq phyloseq
#' @importFrom phyloseq sam_data
#' @importFrom phylsoeq otu_table
#' @importFrom data.table fread
#' @importFrom magrittr %>%
#' @importFrom tidyr separate
#'
#' @return
#' @export
#'
#' @examples
metaphlan_to_phyloseq <- function(mpa_path, metadata = NULL, phyloTree = NULL, version = 4){
  # load raw metaphlan data
  raw_mpa <- fread(mpa_path) %>%
    as.data.frame(raw_mpa)

  rownames(raw_mpa) <- paste("taxon", 1:nrow(raw_mpa), sep = "")

  # subset taxonomic names, and keep the counts per samples only
  otu_ready <- raw_mpa[,3:ncol(raw_mpa)] %>%
    as.matrix()

  # create taxonomy table
  taxonomy_tab <- select(raw_mpa, clade_name) %>%
    separate(col="clade_name", into = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "SGB"),sep = "\\|", fill = "right") %>%
    as.matrix()

    profiles <- phyloseq(otu_table(otu_ready),
                         tax_table(taxonomy_tab),
                         phy_tree(phyloTree),
                         sam_data(metadata))

    return(profiles)
}
