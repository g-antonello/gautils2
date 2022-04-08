#' Adds taxonomic info to a data.frame/tibble
#'
#' This funcion adds some levels of phylogeny based on a phyloseq object onto a `data.frame` or `tibble` containing basic taxonomic information in a column
#'
#' @param df `data.frame` or `tibble` containing some taxonomic information
#' @param col_ASV ASV, or in general taxon, column in the `data`
#' @param physeq phyloseq object, containing a `tax_table` with a column matching `col_ASV` name
#' @param tax_lvls a character vector, specifying the column names to add from the taxonomic table. Default is "all", which includes all levels
#'
#' @import phyloseq
#' @import tidyverse
#'
#' @return A data.frame with taxon identifications added
#' @export
#'
#' @examples
#' data(GlobalPatterns)
#'
#' taxon_prevalences <- microbiome::prevalence(GlobalPatterns) %>%
#'     as.data.frame() %>%
#'     rownames_to_column("OTU") %>% # create a column containing the unique taxon identifier
#'     set_names(c("OTU", "prevalences")) %>% # to make sure we have column names we want
#'     add_ASV_name_to_ASV_results(df = ., col_ASV = "OTU", physeq = GlobalPatterns, tax_lvls = c("Phylum", "Class", "Genus"))
#'
#'
add_ASV_name_to_ASV_results <- function(df, col_ASV,  physeq, tax_lvls = "all") {
  tax <- data.frame(tax_table(physeq))

  # correct the case when the otu/ASV names are in rownames but not on columns
  if(!(col_ASV %in% colnames(tax))){
    tax[col_ASV] <- rownames(tax)
  }

  if("all" %in% tax_lvls){
    m <- merge(df, tax[,rev(colnames(tax))], by = col_ASV, sort = FALSE,all.x = TRUE)
  }else{
    tax <- tax[, rev(c(tax_lvls,col_ASV))]
    m <- merge(df, tax, by = col_ASV, sort = FALSE,all.x = TRUE)
  }
  return(m)
}
