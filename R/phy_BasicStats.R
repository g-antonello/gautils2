#' Basic Phyloseq stats
#' 
#' Generate a data frame with prevalence, mean abundance and variance of the taxa in a phyloseq object
#' 
#' @param x Either a \code{phyloseq} or a  \code{otu.table} or a \code{matrix} object. If you have a matrix, make sure taxa are on the rows
#' @param transform Any transformation function allowed by phy_transform 
#'
#' @return A \code{data.frame} object 
#' @export
#'
#' @examples
#' 
#' data(GlobalPatterns)
#' gp_genus <- tax_glom(GlobalPatterns, "Genus")
#' basic_statistics <- phy_BasicStats(gp_genus, transform = "clr")
#' 

phy_BasicStats <- function(x, transform){
  
  if(!(class(x) %in% c("phyloseq", "otu_table", ""))){
    stop("Object's class not a phyloseq, otu table or a matrix")
  }
  
  if(class(x) %in% c("out_table", "phyloseq")){
    data0 <- abundances(x)
  }else{
    data0 <- as.matrix(x)
  }
  
  physeq_preval <- as.data.frame(prevalence(data0)) %>% 
    rownames_to_column("taxon") %>% 
    set_names(c("taxon", "Prevalence"))
  
  physeq_abund <- data0 %>% 
    phy_transform(transform) %>% 
    abundances() %>% 
    apply(1, mean) %>% 
    as.data.frame() %>% 
    rownames_to_column("taxon") %>% 
    set_names(c("taxon", paste0("Mean(", toupper(transform), "(abundance))")))
  
  physeq_var <- data0 %>% 
    phy_transform(transform) %>% 
    abundances() %>% 
    apply(1, var) %>% 
    as.data.frame() %>% 
    rownames_to_column("taxon")%>% 
    set_names(c("taxon", paste0("Var(", toupper(transform), "(abundance))")))
  
  basic_stats_df <- merge(physeq_preval, physeq_abund, by = "taxon", all.x = TRUE) %>% 
    merge(., physeq_var, by = "taxon", all.x = TRUE)
}
