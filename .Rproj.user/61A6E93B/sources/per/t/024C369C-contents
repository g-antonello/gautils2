
#' Drop NAs from selected variables
#'
#' Given a phyloseq object and a character vector, the function removes all NAs that are in at least one variable
#' @param physeq a phyloseq object
#' @param vars a character vector with variable names to remove NAs from
#'
#' @return a clean phyloseq object
#' @export
#'
#' @examples
#' data("GlobalPatterns")
#' set.seed(1111)
#' GlobalPatternsNA <- GlobalPatterns
#' GlobalPatternsNA@sam_data$newVar1 <- replicate(n = nsamples(GlobalPatternsNA), expr = sample(x =c(NA, 1,0), size = 1, prob = c(0.1, 0.4, 0.5)))
#' GlobalPatternsNA@sam_data$newVar2 <- replicate(n = nsamples(GlobalPatternsNA), expr = sample(x =c(NA, 1,0), size = 1, prob = c(0.1, 0.4, 0.5)))
#'
#' drop_NA_from_phyloseq_vars(physeq = GlobalPatternsNA, vars = c("newVar1", "newVar2"))
#' GlobalPatterns

phy_drop_NA_vars <- function(physeq, vars){
  # get a logical vector to subset variables in those
  subset_logical <- microbiome::meta(physeq) %>%
    select(vars) %>%
    complete.cases()

  physeq_cleaned <- prune_samples(subset_logical, physeq)
  physeq_cleaned <- prune_taxa(taxa_sums(physeq_cleaned) > 0,physeq_cleaned)

  return(physeq_cleaned)
}
