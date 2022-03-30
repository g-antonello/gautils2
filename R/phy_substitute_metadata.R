#' Substitute metadata in phyloseq objects. the two data frames must have row names matching sample names. The sample order doesn't matter, because the functions will reorder the new metatadata rows, to match the sample order in the phyloseq object.
#' @param physeq A phyloseq object contaning or not containing `sample_data`
#' @param new_metadata a data.frame object, with rownames containing `sample_names` of the `physeq` object

phy_substitute_metadata <- function(physeq, new_metadata){
  if(any(!rownames(new_metadata)%in%sample_names(physeq))){ # this is like asking: is any item of the vector NOT true? if responds true, it means rownames are not all there.
    stop ("not all row names of the metadata are in the sample names of the phyloseq object")
  }
  # reorganize metadata
  new_metadata <- new_metadata[match(sample_names(physeq), rownames(new_metadata)),]
  # remove old metadata
  physeq@sam_data <- NULL
  sample_data(physeq) <- sample_data(new_metadata)
  return(physeq)
}
