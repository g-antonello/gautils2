#' Title
#'
#' @param physeq a phyloseq object
#' @param destination_file write the file you want your fasta sequences to be written into
#'
#' @return either a list of sequences or it writes them into the specified file
#' @export
#'
#' @examples
#'
phyloseq_to_fasta <- function(physeq,
                              destination_file = NULL){

  # format option 1: sequences are somewhere in the taxa table
  if(is.null(physeq@refseq)){
    tx <- as.data.frame(physeq@tax_table@.Data)
    pos <- sapply(tx, is.nucleotide)
    if(!any(pos)){stop("there is no reference sequence in the taxonomic table")}
    seqs <- as.character(tx[, pos])
    names(seqs) <- rownames(tx)

    if (is.null(destination_file)){
      return(seqs)
      stop("no destination file for saving refseq data")
    }
  }
  if(!is.null(physeq@refseq)){ # format 2, there is a refseq
    seqs <- as.character(refseq(physeq))

    if(is.null(destination_file)){
      return(refseq(physeq))
      stop("no destination file for saving refseq data")
    }

  }

  # write the fasta file
  cat(file = destination_file, paste(">", names(seqs), "\n", seqs, "\n", sep = ""), sep = "")
  cat(paste("\nfasta file written: '", tools::file_path_as_absolute(destination_file), "'", sep = ""))
}
