#' DNA/RNA sequence checker 
#' 
#' This function checks whether the input sequence is a DNA sequence or a RNA sequence. 
#' @param seq Input sequence (string)
#' @keywords DNA, RNA, sequence
#' @details If the input sequence includes any letters which are not "A", "T", "U", "C", or "G", it will be recognised as invalid.
#' @export
#' @examples
#' seqcheck("ATCG") #A DNA sequence
#' seqcheck("AUCG") #A RNA sequence
#' seqcheck("AJEI") #An invalid sequence
#' seqcheck("atcg") #the function works with both cases

seqcheck <- function(seq) {
  seq <- toupper(seq)
  valid_dna <- "ATCG"
  valid_rna <- "AUCG"
  
  if (all(strsplit(seq, "")[[1]] %in% strsplit(valid_dna, "")[[1]])) {
    return("DNA")
  } else if (all(strsplit(seq, "")[[1]] %in% strsplit(valid_rna, "")[[1]])) {
    return("RNA")
  } else {
    return("Invalid Sequence")
  }
}
