#' Sequence Length Checker
#' 
#' This function checks the length of an input sequence 
#' @param seq Input sequence (string)
#' @keywords sequence, length
#' @details This function can check the length of sequences (as strings).
#' @export
#' @examples
#' seqlength("ATCG") #Sequence must be a string

seqlength <- function(seq){
  nchar <- as.list(strsplit(seq, "")[[1]])
  length(nchar)
}
  