#' Sequence transcription
#' 
#' This function transcribes a given sequence into RNA or DNA
#' @param seq Input sequence (string)
#' @param type "DNA", "RNA". The target sequence type to be transcribed to.
#' @keywords sequence, transcribe, DNA, RNA
#' @export
#' @examples
#' seqtranscribe("AUCG", type = "DNA") #transcribes a RNA sequence into DNA
#' seqtranscribe("ATCG", type = "RNA") #transcribes a DNA sequence into RNA
#' seqtranscribe("ASTQ", type = "DNA") #Invalid Sequence


seqtranscribe <- function(seq, type) {
  seq <- toupper(seq)
  valid_dna <- "ATCG"
  valid_rna <- "AUCG"
  
  if (all(strsplit(seq, "")[[1]] %in% strsplit(valid_dna, "")[[1]])) {
    checker <- "DNA"
  } else if (all(strsplit(seq, "")[[1]] %in% strsplit(valid_rna, "")[[1]])) {
    checker <- "RNA"
  } else {
    checker <- "Invalid Sequence"
  }
  
  if (checker == type) {
    print(seq)
  } else if (checker == "Invalid Sequence") {
      print(checker)
  } else {
      seq <- strsplit(seq, "")[[1]]
      if (type == "DNA") {
        for (i in 1:length(seq)) {
          if (seq[i] == "U") {
            seq[i] <- "T"
          }
          else {
            seq <- seq
          }
        }
      }
      else if (type == "RNA") {
        for (i in 1:length(seq)) {
          if (seq[i] == "T") {
            seq[i] <- "U"
          }
          else {
            seq <- seq
          }
        }
      }
      cat(seq, sep = "")
  }
}


