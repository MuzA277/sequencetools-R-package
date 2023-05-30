#' Sequence Summary
#' 
#' This function returns a summary of a given sequence, including the type of sequence, the length of the sequence, and the percentages of bases present.
#' @param seq Input sequence (string)
#' @keywords sequence, composition, length, DNA, RNA, gc-content, percentage
#' @details If the input sequence includes any letters which are not "A", "T", "U", "C", or "G", it will be recognised as invalid.
#' @export
#' @examples
#' seqsummary("ATCG") #Returns a summary of the sequence

seqsummary <- function(seq) {
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
  
  if (checker == "Invalid Sequence") {
    print(checker)
  }
  else {
    seq <- strsplit(seq, "")[[1]]
    len <- length(seq)
    count_A <- 0
    count_T <- 0
    count_C <- 0
    count_G <- 0
    count_U <- 0
    
    for (i in 1:length(seq)) {
      if (seq[i] == "A") {
        count_A <- count_A + 1
      }
      else if (seq[i] == "T") {
        count_T <- count_T + 1
      }
      else if (seq[i] == "C") {
        count_C <- count_C + 1
      }
      else if (seq[i] == "G") {
        count_G <- count_G + 1
      }
      else if (seq[i] == "U") {
        count_U <- count_U + 1
      }
    }
    
    GC_cont <- ((count_G + count_C)/len)*100
    per_A <- (count_A/len)*100
    per_T <- (count_T/len)*100
    per_C <- (count_C/len)*100
    per_G <- (count_G/len)*100
    per_U <- (count_U/len)*100
    
    cat("Sequence Summary:", "\n\n",
        "  Sequence type:", checker, "\n",
        "  Sequence length", len, "\n\n",
        "Sequence composition:", "\n\n",
        "  GC_content:", GC_cont, "\n",
        "  % Adenine:", per_A, "\n",
        "  % Thymine:", per_T, "\n",
        "  % Cytosine:", per_C, "\n",
        "  % Guanine:", per_G, "\n",
        "  % Uracil:", per_U)
  }
}