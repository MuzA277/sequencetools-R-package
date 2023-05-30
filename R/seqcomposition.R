#' Sequence Composition Finder
#' 
#' This function returns the GC-content or the percentage of a certain base in a given sequence.
#' @param seq Input sequence (string)
#' @param type "GC", "A", "T", "C", "G", "U", "ALL" (default). The type of composition that should be returned. "GC" returns the GC content. "A" returns the percentage of adenine bases present ("T", "G", "C", "U" returns the respective base percentages). "ALL" returns all percentages and the GC content.
#' @keywords sequence, composition, gc-content, percentage
#' @details If the input sequence includes any letters which are not "A", "T", "U", "C", or "G", it will be recognised as invalid.
#' @export
#' @examples
#' seqcomposition("ATCG", "A") #percentage adenine bases
#' seqcomposition("ATCG", "T") #percentage adenine bases
#' seqcomposition("ATCG", "C") #percentage adenine bases
#' seqcomposition("ATCG", "G") #percentage adenine bases
#' seqcomposition("AUCG", "U") #percentage adenine bases
#' seqcomposition("ATCG", "ALL") #GC-content and percentages of bases


seqcomposition <- function(seq, type = "ALL") {
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
    
    
    if (type == "GC") {
      cat(GC_cont,"%")
    }
    else if (type == "A") {
      cat(per_A,"%")
    }
    else if (type == "T") {
      cat(per_T,"%")
    }
    else if (type == "C") {
      cat(per_C,"%")
    }
    else if (type == "G") {
      cat(per_G,"%")
    }
    else if (type == "U") {
      cat(per_U,"%")
    }
    else if (type == "ALL") {
      cat("GC_content:", GC_cont, "\n",
          "% Adenine:", per_A, "\n",
          "% Thymine:", per_T, "\n",
          "% Cytosine:", per_C, "\n",
          "% Guanine:", per_G, "\n",
          "% Uracil:", per_U)
    }
  }
}


