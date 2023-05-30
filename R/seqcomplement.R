#' Complementary/Reverse complementary sequence generator
#' 
#' This function converts an input DNA or RNA sequence into it's complementary or reverse complementary sequence. 
#' @param seq Input sequence (string)
#' @param uppercase TRUE (default) or FALSE, determining whether the output sequence is printed in uppercase (TRUE) or lowercase (FALSE) letters.
#' @param reverse TRUE or FALSE (default), determining whether the output sequence is the complemetary sequence(FALSE) or the reverse complementary sequence (TRUE).
#' @keywords DNA, RNA, sequence, complementary sequence, reverse complementary sequence
#' @details If the input sequence includes any letters which are not "A", "T", "U", "C", or "G", it will be recognised as invalid.
#' @export
#' @examples
#' seqcomplement("ATCG", reverse = TRUE) #A reverse complementary DNA sequence 
#' seqcomplement("AUCG", uppercase = FALSE) #A lowercase complementary RNA sequence
#' seqcomplement("AJEI") #An invalid sequence


seqcomplement <- function(seq, uppercase=TRUE, reverse=FALSE) {
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

  seq_type <- checker
  if (seq_type == "DNA") {
    comp_seq <- vector()
    for (base in strsplit(seq, "")[[1]]) {
      if (base == "A") {
        comp_seq <- append(comp_seq, "T") 
      }
      else if (base == "T") {
        comp_seq <- append(comp_seq, "A") 
      }
      else if (base == "C") {
        comp_seq <- append(comp_seq, "G") 
      }
      else if (base == "G") {
        comp_seq <- append(comp_seq, "C") 
      }
    }
    if (reverse == TRUE) {
      comp_seq <- rev(comp_seq)
      
      if (uppercase == FALSE) {
        comp_seq <- tolower(comp_seq)
        comp_seq <- paste(comp_seq, collapse = "")
        print(comp_seq)
      }
      else {
        comp_seq <- comp_seq
        comp_seq <- paste(comp_seq, collapse = "")
        print(comp_seq)
      }
    }
    else {
      comp_seq <- comp_seq
      
      if (uppercase == FALSE) {
        comp_seq <- tolower(comp_seq)
        comp_seq <- paste(comp_seq, collapse = "")
        print(comp_seq)
      }
      else {
        comp_seq <- comp_seq
        comp_seq <- paste(comp_seq, collapse = "")
        print(comp_seq)
      } 
    }
  }
  else if (seq_type == "RNA") {
    comp_seq <- vector()
    for (base in strsplit(seq, "")[[1]]) {
      if (base == "A") {
        comp_seq <- append(comp_seq, "U") 
      }
      else if (base == "U") {
        comp_seq <- append(comp_seq, "A") 
      }
      else if (base == "C") {
        comp_seq <- append(comp_seq, "G") 
      }
      else if (base == "G") {
        comp_seq <- append(comp_seq, "C") 
      }
    }
    if (reverse == TRUE) {
      comp_seq <- rev(comp_seq)
      
      if (uppercase == FALSE) {
        comp_seq <- tolower(comp_seq)
        comp_seq <- paste(comp_seq, collapse = "")
        print(comp_seq)
      }
      else {
        comp_seq <- comp_seq
        comp_seq <- paste(comp_seq, collapse = "")
        print(comp_seq)
      }
    }
    else {
      comp_seq <- comp_seq
      
      if (uppercase == FALSE) {
        comp_seq <- tolower(comp_seq)
        comp_seq <- paste(comp_seq, collapse = "")
        print(comp_seq)
      }
      else {
        comp_seq <- comp_seq
        comp_seq <- paste(comp_seq, collapse = "")
        print(comp_seq)
      } 
    }
  }
  else {
    return("Invalid Sequence")
  }
}
