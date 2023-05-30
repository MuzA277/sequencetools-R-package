#' Sequence translator
#'
#' This function translates an RNA sequence into an amino acid sequence (expressed in one letter or three letter code). 
#' @param seq Input sequence (string)
#' @param type "1", "3". Output sequence type: 1-letter (1)(Default) or 3-letter (3)
#' @keywords RNA sequence, amino acid sequence, primary structure, 1-letter code, 3-letter code
#' @details The function does not recognise DNA sequences. If the input sequence includes any letters which are not "A", "T", "U", "C", or "G", it will be recognised as invalid.
#' @export
#' @examples
#' seqtranslate("AAAUUUCCCGGG", type = 1) #Translating an RNA sequence into 1-letter amino acid code
#' seqtranslate("AAAUUUCCCGGG", type = 3) #Translating an RNA sequence into 3-letter amino acid code
#' seqtranslate("AAATTTCCCGGG", type = 1) #DNA sequences are invalid


seqtranslate <- function(seq, type = 1) {
  codon_table <- list(
    'A' = c('GCU', 'GCC', 'GCA', 'GCG'),
    'C' = c('UGU', 'UGC'),
    'D' = c('GAU', 'GAC'),
    'E' = c('GAA', 'GAG'),
    'F' = c('UUU', 'UUC'),
    'G' = c('GGU', 'GGC', 'GGA', 'GGG'),
    'I' = c('AUU', 'AUC', 'AUA'),
    'H' = c('CAU', 'CAC'),
    'K' = c('AAA', 'AAG'),
    'L' = c('UUA', 'UUG', 'CUU', 'CUC', 'CUA', 'CUG'),
    'M' = c('AUG'),
    'N' = c('AAU', 'AAC'),
    'P' = c('CCU', 'CCC', 'CCA', 'CCG'),
    'Q' = c('CAA', 'CAG'),
    'R' = c('CGU', 'CGC', 'CGA', 'CGG', 'AGA', 'AGG'),
    'S' = c('UCU', 'UCC', 'UCA', 'UCG', 'AGU', 'AGC'),
    'T' = c('ACU', 'ACC', 'ACA', 'ACG'),
    'V' = c('GUU', 'GUC', 'GUA', 'GUG'),
    'W' = c('UGG'),
    'Y' = c('UAU', 'UAC'),
    '*' = c('UAA', 'UAG', 'UGA')
  )
  codon_df <- data.frame(
    Codon = unlist(codon_table),
    Amino_Acid = rep(names(codon_table), sapply(codon_table, length)),
    stringsAsFactors = FALSE
  )
  row.names(codon_df) <- NULL
  
  code_to_code <- c(
    'A' = 'Ala',
    'C' = 'Cys',
    'D' = 'Asp',
    'E' = 'Glu',
    'F' = 'Phe',
    'G' = 'Gly',
    'H' = 'His',
    'I' = 'Ile',
    'K' = 'Lys',
    'L' = 'Leu',
    'M' = 'Met',
    'N' = 'Asn',
    'P' = 'Pro',
    'Q' = 'Gln',
    'R' = 'Arg',
    'S' = 'Ser',
    'T' = 'Thr',
    'V' = 'Val',
    'W' = 'Trp',
    'Y' = 'Tyr',
    '*' = '*'
  )

  seq <- toupper(seq)
  valid_rna <- "AUCG"
  if (all(strsplit(seq, "")[[1]] %in% strsplit(valid_rna, "")[[1]])) {
    checker <- "RNA"
  } else {
    checker <- "Invalid Sequence"
  }
  
  if (checker == "RNA") {
  seq <- strsplit(seq, "")[[1]]
  n <- 3
  count <- 0
  codons <- vector()
  for (i in 1:(length(seq)-2)) {
    count <- count + 1
    val <- paste(seq[i], seq[i+1], seq[i+2], sep = "")
    if (count == 1) {
      codons <- c(codons, val)
    }
    else if ((count - 1) %% n == 0) {
      codons <- c(codons, val)
    }
    else {
      codons <- codons
    }
  }
  
  amino_code <- vector()
  for(i in codons) {
    value <- codon_df$Amino_Acid[codon_df$Codon == i]
    amino_code <- c(amino_code, value)
  }
  
  if (type == 1) {
    code_1 <- paste(amino_code, collapse = "")  
    print(code_1)
  }
  else if (type == 3) {
    code_3 <- amino_code
    for (i in 1:length(code_3)) {
      code_3[i] <- code_to_code[[code_3[i]]]
    }
    code_3 <- paste(code_3, collapse = " ")
    print(code_3)
  }
  else {
    print("Invalid")
  }
  }
  
  else {
    print(checker)
  }
}

