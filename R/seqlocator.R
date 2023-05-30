#' Sequence locator
#' 
#' This function returns the number of instances of a specified sequence within an input sequence and where they occur.
#' @param main Input sequence (string)
#' @param target Target sequence (string)
#' @keywords sequence, target_sequence, instances, positions
#' @details If the input sequence includes any letters which are not "A", "T", "U", "C", or "G", it will be recognised as invalid.
#' @export
#' @examples
#' seqlocator("ATCGATCG", "AT") #counts number of "AT" in the "ATCGATCG" sequence
#' seqlocator("ATCGATCG", "AC") #no instances of "AC" in the sequence


seqlocator <- function(main, target) {
  main <- strsplit(main, "")[[1]]
  target <- strsplit(target, "")[[1]]
  
  count <- 0
  loc <- c()
  for (i in 1:(length(main) - length(target) + 1)) {
    if (identical(main[i:(i + length(target) - 1)], target)) {
      count <- count + 1
      loc <- c(loc, i)
    }
  }
  
  if (length(loc) == 0) {
    print("No matches")
  }
  else {
    cat("Number of instances:", count, "\n")
    cat("Position(s):",loc)
  }
}




