# sequencetools

An R package for processing genetic code and seqeuence data

`sequencetools` includes several tools for DNA and RNA sequence manipulation:  
- `seqcheck()` - The type of nucleic acid that a given sequence corresponds to.
- `seqcomplement()` - The complementary or reverse complementary sequence of an input sequence.
- `seqcomposition()` - The composition of a given sequence (percentage of a specific base or GC-content).
- `seqlength()` - The length of a given sequence
- `seqlocator()` - The location and number of instances of a specified sequence within a larger input sequence.
- `seqsummary()` - A summary of the type, length, and composition of a given sequence.
- `seqtranscribe()` - The transcribed version of a given sequence (DNA -> RNA or RNA -> DNA)
- `seqtranslate()` - The primary amino structure of a given RNA sequence (1-letter or 3-letter code)  
    
    
## Installation

`sequencetools` can be installed from github using:
```r
install.packages("remotes")
remotes::install_github('muzaffarabdullaev/sequencetools')
```

Alternatively, install from github using:
```r
install.packages("devtools")
devtools::install_github('muzaffarabdullaev/sequencetools')
```

## Status and Contribution
This package is currently under early stage development. 
  
⭐ Contribution is highly welcome! ⭐  
  
If there are problems with the package, feel free to let me know!

## Code of Conduct
Users and Contributors should read the "CODE_OF_CONDUCT.md" file before posting feedback/changes to the repo.  
Thank you!
