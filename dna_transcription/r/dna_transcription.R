# importing libraries
library(tidyverse)

#' transcribe_dna_to_rna(dna)
#'
#' @description
#' Takes a string that representes a DNA sequence,
#' substituting each letter for it's corresponding match
#' in a RNA sequence, returning the transcribed sequence as
#' a character.
#'
#' @param dna A DNA sequence, as a character, to be transcribed
#' @return The transcribed RNA sequence, as a character
transcribe_dna_to_rna <- function(dna) {
    dna_chars <- c("G", "C", "T", "A")
    rna_chars <- c("C", "G", "A", "U")

    rna <- dna %>%
            unlist(stringr:str_split(., "")) %>%
            plyr::mapvalues(., dna_chars, rna_chars) %>%
            paste(., collapse = " ")

    return(rna)
}
