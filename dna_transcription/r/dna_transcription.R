# importing libraries
library(magrittr)

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
    nucleotide_switcher <- c(
        "G" = "C",
        "C" = "G",
        "T" = "A",
        "A" = "U"
    )

    rna <- dna %>%
            stringr::str_split(., "") %>%
            unlist(.) %>%
            dplyr::recode(., !!!nucleotide_switcher) %>%
            paste(., collapse = "")

    return(rna)
}
