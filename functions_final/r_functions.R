library(tidyverse)
library(testthat)

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

test_that("Check if sequences in the challenge are being correctly transcribed", {
    expect_equal(transcribe_dna_to_rna("GGCTA"), "CCGAU")
    expect_equal(transcribe_dna_to_rna("ACTGATA"), "UGACUAU")
})

#' create_summary(df, var)
#'
#' @description
#' Creates a dataframe with summarized statistics for a
#' specific variable
#' 
#' @param df The dataframe containing the variable
#' @param var The variable to be summarized
create_summary <- function(df, column) {
    df_summarized <- df %>%
                         mutate(
                            mean_var = mean({{column}}),
                            sd_var = sd({{column}}),
                            variance_var = var({{column}}),
                            p01 = quantile({{column}}, probs = c(.1)),
                            p25 = quantile({{column}}, probs = c(.25)),
                            p50 = median({{column}}),
                            p90 = quantile({{column}}, probs = c(.9)),
                            p99 = quantile({{column}}, probs = c(.99))
                        )
}

#' create_prop_table(df, var, mar = NULL)
#'
#' @description
#' Creates a proportion table, as a dataframe,
#' for a dataset.
#' 
#' @param df The dataframe containing the variable
#' @param var Used to pivot_longer the dataframe, to
#' properly transform rows containg percentage values
#' into columns
#' @param mar Defines if percentage will be rowise,
#' columnwise or in relation to the dataframe total
create_prop_table <- function(df, column, mar = NULL) {
    prop_df <- df %>%
                table(.) %>%
                prop.table(., margin = mar) %>%
                as.data.frame(.) %>%
                pivot_wider(., names_from = {{column}}, values_from = Freq)
}