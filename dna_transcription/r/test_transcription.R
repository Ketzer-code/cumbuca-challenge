# importing libraries
library(testthat)

# setting wd to avoid problems with sourcing
wd <- getwd()
source(paste0(wd, "/dna_transcription/r/dna_transcription.R"))

test_that("Check if sequences in the challenge are being correctly transcribed", {
    expect_equal(transcribe_dna_to_rna("GGCTA"), "CCGAU")
    expect_equal(transcribe_dna_to_rna("ACTGATA"), "UGACUAU")
})