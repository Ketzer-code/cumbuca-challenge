import unittest

def transcribe_dna_to_rna(dna: str) -> str:
    """
    Transcribes a DNA sequence into a RNA sequence.

    Takes a string that represents a DNA sequence, substituting
    each letter for it's corresponding match in a RNA sequence,
    returning the transcribed sequence as a string.
    
    Parameters
    ----------
    seq : str
        The DNA sequence to be transcribed.
    
    Returns
    -------
    str
        The transcribed RNA sequence.
    """
    
    nucleotide_switcher = {
        "G":"C",
        "C":"G",
        "T":"A",
        "A":"U"
    }
    
    # Get the corresponding RNA letter for each letter in DNA
    rna = [nucleotide_switcher.get(l) for l in dna]

    return "".join(rna)

class TestTranscription(unittest.TestCase):
    """
    Class used to test if DNA sequences
    are being correctly transcribed into
    RNA sequences.

    Methods
    -------
    test_one
        Tests if the first mentioned sequence in the
        challenge is being correctly transcribed.
    test_two
        Tests it the second mentioned sequence in the
        challenge ins being correctly transcribed.
    """
    
    def test_one(self):
        self.assertEqual(
            transcribe_dna_to_rna("GGCTA"),
            "CCGAU",
            "Sequence incorrectly transcribed, should be CCGAU"
        )

    def test_two(self):
        self.assertEqual(
            transcribe_dna_to_rna("ACTGATA"),
            "UGACUAU",
            "Sequence incorrectly transcribed, should be UGACUAU"
        )
if __name__ == '__main__':
  unittest.main(exit = False)