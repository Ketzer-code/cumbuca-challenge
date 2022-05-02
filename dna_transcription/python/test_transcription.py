# importing libraries and functions
import unittest
from dna_transcription import transcribe_dna

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
            transcribe_dna("GGCTA"),
            "CCGAU",
            "Sequence incorrectly transcribed, should be CCGAU"
        )

    def test_two(self):
        self.assertEqual(
            transcribe_dna("ACTGATA"),
            "UGACUAU",
            "Sequence incorrectly transcribed, should be UGACUAU"
        )

if __name__ == '__main__':
    unittest.main()
