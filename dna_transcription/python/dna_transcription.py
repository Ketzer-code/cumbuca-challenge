def transcribe_dna(seq: str) -> str:
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
    rna = [nucleotide_switcher.get(l) for l in seq]

    return "".join(rna)