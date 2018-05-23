module ProteinTranslation(proteins) where

import Data.Maybe (fromJust)

proteins :: String -> Maybe [String]
proteins []  = Just []
proteins rna = if null protein
               then Just []
               else Just $ protein : fromJust (proteins codons)
    where protein = translate codon
          codon   = take 3 rna
          codons  = if (not . null) protein then drop 3 rna else []

translate :: String -> String
translate codon
    | codon == "AUG"                          = "Methionine"
    | codon == "UGG"                          = "Tryptophan"
    | elem codon ["UUU", "UUC"]               = "Phenylalanine"
    | elem codon ["UUA", "UUG"]               = "Leucine"
    | elem codon ["UCU", "UCC", "UCA", "UCG"] = "Serine"
    | elem codon ["UAU", "UAC"]               = "Tyrosine"
    | elem codon ["UGU", "UGC"]               = "Cysteine"
    | elem codon ["UAA", "UAG", "UGA"]        = []
