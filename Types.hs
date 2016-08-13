module Types (
    BAtom(..)
    ) where

data BAtom = BString String
           | BInt Int
           | BList [BAtom]
           | BDict [(BAtom, BAtom)]
           deriving (Show)
