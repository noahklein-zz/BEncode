module Encoder (encode) where

import Types

encode :: BAtom -> String
encode (BString s) = show (length s) ++ ":" ++ s
encode (BInt i)    = "i" ++ show i ++ "e"
encode (BList xs)  = "l" ++ concatMap encode xs ++ "e"
encode (BDict xs)  = "d" ++ concatMap encode' xs ++ "e"
    where encode' (key, val) = encode key ++ encode val
