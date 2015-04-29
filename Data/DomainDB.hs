{-# Language OverloadedStrings #-}

module Data.DomainDB(
  readDomains
) where

import qualified Data.ByteString.Char8 as B
import Network.DNS ( Domain(..), TYPE(A,AAAA) )


readDomains :: String -> IO ( [(Domain, TYPE)] )
readDomains fileName = do
    file <- B.readFile fileName
    return $ map parseDomain $ B.lines file

parseDomain :: B.ByteString -> (Domain, TYPE)
parseDomain l = (dn, t)
    where [t', dn] = B.words l
          t = case t' of
            "A"    -> A
            "AAAA" -> AAAA
