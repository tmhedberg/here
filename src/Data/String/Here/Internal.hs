{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Data.String.Here.Internal (trim, quoteDependentFile) where

import Data.Char

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

trim :: String -> String
trim = trimTail . dropWhile isSpace

trimTail :: String -> String
trimTail "" = ""
trimTail s = take (lastNonBlank s) s
  where lastNonBlank = (+1) . fst . foldl acc (0, 0)
        acc (l, n) c | isSpace c = (l, n + 1)
                     | otherwise = (n, n + 1)

quoteDependentFile :: QuasiQuoter -> QuasiQuoter
quoteDependentFile QuasiQuoter {quoteExp} =
  QuasiQuoter
    { quoteExp = \filename -> do addDependentFile filename
                                 runIO (readFile filename) >>= quoteExp
    }
