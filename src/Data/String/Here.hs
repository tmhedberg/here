{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | Here docs via quasiquotation
module Data.String.Here (here, hereLit) where

import Data.Char

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Quote a here doc, stripping leading and trailing whitespace
here :: QuasiQuoter
here = QuasiQuoter {quoteExp = stringE . trim}

-- | Quote a here doc literally, with no whitespace stripping
hereLit :: QuasiQuoter
hereLit = QuasiQuoter {quoteExp = stringE}

trim :: String -> String
trim = trimTail . dropWhile isSpace

trimTail :: String -> String
trimTail "" = ""
trimTail s = take (lastNonBlank s) s
  where lastNonBlank = (+1) . fst . foldl acc (0, 0)
        acc (l, n) c | isSpace c = (l, n + 1)
                     | otherwise = (n, n + 1)
