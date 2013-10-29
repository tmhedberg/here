{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | Literal, uninterpolated here docs
module Data.String.Here.Uninterpolated (here, hereFile, hereLit) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.String.Here.Internal

-- | Quote a here doc, stripping leading and trailing whitespace
here :: QuasiQuoter
here = QuasiQuoter {quoteExp = stringE . trim}

-- | Quote a here doc literally, with no whitespace stripping
hereLit :: QuasiQuoter
hereLit = QuasiQuoter {quoteExp = stringE}

-- | Splice a file's contents as a here doc
hereFile :: QuasiQuoter
hereFile = quoteDependentFile hereLit
