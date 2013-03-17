{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | Interpolated here docs
module Data.String.Here.Interpolated (i, iTrim) where

import Control.Applicative hiding ((<|>))

import Data.Maybe
import Data.Typeable

import Language.Haskell.Meta
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Text.Parsec
import Text.Parsec.String

import Data.String.Here.Internal

-- | Quote a here doc with embedded antiquoted expressions
--
-- Any expression occurring between @${@ and @}@ (for which the type must have
-- 'Show' and 'Typeable' instances) will be interpolated into the quoted
-- string.
--
-- Characters preceded by a backslash are treated literally. This enables the
-- inclusion of a the literal substring @${@ within your quoted text by writing
-- it as @\\${@. The literal sequence @\\${@ may be written as @\\\\${@.
i :: QuasiQuoter
i = QuasiQuoter {quoteExp = quoteInterp}

-- | Like 'i', but with leading and trailing whitespace trimmed
iTrim :: QuasiQuoter
iTrim = QuasiQuoter {quoteExp = quoteInterp . trim}

data StringPart = Lit String | Esc Char | Anti (Q Exp)

quoteInterp :: String -> Q Exp
quoteInterp s = either (handleError s) combineParts (parseInterp s)

handleError :: String -> ParseError -> Q Exp
handleError expStr parseError = error $
  "Failed to parse interpolated expression in string: "
    ++ expStr
    ++ "\n"
    ++ show parseError

combineParts :: [StringPart] -> Q Exp
combineParts = combine . map toExpQ
  where
    toExpQ (Lit s) = stringE s
    toExpQ (Esc c) = stringE [c]
    toExpQ (Anti expq) = [|toString $expq|]
    combine [] = stringE ""
    combine parts = foldr1 (\subExpr acc -> [|$subExpr ++ $acc|]) parts

toString :: (Show a, Typeable a) => a -> String
toString x = fromMaybe (show x) (cast x)

parseInterp :: String -> Either ParseError [StringPart]
parseInterp = parse p_interp ""

p_interp :: Parser [StringPart]
p_interp = manyTill p_stringPart eof

p_stringPart :: Parser StringPart
p_stringPart = try p_anti <|> p_esc <|> p_lit

p_anti :: Parser StringPart
p_anti = Anti <$> between p_antiOpen p_antiClose p_antiExpr

p_antiOpen :: Parser String
p_antiOpen = string "${"

p_antiClose :: Parser String
p_antiClose = string "}"

p_antiExpr :: Parser (Q Exp)
p_antiExpr = manyTill anyChar (lookAhead p_antiClose)
         >>= either fail (return . return) . parseExp

p_esc :: Parser StringPart
p_esc = Esc <$> (char '\\' *> anyChar)

p_lit :: Parser StringPart
p_lit = fmap Lit $
  try (litCharTil $ lookAhead p_antiOpen <|> lookAhead (string "\\"))
    <|> litCharTil eof
  where litCharTil = manyTill $ noneOf ['\\']
