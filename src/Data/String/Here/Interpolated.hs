{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | Interpolated here docs
module Data.String.Here.Interpolated (i, iTrim, template) where

import Control.Applicative hiding ((<|>))
import Control.Monad.State
import Control.Lens hiding (parts)

import Data.Maybe
import Data.Monoid
import Data.String
import Data.Typeable

import Language.Haskell.Meta
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Text.Parsec
import Text.Parsec.String

import Data.String.Here.Internal

data StringPart = Lit String | Esc Char | Anti (Q Exp)

data HsChompState = HsChompState { _quoteState :: QuoteState
                                 , _braceCt :: Int
                                 , _consumed :: String
                                 }

data QuoteState = None | Single Escaped | Double Escaped

type Escaped = Bool

makeLenses ''HsChompState

-- | Quote a here doc with embedded antiquoted expressions
--
-- Any expression occurring between @${@ and @}@ (for which the type must have
-- 'Show' and 'Typeable' instances) will be interpolated into the quoted
-- string.
--
-- Characters preceded by a backslash are treated literally. This enables the
-- inclusion of the literal substring @${@ within your quoted text by writing
-- it as @\\${@. The literal sequence @\\${@ may be written as @\\\\${@.
i :: QuasiQuoter
i = QuasiQuoter {quoteExp = quoteInterp}

-- | Like 'i', but with leading and trailing whitespace trimmed
iTrim :: QuasiQuoter
iTrim = QuasiQuoter {quoteExp = quoteInterp . trim}

-- | Quote the contents of a file as with 'i'
--
-- This enables usage as a simple template engine
template :: QuasiQuoter
template = quoteFile i

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
    combine parts = foldr1 (\subExpr acc -> [|$subExpr <> $acc|]) parts

toString :: (Show a, Typeable a, IsString b) => a -> b
toString x = fromString $ fromMaybe (show x) (cast x)

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
p_antiExpr = p_untilUnbalancedCloseBrace
         >>= either fail (return . return) . parseExp

p_untilUnbalancedCloseBrace :: Parser String
p_untilUnbalancedCloseBrace = evalStateT go $ HsChompState None 0 ""
  where
    go = do
      c <- lift anyChar
      consumed %= (c:)
      HsChompState {..} <- get
      case _quoteState of
        None -> case c of
          '{' -> braceCt += 1 >> go
          '}' | _braceCt > 0 -> braceCt -= 1 >> go
              | otherwise -> stepBack >> return (reverse $ tail _consumed)
          '\'' -> quoteState .= Single False >> go
          '"' -> quoteState .= Double False >> go
          _ -> go
        Single False -> do case c of '\\' -> quoteState .= Single True
                                     '\'' -> quoteState .= None
                                     _ -> return ()
                           go
        Single True -> quoteState .= Single False >> go
        Double False -> do case c of '\\' -> quoteState .= Double True
                                     '"' -> quoteState .= None
                                     _ -> return ()
                           go
        Double True -> quoteState .= Double False >> go
    stepBack = lift $
      updateParserState
        (\s@State {..} -> s {statePos = incSourceColumn statePos (-1)})
        >> getInput
        >>= setInput . ('}':)

p_esc :: Parser StringPart
p_esc = Esc <$> (char '\\' *> anyChar)

p_lit :: Parser StringPart
p_lit = fmap Lit $
  try (litCharTil $ lookAhead p_antiOpen <|> lookAhead (string "\\"))
    <|> litCharTil eof
  where litCharTil = manyTill $ noneOf ['\\']
