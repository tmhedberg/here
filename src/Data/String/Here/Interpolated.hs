{-# LANGUAGE NamedFieldPuns, RecordWildCards, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | Interpolated here docs
module Data.String.Here.Interpolated (i, iTrim, template) where

import Control.Applicative hiding ((<|>))
import Control.Monad.State

import Data.Char
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

data HsChompState = HsChompState { quoteState :: QuoteState
                                 , braceCt :: Int
                                 , consumed :: String
                                 , prevCharWasIdentChar :: Bool
                                 }

data QuoteState = None | Single EscapeState | Double EscapeState

data EscapeState = Escaped | Unescaped

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
template = quoteDependentFile i

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
p_untilUnbalancedCloseBrace = evalStateT go $ HsChompState None 0 "" False
  where
    go = do
      c <- lift anyChar
      modify $ \st@HsChompState {consumed} -> st {consumed = c:consumed}
      HsChompState {..} <- get
      let next = setIdentifierCharState c >> go
      case quoteState of
        None -> case c of
          '{' -> incBraceCt 1 >> next
          '}' | braceCt > 0 -> incBraceCt (-1) >> next
              | otherwise -> stepBack >> return (reverse $ tail consumed)
          '\'' -> unless prevCharWasIdentChar (setQuoteState $ Single Unescaped)
               >> next
          '"' -> setQuoteState (Double Unescaped) >> next
          _ -> next
        Single Unescaped -> do case c of '\\' -> setQuoteState (Single Escaped)
                                         '\'' -> setQuoteState None
                                         _ -> return ()
                               next
        Single Escaped -> setQuoteState (Single Unescaped) >> next
        Double Unescaped -> do case c of '\\' -> setQuoteState (Double Escaped)
                                         '"' -> setQuoteState None
                                         _ -> return ()
                               next
        Double Escaped -> setQuoteState (Double Unescaped) >> next
    stepBack = lift $
      updateParserState
        (\s@State {..} -> s {statePos = incSourceColumn statePos (-1)})
        >> getInput
        >>= setInput . ('}':)
    incBraceCt n = modify $ \st@HsChompState {braceCt} ->
      st {braceCt = braceCt + n}
    setQuoteState qs = modify $ \st -> st {quoteState = qs}
    setIdentifierCharState c = modify $ \st ->
      st
        {prevCharWasIdentChar = or [isLetter c, isDigit c, c == '_', c == '\'']}

p_esc :: Parser StringPart
p_esc = Esc <$> (char '\\' *> anyChar)

p_lit :: Parser StringPart
p_lit = fmap Lit $
  try (litCharTil $ lookAhead p_antiOpen <|> lookAhead (string "\\"))
    <|> litCharTil eof
  where litCharTil = manyTill $ noneOf ['\\']
