{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| A parser for the boolean language.  The concrete syntax is as follows:

  val ::= T | F
  var ::= [a-z][a-z0-9_]*

  expr ::= val
           var
           'not' expr
           expr 'and' expr
           expr 'or'  expr
           expr 'xor' expr
           (expr)

  Things like 'and' denote keywords.

  Precedence: 'not' > 'xor' > 'and' > 'or'  (but use parentheses anyway).
  if-then-else has to be parenthesised unless it's at the very top.

  The code is based on the tutorial at
  https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

  See also https://markkarpov.com/megaparsec/megaparsec.html
-}

module Zkbec.Language.Parser
    ( TextField (..)
    , parseExpr
    ) where

import           Zkbec.Prelude                  as Prelude hiding (many, try)

import           Zkbec.Language.Core
import           Zkbec.Language.Var

import           Control.Applicative            (pure)
import           Control.Monad.Combinators.Expr as E
import           Data.Field
import           Data.Field.F17
import qualified Data.Field.Galois              as GF
import qualified Data.Map                       as M
import           GHC.TypeLits
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

instance (MonadSupply m, Stream s) => MonadSupply (ParsecT e s m)

-- Consume whitespace
ws :: (MonadParsec e s m, Token s ~ Char) => m ()
ws = L.space space1 empty empty
-- Last two arguments are for comment delimiters.  Let's not have any comments for now.

-- Wrapper to consume whitespace after parsing an item using the wrapped parser
lexeme :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
lexeme = L.lexeme ws

-- Parse a fixed string
symbol :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ [Char]) => String -> m String
symbol = L.symbol ws

-- 'parens' parses something between parenthesis.
parens :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ [Char]) => m a -> m a
parens = between (symbol "(") (symbol ")")

signedDecimal :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ [Char], Integral a) => m a
signedDecimal = L.signed ws (lexeme L.decimal) <|> parens signedDecimal

parseFieldDefault :: (Field f, MonadParsec e s m, Token s ~ Char, Tokens s ~ [Char]) => m f
parseFieldDefault = unAField . fromInteger <$> signedDecimal

type Parser = ParsecT Void String (Prelude.State IdentifierState) -- Void -> No custom error messages

-- Stuff for generating new Unique names during parsing.  Based on Name.hs in PlutusCore.
-- IdentifierState maps names onto Vars and remembers a counter for Unique IDs.
type IdentifierState = (M.Map String Var, Int)

emptyIdentifierState :: IdentifierState
emptyIdentifierState = (mempty, 0)

-- | Look up a variable name. If we've already seen it, return the corresponding Var;
-- otherwise, increase the Unique counter and use it to construct a new Var.
makeVar :: (MonadState IdentifierState m) => String -> m Var
makeVar name = do
    (ss, counter) <- get
    case M.lookup name ss of
        Just v -> pure v
        Nothing -> do
            let v = Var (Unique counter) name
                counter' = counter + 1
            put (M.insert name v ss, counter')
            pure v

-- Note that any value produced by 'showField' must be parsable by 'parseField' even if it appears
-- in a large expression. This is why we always pretty-print rationals in parens currently.
class Field f => TextField f where
    parseField :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ [Char]) => m f
    default parseField :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ [Char]) => m f
    parseField = parseFieldDefault

    -- TODO: use proper precedence-sensitive pretty-printing.
    showField :: f -> String
    default showField :: Show f => f -> String
    showField = show

instance TextField f => Show (AField f) where
    show = showField . unAField

-- GHC will not derive this one automatically
instance (Field f, TextField f) => TextField (AField f) where
    parseField = AField <$> parseField
    showField  = showField . unAField

deriving anyclass instance TextField F17

instance TextField Rational where
    parseField = asum
        [ try $ do
              num <- parseFieldDefault
              _ <- symbol "/"
              den <- parseFieldDefault
              case num `Data.Field.div` den of
                  Just res -> pure res
                  Nothing  -> fail $ show num ++ "/" ++ show den ++ " is not a valid Rational"
        , parseFieldDefault
        , parens parseField
        ]

    showField r
        | denominator r == 1 = show $ numerator r
        | otherwise          = "(" ++ show (numerator r) ++ " / " ++ show (denominator r) ++ ")"

instance KnownNat p => TextField (GF.Prime p) where
    parseField = GF.toP <$> signedDecimal
    showField f
        | isNegative f = show $ i - p
        | otherwise    = show i
        where
            p = natVal @p Proxy
            i = GF.fromP f

-- | The main entry point: parse a string and return Either an error message or an Expr.
parseExpr :: String -> Either String Expr
parseExpr s = first errorBundlePretty . fst $ runState (runParserT top "" s) emptyIdentifierState

-- Parse the whole of an input stream
top :: Parser Expr
top = between ws eof expr

-- Keywords
keywords :: [String]
keywords = ["T", "F", "not", "and", "or", "xor", "if", "then", "else"]

-- Parse a keyword, checking that it's not a prefix of something else
keyword :: String -> Parser ()
keyword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier =  (lexeme . try) (p >>= check)
    where
      p       = (:) <$> lowerChar <*> many (lowerChar <|> digitChar <|> char '_')
      check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- Constants T and F
trueExpr :: Parser Expr
trueExpr =  EConst True <$ keyword "T"

falseExpr :: Parser Expr
falseExpr = EConst False <$ keyword "F"

valExpr :: Parser Expr
valExpr = trueExpr <|> falseExpr


-- Variables
varExpr :: Parser Expr
varExpr = EVar <$> (identifier >>= makeVar)

{- Use the Expr combinators from Control.Monad.Combinators.Expr to parse
   epressions involving prefix and infix operators.  This makes it a
   lot easier to get parsing of expressions right. It deals with
   precedence automatically and avoids problems with left recursion
   that may lead to non-terminating parses if you're not careful about
   binary infix expressions.
-}

-- expr1: things that can appear inside operExpr. This does not
-- include operExpr itself, because that would cause infinite recursion.
-- Note that an operExpr doesn't have to contain an operator: it
-- can just be a single expr1.
-- If an ifExpr has to appear inside an operExpr it has to be parenthesised.
expr1 :: Parser Expr
expr1 =  valExpr <|> varExpr <|> parens expr

-- expr: full expressions
expr :: Parser Expr
expr = operExpr <|> ifExpr

-- operExpr: expressions involving unary and binary operators
operExpr :: Parser Expr
operExpr = makeExprParser expr1 operators

operators :: [[E.Operator Parser Expr]]
operators = -- The order here determines operator precedence.
  [ [Prefix (EAppUnOp  Not <$ keyword "not")]
  , [InfixL (EAppBinOp Xor <$ keyword "xor")]
  , [InfixL (EAppBinOp And <$ keyword "and")]
  , [InfixL (EAppBinOp Or  <$ keyword "or")]
  ]

-- if e then r1 else e2
ifExpr :: Parser Expr
ifExpr = EIf <$> (keyword "if" *> expr) <*> (keyword "then" *> expr) <*> (keyword "else" *> expr)
