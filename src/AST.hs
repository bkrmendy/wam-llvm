module AST where

import Text.Parsec

{-| AST |-}

-- | p(a, H, f(x, T)).
-- p(
--    a,      <- Constant "a"
--    H,      <- Variable "H"
--    f(x, T) <- Structure "f" [Constant "a", Variable "T"]
data Term = Variable String
          | Structure String [Term] deriving Eq

-- | p(a, H, f(x, T))
data Atom = Atom String [Term]

-- | Top level clauses.
data HornClause
  = DefiniteClause Atom [Atom]  -- ^ p(X, Z) :- h(X, a), g(b, Z)
  | GoalClause [Atom]           -- ^ ?- p(X, Z).

newtype Program = Program [HornClause]

{-| PARSING |-}

whitespace :: Parsec String () String
whitespace = many1 (char ' ') <|> many1 (char '\n') <|> many1 (char '\t')

lowerCaseToken :: Parsec String () String
lowerCaseToken = (:) <$> lower <*> many1 alphaNum

variable :: Parsec String () Term
variable = Variable <$> ((:) <$> upper <*> many1 alphaNum)

structure :: Parsec String () Term
structure = Structure <$> lowerCaseToken <*> between (char '(') (char ')') (sepBy1 term (string "," <|> whitespace))

term :: Parsec String () Term
term = try structure <|> variable

atom :: Parsec String () Atom
atom = Atom <$> lowerCaseToken <*> between (char '(') (char ')') (many1 term)

definiteClause :: Parsec String () HornClause
definiteClause = DefiniteClause <$> (atom <* string ":-") <*> sepBy1 atom (string "," *> whitespace)

goalClause :: Parsec String () HornClause
goalClause = GoalClause <$> (string "?-" *> whitespace *> sepBy1 atom (string "," *> whitespace))

hornClause :: Parsec String () HornClause
hornClause = goalClause <|> hornClause

program :: Parsec String () Program
program = Program <$> sepBy1 hornClause whitespace

parseSource :: String -> Either ParseError Program
parseSource = parse program ""

