module Compiler where

import Prelude hiding (Functor)

import Runtime.WAMMain
import IR
import AST
import Compiler.CSnippets
import Utils


import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set


data Register = X Int
              | Y Int deriving (Eq, Ord, Show)

-- | Term representation used for register allocations
data RegTerm = RegVariable String
             | RegStructure Functor [Register] deriving (Eq, Ord)

functorFrom :: String -> [Term] -> Functor
functorFrom name ts = Functor name (length ts)

type Allocation = Map.Map Register RegTerm

-- | Allocates registers for structure sharing
allocateRegisters :: Int        -- ^ next register number
                  -> [Term]     -- ^ queue
                  -> Allocation -- ^ register allocation map
                  -> Allocation -- ^ result
allocateRegisters _ [] allocation = allocation
allocateRegisters n (next:queue) allocation =
  case next of
    Variable v -> allocateRegisters (n + 1) queue (addReg v)
    str@(Structure _ terms) -> allocateRegisters (n + length terms) (queue <> terms) (addStr str)
  where
    addReg v = Map.insert (X n) (RegVariable v) allocation
    addStr (Structure f terms) = Map.insert (X n) (RegStructure (functorFrom f terms) (sRegs $ length terms)) allocation
    sRegs nr = take nr [X i | i <- [(n + 1)..]]

compileQuery :: Atom         -- ^ Term to generate WAM for
             -> Allocation   -- ^ register allocation
             -> Set.Set Register -- ^ seen registers
             -> [WAM]        -- ^ generated WAM sequence
compileQuery a@(Atom name terms) alloc seen = [Call (toFunctor a)]


compileFact :: Atom             -- ^ Term to generate WAM for
            -> Allocation       -- ^ register allocation
            -> Set.Set Register -- ^ seen registers
            -> [WAM]            -- ^ generated WAM sequence
compileFact _ _ _ = undefined

-- | Generate C source
class CSource a where
  cSource :: a -> Text

instance CSource IRProgram where
  cSource (IRProgram clauses query) = wamMain compiledClauses compiledQuery
    where
      compiledClauses = T.unlines (map cSource clauses)
      compiledQuery = cSource query

instance CSource IRClauseMulti where
  cSource (IRClauseMulti _ [clause]) = cSource clause
  cSource (IRClauseMulti _ _) = error "Only L2 supported!"

instance CSource IRClause where
  cSource (IRClause hd []) = cSource hd
  cSource _ = error "Only L1 supported!"

instance CSource Atom where
  cSource (Atom name terms) = T.unlines $ map cSource $ generateWam (Structure name terms) Map.empty Set.empty

instance CSource IRQuery where
  cSource (IRQuery [query]) = _
  cSource _ = error "Only L1 supported!"

instance CSource Register where
  cSource reg = case reg of
    (X i) -> register (T.pack "X") (showT i)
    (Y i) -> register (T.pack "Y") (showT i)

newtype Label = Label Int

instance CSource Functor where
  cSource (Functor name arity) = functor (showT name) (showT arity)

data WAM
  = PutStructure  Functor Register
  | SetVariable   Register
  | SetValue      Register
  | GetStructure  Functor Register
  | UnifyVariable Register
  | UnifyValue    Register
  | Call          Functor
  | Proceed
  | Allocate      Int
  | Deallocate
  deriving Show

instance CSource WAM where
  cSource (PutStructure f reg)  = putStructure (cSource f) (cSource reg)
  cSource (SetVariable reg)     = setVariable (cSource reg)
  cSource (SetValue reg)        = setValue (cSource reg)
  cSource (GetStructure f reg)  = getStructure (cSource f) (cSource reg)
  cSource (UnifyVariable reg)   = unifyVariable (cSource reg)
  cSource (UnifyValue reg)      = unifyValue (cSource reg)
  cSource (Call f)              = call (cSource f)
  cSource Proceed               = proceed
  cSource (Allocate n)          = allocate (showT n)
  cSource Deallocate            = deallocate
  cSource w                     = notImplemented (showT w)