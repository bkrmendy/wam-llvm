module IR where
import Prelude hiding (Functor)
import qualified Data.Map as Map

import AST

data Functor = Functor String Int deriving (Eq, Ord, Show)

newtype IRQuery = IRQuery [Atom]

data IRClause = IRClause Atom [Atom]
data IRClauseMulti = IRClauseMulti Functor [IRClause]

data IRProgram = IRProgram [IRClauseMulti] IRQuery

toFunctor :: Atom -> Functor
toFunctor (Atom name body) = Functor name (length body)

groupDefs :: [(Atom, [Atom])] -> [IRClauseMulti]
groupDefs = map (uncurry IRClauseMulti) . Map.assocs . foldr groupDefsI Map.empty
  where
    groupDefsI ::  (Atom, [Atom]) -> Map.Map Functor [IRClause] -> Map.Map Functor [IRClause]
    groupDefsI (hd, body) defs =
      let
        func = toFunctor hd
        clauses = defs Map.! func
      in
      if Map.member func defs
        then Map.insert func (IRClause hd body:clauses) defs
        else Map.insert func [IRClause hd body] defs


fromProgram :: Program -> IRProgram
fromProgram (Program clauses) = IRProgram defs query
  where
    defs = groupDefs [(hd, body) | (DefiniteClause hd body) <- clauses]
    query = head [IRQuery body | (GoalClause body) <- clauses]