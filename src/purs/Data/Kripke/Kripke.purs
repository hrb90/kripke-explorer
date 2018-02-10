module Data.Kripke.Kripke 
  ( Atom(..)
  , Node
  , Worlds
  , Relation
  , AccessiblePair(..)
  , KripkeFrame
  , Valuation
  , Domain
  , Fact(..)
  , Model
  , Evaluation(..)
  , checkModel
  , runEvaluation
  , nodeDomain
  , testAccess
  , testFact) where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Generic (class Generic)

newtype Atom = Atom String

derive newtype instance eqAtom :: Eq Atom
derive newtype instance genAtom :: Generic Atom

newtype Node = Node String

derive newtype instance eqNode :: Eq Node

type Worlds = Array Node

newtype AccessiblePair = Accessible { to :: Node, from :: Node }

instance eqAccess :: Eq AccessiblePair where
  eq (Accessible u) (Accessible v) = u.to == v.to && u.from == v.from

type Relation = Array AccessiblePair

type KripkeFrame = { worlds :: Worlds, relation :: Relation }

newtype Fact = Fact { world :: Node, atom :: Atom }

instance eqFact :: Eq Fact where
  eq (Fact f) (Fact g) = f.world == g.world && f.atom == g.atom

type Valuation = Array Fact

type Domain = Array Fact

type Model = { frame :: KripkeFrame, valuation :: Valuation, domain :: Domain }

data Evaluation err expr = Evaluation (Model -> Either err (Node -> expr -> Boolean))

nodeDomain :: Domain -> Node -> Array Atom
nodeDomain d w = map (\(Fact x) -> x.atom) $ filter (\(Fact x) -> x.world == w) d

-- Specializes to testing that an atom is part of the valuation of a node
-- Or part of the domain of a node
-- Or to testing that the second argument is accessible from the first

testAccess :: Relation -> Node -> Node -> Boolean
testAccess rel to from = elem (Accessible { to, from }) rel

testFact :: Array Fact -> Atom -> Node -> Boolean
testFact facts atom world = elem (Fact { world, atom }) facts

-- Checks that a model satisfies the laws of a given Evaluation's logic.
checkModel :: forall err a. Evaluation err a -> Model -> Either err Unit
checkModel (Evaluation f) m = void $ f m

-- Runs the evaluation function.
runEvaluation :: forall err expr. Evaluation err expr -> Model -> Node -> expr -> Either err Boolean
runEvaluation (Evaluation f) model world expr = case (f model) of
  Left err -> Left err
  Right eval -> Right (eval world expr)