module Data.Kripke.Validation 
  ( Errors
  , Validation
  , toEither
  , isTransitive
  , validateReflexive
  , validateTransitive
  , validateMonotonic
  , validateDomain
  , validateTotal
  , validateEuclidean ) where
  
import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (and)
import Data.Kripke.Kripke (KripkeFrame, Model, AccessiblePair(..), Fact(..), testAccess, testFact)
import Data.Validation.Semigroup (V, invalid, unV)

type Errors = Array String

type Validation = V Errors Unit

vMap :: String -> Boolean -> Validation
vMap err test
  | test = pure unit
  | otherwise = invalid [err]


toEither :: forall a b. V a b -> Either a b
toEither = unV Left Right

-- Checks that a Kripke frame is reflexive, i.e., every world is accessible from itself
validateReflexive :: KripkeFrame -> Validation
validateReflexive { worlds, relation } = vMap "The accessibility relation is not reflexive" isReflexive
  where reflexInRelation = \n -> testAccess relation n n
        isReflexive = and $ map reflexInRelation worlds

isTransitive :: KripkeFrame -> Boolean
isTransitive { worlds, relation } = and $ do
        (Accessible { to, from }) <- relation
        tutu <- filter (testAccess relation to) worlds
        pure $ testAccess relation from tutu

-- Checks that a Kripke frame is transitive, i.e., if v is accessible from u, 
-- and w is accessible from v, then w is accessible from u
validateTransitive :: KripkeFrame -> Validation
validateTransitive = vMap "The accessibility relation is not transitive" <<< isTransitive

-- Checks that a Kripke frame is Euclidean, i.e., if u and v are both accessible from w,
-- then they are accessible from each other.
validateEuclidean :: KripkeFrame -> Validation
validateEuclidean { worlds, relation } = vMap "The accessibility relation is not Euclidean" isEuclidean
  where isEuclidean = and $ do
          w <- worlds
          u <- filter (testAccess relation w) worlds
          v <- filter (testAccess relation w) worlds
          pure $ testAccess relation u v

-- Checks that a Kripke frame is total, i.e., for every pair of worlds w, v,
-- one of the worlds is accessible from the other.
validateTotal :: KripkeFrame -> Validation
validateTotal { worlds, relation } = vMap "The accessibility relation is not total" isTotal
  where isTotal = and $ do
          w <- worlds
          v <- worlds
          pure $ (testAccess relation w v || testAccess relation v w)

-- Checks that the domain is monotonic, i.e., if a variable x is in the domain of w, 
-- and u is accessible from w, x is in the domain of u.
validateMonotonicD :: Model -> Validation
validateMonotonicD { frame: { worlds, relation }, domain } = vMap "The domain is not monotonic" isMonotonic
  where isMonotonic = and $ do
          (Fact { atom, world }) <- domain
          newWorld <- filter (testAccess relation world) worlds
          pure $ testFact domain atom newWorld

-- Same as above, but for the valuation.
validateMonotonicV :: Model -> Validation
validateMonotonicV { frame: { worlds, relation }, valuation } = vMap "The valuation is not monotonic" isMonotonic
  where isMonotonic = and $ do
          (Fact { atom, world }) <- valuation
          newWorld <- filter (testAccess relation world) worlds
          pure $ testFact valuation atom newWorld

validateMonotonic :: Model -> Validation
validateMonotonic m = validateMonotonicD m *> validateMonotonicV m

-- Checks that the valuation for each world is a subset of the domain
validateDomain :: Model -> Validation
validateDomain { valuation, domain } = vMap "There are valuations for variables not in the domain of the corresponding world" domainMakesSense
  where domainMakesSense = and $ map (\(Fact { atom, world }) -> testFact domain atom world) valuation