module Data.Kripke.Transformations (reflexiveClosure, transitiveClosure) where
  
import Prelude

import Data.Array (filter)
import Data.Foldable (class Foldable, elem)
import Data.Kripke.Kripke (KripkeFrame, AccessiblePair(..), testAccess)
import Data.Kripke.Validation (isTransitive)

notIn :: forall f a. Foldable f => Eq a => f a -> a -> Boolean
notIn xs = not <<< flip elem xs

reflexiveClosure :: KripkeFrame -> KripkeFrame
reflexiveClosure { worlds, relation } = { worlds, relation: relation <> extra }
  where extra = filter (notIn relation) (map (\w -> Accessible { to: w, from : w }) worlds)

transitiveClosure :: KripkeFrame -> KripkeFrame
transitiveClosure frame@{ worlds, relation }
  | isTransitive frame = frame
  | otherwise = transitiveClosure $ { worlds, relation: relation <> extra }
      where extra = filter (notIn relation) $ do
              (Accessible { to, from }) <- relation
              newTo <- filter (testAccess relation to) worlds
              pure $ Accessible { from, to: newTo }