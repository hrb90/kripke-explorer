module Logics.Intuitionistic.Validation (validate) where

import Prelude

import Data.Either (Either)
import Data.Kripke.Kripke (Model)
import Data.Kripke.Validation (Validation, toEither, validateDomain, validateMonotonic, validateReflexive, validateTransitive)

validate' :: Model -> Validation
validate' m@{ frame } = validateDomain m
                          *> validateMonotonic m
                          *> validateReflexive frame
                          *> validateTransitive frame

validate :: Model -> Either (Array String) Unit
validate = toEither <<< validate'