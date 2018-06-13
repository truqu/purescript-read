module Data.String.Read
  -- * Classes
  ( class Read, read
  , class Zero, zero

  -- * Utility
  , readDefault
  ) where

import Prelude ((>>>), pure)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (charAt)


-- | Represent types that can be read from strings (enum-like ADT)
class Read a where
  read
    :: String
    -> Maybe a


-- | Represent types that have a zero value
class Zero a where
  zero :: a


-- | Read a value `a` from a `String` but fallback on `Zero a` on failure
readDefault
  :: forall a. Read a => Zero a
  => String
  -> a
readDefault =
  read >>> fromMaybe zero


instance readString :: Read String where
  read =
    pure


instance readChar :: Read Char where
  read =
    charAt 0


instance readBoolean :: Read Boolean where
  read s =
    case s of
      "true"  -> pure true
      "false" -> pure false
      _       -> Nothing


instance zeroString :: Zero String where
  zero =
    ""


instance zeroBoolean :: Zero Boolean where
  zero =
    false
