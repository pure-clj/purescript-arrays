-- | Helper functions for working with mutable arrays using the `ST` effect.
-- |
-- | This module can be used when performance is important and mutation is a local effect.

module Data.Array.ST
  ( STArray(..)
  , Assoc
  , withArray
  , empty
  , peek
  , poke
  , push
  , modify
  , pushAll
  , splice
  , sort
  , sortBy
  , sortWith
  , freeze
  , thaw
  , unsafeFreeze
  , unsafeThaw
  , toAssocArray
  ) where

import Prelude

import Control.Monad.ST (ST, kind Region)
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)

-- | A reference to a mutable array.
-- |
-- | The first type parameter represents the memory region which the array belongs to.
-- | The second type parameter defines the type of elements of the mutable array.
-- |
-- | The runtime representation of a value of type `STArray h a` is not the same as that of `Array a`.
-- | `STArray` is a `java.util.ArrayList` while `Array` is a Clojure persistent vector.
foreign import data STArray :: Region -> Type -> Type

-- | An element and its index.
type Assoc a = { value :: a, index :: Int }

-- | Perform an effect requiring a mutable array on a copy of an immutable array,
-- | safely returning the result as an immutable array.
withArray
  :: forall h a b
   . (STArray h a -> ST h b)
   -> Array a
   -> ST h (Array a)
withArray f xs = do
  result <- thaw xs
  _ <- f result
  unsafeFreeze result

-- | Cannot use unsafeCoerce in Clojure as `Array`s and `STArray`s
-- | are different classes
unsafeFreeze :: forall h a. STArray h a -> ST h (Array a)
unsafeFreeze = copyBackImpl

-- | Cannot use unsafeCoerce in Clojure as `Array`s and `STArray`s
-- | are different classes
unsafeThaw :: forall h a. Array a -> ST h (STArray h a)
unsafeThaw = copyImpl

-- | Create an empty mutable array.
foreign import empty :: forall h a. ST h (STArray h a)

-- | Create a mutable copy of an immutable array.
thaw :: forall h a. Array a -> ST h (STArray h a)
thaw = copyImpl

-- | Sort a mutable array in place.
sort :: forall a h. Ord a => STArray h a -> ST h (STArray h a)
sort = sortBy compare

-- | Sort a mutable array in place using a comparison function.
sortBy
  :: forall a h
   . (a -> a -> Ordering)
  -> STArray h a
  -> ST h (STArray h a)
sortBy comp = sortByImpl comp'
  where
  comp' x y = case comp x y of
    GT -> 1
    EQ -> 0
    LT -> -1

foreign import sortByImpl
  :: forall a h
   . (a -> a -> Int)
  -> STArray h a
  -> ST h (STArray h a)

-- | Sort a mutable array in place based on a projection.
sortWith
  :: forall a b h
   . Ord b
  => (a -> b)
  -> STArray h a
  -> ST h (STArray h a)
sortWith f = sortBy (comparing f)

-- | Create an immutable copy of a mutable array.
freeze :: forall h a. STArray h a -> ST h (Array a)
freeze = copyBackImpl

foreign import copyImpl :: forall h a b. a -> ST h b

foreign import copyBackImpl :: forall h a b. a -> ST h b

-- | Read the value at the specified index in a mutable array.
peek
  :: forall h a
   . Int
  -> STArray h a
  -> ST h (Maybe a)
peek = peekImpl Just Nothing

foreign import peekImpl
  :: forall h a r
   . (a -> r)
  -> r
  -> Int
  -> STArray h a
  -> (ST h r)

-- | Change the value at the specified index in a mutable array.
foreign import poke :: forall h a. Int -> a -> STArray h a -> ST h Boolean

-- | Append an element to the end of a mutable array. Returns the new length of
-- | the array.
push :: forall h a. a -> STArray h a -> ST h Int
push a = pushAll [a]

-- | Append the values in an immutable array to the end of a mutable array.
-- | Returns the new length of the mutable array.
foreign import pushAll
  :: forall h a
   . Array a
  -> STArray h a
  -> ST h Int

-- | Mutate the element at the specified index using the supplied function.
modify :: forall h a. Int -> (a -> a) -> STArray h a -> ST h Boolean
modify i f xs = do
  entry <- peek i xs
  case entry of
    Just x  -> poke i (f x) xs
    Nothing -> pure false

-- | Remove and/or insert elements from/into a mutable array at the specified index.
foreign import splice
  :: forall h a
   . Int
  -> Int
  -> Array a
  -> STArray h a
  -> ST h (Array a)

-- | Create an immutable copy of a mutable array, where each element
-- | is labelled with its index in the original array.
foreign import toAssocArray :: forall h a. STArray h a -> ST h (Array (Assoc a))
