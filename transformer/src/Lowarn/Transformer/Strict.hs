{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Lowarn.Transformer.Strict
  ( StrictTransformable (..),
    forceTransformer,
  )
where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad
import Lowarn (Transformer (..))
import Lowarn.Transformer (Transformable (..))

class NFData b => StrictTransformable a b where
  {-# MINIMAL transformer' | transform' #-}

  -- | Gives a transformer that attempts to transform data from type @a@ to type
  -- @b@.
  transformer' :: Transformer a b
  transformer' = Transformer $! transform'

  -- | Attempt to transform data from type @a@ to type @b@, giving @Nothing@ if
  -- this is not possible.
  transform' :: a -> IO (Maybe b)
  transform' = unTransformer $! transformer'

instance (Transformable a b, NFData b) => StrictTransformable a b where
  transformer' :: Transformer a b
  transformer' =
    Transformer $
      transform
        >=> maybe
          (return Nothing)
          ((unTransformer $! forceTransformer) $!)

forceTransformer :: (NFData a) => Transformer a a
forceTransformer = Transformer $ evaluate . force . Just
