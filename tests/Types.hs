{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Types (Foo(..)) where

import Language.Haskell.TH.Syntax hiding (newName)
#if MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Syntax.Compat
#endif

data Foo = MkFoo deriving (Eq, Show)

-- An example of how to use liftTypedFromUntypedSplice to minimize the amount
-- of CPP one has to use when manually defining `liftTyped` in `Lift` instance.
-- This example is contrived, since you could just as well derive this
-- particular `Lift` instance, but the same template will carry over to `Lift`
-- instances that cannot be derived.
instance Lift Foo where
  lift MkFoo = [| MkFoo |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedFromUntypedSplice
#endif
