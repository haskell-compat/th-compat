{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

-- | The 'Quote' class (first proposed in
-- <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0246-overloaded-bracket.rst GHC Proposal 246>)
-- was introduced in @template-haskell-2.17.0.0@. This module exists to
-- define a version of 'Quote' that is backward-compatible with older
-- @template-haskell@ releases and is forward-compatible with the existing
-- 'Quote' class.
--
-- In addition to 'Quote', this module also backports versions of functions in
-- "Language.Haskell.TH.Syntax" that work over any 'Quote' instance instead of
-- just 'Q'. Since this module is designed to coexist with the existing
-- definitions in @template-haskell@ as much as possible, the backported
-- functions are suffixed with @-Quote@ to avoid name clashes. For instance,
-- the backported version of 'lift' is named 'liftQuote'.
--
-- The one exception to the no-name-clashes policy is the backported 'newName'
-- method of 'Quote'. We could have conceivably named it 'newNameQuote', but
-- then it would not have been possible to define backwards-compatible 'Quote'
-- instances without the use of CPP. As a result, some care must be exercised
-- when combining this module with "Language.Haskell.TH" or
-- "Language.Haskell.TH.Syntax" on older versions of @template-haskell@, as
-- they both export a version of 'newName' with a different type. Here is an
-- example of how to safely combine these modules:
--
-- @
-- &#123;-&#35; LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell &#35;-&#125;
--
-- import Control.Monad.State (MonadState(..), State, evalState)
-- import "Language.Haskell.TH" hiding ('newName')
-- import "Language.Haskell.TH.Syntax" hiding ('newName')
-- import "Language.Haskell.TH.Syntax.Quote"
--
-- newtype PureQ a = MkPureQ (State Uniq a)
--   deriving (Functor, Applicative, Monad, MonadState Uniq)
--
-- runPureQ :: PureQ a -> a
-- runPureQ m = case m of MkPureQ m' -> evalState m' 0
--
-- instance 'Quote' PureQ where
--   'newName' s = state $ \i -> (mkNameU s i, i + 1)
--
-- main :: IO ()
-- main = putStrLn $ runPureQ $ do
--   a <- newName "a"
--   return $ nameBase a
-- @
--
-- We do not make an effort to backport any combinators from the
-- "Language.Haskell.TH.Lib" module, as the surface area is simply too large.
-- If you wish to generalize code that uses these combinators to work over
-- 'Quote' in a backwards-compatible way, use the 'unsafeQToQuote' function.
module Language.Haskell.TH.Syntax.Quote (
    -- * The @Quote@ class
    Quote(..)
    -- * Functions that use @Quote@
    -- ** The @unsafeQToQuote@ function
  , unsafeQToQuote
    -- ** Functions from @Language.Haskell.TH.Syntax@
#if MIN_VERSION_template_haskell(2,9,0)
  , unTypeQQuote
  , unsafeTExpCoerceQuote
#endif
  , liftQuote
#if MIN_VERSION_template_haskell(2,16,0)
  , liftTypedQuote
#endif
  , liftStringQuote
  ) where

import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class (MonadIO(..))
import Language.Haskell.TH (Exp)
import qualified Language.Haskell.TH.Lib as Lib ()
import Language.Haskell.TH.Syntax (Q, runQ, Quasi(..))
import qualified Language.Haskell.TH.Syntax as Syntax

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif

#if MIN_VERSION_template_haskell(2,16,0)
import GHC.Exts (RuntimeRep, TYPE)
#endif

-- TODO RGS: Use MIN_VERSION_template_haskell(2,17,0) when that's possible
#if __GLASGOW_HASKELL__ >= 811
import Language.Haskell.TH.Syntax (Quote(..), unsafeTExpCoerce, unTypeQ)
#else
import Language.Haskell.TH (Name)
#endif

-- TODO RGS: Use !(MIN_VERSION_template_haskell(2,17,0)) when that's possible
#if __GLASGOW_HASKELL__ < 811
-- | The 'Quote' class implements the minimal interface which is necessary for
-- desugaring quotations.
--
-- * The @Monad m@ superclass is needed to stitch together the different
-- AST fragments.
-- * 'newName' is used when desugaring binding structures such as lambdas
-- to generate fresh names.
--
-- Therefore the type of an untyped quotation in GHC is `Quote m => m Exp`
--
-- For many years the type of a quotation was fixed to be `Q Exp` but by
-- more precisely specifying the minimal interface it enables the `Exp` to
-- be extracted purely from the quotation without interacting with `Q`.
class ( Monad m
# if   !(MIN_VERSION_template_haskell(2,7,0))
      , Functor m
# elif !(MIN_VERSION_template_haskell(2,10,0))
      , Applicative m
# endif
      ) => Quote m where
  {- |
  Generate a fresh name, which cannot be captured.

  For example, this:

  @f = $(do
    nm1 <- newName \"x\"
    let nm2 = 'mkName' \"x\"
    return ('LamE' ['VarP' nm1] (LamE [VarP nm2] ('VarE' nm1)))
   )@

  will produce the splice

  >f = \x0 -> \x -> x0

  In particular, the occurrence @VarE nm1@ refers to the binding @VarP nm1@,
  and is not captured by the binding @VarP nm2@.

  Although names generated by @newName@ cannot /be captured/, they can
  /capture/ other names. For example, this:

  >g = $(do
  >  nm1 <- newName "x"
  >  let nm2 = mkName "x"
  >  return (LamE [VarP nm2] (LamE [VarP nm1] (VarE nm2)))
  > )

  will produce the splice

  >g = \x -> \x0 -> x0

  since the occurrence @VarE nm2@ is captured by the innermost binding
  of @x@, namely @VarP nm1@.
  -}
  newName :: String -> m Name

instance Quote Q where
  newName = qNewName
#endif

#if MIN_VERSION_template_haskell(2,9,0)
-- | Discard the type annotation and produce a plain Template Haskell
-- expression
--
-- Levity-polymorphic since /template-haskell-2.16.0.0/.
--
-- This is a variant of the 'unTypeQ' function that is always guaranteed to
-- use a 'Quote' constraint, even on old versions of @template-haskell@.
unTypeQQuote ::
# if MIN_VERSION_template_haskell(2,16,0)
  forall (r :: RuntimeRep) (a :: TYPE r) m .
# else
  forall a m .
# endif
  Quote m => m (Syntax.TExp a) -> m Exp
-- TODO RGS: Use MIN_VERSION_template_haskell(2,17,0) when that's possible
# if __GLASGOW_HASKELL__ >= 811
unTypeQQuote = unTypeQ
# else
unTypeQQuote m = do { Syntax.TExp e <- m
                    ; return e }
# endif

-- | Annotate the Template Haskell expression with a type
--
-- This is unsafe because GHC cannot check for you that the expression
-- really does have the type you claim it has.
--
-- Levity-polymorphic since /template-haskell-2.16.0.0/.
--
-- This is a variant of the 'unsafeTExpCoerce' function that is always
-- guaranteed to use a 'Quote' constraint, even on old versions of
-- @template-haskell@.
unsafeTExpCoerceQuote ::
# if MIN_VERSION_template_haskell(2,16,0)
  forall (r :: RuntimeRep) (a :: TYPE r) m .
# else
  forall a m .
# endif
  Quote m => m Exp -> m (Syntax.TExp a)
-- TODO RGS: Use MIN_VERSION_template_haskell(2,17,0) when that's possible
# if __GLASGOW_HASKELL__ >= 811
unsafeTExpCoerceQuote = unsafeTExpCoerce
# else
unsafeTExpCoerceQuote m = do { e <- m
                             ; return (Syntax.TExp e) }
# endif
#endif

-- | Turn a value into a Template Haskell expression, suitable for use in
-- a splice.
--
-- This is a variant of the 'Syntax.lift' function that is always guaranteed to
-- use a 'Quote' constraint, even on old versions of @template-haskell@.
liftQuote :: (Syntax.Lift t, Quote m) => t -> m Exp
-- TODO RGS: Use MIN_VERSION_template_haskell(2,17,0) when that's possible
#if __GLASGOW_HASKELL__ >= 811
liftQuote = Syntax.lift
#else
liftQuote = unsafeQToQuote . Syntax.lift
#endif

#if MIN_VERSION_template_haskell(2,16,0)
-- | Turn a value into a Template Haskell typed expression, suitable for use
-- in a typed splice.
--
-- /since template-haskell-2.16.0.0/
--
-- This is a variant of the 'Syntax.liftTyped' function that is always
-- guaranteed to use a 'Quote' constraint, even on old versions of
-- @template-haskell@.
liftTypedQuote :: (Syntax.Lift t, Quote m) => t -> m (Syntax.TExp t)
-- TODO RGS: Use MIN_VERSION_template_haskell(2,17,0) when that's possible
# if __GLASGOW_HASKELL__ >= 811
liftTypedQuote = Syntax.liftTyped
# else
liftTypedQuote = unsafeQToQuote . Syntax.liftTyped
# endif
#endif

-- | This is a variant of the 'Syntax.liftString' function that is always
-- guaranteed to use a 'Quote' constraint, even on old versions of
-- @template-haskell@.
liftStringQuote :: Quote m => String -> m Exp
-- TODO RGS: Use MIN_VERSION_template_haskell(2,17,0) when that's possible
#if __GLASGOW_HASKELL__ >= 811
liftStringQuote = Syntax.liftString
#else
liftStringQuote = unsafeQToQuote . Syntax.liftString
#endif

-- | Use a 'Q' computation in a 'Quote' context. This function is only safe
-- when the 'Q' computation performs actions from the 'Quote' instance for 'Q'
-- or any of `Quote`'s subclasses ('Functor', 'Applicative', and 'Monad').
-- Attempting to perform actions from the 'MonadFail', 'MonadIO', or 'Quasi'
-- instances for 'Q' will result in runtime errors.
--
-- This is useful when you have some 'Q'-valued functions that only performs
-- actions from 'Quote' and wish to generalise it from 'Q' to 'Quote' without
-- having to rewrite the internals of the function. This is especially handy
-- for code defined in terms of combinators from "Language.Haskell.TH.Lib",
-- which were all hard-coded to 'Q' prior to @template-haskell-2.17.0.0@. For
-- instance, consider this function:
--
-- @
-- apply :: 'Exp' -> 'Exp' -> 'Q' 'Exp'
-- apply f x = 'Lib.appE' (return x) (return y)
-- @
--
-- There are two ways to generalize this function to use 'Quote' in a
-- backwards-compatible way. One way to do so is to rewrite @apply@ to avoid
-- the use of 'Lib.appE', like so:
--
-- @
-- applyQuote :: 'Quote' m => 'Exp' -> 'Exp' -> m 'Exp'
-- applyQuote f x = return ('Syntax.AppE' x y)
-- @
--
-- For a small example like @applyQuote@, there isn't much work involved. But
-- this can become tiresome for larger examples. In such cases,
-- 'unsafeQToQuote' can do the heavy lifting for you. For example, @applyQuote@
-- can also be defined as:
--
-- @
-- applyQuote :: 'Quote' m => 'Exp' -> 'Exp' -> m 'Exp'
-- applyQuote f x = 'unsafeQToQuote' (apply f x)
-- @
unsafeQToQuote :: Quote m => Q a -> m a
unsafeQToQuote = unQTQ . runQ

-- | An internal definition that powers 'unsafeQToQuote'. Its 'Quasi' instance
-- defines 'qNewName' in terms of 'newName' from 'Quote', but defines every
-- other method of 'Quasi' to be an error, since they cannot be implemented
-- using 'Quote' alone. Similarly, its 'MonadFail' and 'MonadIO' instances
-- define 'fail' and 'liftIO', respectively, to be errors.
newtype QuoteToQuasi (m :: * -> *) a = QTQ { unQTQ :: m a }
  deriving (Functor, Applicative, Monad)

qtqError :: String -> a
qtqError name = error $ "`unsafeQToQuote` does not support code that uses " ++ name

instance Monad m => Fail.MonadFail (QuoteToQuasi m) where
  fail = qtqError "MonadFail.fail"

instance Monad m => MonadIO (QuoteToQuasi m) where
  liftIO = qtqError "liftIO"

instance Quote m => Quasi (QuoteToQuasi m) where
  qNewName s = QTQ (newName s)

  qRecover            = qtqError "qRecover"
  qReport             = qtqError "qReport"
  qReify              = qtqError "qReify"
  qLocation           = qtqError "qLocation"
  qRunIO              = qtqError "qRunIO"
#if MIN_VERSION_template_haskell(2,7,0)
  qReifyInstances     = qtqError "qReifyInstances"
  qLookupName         = qtqError "qLookupName"
  qAddDependentFile   = qtqError "qAddDependentFile"
# if MIN_VERSION_template_haskell(2,9,0)
  qReifyRoles         = qtqError "qReifyRoles"
  qReifyAnnotations   = qtqError "qReifyAnnotations"
  qReifyModule        = qtqError "qReifyModule"
  qAddTopDecls        = qtqError "qAddTopDecls"
  qAddModFinalizer    = qtqError "qAddModFinalizer"
  qGetQ               = qtqError "qGetQ"
  qPutQ               = qtqError "qPutQ"
# endif
# if MIN_VERSION_template_haskell(2,11,0)
  qReifyFixity        = qtqError "qReifyFixity"
  qReifyConStrictness = qtqError "qReifyConStrictness"
  qIsExtEnabled       = qtqError "qIsExtEnabled"
  qExtsEnabled        = qtqError "qExtsEnabled"
# endif
#elif MIN_VERSION_template_haskell(2,5,0)
  qClassInstances     = qtqError "qClassInstances"
#endif
#if MIN_VERSION_template_haskell(2,13,0)
  qAddCorePlugin      = qtqError "qAddCorePlugin"
#endif
#if MIN_VERSION_template_haskell(2,14,0)
  qAddForeignFilePath = qtqError "qAddForeignFilePath"
  qAddTempFile        = qtqError "qAddTempFile"
#elif MIN_VERSION_template_haskell(2,12,0)
  qAddForeignFile     = qtqError "qAddForeignFile"
#endif
#if MIN_VERSION_template_haskell(2,16,0)
  qReifyType          = qtqError "qReifyType"
#endif
