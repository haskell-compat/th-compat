{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE RoleAnnotations #-}
#endif

-- | This module exists to make it possible to define code that works across
-- a wide range of @template-haskell@ versions with as little CPP as possible.
-- To that end, this module currently backports the following
-- @template-haskell@ constructs:
--
-- * The 'Quote' class
--
-- * The 'Code' type
--
-- Refer to the Haddocks below for examples of how to use each of these in a
-- backwards-compatible way.
module Language.Haskell.TH.Syntax.Compat (
    -- * The @Quote@ class
    -- $quote
    Quote(..)
    -- * @Quote@ functionality
    -- ** The @unsafeQToQuote@ function
  , unsafeQToQuote
    -- ** Functions from @Language.Haskell.TH.Syntax@
#if MIN_VERSION_template_haskell(2,9,0)
  , unTypeQQuote
  , unsafeTExpCoerceQuote
#endif
  , liftQuote
#if MIN_VERSION_template_haskell(2,9,0)
  , liftTypedQuote
#endif
  , liftStringQuote

#if MIN_VERSION_template_haskell(2,9,0)
    -- * The @Code@ and @CodeQ@ types
    -- $code
  , Code(..), CodeQ
    -- * @Code@ functionality
    -- ** The @IsCode@ class
  , IsCode(..)
    -- ** Limitations of @IsCode@
    -- $limitations
    -- ** Functions from @Language.Haskell.TH.Syntax@
  , unsafeCodeCoerce
  , liftCode
  , unTypeCode
  , hoistCode
  , bindCode
  , bindCode_
  , joinCode

  -- * @Splice@
  , Splice
  , SpliceQ
  , liftTypedFromUntypedSplice
#endif
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

#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Lib (CodeQ)
import Language.Haskell.TH.Syntax
  ( Code(..), Quote(..)
  , bindCode, bindCode_, hoistCode, joinCode, liftCode, unsafeCodeCoerce, unTypeCode
  , unsafeTExpCoerce, unTypeQ )
#else
import Language.Haskell.TH (Name)
#endif

-------------------------------------------------------------------------------
-- Quote
-------------------------------------------------------------------------------

-- $quote
-- The 'Quote' class (first proposed in
-- <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0246-overloaded-bracket.rst GHC Proposal 246>)
-- was introduced in @template-haskell-2.17.0.0@. This module defines a version
-- of 'Quote' that is backward-compatible with older @template-haskell@
-- releases and is forward-compatible with the existing 'Quote' class.
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
-- import "Language.Haskell.TH.Syntax.Compat"
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

#if !(MIN_VERSION_template_haskell(2,17,0))
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
--
-- As this function interacts with typed Template Haskell, this function is
-- only defined on @template-haskell-2.9.0.0@ (GHC 7.8) or later.
unTypeQQuote ::
# if MIN_VERSION_template_haskell(2,16,0)
  forall (r :: RuntimeRep) (a :: TYPE r) m .
# else
  forall a m .
# endif
  Quote m => m (Syntax.TExp a) -> m Exp
# if MIN_VERSION_template_haskell(2,17,0)
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
--
-- As this function interacts with typed Template Haskell, this function is
-- only defined on @template-haskell-2.9.0.0@ (GHC 7.8) or later.
unsafeTExpCoerceQuote ::
# if MIN_VERSION_template_haskell(2,16,0)
  forall (r :: RuntimeRep) (a :: TYPE r) m .
# else
  forall a m .
# endif
  Quote m => m Exp -> m (Syntax.TExp a)
# if MIN_VERSION_template_haskell(2,17,0)
unsafeTExpCoerceQuote = unsafeTExpCoerce
# else
unsafeTExpCoerceQuote m = do { e <- m
                             ; return (Syntax.TExp e) }
# endif
#endif

-- | Turn a value into a Template Haskell expression, suitable for use in
-- a splice.
--
-- This is a variant of the 'Syntax.lift' method of 'Syntax.Lift' that is
-- always guaranteed to use a 'Quote' constraint, even on old versions of
-- @template-haskell@.
--
-- Levity-polymorphic since /template-haskell-2.17.0.0/.
liftQuote ::
#if MIN_VERSION_template_haskell(2,17,0)
  forall (r :: RuntimeRep) (t :: TYPE r) m .
#else
  forall t m .
#endif
  (Syntax.Lift t, Quote m) => t -> m Exp
#if MIN_VERSION_template_haskell(2,17,0)
liftQuote = Syntax.lift
#else
liftQuote = unsafeQToQuote . Syntax.lift
#endif

#if MIN_VERSION_template_haskell(2,9,0)
-- | Turn a value into a Template Haskell typed expression, suitable for use
-- in a typed splice.
--
-- This is a variant of the 'Syntax.liftTyped' method of 'Syntax.Lift' that is
-- always guaranteed to use a 'Quote' constraint and return a 'Code', even on
-- old versions of @template-haskell@.
--
-- As this function interacts with typed Template Haskell, this function is
-- only defined on @template-haskell-2.9.0.0@ (GHC 7.8) or later. While the
-- 'Syntax.liftTyped' method of 'Syntax.Lift' was first introduced in
-- @template-haskell-2.16.0.0@, we are able to backport it back to
-- @template-haskell-2.9.0.0@ by making use of the 'Syntax.lift' method on
-- older versions of @template-haskell@. This crucially relies on the
-- 'Syntax.Lift' law that @'lift' x ≡ 'unTypeQ' ('liftTyped' x)@ to work,
-- so beware if you use 'liftTypedQuote' with an unlawful 'Syntax.Lift'
-- instance.
--
-- Levity-polymorphic since /template-haskell-2.17.0.0/.
liftTypedQuote ::
# if MIN_VERSION_template_haskell(2,17,0)
  forall (r :: RuntimeRep) (t :: TYPE r) m .
# else
  forall t m .
# endif
  (Syntax.Lift t, Quote m) => t -> Code m t
# if MIN_VERSION_template_haskell(2,17,0)
liftTypedQuote = Syntax.liftTyped
# elif MIN_VERSION_template_haskell(2,16,0)
liftTypedQuote = liftCode . unsafeQToQuote . Syntax.liftTyped
# else
liftTypedQuote = unsafeCodeCoerce . liftQuote
# endif
#endif

-- | This is a variant of the 'Syntax.liftString' function that is always
-- guaranteed to use a 'Quote' constraint, even on old versions of
-- @template-haskell@.
liftStringQuote :: Quote m => String -> m Exp
#if MIN_VERSION_template_haskell(2,17,0)
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

-------------------------------------------------------------------------------
-- Code
-------------------------------------------------------------------------------

-- $code
-- The 'Code' type (first proposed in
-- <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0195-code-texp.rst GHC Proposal 195>)
-- was introduced in @template-haskell-2.17.0.0@. This module defines a version
-- of 'Code' that is backward-compatible with older @template-haskell@
-- releases and is forward-compatible with the existing 'Code' class.
-- In addition to 'Code', this module also backports the functions in
-- "Language.Haskell.TH.Syntax" that manipulate 'Code' values.
--
-- One troublesome aspect of writing backwards-compatible code involving 'Code'
-- is that GHC 9.0 changed the types of typed Template Haskell splices. Before,
-- they were of type @'Q' ('TExp' a)@, but they are now of type @'Code' 'Q' a@.
-- The 'IsCode' class can be used to paper over the difference between these
-- two types. For more details, consult the Haddocks for 'IsCode'.
--
-- Because 'Code' interacts with typed Template Haskell, the 'Code' type and
-- any function that mentions 'Code' in its type are only defined on
-- @template-haskell-2.9.0.0@ (GHC 7.8) or later.

#if MIN_VERSION_template_haskell(2,9,0)
-- | A class that allows one to smooth over the differences between
-- @'Code' 'm' a@ (the type of typed Template Haskell quotations on
-- @template-haskell-2.17.0.0@ or later) and @'m' ('TExp' a)@ (the type of
-- typed Template Haskell quotations on older versions of @template-haskell@).
-- Here are two examples that demonstrate how to use each method of 'IsCode':
--
-- @
-- &#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
--
-- import "Language.Haskell.TH"
-- import "Language.Haskell.TH.Syntax.Compat"
--
-- -- 'toCode' will ensure that the end result is a 'Code', regardless of
-- -- whether the quote itself returns a 'Code' or a 'TExp'.
-- myCode :: 'Code' 'Q' Int
-- myCode = 'toCode' [|| 42 ||]
--
-- -- 'fromCode' will ensure that the input 'Code' is suitable for splicing
-- (i.e., it will return a 'Code' or a 'TExp' depending on the
-- @template-haskell@ version in use).
-- fortyTwo :: Int
-- fortyTwo = $$('fromCode' myCode)
-- @
--
-- Levity-polymorphic since /template-haskell-2.16.0.0/.
class IsCode q
# if MIN_VERSION_template_haskell(2,16,0)
             (a :: TYPE r)
# else
             a
# endif
             c | c -> a q where
  -- | Convert something to a 'Code'.
  toCode   :: c -> Code q a
  -- | Convert to something from a 'Code'.
  fromCode :: Code q a -> c

-- | Levity-polymorphic since /template-haskell-2.16.0.0/.
instance Quote q => IsCode q
# if MIN_VERSION_template_haskell(2,16,0)
                           (a :: TYPE r)
# else
                           a
# endif
                           (Code q a) where
  toCode   = id
  fromCode = id

-- | Levity-polymorphic since /template-haskell-2.16.0.0/.
instance texp ~ Syntax.TExp a => IsCode Q
# if MIN_VERSION_template_haskell(2,16,0)
                                        (a :: TYPE r)
# else
                                        a
# endif
                                        (Q texp) where
  toCode   = liftCode
  fromCode = examineCode

-- $limitations
-- 'IsCode' makes it possible to backport code involving typed Template Haskell
-- quotations and splices where the types are monomorphized to 'Q'. GHC 9.0
-- and later, however, make it possible to use typed TH quotations and splices
-- that are polymorphic over any 'Quote' instance. Unfortunately, the
-- @th-compat@ library does not yet have a good story for backporting
-- 'Quote'-polymorphic quotations or splices. For example, consider this code:
--
-- @
-- instance ('Syntax.Lift' a, 'Quote' q, 'Num' a) => 'Num' ('Code' q a) where
--   -- ...
--   x + y = [|| $$x + $$y ||]
--   -- ...
-- @
--
-- How might we backport this code? If we were in a setting where @q@ were
-- monomorphized to 'Q', we could simply write this:
--
-- @
--   x + y = 'toCode' [|| $$('fromCode' x) + $$('fromCode' y) ||]
-- @
--
-- In a 'Quote'-polymorphic setting, however, we run into issues. While this
-- will compile on GHC 9.0 or later, it will not compile on earlier GHC
-- versions because all typed TH quotations and splices must use 'Q'. At
-- present, the @th-compat@ library does not offer any solution to this
-- problem.

-- | Levity-polymorphic since /template-haskell-2.16.0.0/.
# if !(MIN_VERSION_template_haskell(2,17,0))
type role Code representational nominal
newtype Code m
#  if MIN_VERSION_template_haskell(2,16,0)
             (a :: TYPE (r :: RuntimeRep))
#  else
             a
#  endif
  = Code
  { examineCode :: m (Syntax.TExp a) -- ^ Underlying monadic value
  }

type CodeQ = Code Q
# if MIN_VERSION_template_haskell(2,16,0)
                    :: (TYPE r -> *)
# endif

-- | Unsafely convert an untyped code representation into a typed code
-- representation.
--
-- Levity-polymorphic since /template-haskell-2.16.0.0/.
unsafeCodeCoerce ::
#  if MIN_VERSION_template_haskell(2,16,0)
  forall (r :: RuntimeRep) (a :: TYPE r) m .
#  else
  forall a m .
#  endif
  Quote m => m Exp -> Code m a
unsafeCodeCoerce m = Code (unsafeTExpCoerceQuote m)

-- | Lift a monadic action producing code into the typed 'Code'
-- representation
--
-- Levity-polymorphic since /template-haskell-2.16.0.0/.
liftCode ::
#  if MIN_VERSION_template_haskell(2,16,0)
  forall (r :: RuntimeRep) (a :: TYPE r) m .
#  else
  forall a m .
#  endif
  m (Syntax.TExp a) -> Code m a
liftCode = Code

-- | Extract the untyped representation from the typed representation
--
-- Levity-polymorphic since /template-haskell-2.16.0.0/.
unTypeCode ::
#  if MIN_VERSION_template_haskell(2,16,0)
  forall (r :: RuntimeRep) (a :: TYPE r) m .
#  else
  forall a m .
#  endif
  Quote m => Code m a -> m Exp
unTypeCode = unTypeQQuote . examineCode

-- | Modify the ambient monad used during code generation. For example, you
-- can use `hoistCode` to handle a state effect:
-- @
--  handleState :: Code (StateT Int Q) a -> Code Q a
--  handleState = hoistCode (flip runState 0)
-- @
--
-- Levity-polymorphic since /template-haskell-2.16.0.0/.
hoistCode ::
#  if MIN_VERSION_template_haskell(2,16,0)
  forall m n (r :: RuntimeRep) (a :: TYPE r) .
#  else
  forall m n a .
#  endif
  Monad m => (forall x . m x -> n x) -> Code m a -> Code n a
hoistCode f (Code a) = Code (f a)


-- | Variant of (>>=) which allows effectful computations to be injected
-- into code generation.
--
-- Levity-polymorphic since /template-haskell-2.16.0.0/.
bindCode ::
#  if MIN_VERSION_template_haskell(2,16,0)
  forall m a (r :: RuntimeRep) (b :: TYPE r) .
#  else
  forall m a b .
#  endif
  Monad m => m a -> (a -> Code m b) -> Code m b
bindCode q k = liftCode (q >>= examineCode . k)

-- | Variant of (>>) which allows effectful computations to be injected
-- into code generation.
--
-- Levity-polymorphic since /template-haskell-2.16.0.0/.
bindCode_ ::
#  if MIN_VERSION_template_haskell(2,16,0)
  forall m a (r :: RuntimeRep) (b :: TYPE r) .
#  else
  forall m a b .
#  endif
  Monad m => m a -> Code m b -> Code m b
bindCode_ q c = liftCode ( q >> examineCode c)

-- | A useful combinator for embedding monadic actions into 'Code'
-- @
-- myCode :: ... => Code m a
-- myCode = joinCode $ do
--   x <- someSideEffect
--   return (makeCodeWith x)
-- @
--
-- Levity-polymorphic since /template-haskell-2.16.0.0/.
joinCode ::
#  if MIN_VERSION_template_haskell(2,16,0)
  forall m (r :: RuntimeRep) (a :: TYPE r) .
#  else
  forall m a .
#  endif
  Monad m => m (Code m a) -> Code m a
joinCode = flip bindCode id
# endif

-- | @'Splice' m a@ is a type alias for:
--
-- * @'Code' m a@, if using @template-haskell-2.17.0.0@ or later, or
--
-- * @m ('Syntax.TExp' a)@, if using an older version of @template-haskell@.
--
-- This should be used with caution, as its definition differs depending on
-- which version of @template-haskell@ you are using. It is mostly useful for
-- contexts in which one is writing a definition that is intended to be used
-- directly in a typed Template Haskell splice, as the types of TH splices
-- differ between @template-haskell@ versions as well. One example of a type
-- that uses 'Splice' is the type signature for 'lifTypedFromUntypedSplice'.
--
-- Levity-polymorphic since /template-haskell-2.16.0.0/.
# if MIN_VERSION_template_haskell(2,17,0)
type Splice  = Code :: (forall r. (* -> *) -> TYPE r -> *)
# elif MIN_VERSION_template_haskell(2,16,0)
type Splice m (a :: TYPE r) = m (Syntax.TExp a)
# else
type Splice m a = m (Syntax.TExp a)
# endif

-- | @'SpliceQ' a@ is a type alias for:
--
-- * @'Code' 'Q' a@, if using @template-haskell-2.17.0.0@ or later, or
--
-- * @'Q' ('Syntax.TExp' a)@, if using an older version of @template-haskell@.
--
-- This should be used with caution, as its definition differs depending on
-- which version of @template-haskell@ you are using. It is mostly useful for
-- contexts in which one is writing a definition that is intended to be used
-- directly in a typed Template Haskell splice, as the types of TH splices
-- differ between @template-haskell@ versions as well.
--
-- Levity-polymorphic since /template-haskell-2.16.0.0/.
# if MIN_VERSION_template_haskell(2,17,0)
type SpliceQ = Splice Q :: (TYPE r -> *)
# elif MIN_VERSION_template_haskell(2,16,0)
type SpliceQ (a :: TYPE r) = Splice Q a
# else
type SpliceQ a = Splice Q a
# endif

-- | A variant of 'liftTypedQuote' that is:
--
-- 1. Always implemented in terms of 'Syntax.lift' behind the scenes, and
--
-- 2. Returns a 'Splice'. This means that the return type of this function will
--    be different depending on which version of @template-haskell@ you are
--    using. (See the Haddocks for 'Splice' for more information on this
--    point.)
--
-- This is primarily useful for minimizing CPP in one particular scenario:
-- implementing 'Syntax.liftTyped' in hand-written 'Syntax.Lift' instances
-- where the corresponding 'Syntax.lift' implementation cannot be derived. For
-- instance, consider this example from the @text@ library:
--
-- @
-- instance 'Syntax.Lift' Text where
--   'Syntax.lift' = appE (varE 'pack) . stringE . unpack
-- #if MIN_VERSION_template_haskell(2,17,0)
--   'Syntax.liftTyped' = 'unsafeCodeCoerce' . 'Syntax.lift'
-- #elif MIN_VERSION_template_haskell(2,16,0)
--   'Syntax.liftTyped' = 'Syntax.unsafeTExpCoerce' . 'Syntax.lift'
-- #endif
-- @
--
-- The precise details of how this 'Syntax.lift' implementation works are not
-- important, only that it is something that @DeriveLift@ could not generate.
-- The main point of this example is to illustrate how tiresome it is to write
-- the CPP necessary to define 'Syntax.liftTyped' in a way that works across
-- multiple versions of @template-haskell@. With 'liftTypedFromUntypedSplice',
-- however, this becomes slightly easier to manage:
--
-- @
-- instance 'Syntax.Lift' Text where
--   'Syntax.lift' = appE (varE 'pack) . stringE . unpack
-- #if MIN_VERSION_template_haskell(2,16,0)
--   'Syntax.liftTyped' = 'liftTypedFromUntypedSplice'
-- #endif
-- @
liftTypedFromUntypedSplice :: (Syntax.Lift t, Quote m) => t -> Splice m t
# if MIN_VERSION_template_haskell(2,17,0)
liftTypedFromUntypedSplice = unsafeCodeCoerce . liftQuote
# else
liftTypedFromUntypedSplice = unsafeTExpCoerceQuote . liftQuote
# endif
#endif
