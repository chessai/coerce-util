module Data.Coerce.Util
  (
    -- * Function coercion utils
    (#.)
  , (.#)

    -- * Shorten coercion type signatures
  , Binary
  , CoerceBinary
  , WrapBinary
  , WrappedBinary
    
    -- * Newtype utils 
  , showsNewtype
  , readsNewtype
  ) where

import Data.Coerce

import Text.Read
import Text.Read.Lex

-- | A definition for 'Data.Functor.Classes.liftShowsPrec',
-- suitable for newtypes.
--
-- Given a newtype declared as:
--
-- @newtype T a = T { unT :: a }@
--
-- The 'Data.Functor.Classes.Show1' definition can be given as:
--
-- @
-- instance Show1 T where
--   liftShowsPrec = showsNewtype "T" "unT"
-- @
showsNewtype
  :: Coercible b a
  => String
  -> String
  -> (Int -> a -> ShowS)
  -> ([a] -> ShowS)
  -> Int
  -> b
  -> ShowS
showsNewtype cons acc = s
  where
    s sp _ n x =
        showParen (n > 10)
      $ showString cons
      . showString " {"
      . showString acc
      . showString " = "
      . sp 0 (coerce x)
      . showChar '}'
{-# INLINE showsNewtype #-}

-- | A definition for 'Data.Functor.Classes.liftReadsPrec',
-- suitable for newtypes.
--
-- Given a newtype declared as:
--
-- @
-- newtype T a = T { unT :: a }
-- @
--
-- The 'Data.Functor.Classes.Read1' definition can be given as:
--
-- @
-- instance Read1 T where
--   liftReadsPrec = readsNewtype "T" "unT"
-- @
readsNewtype
  :: Coercible a b
  => String
  -> String
  -> (Int -> ReadS a)
  -> ReadS [a]
  -> Int
  -> ReadS b
readsNewtype cons acc = r
  where
    r rp _ = readPrec_to_S $ parens $ prec 10 $ do
      lift $ expect (Ident cons)
      Punc "{" <- lexP
      lift $ expect (Ident acc)
      Punc "=" <- lexP
      x <- reset (readS_to_Prec rp)
      Punc "}" <- lexP
      pure (coerce x)
{-# INLINE readsNewtype #-}

type Binary a = a -> a -> a
type CoerceBinary a b = Binary a -> Binary b
type WrapBinary f a = Binary a -> WrappedBinary f a
type WrappedBinary f a = Binary (f a)

-- | Coercive left-composition.
infixr 9 #.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce
{-# INLINE (#.) #-}

-- | Coercive right-composition.
infixr 9 .#
(.#) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f
{-# INLINE (.#) #-}

