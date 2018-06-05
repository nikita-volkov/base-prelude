{-# LANGUAGE CPP #-}
-- |
-- This module reexports most of the definitions from the \"base\" package,
-- which are meant to be imported unqualified.
--
-- For details check out the source.
module BasePrelude
(
  module Exports,
  -- * Reimplementations of functions presented in versions of \"base\" newer than 4.6
  -- ** Data.Bool
  bool,
  -- ** Data.Function
  (&),
  -- ** Data.Functor
  ($>),
  -- ** Data.List
  isSubsequenceOf,
  sortOn,
  uncons,
  -- ** Debug.Trace
  traceShowId,
  traceM,
  traceShowM,
)
where

-- Reexports
-------------------------

import Control.Applicative as Exports
import Control.Arrow as Exports hiding (first, second)
import Control.Category as Exports
import Control.Concurrent as Exports
import Control.Exception as Exports
import Control.Monad as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.Fix as Exports hiding (fix)
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail as Exports (MonadFail)
#endif
#if MIN_VERSION_base(4,9,0)
import Control.Monad.IO.Class as Exports
#endif
import Control.Monad.ST as Exports
#if MIN_VERSION_base(4,8,0)
import Data.Bifunctor as Exports
#endif
import Data.Bits as Exports
import Data.Bool as Exports
import Data.Char as Exports
#if MIN_VERSION_base(4,7,0)
import Data.Coerce as Exports
#endif
import Data.Complex as Exports
import Data.Data as Exports
import Data.Dynamic as Exports
import Data.Either as Exports
import Data.Fixed as Exports
import Data.Foldable as Exports
import Data.Functor as Exports
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes as Exports
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Compose as Exports
#endif
#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity as Exports
#endif
import Data.Function as Exports hiding ((.), id)
import Data.Int as Exports
import Data.IORef as Exports
import Data.Ix as Exports
import Data.List as Exports hiding (concat, foldr, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, find, maximumBy, minimumBy, mapAccumL, mapAccumR, foldl')
#if MIN_VERSION_base(4,9,0)
import Data.List.NonEmpty as Exports (NonEmpty(..))
#endif
import Data.Maybe as Exports
#if MIN_VERSION_base(4,9,0)
import Data.Monoid as Exports hiding ((<>), First(..), Last(..))
#else
import Data.Monoid as Exports
#endif
import Data.Ord as Exports
#if MIN_VERSION_base(4,7,0)
import Data.Proxy as Exports
#endif
import Data.Ratio as Exports
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup as Exports
#endif
import Data.STRef as Exports
import Data.String as Exports
import Data.Traversable as Exports
import Data.Tuple as Exports
import Data.Unique as Exports
import Data.Version as Exports
#if MIN_VERSION_base(4,8,0)
import Data.Void as Exports
#endif
import Data.Word as Exports
import Debug.Trace as Exports
import Foreign.Storable as Exports
import Foreign.Ptr as Exports
import Foreign.ForeignPtr as Exports
import Foreign.StablePtr as Exports
import GHC.Conc as Exports hiding (withMVar, threadWaitWriteSTM, threadWaitWrite, threadWaitReadSTM, threadWaitRead)
import GHC.Exts as Exports (lazy, inline, sortWith, groupWith)
import GHC.Generics as Exports (Generic)
import GHC.IO.Exception as Exports
import Numeric as Exports
#if MIN_VERSION_base(4,8,0)
import Numeric.Natural as Exports
#endif
import Prelude as Exports hiding (concat, foldr, mapM_, sequence_, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, mapM, sequence, id, (.))
import System.Environment as Exports
import System.Exit as Exports
import System.IO as Exports (Handle, hClose)
import System.IO.Error as Exports
import System.IO.Unsafe as Exports
import System.Mem as Exports
import System.Mem.StableName as Exports
import System.Timeout as Exports
import Text.ParserCombinators.ReadP as Exports (ReadP, ReadS, readP_to_S, readS_to_P)
import Text.ParserCombinators.ReadPrec as Exports (ReadPrec, readPrec_to_P, readP_to_Prec, readPrec_to_S, readS_to_Prec)
import Text.Printf as Exports (printf, hPrintf)
import Text.Read as Exports (Read(..), readMaybe, readEither)
import Unsafe.Coerce as Exports


---------------------------------
-- Reimplementations for base-4.7
---------------------------------

#if !MIN_VERSION_base(4,7,0)

-- | Case analysis for the 'Bool' type.
-- @bool a b p@ evaluates to @a@ when @p@ is @False@, and evaluates to @b@
-- when @p@ is @True@.
bool :: a -> a -> Bool -> a
bool f t b = if b then t else f

{-|
Like 'traceShow' but returns the shown value instead of a third value.
-}
traceShowId :: (Show a) => a -> a
traceShowId a = trace (show a) a

{-|
Like 'trace' but returning unit in an arbitrary monad. Allows for convenient
use in do-notation. Note that the application of 'trace' is not an action in the
monad, as 'traceIO' is in the 'IO' monad.

> ... = do
>   x <- ...
>   traceM $ "x: " ++ show x
>   y <- ...
>   traceM $ "y: " ++ show y
-}
traceM :: (Monad m) => String -> m ()
traceM string = trace string $ return ()

{-|
Like 'traceM', but uses 'show' on the argument to convert it to a 'String'.

> ... = do
>   x <- ...
>   traceShowM $ x
>   y <- ...
>   traceShowM $ x + y
-}
traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM = traceM . show

infixl 4 $>

-- | Flipped version of '<$'.
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

#endif

---------------------------------
-- Reimplementations for base-4.8
---------------------------------

#if !MIN_VERSION_base(4,8,0)

infixl 1 &

-- | '&' is a reverse application operator.  This provides notational
-- convenience.  Its precedence is one higher than that of the forward
-- application operator '$', which allows '&' to be nested in '$'.
(&) :: a -> (a -> b) -> b
x & f = f x

-- | The 'isSubsequenceOf' function takes two lists and returns 'True' if the
-- first list is a subsequence of the second list.
--
-- @'isSubsequenceOf' x y@ is equivalent to @'elem' x ('subsequences' y)@.
--
-- ==== __Examples__
--
-- >>> isSubsequenceOf "GHC" "The Glorious Haskell Compiler"
-- True
-- >>> isSubsequenceOf ['a','d'..'z'] ['a'..'z']
-- True
-- >>> isSubsequenceOf [1..10] [10,9..0]
-- False
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []    _                    = True
isSubsequenceOf _     []                   = False
isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
                               | otherwise = isSubsequenceOf a b

-- | Decompose a list into its head and tail. If the list is empty,
-- returns 'Nothing'. If the list is non-empty, returns @'Just' (x, xs)@,
-- where @x@ is the head of the list and @xs@ its tail.
uncons        :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)

-- | Sort a list by comparing the results of a key function applied to each
-- element.  @sortOn f@ is equivalent to @sortBy . comparing f@, but has the
-- performance advantage of only evaluating @f@ once for each element in the
-- input list.  This is called the decorate-sort-undecorate paradigm, or
-- Schwartzian transform.
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

#endif
