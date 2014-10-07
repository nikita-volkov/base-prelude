-- |
-- This module reexports most of the definitions from the \"base\" package,
-- which are meant to be imported unqualified.
-- 
-- For details check out the source.
module BasePrelude
( 
  module Exports,
  -- * Reimplementations of functions presented in versions of \"base\" newer than 4.5
  -- ** Data.Bool
  bool,
  -- ** Debug.Trace
  traceShowId,
  traceM,
  traceShowM,
)
where

-- Reexports
-------------------------

import Control.Applicative as Exports
import Control.Arrow as Exports
import Control.Category as Exports
import Control.Concurrent as Exports
import Control.Exception as Exports
import Control.Monad as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.ST as Exports
import Data.Bits as Exports
import Data.Bool as Exports hiding (bool)
import Data.Char as Exports 
import Data.Complex as Exports 
import Data.Data as Exports
import Data.Either as Exports
import Data.Fixed as Exports
import Data.Foldable as Exports
import Data.Function as Exports hiding ((.), id)
import Data.Int as Exports
import Data.IORef as Exports
import Data.Ix as Exports
import Data.List as Exports hiding (concat, foldr, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, find, maximumBy, minimumBy, mapAccumL, mapAccumR, foldl')
import Data.Maybe as Exports
import Data.Monoid as Exports
import Data.Ord as Exports (Down(..))
import Data.Ratio as Exports
import Data.STRef as Exports
import Data.String as Exports
import Data.Traversable as Exports
import Data.Tuple as Exports
import Data.Unique as Exports
import Data.Word as Exports
import Debug.Trace as Exports hiding (traceShowId, traceM, traceShowM)
import GHC.Conc as Exports hiding (withMVar, threadWaitWriteSTM, threadWaitWrite, threadWaitReadSTM, threadWaitRead)
import GHC.Exts as Exports (lazy, inline)
import GHC.Generics as Exports (Generic)
import GHC.IO.Exception as Exports
import Prelude as Exports hiding (concat, foldr, mapM_, sequence_, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, mapM, sequence, FilePath, id, (.))
import System.Environment as Exports
import System.Exit as Exports
import System.IO as Exports (Handle, hClose)
import System.IO.Error as Exports
import System.IO.Unsafe as Exports
import System.Mem as Exports
import System.Mem.StableName as Exports
import System.Timeout as Exports
import Text.Read as Exports (readMaybe, readEither)
import Unsafe.Coerce as Exports

-- Reimplementations
-------------------------

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
>   traceMShow $ x
>   y <- ...
>   traceMShow $ x + y
-}
traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM = traceM . show
