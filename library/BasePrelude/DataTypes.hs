-- |
-- A module that reexports only the data types
-- defined across various modules of the \"base\" package.
--
-- By data types we mean that it is the ones we use
-- to define data structures.
-- It is not abstraction integration wrappers,
-- like 'Data.Semigroup.First'.
-- It is not resource types like 'System.IO.Handle'.
module BasePrelude.DataTypes
  ( -- * From "Prelude"
    Prelude.Bool (..),
    Prelude.Char,
    Prelude.Double,
    Prelude.Either (..),
    Prelude.Float,
    Prelude.Integer,
    Prelude.Maybe (..),
    Prelude.String,

    -- * From "Data.Int"
    Data.Int.Int,
    Data.Int.Int8,
    Data.Int.Int16,
    Data.Int.Int32,
    Data.Int.Int64,

    -- * From "Data.Word"
    Data.Word.Word,
    Data.Word.Word8,
    Data.Word.Word16,
    Data.Word.Word32,
    Data.Word.Word64,

    -- * From "Data.Complex"
    Data.Complex.Complex (..),

    -- * From "Data.Ratio"
    Data.Ratio.Rational,

    -- * From "Numeric.Natural"
    Numeric.Natural.Natural,

    -- * From "Data.List.NonEmpty"
    Data.List.NonEmpty.NonEmpty (..),
  )
where

import qualified Data.Complex
import qualified Data.Int
import qualified Data.List.NonEmpty
import qualified Data.Ratio
import qualified Data.Word
import qualified Numeric.Natural
import qualified Prelude
