-- |
-- A collection of common operators provided across
-- various modules of the \"base\" package.
module BasePrelude.Operators
  ( -- * From "Control.Applicative"
    (Control.Applicative.*>),
    (Control.Applicative.<*),
    (Control.Applicative.<*>),
    (Control.Applicative.<**>),
    (Control.Applicative.<|>),

    -- * From "Control.Monad"
    (Control.Monad.<=<),
    (Control.Monad.=<<),
    (Control.Monad.>=>),
    (Control.Monad.>>),
    (Control.Monad.>>=),

    -- * From "Data.Bits"
    (Data.Bits..&.),
    (Data.Bits..|.),

    -- * From "Data.Bool"
    (Data.Bool.&&),
    (Data.Bool.||),
    (Data.Eq./=),
    (Data.Eq.==),

    -- * From "Data.Function"
    (Data.Function.$),
    (Data.Function.&),
    (Data.Function..),

    -- * From "Data.Functor"
    (Data.Functor.$>),
    (Data.Functor.<$),
    (Data.Functor.<$>),
    (Data.Functor.<&>),

    -- * From "Data.Functor.Contravariant"
    (Data.Functor.Contravariant.>$),
    (Data.Functor.Contravariant.>$<),
    (Data.Functor.Contravariant.>$$<),
    (Data.Functor.Contravariant.$<),

    -- * From "Data.Ord"
    (Data.Ord.<),
    (Data.Ord.<=),
    (Data.Ord.>),
    (Data.Ord.>=),

    -- * From "Data.Ratio"
    (Data.Ratio.%),

    -- * From "Data.Semigroup"
    (Data.Semigroup.<>),

    -- * From "Prelude"
    (Prelude.$!),
    (Prelude.*),
    (Prelude.+),
    (Prelude.-),
    (Prelude./),
    (Prelude.^),
    (Prelude.^^),
  )
where

import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Bits
import qualified Data.Bool
import qualified Data.Eq
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Functor.Contravariant
import qualified Data.Ord
import qualified Data.Ratio
import qualified Data.Semigroup
import qualified Prelude
