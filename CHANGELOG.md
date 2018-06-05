# 1.3

* Give preference to Semigroup definitions over Monoid

# 1.2

* Replace the `Foreign` export with `Foreign.Storable`, `Foreign.Ptr`, `Foreign.ForeignPtr`, `Foreign.StablePtr`. It's more conservative and way less likely to cause name collisions.

# 1.1

* Export `Foreign`

# 1.0.1

* Relaxed the "base" dependency

# 1

No changes.

# 0.2

* Reexported `Data.Bifunctor`.

* `first` and `second` are now (conditionally) exported from `Data.Bifunctor`, not `Control.Arrow`; note that if your version of base is lower than 4.8, `first` and `second` won't be available at all.

# 0.1.21

* Reexported `printf` and `hPrintf` from `Text.Printf`.

# 0.1.20

* Reexported `Numeric`.

# 0.1.19

* Avoided the clash between `(&)` and `sortOn` defined in the package and versions of these functions imported from base.

# 0.1.18

* Added implementations of `(&)` and `sortOn` (normally not available in older versions of base).

# 0.1.17

* Reexported `Control.Monad.Fix`.
