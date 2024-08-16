# Magus - an R7RS impl for Magicflute

## Numbers and exactness

Scheme has an exact/inexact concept where inexactness in infectious.
We add something on top of that which is in the case that there is a user definition that
is converted to inexact, the syntax is not allowed to explicitly mark itself as exact, so that
#e3.5 is *exactly* 3.5 (or $\frac{35}{10}$), not the floating-point 3.5 for any reason, but 3.5 allows
the most expedient implementation.
