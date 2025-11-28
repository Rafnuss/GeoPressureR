# Compute wind support and drift from a wind and ground speed vectors

Wind support, or wind profit, is the projection of the wind vector
\\\vec{v}\_w\\ onto the groundspeed vector \\\vec{v}\_g\\,
\$\$\text{Wind Support} = \frac{\vec{v}\_g \cdot
\vec{v}\_w}{\|\vec{v}\_g\|}\$\$, with \\\cdot\\ being the [cross
product](https://en.wikipedia.org/wiki/Dot_product).

A positive value indicates that wind was blowing in the direction of
movement, while a negative one indicates a head wind.

`windspeed` and `groundspeed` should be expressed as complex value where
the real part corresponds to the east-west component and imaginary part
to the north-south component.

Alternatively to wind support, you can compute the drift (or crosswind
component) value with `drift = TRUE`, \$\$\text{Drift} =
\frac{\|\vec{v}\_g \times \vec{v}\_w\|}{\|\vec{v}\_g\|}\$\$, with
\\\times\\ being the [cross
product](https://en.wikipedia.org/wiki/Cross_product).

A positive value of drift indicates that the wind is pushing the bird to
the right of its intended path, while a negative indicates that the wind
is pushing the bird to the left.

You can use [`abs()`](https://rdrr.io/r/base/MathFun.html) (or
[`Mod()`](https://rdrr.io/r/base/complex.html)) to compute the norm (or
absolute value) of the speed vector and `speed_to_bearing()` to compute
the bearing/orientation of the speed vector. The latter computes the
trigonometric angle of the speed vector with
[`Arg()`](https://rdrr.io/r/base/complex.html) and convert this angle to
a bearing (North = 0° instead of 90°).

## Usage

``` r
windsupport(windspeed, groundspeed, drift = FALSE)
```

## Arguments

- windspeed:

  windspeed as complex value

- groundspeed:

  groundspeed as complex value

- drift:

  return drift instead of windsupport

## Value

wind support as scalar value (same unit as `gs` and `ws`)
