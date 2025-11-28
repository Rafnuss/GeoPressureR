# Compute the bearing of a speed vector

This function convert a speed vector represented in complex number into
a bearing angle (0°=N ; 90°=E...). This conversion is needed because
trigonometric angle are different than bearing.

## Usage

``` r
speed2bearing(
  speed,
  speed_ref = complex(real = 0, imaginary = 1),
  positive = TRUE
)
```

## Arguments

- speed:

  speed as complex value

- speed_ref:

  reference vector of the angle. Default is the North in order to return
  bearing. Use `speed_ref = 1` for trigonometric orientation (clockwise
  from East).

- positive:

  logical to ensure the bearing is positive between 0-360 degree.

## Value

bearing angle in degree
