# Compute probability of a bird speed

Converts a speed (airspeed or ground speed) to a probability based on
the movement model.

## Usage

``` r
speed2prob(speed, movement)
```

## Arguments

- speed:

  airspeed or ground speed in km/h

- movement:

  a list of the movement model parameter defined with
  `graph_set_movement`

## Value

Probability values corresponding to the speed provided.

## See also

Other movement:
[`bird_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/bird_create.md),
[`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_set_movement.md),
[`graph_transition()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_transition.md),
[`plot_graph_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_graph_movement.md),
[`tag_download_wind()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_download_wind.md)

## Examples

``` r
speed <- seq(1, 120)
low_speed_fix <- 20 # minimum speed allowed
prob <- speed2prob(
  speed,
  list(
    method = "gamma",
    shape = 7,
    scale = 7,
    low_speed_fix = low_speed_fix
  )
)
plot(speed, prob,
  type = "l",
  xlab = "Groundspeed [km/h]",
  ylab = "Probability"
)
abline(v = low_speed_fix)


# Using airspeed
bird <- bird_create("Acrocephalus arundinaceus")
prob <- speed2prob(
  speed,
  list(
    method = "power",
    bird = bird,
    power2prob = \(power) (1 / power)^3,
    low_speed_fix = low_speed_fix
  )
)
plot(speed, prob, type = "l", xlab = "Airspeed [km/h]", ylab = "Probability")
abline(v = low_speed_fix)
```
