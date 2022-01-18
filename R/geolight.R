
## Solar Zenith/Sunrise/Sunset calculations
##
## The functions presented here are based on code and the excel
## spreadsheet from the NOAA site
##
##       http://www.esrl.noaa.gov/gmd/grad/solcalc/
##


##' Calculate solar time, the equation of time and solar declination
##'
##' The solar time, the equation of time and the sine and cosine of
##' the solar declination are calculated for the times specified by
##' \code{tm} using the same methods as
##' \url{www.esrl.noaa.gov/gmd/grad/solcalc/}.
##' @title Solar Time and Declination
##' @param tm a vector of POSIXct times.
##' @return A list containing the following vectors.
##' \item{\code{solarTime}}{the solar time (degrees)}
##' \item{\code{eqnTime}}{the equation of time (minutes of time)}
##' \item{\code{sinSolarDec}}{sine of the solar declination}
##' \item{\code{cosSolarDec}}{cosine of the solar declination}
##' @seealso \code{\link{zenith}}
##' @examples
##' ## Current solar time
##' solar(Sys.time())
##' @export
solar <- function(tm) {

  rad <- pi/180

  ## Time as Julian day (R form)
  Jd <- as.numeric(tm)/86400.0+2440587.5

  ## Time as Julian century [G]
  Jc <- (Jd-2451545)/36525

  ## The geometric mean sun longitude (degrees) [I]
  L0 <- (280.46646+Jc*(36000.76983+0.0003032*Jc))%%360


  ## Geometric mean anomaly for the sun (degrees) [J]
  M <- 357.52911+Jc*(35999.05029-0.0001537*Jc)

  ## The eccentricity of earth's orbit [K]
  e <- 0.016708634-Jc*(0.000042037+0.0000001267*Jc)

  ## Equation of centre for the sun (degrees) [L]
  eqctr <- sin(rad*M)*(1.914602-Jc*(0.004817+0.000014*Jc))+
    sin(rad*2*M)*(0.019993-0.000101*Jc)+
    sin(rad*3*M)*0.000289

  ## The true longitude of the sun (degrees) [M]
  lambda0 <- L0 + eqctr

  ## The apparent longitude of the sun (degrees) [P]
  omega <- 125.04-1934.136*Jc
  lambda <- lambda0-0.00569-0.00478*sin(rad*omega)


  ## The mean obliquity of the ecliptic (degrees) [Q]
  seconds <- 21.448-Jc*(46.815+Jc*(0.00059-Jc*(0.001813)))
  obliq0 <- 23+(26+(seconds/60))/60

  ## The corrected obliquity of the ecliptic (degrees) [R]
  omega <- 125.04-1934.136*Jc
  obliq <- obliq0 + 0.00256*cos(rad*omega)

  ## The equation of time (minutes of time) [U,V]
  y <- tan(rad*obliq/2)^2
  eqnTime <- 4/rad*(y*sin(rad*2*L0) -
                      2*e*sin(rad*M) +
                      4*e*y*sin(rad*M)*cos(rad*2*L0) -
                      0.5*y^2*sin(rad*4*L0) -
                      1.25*e^2*sin(rad*2*M))

  ## The sun's declination (radians) [T]
  solarDec <- asin(sin(rad*obliq)*sin(rad*lambda))
  sinSolarDec <- sin(solarDec)
  cosSolarDec <- cos(solarDec)

  ## Solar time unadjusted for longitude (degrees) [AB!!]
  ## Am missing a mod 360 here, but is only used within cosine.
  solarTime <- ((Jd-0.5)%%1*1440+eqnTime)/4
  #solarTime <- ((Jd-2440587.5)*1440+eqnTime)/4

  ## Return solar constants
  list(solarTime=solarTime,
       eqnTime=eqnTime,
       sinSolarDec=sinSolarDec,
       cosSolarDec=cosSolarDec)
}


##' Calculate the solar zenith angle for given times and locations
##'
##' \code{zenith} uses the solar time and declination calculated by
##' \code{solar} to compute the solar zenith angle for given times and
##' locations, using the same methods as
##' \url{www.esrl.noaa.gov/gmd/grad/solcalc/}.  This function does not
##' adjust for atmospheric refraction see \code{\link{refracted}}.
##' @title Solar Zenith Angle
##' @param sun list of solar time and declination computed by
##' \code{solar}.
##' @param lon vector of longitudes.
##' @param lat vector latitudes.
##' @return A vector of solar zenith angles (degrees) for the given
##' locations and times.
##' @seealso \code{\link{solar}}
##' @examples
##' ## Approx location of Sydney Harbour Bridge
##' lon <- 151.211
##' lat <- -33.852
##' ## Solar zenith angle for noon on the first of May 2000
##' ## at the Sydney Harbour Bridge
##' s <- solar(as.POSIXct("2000-05-01 12:00:00","EST"))
##' zenith(s,lon,lat)
##' @export
zenith <- function(sun,lon,lat) {

  rad <- pi/180

  ## Suns hour angle (degrees) [AC!!]
  hourAngle <- sun$solarTime+lon-180
  #hourAngle <- sun$solarTime%%360+lon-180

  ## Cosine of sun's zenith [AD]
  cosZenith <- (sin(rad*lat)*sun$sinSolarDec+
                  cos(rad*lat)*sun$cosSolarDec*cos(rad*hourAngle))

  ## Limit to [-1,1] [!!]
  cosZenith[cosZenith > 1] <- 1
  cosZenith[cosZenith < -1] <- -1

  ## Ignore refraction correction
  acos(cosZenith)/rad
}



##' Adjust the solar zenith angle for atmospheric refraction.
##'
##' Given a vector of solar zeniths computed by \code{\link{zenith}},
##' \code{refracted} calculates the solar zeniths adjusted for the
##' effect of atmospheric refraction.
##'
##' \code{unrefracted} is the inverse of \code{refracted}. Given a
##' (single) solar zenith adjusted for the effect of atmospheric
##' refraction, \code{unrefracted} calculates the solar zenith as
##' computed by \code{\link{zenith}}.
##'
##' @title Atmospheric Refraction
##' @param zenith zenith angle (degrees) to adjust.
##' @return vector of zenith angles (degrees) adjusted for atmospheric
##' refraction.
##' @examples
##' ## Refraction causes the sun to appears higher on the horizon
##' refracted(85:92)
##' @export
refracted <- function(zenith) {
  rad <- pi/180
  elev <- 90-zenith
  te <- tan((rad)*elev)
  ## Atmospheric Refraction [AF]
  r <- ifelse(elev>85,0,
              ifelse(elev>5,58.1/te-0.07/te^3+0.000086/te^5,
                     ifelse(elev>-0.575,
                            1735+elev*(-518.2+elev*(103.4+elev*(-12.79+elev*0.711))),-20.772/te)))
  ## Corrected Zenith [90-AG]
  zenith-r/3600
}




##' Search for pairs of twilights spanning night.
##'
##' Search for sunset, sunrise pairs that correspond to a given light
##' threshold.
##'
##' Given a set of times (\code{include}) known to fall in the night,
##' \code{findTwilights} determines the twilights that span these times, and
##' computes the corresponding midnights. It then searches for periods
##' of darkness that lie approximately 24 hours from these midnights,
##' repeating the process until no new twilight pairs are found.
##'
##' If \code{interleave=TRUE}, the sunrise and sunset times are
##' interleaved andreturned as a single sequence of twilights,
##' otherwise sunset and sunrise times are returned separately. The
##' function \code{interleave.twilights} takes a dataframe of separate
##' sunset and sunrise times and interleaves them to form a sequence
##' of twilight times.
##'
##' @title Search for twilight times
##' @param tagdata a dataframe with columns \code{Date} and
##' \code{Light} that are the sequence of sample times (as POSIXct)
##' and light levels recorded by the tag.
##' @param threshold the light threshold that defines twilight.
##' @param include a vector of times as POSIXct. Nights that span these
##' times are included in the search.
##' @param exclude a vector of POSIXct times. Nights that span these
##' times are excluded from the search.
##' @param extend a time in minutes. The function seeks periods of
##' darkness that differ from one another by 24 hours plus or minus
##' this interval.
##' @param dark.min a time in minutes. Periods of darkness shorter
##' than this interval will be excluded.
##' @return A dataframe with columns
##' \item{\code{Twilight}}{times of twilight}
##' \item{\code{Rise}}{logical indicating sunrise}
##' where each row corresponds to a single twilight.
##' @export
findTwilights <- function(tagdata,threshold,include,
                          exclude=NULL,extend=0,dark.min=0) {
  ## Extract date and light data
  date <- tagdata$Date
  light <- tagdata$Light


  ## Is any x in each [a,b]
  containsAny <- function(a,b,x) {
    f <- logical(length(a))
    for(k in seq_along(x))
      f <- f | (a <= x[k] & b >= x[k])
    f
  }

  ## Convert to minutes
  extend <- 60*extend
  dark.min <- 60*dark.min

  ## Calculate intervals [a,b] of darkness
  ## a is first points before light drops below threshold
  ## b is first points before light rises to or above threshold
  l <- (light>=threshold)
  f <- diff(l)
  a <- which(f==-1)
  b <- which(f==1)
  ## Keep only fall-rise pairs
  if(b[1] < a[1]) b <- b[-1]
  a <- a[1:length(b)]

  ## Only keep intervals that do not include excluded data and are
  ## less than 24 hours in length
  keep <- (!containsAny(date[a],date[b+1],exclude) &
             (as.numeric(date[b+1])-as.numeric(date[a]) < 86400) &
             (as.numeric(date[b+1])-as.numeric(date[a]) > dark.min))

  a <- a[keep]
  b <- b[keep]

  ## Compute bounding dates and midpoint
  aDate <- date[a]
  bDate <- date[b+1]
  mDate <- aDate+(bDate-aDate)/2

  ## Iteratively expand set of twilights by searching for additional
  ## twilights +/- 24 hrs from midpoints of existing set.
  keep <- logical(length(a))
  add <- containsAny(aDate,bDate,include) & !keep
  while(any(add)) {
    keep <- keep | add
    mid <- c(mDate[add]-86400,mDate[add]+86400)
    add <- containsAny(aDate-extend,bDate+extend,mid) & !keep
  }
  a <- a[keep]
  b <- b[keep]

  ## Interpolate times to get exact twilights.
  ss <- date[a]+(threshold-light[a])/(light[a+1]-light[a])*(date[a+1]-date[a])
  sr <- date[b]+(threshold-light[b])/(light[b+1]-light[b])*(date[b+1]-date[b])

  data.frame(Twilight=.POSIXct(as.vector(t(cbind(ss,sr))),"GMT"),
             Rise=rep(c(F,T),length(ss)))
}
