# Code obtained from https://bitbucket.org/labprentice/splash/src/master/releases/v1.0/r_version/solar.R
#
# R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
#
# solar.R
#
# VERSION: 1.0-r2
# LAST UPDATED: 2016-08-19
#
# ~~~~~~~~
# license:
# ~~~~~~~~
# Copyright (C) 2016 Prentice Lab
#
# This file is part of the SPLASH model.
#
# SPLASH is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 2.1 of the License, or
# (at your option) any later version.
#
# SPLASH is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with SPLASH.  If not, see <http://www.gnu.org/licenses/>.
#
# ~~~~~~~~~
# citation:
# ~~~~~~~~~
# T. W. Davis, I. C. Prentice, B. D. Stocker, R. J. Whitley, H. Wang, B. J.
# Evans, A. V. Gallego-Sala, M. T. Sykes, and W. Cramer, Simple process-led
# algorithms for simulating habitats (SPLASH): Robust indices of radiation
# evapo-transpiration and plant-available moisture, Geoscientific Model
# Development, 2016 (in progress)
#
# ~~~~~~~~~~~~
# description:
# ~~~~~~~~~~~~
# This script contains functions to calculate daily radiation, i.e.:
#   berger_tls(double n, double N)
#   density_h2o(double tc, double pa)
#   dcos(double d)
#   dsin(double d)
#
# ~~~~~~~~~~
# changelog:
# ~~~~~~~~~~
# - fixed Cooper's and Spencer's declination angle equations [14.11.25]
# - replaced simplified_kepler with full_kepler method [14.11.25]
# - added berger_tls function [15.01.13]
# - updated evap function (similar to stash.py EVAP class) [15.01.13]
# - updated some documentation [16.05.27]
# - fixed HN- equation (iss#13) [16.08.19]
#
#### IMPORT SOURCES ##########################################################

# R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
#
# const.R
#
# VERSION: 1.0-r1
# LAST UPDATED: 2016-05-27
#
# ~~~~~~~~
# license:
# ~~~~~~~~
# Copyright (C) 2016 Prentice Lab
#
# This file is part of the SPLASH model.
#
# SPLASH is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 2.1 of the License, or
# (at your option) any later version.
#
# SPLASH is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with SPLASH.  If not, see <http://www.gnu.org/licenses/>.
#
# ~~~~~~~~~
# citation:
# ~~~~~~~~~
# T. W. Davis, I. C. Prentice, B. D. Stocker, R. J. Whitley, H. Wang, B. J.
# Evans, A. V. Gallego-Sala, M. T. Sykes, and W. Cramer, Simple process-led
# algorithms for simulating habitats (SPLASH): Robust indices of radiation
# evapo-transpiration and plant-available moisture, Geoscientific Model
# Development, 2016 (in progress)
#
# ~~~~~~~~~~~~
# description:
# ~~~~~~~~~~~~
# This script contains the global constants defined in SPLASH.
#
# NOTE: orbital parameters: eccentricity, obliquity, and longitude of the
# perihelion, are assumed constant while they infact vary slightly over time.
# There are methods for their calculation (e.g., Meeus, 1991). Eccentricity
# varies 0.005--0.072 and is decreasing at rate of 0.00004 per century.
# Obliquity varies 22.1--24.5 degrees with a period of ~41000 years.
#
# ~~~~~~~~~~
# changelog:
# ~~~~~~~~~~
# - updated values and references for ka and kR [14.10.31]
# - reduced list of constants [15.01.13]
# - updated kR and kTo values and references [15.03.24]

# source(here::here("R/___orbpar.R")) # TODO: get this, too

kA <- 107           # constant for Rl (Monteith & Unsworth, 1990)
kalb_sw <- 0.17     # shortwave albedo (Federer, 1968)
kalb_vis <- 0.03    # visible light albedo (Sellers, 1985)
kb <- 0.20          # constant for Rl (Linacre, 1968; Kramer, 1957)
kc <- 0.25          # constant for Rs (Linacre, 1968)
kCw <- 1.05         # supply constant, mm/hr (Federer, 1982)
kd <- 0.50          # constant for Rs (Linacre, 1968)
kfFEC <- 2.04       # from-flux-to-energy, umol/J (Meek et al., 1984)
kG <- 9.80665       # gravitational acceleration, m/s^2 (Allen, 1973)
kGsc <- 1360.8      # solar constant, W/m^2 (Kopp & Lean, 2011)
kL <- 0.0065        # adiabatic lapse rate, K/m (Cavcar, 2000)
kMa <- 0.028963     # molecular weight of dry air, kg/mol (Tsilingiris, 2008)
kMv <- 0.01802      # mol. weight of water vapor, kg/mol (Tsilingiris, 2008)
kSecInDay <- 86400  # number of seconds in a day
kPo <- 101325       # standard atmosphere, Pa (Allen, 1973)
kR <- 8.31447       # universal gas constant, J/mol/K (Moldover et al., 1988)
kTo <- 288.15       # base temperature, K (Berberan-Santos et al., 1997)
kWm <- 150          # soil moisture capacity, mm (Cramer-Prentice, 1988)
kw <- 0.26          # PET entrainment, (1+kw)*EET (Priestley-Taylor, 1972)
pir <- pi/180       # pi in radians

#### DEFINE FUNCTIONS ########################################################

# ************************************************************************
# Name:     berger_tls
# Inputs:   - double, day of year (n)
#           - double, days in year (N)
# Returns:  numeric list, true anomaly and true longitude
# Features: Returns true anomaly and true longitude for a given day.
# Depends:  - ke ............. eccentricity of earth's orbit, unitless
#           - komega ......... longitude of perihelion
#  Ref:     Berger, A. L. (1978), Long term variations of daily insolation
#             and quaternary climatic changes, J. Atmos. Sci., 35, 2362-2367.
# ************************************************************************
berger_tls <- function(n, N, ke, komega) {
  # Variable substitutes:
  xee <- ke^2
  xec <- ke^3
  xse <- sqrt(1 - ke^2)
  
  # Mean longitude for vernal equinox:
  xlam <- (ke/2.0 + xec/8.0)*(1 + xse)*sin(komega*pir) -
    xee/4.0*(0.5 + xse)*sin(2.0*komega*pir) +
    xec/8.0*(1.0/3.0 + xse)*sin(3.0*komega*pir)
  xlam <- 2.0*xlam/pir
  
  # Mean longitude for day of year:
  dlamm <- xlam + (n - 80.0)*(360.0/N)
  
  # Mean anomaly:
  anm <- dlamm - komega
  ranm <- anm*pir
  
  # True anomaly (uncorrected):
  ranv <- ranm + (2.0*ke - xec/4.0)*sin(ranm) +
    5.0/4.0*xee*sin(2.0*ranm) +
    13.0/12.0*xec*sin(3.0*ranm)
  anv <- ranv/pir
  
  # True longitude:
  my_tls <- anv + komega
  if (my_tls < 0){
    my_tls <- my_tls + 360
  } else if (my_tls > 360) {
    my_tls <- my_tls - 360
  }
  
  # True anomaly:
  my_nu <- my_tls - komega
  if (my_nu < 0){
    my_nu <- my_nu + 360
  }
  return (c(my_nu, my_tls))
}


# ************************************************************************
# Name:     dcos
# Inputs:   double (d), angle in degrees
# Returns:  double, cosine of angle
# Features: This function calculates the cosine of an angle (d) given
#           in degrees.
# Depends:  pir
# Ref:      This script is based on the Javascript function written by
#           C Johnson, Theoretical Physicist, Univ of Chicago
#           - 'Equation of Time' URL: http://mb-soft.com/public3/equatime.html
#           - Javascript URL: http://mb-soft.com/believe/txx/astro22.js
# ************************************************************************
dcos <- function(d) {
  cos(d*pir)
}
dcos2 <- function(d) cospi(d/180)
# dcos(seq(0,360,20))
# cospi(seq(0,360,20)/180)
# dsin(seq(0,360,20))
# dsin2(seq(0,360,20))
dsin2 <- function(d) sinpi(d/180)

# ************************************************************************
# Name:     dsin
# Inputs:   double (d), angle in degrees
# Returns:  double, sine of angle
# Features: This function calculates the sine of an angle (d) given
#           in degrees.
# Depends:  pir
# ************************************************************************
dsin <- function(d) {
  sin(d*pir)
}


# ************************************************************************
# Name:     calc_daily_solar
# Inputs:   - double, latitude, degrees (lat)
#           - double, day of year (n)
#           - double, elevation (elv)  *optional
#           - double, year (y)         *optional
#           - double, fraction of sunshine hours (sf)        *optional
#           - double, mean daily air temperature, deg C (tc) *optional
# Returns:  list object (et.srad)
#             $nu_deg ............ true anomaly, degrees
#             $lambda_deg ........ true longitude, degrees
#             $dr ................ distance factor, unitless
#             $delta_deg ......... declination angle, degrees
#             $hs_deg ............ sunset angle, degrees
#             $ra_j.m2 ........... daily extraterrestrial radiation, J/m^2
#             $tau ............... atmospheric transmittivity, unitless
#             $ppfd_mol.m2 ....... daily photosyn. photon flux density, mol/m^2
#             $hn_deg ............ net radiation hour angle, degrees
#             $rn_j.m2 ........... daily net radiation, J/m^2
#             $rnn_j.m2 .......... daily nighttime net radiation, J/m^2
# Features: This function calculates daily radiation fluxes.
# Depends:  - kalb_sw ........ shortwave albedo
#           - kalb_vis ....... visible light albedo
#           - kb ............. empirical constant for longwave rad
#           - kc ............. empirical constant for shortwave rad
#           - kd ............. empirical constant for shortwave rad
#           - ke ............. eccentricity
#           - keps ........... obliquity
#           - kfFEC .......... from-flux-to-energy conversion, umol/J
#           - kGsc ........... solar constant
#           - berger_tls() ... calc true anomaly and longitude
#           - dcos() ......... cos(x*pi/180), where x is in degrees
#           - dsin() ......... sin(x*pi/180), where x is in degrees
#           - julian_day() ... date to julian day
# ************************************************************************
calc_daily_solar <- function(lat, n, elv=0, y=0, sf=1, # commented out since not needed for ppfd: tc=23.0, 
                             year=2000) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~ FUNCTION WARNINGS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  if (lat > 90 || lat < -90) {
    stop("Warning: Latitude outside range of validity (-90 to 90)!")
  }
  if (n < 1 || n > 366) {
    stop("Warning: Day outside range of validity (1 to 366)!")
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~ FUNCTION VARIABLES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  solar <- list()
  
  # obtain orbital parameters
  orb_out <- orbpar(year)
  
  # obliquity
  keps <- orb_out$obliq
  
  # eccentricity
  ke <- orb_out$eccen
  
  # longitude of perihelion
  komega <- orb_out$long_perihel
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 01. Calculate the number of days in yeark (kN), days
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (y == 0) {
    kN <- 365
  } else {
    kN <- (julian_day(y + 1, 1, 1) - julian_day(y, 1, 1))
  }
  solar$kN <- kN
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 02. Calculate heliocentric longitudes (nu and lambda), degrees
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  my_helio <- berger_tls(n, kN, ke, komega)
  nu <- my_helio[1]
  lam <- my_helio[2]
  solar$nu_deg <- nu
  solar$lambda_deg <- lam
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 03. Calculate distance factor (dr), unitless
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Berger et al. (1993)
  kee <- ke^2
  rho <- (1 - kee)/(1 + ke*dcos(nu))
  dr <- (1/rho)^2
  solar$rho <- rho
  solar$dr <- dr
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 04. Calculate the declination angle (delta), degrees
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Woolf (1968)
  delta <- asin(dsin(lam)*dsin(keps))
  delta <- delta/pir
  solar$delta_deg <- delta
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 05. Calculate variable substitutes (u and v), unitless
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ru <- dsin(delta)*dsin(lat)
  rv <- dcos(delta)*dcos(lat)
  solar$ru <- ru
  solar$rv <- rv
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 06. Calculate the sunset hour angle (hs), degrees
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Note: u/v equals tan(delta) * tan(lat)
  if (ru/rv >= 1.0) {
    hs <- 180  # Polar day (no sunset)
  } else if (ru/rv <= -1.0) {
    hs <- 0 # Polar night (no sunrise)
  } else {
    hs <- acos(-1.0*ru/rv)
    hs <- hs / pir
  }
  solar$hs_deg <- hs
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 07. Calculate daily extraterrestrial radiation (ra_d), J/m^2
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ref: Eq. 1.10.3, Duffy & Beckman (1993)
  ra_d <- (86400/pi)*kGsc*dr*(ru*pir*hs + rv*dsin(hs))
  solar$ra_j.m2 <- ra_d
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 08. Calculate transmittivity (tau), unitless
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ref:  Eq. 11, Linacre (1968); Eq. 2, Allen (1996)
  tau_o <- (kc + kd*sf)
  tau <- tau_o*(1 + (2.67e-5)*elv)
  
  solar$tau_o <- tau_o
  solar$tau <- tau
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 09. Calculate daily photosynthetic photon flux density (ppfd_d), mol/m^2
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ppfd_d <- (1e-6)*kfFEC*(1 - kalb_vis)*tau*ra_d
  solar$ppfd_mol.m2 <- ppfd_d
  
  # commented out since only ppfd is needed: # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # commented out since only ppfd is needed: # 10. Estimate net longwave radiation (rnl), W/m^2
  # commented out since only ppfd is needed: # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # commented out since only ppfd is needed: rnl <- (kb + (1 - kb)*sf)*(kA - tc)
  # commented out since only ppfd is needed: solar$rnl_w.m2 <- rnl
  # commented out since only ppfd is needed: 
  # commented out since only ppfd is needed: # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # commented out since only ppfd is needed: # 11. Calculate variable substitue (rw), W/m^2
  # commented out since only ppfd is needed: # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # commented out since only ppfd is needed: rw <- (1 - kalb_sw)*tau*kGsc*dr
  # commented out since only ppfd is needed: solar$rw <- rw
  # commented out since only ppfd is needed: 
  # commented out since only ppfd is needed: # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # commented out since only ppfd is needed: # 12. Calculate net radiation cross-over angle (hn), degrees
  # commented out since only ppfd is needed: # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # commented out since only ppfd is needed: if ((rnl - rw*ru)/(rw*rv) >= 1.0) {
  # commented out since only ppfd is needed:   hn <- 0  # Net radiation is negative all day
  # commented out since only ppfd is needed: } else if ((rnl - rw*ru)/(rw*rv) <= -1.0) {
  # commented out since only ppfd is needed:   hn <- 180 # Net radiation is positive all day
  # commented out since only ppfd is needed: } else {
  # commented out since only ppfd is needed:   hn <- acos((rnl - rw*ru)/(rw*rv))
  # commented out since only ppfd is needed:   hn <- hn/pir
  # commented out since only ppfd is needed: }
  # commented out since only ppfd is needed: solar$hn_deg <- hn
  # commented out since only ppfd is needed: 
  # commented out since only ppfd is needed: # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # commented out since only ppfd is needed: # 13. Calculate daytime net radiation (rn_d), J/m^2
  # commented out since only ppfd is needed: # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # commented out since only ppfd is needed: rn_d <- (86400/pi)*(hn*pir*(rw*ru - rnl) + rw*rv*dsin(hn))
  # commented out since only ppfd is needed: solar$rn_j.m2 <- rn_d
  # commented out since only ppfd is needed: 
  # commented out since only ppfd is needed: # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # commented out since only ppfd is needed: # 14. Calculate nighttime net radiation (rnn_d), J/m^2
  # commented out since only ppfd is needed: # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # commented out since only ppfd is needed: # fixed iss#13
  # commented out since only ppfd is needed: rnn_d <- (86400/pi)*(
  # commented out since only ppfd is needed:   rw*rv*(dsin(hs) - dsin(hn)) +
  # commented out since only ppfd is needed:     rw*ru*(hs - hn)*pir -
  # commented out since only ppfd is needed:     rnl*(pi - hn*pir)
  # commented out since only ppfd is needed: )
  # commented out since only ppfd is needed: solar$rnn_j.m2 <- rnn_d
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~ RETURN VALUES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  return(solar)
}


# ************************************************************************
# Name:     julian_day
# Inputs:   - double, year (y)
#           - double, month (m)
#           - double, day of month (i)
# Returns:  double, Julian day
# Features: This function converts a date in the Gregorian calendar
#           to a Julian day number (i.e., a method of consecutative
#           numbering of days---does not have anything to do with
#           the Julian calendar!)
#           * valid for dates after -4712 January 1 (i.e., jde >= 0)
# Ref:      Eq. 7.1 J. Meeus (1991), Chapter 7 "Julian Day", Astronomical
#             Algorithms
# ************************************************************************
julian_day <- function(y, m, i) {
  if (m <= 2) {
    y <- y - 1
    m <- m + 12
  }
  a <- floor(y/100)
  b <- 2 - a + floor(a/4)
  
  jde <- floor(365.25*(y + 4716)) + floor(30.6001*(m + 1)) + i + b - 1524.5
  return(jde)
}

























# 
# 
# # use it: 
# daily_solar <- c()
# daily_ppfd_molm2 <- c()
# for (doy in 1:366) {
#   daily_solar[[doy]] <- calc_daily_solar(
#     lat = 46,    # double, latitude, degrees
#     n   = doy,   # double, day of year
#     elv = 0,     # double, elevation
#     y   = 0,     # double, year
#     sf  = 1,     # double, fraction of sunshine hours
#     tc  = 23.0,  # double, mean daily air temperature, deg C
#     year= 2000  
#   )
#   daily_ppfd_molm2[doy] <- daily_solar[[doy]]$ppfd_mol.m2
# }
# # Returns:  list object (et.srad)
# daily_solar[[1]]$nu_deg      # true anomaly, degrees
# daily_solar[[1]]$lambda_deg  # true longitude, degrees
# daily_solar[[1]]$dr          # distance factor, unitless
# daily_solar[[1]]$delta_deg   # declination angle, degrees
# daily_solar[[1]]$hs_deg      # sunset angle, degrees
# daily_solar[[1]]$ra_j.m2     # daily extraterrestrial radiation, J/m^2
# daily_solar[[1]]$tau         # atmospheric transmittivity, unitless
# daily_solar[[1]]$ppfd_mol.m2 # daily photosyn. photon flux density, mol/m^2
# daily_solar[[1]]$hn_deg      # net radiation hour angle, degrees
# daily_solar[[1]]$rn_j.m2     # daily net radiation, J/m^2
# daily_solar[[1]]$rnn_j.m2    # daily nighttime net radiation, J/m^2
# 
# plot(daily_ppfd_molm2)
# 
# # TODO: create a data.frame with ppdfp_mol_m2
# 
# 
# # TODO: separately create a data.frame with patm_
# #        ppfd_orbital
# #        patm_elevation
# 
# 
# 
# 
# 
