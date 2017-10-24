inunits <- function (units, n, sourceunits = "mm") {
  multiplier <- NA
  if (sourceunits == "mm") {
    if (units == "inches") {
      multiplier <- 0.0393701
    } else if (units == "cm") {
      multiplier <- 0.1
    } else if (units == "mm") {
      multiplier <- 1
    }
  }
  n*multiplier
}

papersize <- function (paper, units="inches") {
  width <- NA
  height <- NA
  if (paper == "a4" || paper== "a4paper") {
    width <- inunits(units=units, 210)
    height <- inunits(units=units, 297)
  } else if (paper == "a3") {
    width <- inunits(units=units, 297)
    height <- inunits(units=units, 420)
  }else if (paper == "a2") {
    width <- inunits(units=units, 420)
    height <- inunits(units=units, 594)
  }else if (paper == "a1") {
    width <- inunits(units=units, 594)
    height <- inunits(units=units, 841)
  }else if (paper == "a0") {
    width <- inunits(units=units, 841)
    height <- inunits(units=units, 1189)
  }
  list(width=width, height=height)
}
