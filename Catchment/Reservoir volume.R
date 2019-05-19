library(tidyverse)
library(lattice)

prettyboy <- read_csv("Hydroinformatics/prettyboy.csv")

library(akima)
coords <- with(prettyboy, interp(x = Easting, y = Northing, z = Depth, duplicate = "strip"))

wireframe(-coords$z, shade = TRUE)





