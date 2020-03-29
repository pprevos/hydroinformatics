# RESERVOIRS
library(tidyverse)
library(RColorBrewer)
library(gridExtra)

# Read data
if (!file.exists("Reservoirs/prettyboy.csv")) {
    url <- "http://www.mgs.md.gov/ReservoirDataPoints/PrettyBoy1998.dat"
    prettyboy <- read.csv(url, skip = 2, header = FALSE)
    names(prettyboy) <- read.csv(url, nrows = 1, header = FALSE, stringsAsFactors = FALSE)
    write_csv(prettyboy, "Hydroinformatics/Reservoirs/prettyboy.csv")
} else prettyboy <- read_csv("Reservoirs/prettyboy.csv")
head(prettyboy)

# Remove extremes, duplicates and Anomaly
ext <- c(which(prettyboy$Easting == min(prettyboy$Easting)), 
         which(prettyboy$Easting == max(prettyboy$Easting)),
         which(duplicated(prettyboy)))
prettyboy <- prettyboy[-ext, ]

# Visualise reservoir
bathymetry_colours <- c(rev(brewer.pal(3, "Greens"))[-2:-3], brewer.pal(9, "Blues")[-1:-3])
ggplot(prettyboy, aes(x = Easting, y = Northing, colour = Depth)) + 
    geom_point(size = .1) + coord_equal() + 
    scale_colour_gradientn(colors = bathymetry_colours) 
ggsave("Reservoirs/prettyboy_bathymetry.png")

# CUSTOMERS
library(psych)

# Read data
customers <- read_csv("Customers/customer_survey.csv")

# Exploratory Analyis
p1 <- customers[,1:10] %>% 
    gather %>% 
    ggplot(aes(x = key, y = value)) + 
        geom_boxplot() + 
        xlab("Item") + ylab("Response") + ggtitle("Personal Involvement Index")

# Factor analysis
fap <- fa.parallel(customers[,1:10])
grid.arrange(p1, ncol= 2)
customers[,1:10] %>%
    fa(nfactors = fap$nfact, rotate = "promax") %>% 
    fa.diagram(main = "Factor Analysis")
