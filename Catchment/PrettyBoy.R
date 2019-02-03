# Pretty Boy
# http://www.mgs.md.gov/publications/data_pages/reservoir_bathymetry.html

library(tidyverse)
library(RColorBrewer)

if (!file.exists("Catchment/PrettyBoy.csv")) {
  pb_data <- readLines("http://www.mgs.md.gov/ReservoirDataPoints/PrettyBoy1998.dat") %>%
    strsplit(",")
  prettyboy <- do.call(rbind, pb_data[-1:-2]) %>%
    as_data_frame()
  names(prettyboy) <- pb_data[[1]] %>%
    strsplit(",") %>%
    gsub("[[:punct:]]", "", .)
  write_csv("Catchment/PrettyBoy.csv")
} else {
  prettyboy <- read_csv("Catchment/PrettyBoy.csv")
}

ext <- c(which(prettyboy$Easting == min(prettyboy$Easting)), 
         which(prettyboy$Easting == max(prettyboy$Easting)),
         which(duplicated(prettyboy[,1:2])))
prettyboy <- prettyboy[-ext, ]



plot(prettyboy$Easting, prettyboy$Northing, pch = ".")

polygon(prettyboy[chull(prettyboy$Easting, prettyboy$Northing), ])

bathymetry_colours <- c(rev(brewer.pal(3, "Greens"))[-2:-3], 
                        brewer.pal(9, "Blues")[-1:-3])
ggplot(prettyboy, aes(Easting, Northing, colour = Depth * 0.3048)) + 
  geom_point(size = .1) + 
  coord_equal() + labs(colour = "Depth [m]") + 
  scale_colour_gradientn(colors = bathymetry_colours) 
ggsave("Images/PrettyBoy_bathymetry.png", dpi = 300)


prettyboy %>% filter(Depth == 0) %>%
  ggplot(aes(Easting, Northing)) + geom_path() + coord_equal()


library(alphahull)
n <- 200
theta<-runif(n,0,2*pi) 
r<-sqrt(runif(n,0.25^2,0.5^2)) 
x<-cbind(0.5+r*cos(theta),0.5+r*sin(theta)) 
alpha <- 0.15
alphahull <- ahull(x, alpha = alpha)



alphahull <- ahull(prettyboy[,1:2], alpha = 0.3)
plot(alphahull, col = c(6, rep(1, 5)), pch = ".")








