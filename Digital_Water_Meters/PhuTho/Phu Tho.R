# --------------------------
# Viet Tri Water consumption

# Read the data
water <- read.csv("Hydroinformatics/PhuTho/MeterReads.csv")
water$Consumption <- water$read_new - water$read_old

head(water)
summary(water$Consumption)

# Load map library
library(ggmap)

# Find the middle of the points
centre <- c(mean(range(water$lon)), mean(range(water$lat)))

# Download the satellite image
viettri <- get_map(centre, zoom = 17, maptype = "hybrid")
g <- ggmap(viettri)
g

# Plot the data
g + # Satelite image
    # Add the points
    geom_point(data = water, aes(x = lon, y = lat, size = Consumption), 
        shape = 21, colour = "dodgerblue4", fill = "dodgerblue", alpha = .5) +
    scale_size_area(max_size = 20) + 
    # Size of the biggest point 
    ggtitle("Việt Trì sự tiêu thụ nước")

# Save image to disk
ggsave("Hydroinformatics/PhuTho/VietTri_heat.png")

g + geom_point(data = water, aes(x = lon, y = lat, colour = Consumption), alpha = 0.8, size = 3) + 
    scale_color_gradientn(colours = c("green", "orange", "red"))

# Heat Map

water <- subset(water, Consumption<100)

water_count <- data.frame(lat = rep(water[,"lat"], water$Consumption), 
                          lon = rep(water[,"lon"], water$Consumption))

g + stat_bin_2d(data = water_count, aes(x = lon, y = lat),
    size = .5, bins = 30, alpha = 1/2) + 
    scale_fill_gradientn(colours = c("green", "orange", "red"))



