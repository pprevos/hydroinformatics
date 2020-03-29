# Digital Metering Simulation
# https://r.prevos.net/simulating-water-consumption/ 

# Libraries
library(tidyverse)
# Boundary conditions
n <- 100 # Number of simulated meters
d <- 366 # Number of days to simulate
s <- as.POSIXct("2020-01-01", tz = "UTC") # Start of simulation

set.seed(1969) # Seed random number generator for reproducibility
rtu <- sample(1E6:2E6, n, replace = FALSE) # 6-digit id
offset <- sample(0:3599, n, replace = TRUE) # Unique Random offset for each RTU

# Generic Diurnal Curve
diurnal <- round(c(1.36, 1.085, 0.98, 1.05, 1.58, 3.87, 9.37, 13.3, 12.1, 10.3, 8.44, 7.04, 6.11, 5.68, 5.58, 6.67, 8.32, 10.0, 9.37, 7.73, 6.59, 5.18, 3.55, 2.11)) - 1
data.frame(TimeUTC = 0:23,
           Flow = diurnal) %>% 
  ggplot(aes(x = TimeUTC, y = Flow)) + 
  geom_area(fill = "dodgerblue2", alpha = 0.5) +
  scale_x_continuous(breaks = 0:23) + ylab("Flow [L/h/p]") + 
  ggtitle("Idealised diurnal curve for households")
ggsave("Hydroinformatics/DigitalMetering/diurnal_curve.png", dpi = 300)

tdiff <- 11
diurnal <- c(diurnal[(tdiff + 1): 24], diurnal[1:tdiff])

# Occupants
occupants <- rpois(n, 1.5) + 1 # Number of occupants per connection
as.data.frame(occupants) %>%
  ggplot(aes(occupants)) + geom_bar(fill = "dodgerblue2", alpha = 0.5) + 
  xlab("Occupants") + ylab("Connections") + ggtitle("Occupants per connection")
ggsave("Hydroinformatics/DigitalMetering/occupants.png", dpi = 300)

# Leak simulation
leaks <- rbinom(n, 1, prob = .1) * sample(10:50, n, replace = TRUE)
data.frame(DevEUI = rtu, Leak = leaks) %>%
  subset(Leak > 0)

# Digital metering data simulation
meter_reads <- matrix(ncol = 5, nrow = 24 * n * d)
colnames(meter_reads) <- c("DevEUI", "TimeStampUTC", "Count", "Forced", "Tamper")

for (i in 1:n) {
  r <- ((i - 1) * 24 * d + 1):(i * 24 * d)
  meter_reads[r, 1] <- rep(rtu[i], each = (24 * d))
  meter_reads[r, 2] <- seq.POSIXt(s, by = "hour", length.out = 24 * d) + offset[i]
  meter_reads[r, 3] <- round(cumsum((rep(diurnal * occupants[i], d) + leaks[i]) * 
                                      runif(24 * d, 0.9, 1.1))/5)
  meter_reads[r,4] <- 0
  meter_reads[r,5] <- sample(c(rep(0,1000), 1, 2), (24 * d), replace = TRUE)
} 

meter_reads <- meter_reads %>% 
  as_data_frame() %>%
  mutate(TimeStampUTC = as.POSIXct(TimeStampUTC, origin = "1970-01-01", tz ="UTC"))
meter_reads

# Missing Data Points

# Initialise temp variable
meter_reads <- mutate(meter_reads, remove = 0)
# Define faulty RTUs (2% of fleet)
faulty <- rtu[rbinom(n, 1, prob = 0.02) == 1]
meter_reads$remove[meter_reads$DevEUI %in% faulty] <- rbinom(sum(meter_reads$DevEUI %in% faulty), 1, prob = .95)

# Data loss
missing <- sample(1:(nrow(meter_reads) - 5), 0.01 * nrow(meter_reads))
for (m in missing){
  meter_reads[m:(m + sample(1:5, 1)), "remove"] <- 1
}

# Remove data points
meter_reads <- filter(meter_reads, remove == 0) %>%
  select(-remove)

# Store data
write.csv(meter_reads, "Hydroinformatics/DigitalMetering/meter_reads.csv", row.names = FALSE)

# Visualise
filter(meter_reads, DevEUI %in% rtu[2]) %>%
  mutate(TimeStampAEST = as.POSIXct(format(TimeStampUTC, 
                                           tz = "Australia/Melbourne"))) %>%
  filter(TimeStampAEST >= as.POSIXct("2020-02-06") & 
         TimeStampAEST <= as.POSIXct("2020-02-08")) %>%
  arrange(DevEUI, TimeStampAEST) %>% 
  ggplot(aes(x = TimeStampAEST, y = Count, colour = factor(DevEUI)))  + 
    geom_line() + geom_point() 

ggsave("Hydroinformatics/DigitalMetering/consumption.png", dpi = 300)



