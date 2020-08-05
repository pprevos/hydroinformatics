library(tidyverse)
library(lubridate)
library(magrittr)

# Read Data
meter_reads <- read.csv("Hydroinformatics/DigitalMetering/meter_reads.csv")
rtu <- unique(meter_reads$DevEUI)
meter_reads$TimeStampUTC <- as.POSIXct(meter_reads$TimeStampUTC, tz = "UTC")

# Slice Data
slice_reads <- function(rtus, dates = range(meter_reads$TimeStampUTC)) {
  filter(meter_reads, DevEUI %in% rtus) %>%
    mutate(TimeStampAEST = as.POSIXct(format(TimeStampUTC, 
                                             tz = "Australia/Melbourne"))) %>%
    filter(TimeStampAEST >= as.POSIXct(dates[1]) & 
             TimeStampAEST <= as.POSIXct(dates[2])) %>%
    arrange(DevEUI, TimeStampAEST)
}

# Interpolation
interpolate_count <- function(rtus, timestamps) {
  timestamps <- as.POSIXct(timestamps, tz = "Australia/Melbourne")
  results <- vector("list", length(rtus))
  for (r in seq_along(rtus)) {
    interp <- slice_reads(rtus[r]) %$%
      approx(TimeStampAEST, Count, timestamps)
    results[[r]] <- data_frame(DevEUI = rep(rtus[r], length(timestamps)),
                               TimeStampAEST = timestamps,
                               Count = interp$y)
  }
  return(do.call(rbind, results))
}

# Connection diurnal flow (interpolated)

plot_diurnal_connections_int <- function(rtus, dates) {
  timestamps <- seq.POSIXt(as.POSIXct(dates[1]), as.POSIXct(dates[2]), by = "hour")
  interpolate_count(rtus, timestamps) %>% 
    mutate(Rate = c(0, diff(Count * 5)),
           Hour = as.integer(format(TimeStampAEST, "%H"))) %>% 
    filter(Rate >= 0) %>%
    group_by(Hour) %>%
    summarise(min = min(Rate), mean = mean(Rate), max = max(Rate)) %>%
    ggplot(aes(x = Hour, ymin = min, ymax = max)) + 
      geom_ribbon(fill = "lightblue", alpha = 0.5) + 
      geom_line(aes(x = Hour, y = mean), col = "orange", size = 1) +
      ggtitle("Connections Diurnal flow (interpolated)") + ylab("Flow rate [L/h]")
}

ptm <- proc.time()
p1 <- plot_diurnal_connections_int(rtu[12:20], c("2020-02-01", "2020-03-01"))
p1
print(proc.time() - ptm)

ggsave("diurnal_interpolated.png", dpi = 300)

# Connection diurnal flow (rounded)

plot_diurnal_connections_round <- function(rtus, dates) {
  slice_reads(rtus, dates) %>%
    group_by(DevEUI) %>%
    mutate(Hour = as.integer(format(TimeStampAEST + 1800, "%H")),
           Time = c(0, diff(TimeStampAEST)),
           Flow = c(0, diff(Count * 5)),
           Rate = Flow / Time) %>%
    group_by(Hour) %>%
    filter(Time > 0) %>%
    summarise(min = min(Rate), mean = mean(Rate), max = max(Rate)) %>%
    ggplot(aes(x = Hour, ymin = min, ymax = max)) + 
    geom_ribbon(fill = "lightblue", alpha = 0.5) + 
    geom_line(aes(x = Hour, y = mean), col = "orange", size = 1) +
    ggtitle("Connections Diurnal flow (rounded)") + ylab("Flow rate [L/h]")
}

plot_diurnal_network_round <- function(rtus, dates) {
  slice_reads(rtus, dates) %>%
    group_by(DevEUI) %>%
    mutate(Hour = as.integer(format(TimeStampAEST + 1800, "%H")),
           Time = c(0, diff(TimeStampAEST)),
           Flow = c(0, diff(Count * 5)),
           Rate = Flow / Time) %>%
    filter(Rate >= 0) %>%
    mutate(RoundedTime) <- 
    
}



ptm <- proc.time()
p2 <- plot_diurnal_connections_round(rtu[12:20], c("2020-02-01", "2020-03-01"))
p2
print(proc.time() - ptm)

ggsave("diurnal_nearest.png", dpi = 300)



p1 <- p1$data %>% mutate(Method = "Interpolated")
p2 <- p2$data %>% mutate(Method = "Rounded")

rbind(p1, p2) %>%
  ggplot(aes(x = Hour, y = mean, colour = Method)) + geom_line() + 
  geom_line(aes(x = Hour, y = max, colour = Method)) +
  geom_line(aes(x = Hour, y = min, colour = Method))





