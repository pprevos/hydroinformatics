# ANalysing Digital Water Meter Data
# https://r.prevos.net/digital-water-meter-data/

# INIT
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
    mutate(TimeStampAEST = as.POSIXct(format(TimeStampUTC, tz = "Australia/Melbourne"))) %>%
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

interpolate_count(rtu[2:3], seq.POSIXt(as.POSIXct("2020-02-01"), as.POSIXct("2020-02-2"), by = "day"))

slice_reads(rtu[2], c("2020-02-06", "2020-02-08")) %>%
  ggplot(aes(x = TimeStampAEST, y = Count))  + 
  geom_line(col = "grey", size = 1) + 
  geom_point(col = "red") + 
  geom_point(data = interpolate_count(rtu[2], as.POSIXct("2020-02-06") + (0:2)*24*3600), colour = "blue") + 
  ggtitle(paste("DevEUI", rtu[2]))
ggsave("Hydroinformatics/DigitalMetering/consumption.png", dpi = 300)

# Daily Consumption
daily_consumption <- function(rtus, dates) {
  timestamps <- seq.POSIXt(as.POSIXct(min(dates)) - 24 * 3600,
                           as.POSIXct(max(dates)), by = "day")
  interpolate_count(rtus, timestamps) %>%
    group_by(DevEUI) %>%
    mutate(Consumption = c(0, diff(Count)) * 5,
           Date = format(TimeStampAEST, "%F")) %>%
    filter(TimeStampAEST != timestamps[1]) %>%
    select(DevEUI, Date, Consumption)
}

daily_consumption(rtu[32:33], c("2020-02-01", "2020-02-7")) %>%
  ggplot(aes(x = Date, y = Consumption)) + geom_col() + 
  facet_wrap(~DevEUI) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Hydroinformatics/DigitalMetering/daily.png", dpi = 300)

daily_consumption(rtu[32:33], c("2020-02-01", "2020-02-7")) %>%
  group_by(Date) %>%
  summarise(Consumption = sum(Consumption)) %>%
  ggplot(aes(x = Date, y = Consumption)) + geom_col() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Diurnal Flow (connections)
plot_diurnal_connections <- function(rtus, dates) {
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
    ggtitle("Connections Diurnal flow") + ylab("Flow rate [L/h]")
}

plot_diurnal_connections(rtu[12:20], c("2020-02-01", "2020-03-01"))
ggsave("Hydroinformatics/DigitalMetering/diurnal_line.png", dpi = 300)

# Dirunal flow (network)
plot_diurnal_network <- function(rtus, dates) {
  timestamps <- seq.POSIXt(as.POSIXct(dates[1]), as.POSIXct(dates[2]), by = "hour")
  interpolate_count(rtus, timestamps) %>% 
    mutate(Rate = c(0, diff(Count * 5))) %>% 
    filter(Rate >= 0) %>%
    group_by(TimeStampAEST) %>%
    summarise (Rate = sum(Rate)) %>%
    mutate(Hour = as.integer(format(TimeStampAEST, "%H"))) %>%
    group_by(Hour) %>%
    summarise(min = min(Rate), mean = mean(Rate), max = max(Rate)) %>%
    ggplot(aes(x = Hour, ymin = min, ymax = max)) + 
    geom_ribbon(fill = "lightblue", alpha = 0.5) + 
    geom_line(aes(x = Hour, y = mean), col = "orange", size = 1) +
    ggtitle("Network Diurnal flow") + ylab("Flow rate [L/h]")
}


plot_diurnal_network(rtu[12:20], c("2020-02-01", "2020-03-01"))

plot_diurnal_box <- function(rtus, dates) {
  timestamps <- seq.POSIXt(as.POSIXct(dates[1]), as.POSIXct(dates[2]), by = "hour")
  interpolate_count(rtus, timestamps) %>% 
    mutate(Rate = c(0, diff(Count * 5)),
           Hour = as.integer(format(TimeStampAEST, "%H"))) %>% 
    filter(Rate >= 0) %>%
    group_by(Hour) %>%
    ggplot(aes(x = factor(Hour), y = Rate)) + 
    geom_boxplot() + 
    ggtitle("Diurnal flow") + ylab("Flow rate [L/h]") + xlab("Time")
}

plot_diurnal_box(rtu[12:20], c("2020-02-01", "2020-03-01"))
ggsave("Hydroinformatics/DigitalMetering/diurnal_box.png", dpi = 300)

# Leak Detection
detect_leaks <- function(rtus, dates) {
  slice_reads(rtus, dates) %>%
    mutate(Time = c(0, diff(TimeStampAEST)),
           Flow = c(0, diff(Count * 5)),
           Rate = Flow / Time) %>% 
    filter(Time > 0) %>%
    group_by(DevEUI) %>%
    summarise(Leak= min(Flow)) %>%
    filter(Leak > 0)
}
detect_leaks(rtu, c("2020-02-01", "2020-03-01"))
plot_diurnal_box(1335858, c("2020-02-01", "2020-03-01"))
