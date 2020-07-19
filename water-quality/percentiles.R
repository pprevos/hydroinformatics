## Percentile calculations

## example
set.seed(1969)
test.data <- rnorm(n = 10000, mean = 100, sd = 15)

library(ggplot2)
ggplot(as.data.frame(test.data), aes(test.data)) +
    geom_histogram(binwidth = 1, aes(y = ..density..), fill = "dodgerblue") +
    geom_line(stat = "function", fun = dnorm, args = list(mean = 100, sd = 15), colour = "red", size = 1) +
    geom_area(stat = "function", fun = dnorm, args = list(mean = 100, sd = 15),
    colour = "red", fill = "red", alpha = 0.5, xlim = quantile(test.data, c(0.5, 0.75))) +
  theme_bw(base_size = 8)
ggsave("percentile-example.png", width = 4, height = 3)

## Weibull method
weibull.quantile <- function(x, p) {
    # Order Samples from large to small
    x <- x[order(x, decreasing = FALSE)]
    # Determine ranking of percentile according to Weibull (1939)
    r <- p * (length(x) + 1)
    # Linear interpolation
    rfrac <- (r - floor(r))
    return((1 - rfrac) * x[floor(r)] + rfrac * x[floor(r) + 1])
}
weibull.quantile(test.data, 0.95)

quantile(test.data, 0.95, type = 6)

## Visualise Data
library(readr)
library(dplyr)
gormsey <- read_csv("water-quality/gormsey.csv")
turbidity <- filter(gormsey, Measure == "Turbidity" &
                             Town %in% c("Pontybridge", "Strathmore"))

ggplot(turbidity, aes(Result)) +
    geom_histogram(binwidth = .1, fill = "dodgerblue", aes(y = ..density..)) +
    facet_wrap(~Town) +
    theme_bw(base_size = 10)
ggsave("gormsey-turbidity.png", width = 4, height = 3)

p95 <- tapply(turbidity$Result, turbidity$Town,
              function(x) sapply(1:9, function(m) quantile(x, 0.95, type = m)))
