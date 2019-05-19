## Call centre modelling using the Erlang C formula
## The Devil is in the Data
## http://lucidmanager.org/call-centre-planning/

## Atribution
## Erlang algorithm derived from https://en.wikipedia.org/wiki/Erlang_(unit)
## Service level formula derived from callcentrehelper.com
## https://callcentrehelper.com/erlang-c-formula-example-121281.htm
## Concept for code structure derived from Patrick Hubers
## https://github.com/phubers/erlang

intensity <- function(rate, duration, interval = 60) {
    (rate / (60 * interval)) * duration
}

erlang_c <- function(agents, rate, duration, interval = 60) {
    int <- intensity(rate, duration, interval)
    erlang_b_inv <- 1
    for (i in 1:agents) {
        erlang_b_inv <- 1 + erlang_b_inv * i / int
    }
    erlang_b <- 1 / erlang_b_inv
    agents * erlang_b / (agents - int * (1 - erlang_b))
}

service_level <- function(agents, rate, duration, target, interval = 60) {
    pw <- erlang_c(agents, rate, duration, interval)
    int <- intensity(rate, duration, interval)
    1 - (pw * exp(-(agents - int) * (target / duration)))
}

resource <- function(rate, duration, target, gos_target, interval = 60) {
    agents <-round(intensity(rate, duration, interval) + 1)
    gos <- service_level(agents, rate, duration, target, interval)
    while (gos < gos_target * (gos_target > 1) / 100) {
        agents <- agents + 1
        gos <- service_level(agents, rate, duration, target, interval)
    }
    return(c(agents, gos))
}

resource(100, 180, 20, 90, 30)

## Monte Carlo Simulation
library(tidyverse)

intensity_mc <- function(rate_m, rate_sd, duration_m, duration_sd, interval = 60, sims = 1000) {
    (rnorm(sims, rate_m, rate_sd) / (60 * interval)) * rnorm(sims, duration_m, duration_sd)
}

intensity_mc(100, 10, 180, 20, interval = 30) %>%
    summary

erlang_c_mc <- function(agents, rate_m, rate_sd, duration_m, duration_sd, interval = 60) {
    int <- intensity_mc(rate_m, rate_sd, duration_m, duration_sd, interval)
    erlang_b_inv <- 1
    for (i in 1:agents) {
        erlang_b_inv <- 1 + erlang_b_inv * i / int
    }
    erlang_b <- 1 / erlang_b_inv
    agents * erlang_b / (agents - int * (1 - erlang_b))
}

service_level_mc <- function(agents, rate_m, rate_sd, duration_m, duration_sd, target, interval = 60, sims = 1000) {
    pw <- erlang_c_mc(agents, rate_m, rate_sd, duration_m, duration_sd, interval)
    int <- intensity_mc(rate_m, rate_sd, duration_m, duration_sd, interval, sims)
    1 - (pw * exp(-(agents - int) * (target / rnorm(sims, duration_m, duration_sd))))
}

data_frame(ServiceLevel = service_level_mc(agents = 12,
                                           rate_m = 100,
                                           rate_sd = 10,
                                           duration_m = 180,
                                           duration_sd = 20,
                                           target = 20,
                                           interval = 30,
                                           sims = 1000)) %>%
    ggplot(aes(ServiceLevel)) +
        geom_histogram(binwidth = 0.1, fill = "#008da1")
ggsave("service_levels.png", dpi = 300)

service_level_mc(15, 100, 10, 180, 20, 20, 30, sims = 1000) %>%
    quantile(c(.05, .5, .95))

