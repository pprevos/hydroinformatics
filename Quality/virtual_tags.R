# Simulate measured data
set.seed(1234)
n <- 300
wtp <- data.frame(DateTime = seq.POSIXt(ISOdate(1910, 1, 1), length.out = n, by = 60),
                  WTP = rlnorm(n, log(.1), .01))
library(ggplot2)
p <- ggplot(wtp, aes(x = DateTime, y = WTP)) + geom_line(colour = "grey") + 
    ylim(0.09, 0.11) + ylab("Turbidity") + ggtitle("Turbidity simulation")
p
ggsave("Hydroinformatics/Turbidity.png")

# Historise data
threshold <- 0.03
h <- 1 # First historised point
# Starting conditions
wtp$historise <- FALSE
wtp$historise[c(1, n)] <- TRUE
# Testing for delta <> threshold
for (i in 2:nrow(wtp)) {
    delta <- wtp$WTP[i] / wtp$WTP[h]
    if (delta > (1 + threshold) | delta < (1 - threshold)) {
        wtp$historise[i] <- TRUE
        h <- i
    }
}
historian <- subset(wtp, historise == TRUE)
historian$Source <- "Historian"
p <- p + geom_point(data = historian, aes(x = DateTime, y = WTP)) + ggtitle("Historised data")
p
ggsave("Hydroinformatics/Historian.png")

# Create Virtual Ttags
vt <- function(t) approx(historian$DateTime, historian$WTP, xout = t, method = "constant")
turbidity <- lapply(as.data.frame(wtp$DateTime), vt)
wtp$VirtualTag <- turbidity[[1]]$y
p + geom_line(data = wtp, aes(x = DateTime, y = VirtualTag), colour = "red") + ggtitle("Virtual Tags")
ggsave("Hydroinformatics/VirtualTags.png")

