# Percentiles in water quality

# Simulate normally distributed data
set.seed(1969)
test.data <- rnorm(n=10000, mean=100, sd=15)

# Determine quantiles
quantile(test.data)
quantile(test.data, probs=0.95)

# Visualise Data and save image
library(ggplot2)
ggplot(as.data.frame(test.data), aes(test.data)) + 
    geom_histogram(binwidth=1, aes(y=..density..), fill="dodgerblue") + 
    geom_line(stat="function", fun=dnorm, args=list(mean=100, sd=15), colour="red", size=1) + 
    geom_area(stat="function", fun=dnorm, args=list(mean=100, sd=15), 
        colour="red", fill="red", alpha=.5, xlim=quantile(test.data, c(.5, .75))) + 
    theme(text=element_text(size=16))
ggsave("Images/percentiles.png")    

# Various methods
sapply(1:9, function(m) quantile(test.data, 0.95, type=m))

weibull.quantile <- function(x, p) {
    # Order Samples from large to small
    x <- x[order(x, decreasing=FALSE)]
    # Determine ranking of percentile according to Weibull (1939)
    r <- p * (length(x) + 1)
    # Linear interpolation
    rfrac <- (r - floor(r))
    ((1 - rfrac) * x[floor(r)] + rfrac * x[floor(r) + 1])
}

# Load turbidity data
library(RODBC)
dwh <- odbcDriverConnect("driver={SQL Server};server=DWH;DATABASE=CW_DataWarehouse;trusted_connection=true")
turbidity <- sqlQuery(dwh, "SELECT Date_Sampled, Result , Subsite_Code, Zone FROM WSL_Retic_Sample_Results WHERE 
                      Subsite_Type='Customer Tap' AND Analyte='TURBID' AND 
                      System='Laanecoorie' AND Date_Sampled >= '01-01-2016' AND Date_Sampled <= '12-31-2016'")
odbcClose(dwh)

ggplot(turbidity, aes(Result)) + 
    geom_histogram(binwidth=.05, fill="dodgerblue", aes(y=..density..)) + 
    facet_wrap(~Zone) + 
    theme(text=element_text(size=16))
ggsave("Images/turbidity.png")

tapply(turbidity$Result, turbidity$Zone, 
       function(x) sapply(1:9, function(m) quantile(x, 0.95, type=m)))

