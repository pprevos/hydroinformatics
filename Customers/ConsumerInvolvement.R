## Measuring consumer Involvement: Factor Analysis in R
## The Devil is in the Data
## https://lucidmanager.org/measauring-consumer-involvement
## Peter Prevos

library(tidyverse)
library(psych)
consumers <- read_csv("customers_quan.csv") %>%
    select(starts_with("p"))
dim(consumers)

## Data clesaning
sdevs <- apply(consumers, 1, sd, na.rm = TRUE)
incomplete <- apply(consumers, 1, function(i) any(is.na(i)))
consumers <- consumers[sdevs != 0 & !incomplete, ]
dim(consumers)

## Exploratory Analysis
consumers %>%
    rownames_to_column(var = "Subject") %>%
    gather(Item, Response, -Subject) %>%    
    ggplot(aes(Item, Response)) + geom_boxplot(fill = "#f7941d") +
    ggtitle("personal Involvement Index",
            subtitle = paste("Tap Water Consumers USA and Australia (n =",
                             nrow(consumers), ")"))
ggsave("involvement_explore.png", dpi = 300)

png("involvement_correlation.png", width = 1024, height = 1024)
corPlot(consumers)
dev.off()

piiFac <- fa(consumers, nfactors = 2, rotate = "oblimin")

png("involvement_factors.png", width = 1024, height = 768)
fa.diagram(piiFac)
dev.off()


