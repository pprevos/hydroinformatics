## Consumer Involvement
library(tidyverse)
library(psych)

consumers <- read_csv("customers/customers_quan.csv") %>%
  select(starts_with("p"))
dim(consumers)

## Data cleansing
sdevs <- apply(consumers, 1, sd, na.rm = TRUE)
incomplete <- apply(consumers, 1, function(i) any(is.na(i)))
consumers <- consumers[sdevs != 0 & !incomplete, ]
dim(consumers)

## Exploratory Analysis
consumers %>%
  rownames_to_column(var = "Subject") %>%
  gather(Item, Response, -Subject) %>%    
  ggplot(aes(Item, Response)) + geom_boxplot(fill = "#f7941d") +
  theme_bw(base_size = 10) + 
  ggtitle("personal Involvement Index",
          subtitle = paste("Tap Water Consumers USA and Australia (n =",
                           nrow(consumers), ")"))
ggsave("involvement-explore.png", width = 6, height = 4)

##png("involvement-correlation.png")
corPlot(consumers)
##dev.off()

## Factor Analysis
piiFac <- fa(consumers, nfactors = 2, rotate = "varimax")

##png("involvement-factors.png")
fa.diagram(piiFac)
##dev.off()
