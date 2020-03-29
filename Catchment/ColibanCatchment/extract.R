# Extract data from databases
library(RODBC)
library(tidyverse)

sandbox <- odbcDriverConnect("driver={SQL Server}; server=SQLSandbox; DATABASE=WQSandbox")
delwp <- odbcDriverConnect("driver={SQL Server}; server=Hydroinformatics; DATABASE=StreamFlow")
reservoirs <- sqlQuery(delwp, "SELECT Date, StationFID, CWName, Level, Volume, FSL, FSLVolume FROM SF_Reservoir_Daily WHERE StationFID<>'406219A'")
streamflows <- sqlQuery(delwp, "SELECT Date, StationFID, CWName, Flow FROM SF_StreamFlow_Daily WHERE StationFID IN ('406200B', '406210A', '406250A', '406280A', '406281A')")
stations <- sqlFetch(delwp, "StreamFlow_Stations")  
rain <- sqlQuery(sandbox, "SELECT ModDttm,  Plant_Name, Rainfall FROM SF_AMS_Inspection_Results WHERE RainfallYN='Y'")
odbcCloseAll()

reservoirs <- mutate(reservoirs,
                     Date = as.Date(Date, tz = ""),
                     CWName = str_replace(CWName, " Head Gauge", ""))

stats <- tibble(Plant_Name = c("HMR", "HLR"),
                   Reservoir = c("Malmsbury", "Lauristion"))

rain <- left_join(rain, stats) %>%
    mutate(Date = as.Date(ModDttm, tz = "")) %>%
    select(Date, Reservoir, Rainfall)

stations <- filter(stations, StationID %in% c(as.character(unique(reservoirs$StationFID)), 
  as.character(unique(streamflows$StationFID))))

write.csv(rain, "rain.csv", row.names = FALSE)
write.csv(reservoirs, "reservoirs.csv", row.names = FALSE)
write.csv(streamflows, "streamflows.csv", row.names = FALSE)
write.csv(stations, "stations.csv", row.names = FALSE)