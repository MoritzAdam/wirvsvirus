library(readr)
library(tidyverse)

#Quellen: Regionalstatistische Raumtypologie (RegioStaR) für die Mobilitäts- und Verkehrsforschung, MiD2017

Landkreis <- read_csv("mobility/Landkreis.csv")
Regiotypen <- read_csv("mobility/Regiotypen.csv")
Predictors <- read_delim("predictors_landkreise_tabstop.csv", 
                         "\t", escape_double = FALSE, trim_ws = TRUE)
Raumtypen <- read_csv("mobility/Raumtypen_index.csv")

df <- data.frame(ID=c("71", "72", "73", "74", "75", "76", "77"), Regio=c("Stadtregion - Metropole", "Stadtregion - Regiopole und Großstadt", "Stadtregion - Mittelstadt, städtischer Raum", "Stadtregion - Kleinstädtischer, dörflicher Raum", "Ländliche Region - Zentrale Stadt", "Ländliche Region - Städtischer Raum", "Ländliche Region - Kleinstädtischer, dörflicher Raum"))

Regiotypen$Schluessel <- as.numeric(gsub('.{3}$', '', Regiotypen$gem))
regio_data <- as_tibble(Regiotypen)

regio_data_landkreise <- regio_data %>% 
  group_by(Schluessel) %>% 
  summarise(Raumtyp = mean(RegioStaRGem7))

regio_data_landkreise[1,1] <- 1100
regio_data_landkreise[2,1] <- 2000 #quick fix, does anybody now why as.numerice(gsub('.{3}$', '', 20000)))=2 and not 2000?

regio_data_landkreise <- arrange(regio_data_landkreise, Schluessel )
regio_data_landkreise$Raumtyp <- round(regio_data_landkreise$Raumtyp, 0)

predictors_data <- as_tibble(Predictors)
raumtypen_data <- as_tibble(Raumtypen)

raumtypen_data <- raumtypen_data %>% select (-c("Bevölkerung (Mio)",  "Personenkilometer (Mio)" )) %>% right_join(regio_data_landkreise, . , by = "Raumtyp") %>% arrange(., Schluessel)

predictors_data <- predictors_data %>% 
  left_join(., raumtypen_data, by = "Schluessel") 

write_csv(predictors_data, "~/Documents/WirVsVirus/wirvsvirus/predictors_data_with_mobility.csv")
          