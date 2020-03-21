library(readr)  # for read_csv
library(knitr)  # for kable
library(dplyr)
library(plyr)


#import data for fallzahlen
fallzahlen_file <- "https://raw.githubusercontent.com/MoritzAdam/wirvsvirus/master/data_landkreise/data_landkreise_2020-03-20.csv"

Affairs <- read_csv(fallzahlen_file)
fallzahlen <- as.data.frame(Affairs)
fallzahlen$id   #id for landkreise (LK)

#import predictors
predictors_lk_file <- "https://raw.githubusercontent.com/MoritzAdam/wirvsvirus/master/predictors_landkreise.csv"
predictors_lk <- as.data.frame(read.table(predictors_lk_file, sep = "\t", header = T))
# colnames(predictors_lk) <- c("id", "name", "wert", "flaeche", "bevoelkerung", "maennlich", "weiblich", "Anteil0-17", "Anteil18-25", "Anteil25-44", "Anteil25-64", "Anteil65-", "Krankenbetten/1000", "Einkommen")


#sum cases per lk
for (i in length(predictors_lk$Schluessel)){
  fallzahlen_lk <- data.frame("id" = predictors_lk$Schluessel, "cum_cases" = sum(fallzahlen %>% filter(id==predictors_lk$Schluessel[[1]])))
}