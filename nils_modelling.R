setwd("Desktop/Privat/wirvsvirus/wirvsvirus/")
library(fields)
library(httr)
library(rlist)
library(jsonlite)
library(tidyverse)

bundesland_ts <- read.csv("RKI_ts_bundesland/data/covid19-germany.csv",colClasses = c("numeric","character","numeric","numeric","Date"))
bundesland_spread <- read.csv("speed_of_spread.cvs",colClasses = c("character","character","numeric","numeric"))
bundesland_short <- data.frame(short=bundesland_spread$From..https...www.datawrapper.de._.TxHt7.,long_spead=bundesland_spread$Bundesland,long_ts=c("North Rhine-Westphalia","Rhineland-Palatinate","Baden-Württemberg","Hamburg","Saarland",NA,"Schleswig-Holstein","Bremen","Hesse","Lower Saxony","Bavaria","Saxony","Saxony-Anhalt","Brandenburg","Thuringia","Mecklenburg-Western Pomerania","Berlin"))
bundesland_ts_matrix <- array(NA,dim=c(length(bundesland_short$short),length(unique(bundesland_ts$date))))
for (i in 1:length(bundesland_short$short)) {
  if (bundesland_short$short[i] != "D") {
  bundesland_ts_matrix[i,] <- bundesland_ts$infections[which(bundesland_ts$federalstate == bundesland_short$long_ts[i])]
  }
}
for (i in 1:17) {
  if (bundesland_short$short[i] != "D") {
    plot(unique(bundesland_ts$date),bundesland_ts_matrix[i,])
  }
}
?lm
exp_coef <- 0
for (i in 1:17) {
  if (bundesland_short$short[i] != "D") {
    test_lm <- lm(log(y) ~ x,data=data.frame(y=sapply(bundesland_ts_matrix[i,],function(x) max(0.1,x)),x=unique(bundesland_ts$date)))
    exp_coef[i] <- test_lm$coefficients[2]
    plot(unique(bundesland_ts$date),bundesland_ts_matrix[i,],main=bundesland_short$long_ts[i])
    lines(unique(bundesland_ts$date),exp(predict(test_lm)))
  }
}
plot(exp_coef)

rki_data_landkreise <- query_arcgis_all()
tesrt <- unique(rki_data_landkreise$IdLandkreis)

sum(rki_data_landkreise$AnzahlFall)
sum(bundesland_ts_matrix[,18],na.rm=T)

sort(unique(rki_data_landkreise$Meldedatum))
landkreise <- data.frame(ID=unique(rki_data_landkreise$IdLandkreis),name=unique(rki_data_landkreise$Landkreis))
relevant_dates <- seq.Date(as.Date(min(unique(rki_data_landkreise$Meldedatum))),as.Date(max(unique(rki_data_landkreise$Meldedatum))),by="day")
bundeslaender <- data.frame(ID=unique(rki_data_landkreise$IdBundesland),name=unique(rki_data_landkreise$Bundesland),id_cross=c()); bundeslaender$name <- as.character(bundeslaender$name)
rki_landkreise_matrix <- array(0,dim=c(length(landkreise$ID),length(relevant_dates)))
rki_bundesland_matrix <- array(0,dim=c(length(bundeslaender$ID),length(relevant_dates)))

for (i in 1:length(rki_data_landkreise$id)) {
  rki_landkreise_matrix[which(landkreise$ID == rki_data_landkreise$IdLandkreis[i]),which(relevant_dates == as.Date(rki_data_landkreise$Meldedatum[i]))] <- rki_landkreise_matrix[which(landkreise$ID == rki_data_landkreise$IdLandkreis[i]),which(relevant_dates == as.Date(rki_data_landkreise$Meldedatum[i]))] + rki_data_landkreise$AnzahlFall[i]
}

for (i in 1:length(rki_data_landkreise$id)) {
  rki_bundesland_matrix[which(bundeslaender$ID == rki_data_landkreise$IdBundesland[i]),which(relevant_dates == as.Date(rki_data_landkreise$Meldedatum[i]))] <- rki_bundesland_matrix[which(bundeslaender$ID == rki_data_landkreise$IdBundesland[i]),which(relevant_dates == as.Date(rki_data_landkreise$Meldedatum[i]))] + rki_data_landkreise$AnzahlFall[i]
}

image.plot(rki_landkreise_matrix)
for (i in 250:300) {
  plot(relevant_dates,rki_landkreise_matrix[i,],type="o",pch=20,main=landkreise$name[i])
}

image.plot(rki_bundesland_matrix)
for (i in 1:17) {
  plot(relevant_dates,rki_bundesland_matrix[i,],type="o",pch=20,main=bundeslaender$name[i])
}

rki_bundesland_cum <- t(apply(rki_bundesland_matrix,1,cumsum))
for (i in 1:17) {
  plot(relevant_dates,rki_bundesland_cum[i,],type="o",pch=20,main=bundeslaender$name[i])
}
exp_coef_2 <- 0
for (i in 1:17) {
  test_lm <- lm(log(y) ~ x,data=data.frame(y=rki_bundesland_cum[i,which(rki_bundesland_cum[i,]>0)],x=relevant_dates[which(rki_bundesland_cum[i,]>0)]))
  exp_coef_2[i] <- test_lm$coefficients[2]
  plot(relevant_dates,rki_bundesland_cum[i,],main=bundeslaender$name[i])
  lines(relevant_dates[which(rki_bundesland_cum[i,]>0)],exp(predict(test_lm)))
}
bundeslaender$rev_id <- c(6,7,4,10,8,1,9,2,3,11,5,17,14,16,12,13,15)
plot(exp_coef_2)
points(exp_coef[bundeslaender$rev_id],col="red")

# Exponential fits --> OK
# Breakpoints analysis in log-space?
# Compare with spread coefficients
# Use Day of week as predictor (or seven-day weighted mean)? --> Weighted mean würde daten deutlich smoothen

library(bfast)
for (i in 2:17) {
  tmp <- bfast(ts(log(rki_bundesland_cum[i,which(rki_bundesland_cum[i,]>0)]),frequency = 1),h=0.4,season="none",max.iter = 10)
  plot(tmp,main=bundeslaender$name[i])
}


