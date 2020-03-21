setwd("Desktop/Privat/wirvsvirus/wirvsvirus/")
test <- readxl::read_xlsx("Risklayer Kreisebene Quellen - Studie 21032020 0330.xlsx")
colnames(test)
bundesland_ts <- read.csv("RKI_ts_bundesland/data/covid19-germany.csv",colClasses = c("numeric","character","numeric","numeric","Date"))
plot(test$date,test$infections)
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



# Exponential fits
# Breakpoints analysis in log-space?
# Compare with spread coefficients
# Use Day of week as predictor (or seven-day weighted mean)? --> Weighted mean würde daten deutlich smoothen