setwd("Desktop/Privat/wirvsvirus/wirvsvirus/")
library(fields)
library(httr)
library(rlist)
library(jsonlite)
library(tidyverse)
library(glmnet)

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
bundeslaender <- data.frame(ID=unique(rki_data_landkreise$IdBundesland),name=unique(rki_data_landkreise$Bundesland),id_cross=c(7,4,10,8,1,9,2,3,11,5,17,14,16,12,13,15)); bundeslaender$name <- as.character(bundeslaender$name)
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
breakpoints <- list()
breakpoints_num <- 0
for (i in 1:16) {
  breakpoints[[i]] <- bfast(ts(log(rki_bundesland_cum[i,which(rki_bundesland_cum[i,1:51]>0)]),frequency = 1),h=0.2,season="none",max.iter = 10)
  breakpoints_num[i] <- breakpoints[[i]]$Time
  plot(breakpoints[[i]],main=bundeslaender$name[i])
}
breakpoints_num <- sapply(breakpoints_num,function(x) max(1,x,na.rm=T))

predictors_laender <- read.csv("predictors_laenderebene.csv",sep="\t")
predictors_laender$Altenquotient.2018
predictors_kreise <- read.csv("predictors_landkreise.csv",sep="\t")

fallzahlen <- as_tibble(rki_data_landkreise)
predictors_lk <- as_tibble(predictors_kreise)
preidctorsandlks <- predictors_lk %>% 
  select(-Landkreis) %>% 
  rename(IdLandkreis = Schluessel) %>% 
  inner_join(fallzahlen %>% 
               filter(IdBundesland > 0) %>% 
               mutate_at(vars(IdLandkreis), as.numeric))

# Richtung 1
predictors_kreise %>% select(-Landkreis) %>% rename(IdLandkreis = Schluessel) %>% anti_join(fallzahlen %>% filter(IdBundesland > 0) %>% mutate_at(vars(IdLandkreis), as.numeric))
# Richtung 2
anti_join(fallzahlen %>% filter(IdBundesland > 0) %>% mutate_at(vars(IdLandkreis), as.numeric) %>% distinct(IdLandkreis), predictors_lk %>% select(-Landkreis) %>% rename(IdLandkreis = Schluessel))


# Dataframe LK / BL
# ID, name, aktuelle fallzahl, wachstumrate (im prinzip aus den daten, über die du den loess smoother gelegt hast), prediktoren

wachstum_coeff <- 0
for (i in 1:16) {
  test_lm <- lm(log(y) ~ x,data=data.frame(y=rki_bundesland_cum[i,which(rki_bundesland_cum[i,1:51]>0)[-(1:breakpoints_num[i])]],x=relevant_dates[which(rki_bundesland_cum[i,1:51]>0)[-(1:breakpoints_num[i])]]))
  wachstum_coeff[i] <- test_lm$coefficients[2]
}

predictors_laender <- read.csv("predictors_laenderebene.csv",colClasses = "character",sep="\t")
predictors_landkreise <- read.csv("predictors_landkreise_mit_mobilitaet.csv",colClasses = "character")
reorder_laender <- c(1,13,3,4,2,12,11,6,7,5,10,14,15,16,9,8)
regression_data_land <- data.frame(
  id = bundeslaender$ID[reorder_laender],
  shortname = as.character(predictors_laender$Land),
  langname = as.character(bundeslaender$name)[reorder_laender],
  fallzahl = rki_bundesland_cum[reorder_laender,51]/as.numeric(predictors_laender$Einwohner.in.Tsd...Stand.31..Dez..2018.)*100,
  wachstum = wachstum_coeff[reorder_laender],
  altenquotient = as.numeric(predictors_laender$Altenquotient.2018),
  intensivbetten = as.numeric(predictors_laender$Anzahl.Intensivbetten..Tsd..Einwohner),
  dichte = as.numeric(predictors_laender$Bevölerungsdichte.1.km.2)
)

tmp_cv <- cv.glmnet(as.matrix(regression_data_land[,6:8]),regression_data_land$fallzahl)
tmp_best_lamda <- tmp_cv$lambda.min
tmp_lasso <- glmnet(as.matrix(regression_data_land[,6:8]),regression_data_land$fallzahl,alpha = 1, lambda = tmp_best_lamda)
sel_predictors <- which(as.matrix(tmp_lasso$beta) != 0)
plot(regression_data_land$fallzahl,pch=20,col="red")
points(predict(tmp_cv,newx = as.matrix(regression_data_land[,6:8])),pch=20,col="blue")
plot(regression_data_land$altenquotient,regression_data_land$fallzahl,pch=20)
cor(regression_data_land$altenquotient,regression_data_land$fallzahl)
tmp_lasso$beta
cor(as.numeric(regression_data_land$fallzahl),predict(tmp_lasso,newx = as.matrix(regression_data_land[,6:8])))


tmp_cv <- cv.glmnet(as.matrix(regression_data_land[,6:8]),regression_data_land$wachstum)
tmp_best_lamda <- tmp_cv$lambda.min
tmp_lasso <- glmnet(as.matrix(regression_data_land[,6:8]),regression_data_land$wachstum,alpha = 1, lambda = tmp_best_lamda)
sel_predictors <- which(as.matrix(tmp_lasso$beta) != 0)
plot(regression_data_land$wachstum,pch=20,col="red")
points(predict(tmp_cv,newx = as.matrix(regression_data_land[,6:8])),pch=20,col="blue")
plot(regression_data_land$altenquotient,regression_data_land$wachstum,pch=20)
cor(regression_data_land$altenquotient,regression_data_land$wachstum)
tmp_lasso$beta
cor(as.numeric(regression_data_land$wachstum),predict(tmp_lasso,newx = as.matrix(regression_data_land[,6:8])))


# [1] "Landkreis"                               "Meldedatum"                              "IdLandkreis"                            
# [4] "sum_LK"                                  "csum"                                    "Wert"                                   
# [7] "Fläche"                                  "Bevölkerung(2018)"                       "männlich"                               
# [10] "weiblich"                                "Anteil Alter0-17 in (%)"                 "Anteil Alter18-24 in (%)"               
# [13] "Anteil Alter25-44 in (%)"                "Anteil Alter25-64 in (%)"                "Anteil Alter65-alt in (%)"              
# [16] "Krankenhausbetten/1000 Einwohner"        "Einkommen"                               "Raumtyp"                                
# [19] "Regiontyp"                               "Raumtyp_Name"                            "Personenkilometer pro Einwohner pro Tag"
# [22] "Anteil unter 3 Wege pro Tag"             "Anteil 3 oder mehr Wege pro Tag"         "csum_LK_pro_1kEinwohner"                
# [25] "gr"                 
predictors <- c(8:18,21:23)
tmp_cv <- cv.glmnet(as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors])),as.numeric(LK_dat_proc_filtered$csum_LK_pro_1kEinwohner*100))
tmp_best_lamda <- tmp_cv$lambda.min
tmp_lasso <- glmnet(as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors])),as.numeric(LK_dat_proc_filtered$csum_LK_pro_1kEinwohner*100),alpha = 1, lambda = tmp_best_lamda)
sel_predictors <- which(as.matrix(tmp_lasso$beta) != 0)
plot(as.numeric(LK_dat_proc_filtered$csum_LK_pro_1kEinwohner*100),pch=20,col="red")
points(predict(tmp_lasso,newx = as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors]))),pch=20,col="blue")
#plot(regression_data_land$altenquotient,regression_data_land$fallzahl,pch=20)
#cor(regression_data_land$altenquotient,regression_data_land$fallzahl)
cor(as.numeric(LK_dat_proc_filtered$csum_LK_pro_1kEinwohner*100),predict(tmp_lasso,newx = as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors]))))
tmp_lasso$beta

predictors <- c(8:18,21:23)
ref_data <- LK_dat_proc_filtered$csum_LK_pro_1kEinwohner*100
ref_data <- LK_dat_proc_filtered$gr
tmp_cv <- cv.glmnet(as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors])),as.numeric(ref_data))
tmp_best_lamda <- tmp_cv$lambda.min
tmp_lasso <- glmnet(as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors])),as.numeric(ref_data),alpha = 1, lambda = tmp_best_lamda)
sel_predictors <- which(as.matrix(tmp_lasso$beta) != 0)
plot(as.numeric(ref_data),pch=20,col="red")
points(predict(tmp_lasso,newx = as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors]))),pch=20,col="blue")
#plot(regression_data_land$altenquotient,regression_data_land$fallzahl,pch=20)
#cor(regression_data_land$altenquotient,regression_data_land$fallzahl)
cor(as.numeric(ref_data),predict(tmp_lasso,newx = as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors]))))
tmp_lasso$beta
cor(as.numeric(ref_data),as.numeric(LK_dat_proc_filtered$Einkommen))

cor(as.numeric(ref_data),as.numeric(LK_dat_proc_filtered$`Anteil Alter18-24 in (%)`))
cor(as.numeric(ref_data),as.numeric(LK_dat_proc_filtered$`Anteil Alter65-alt in (%)`))
sapply(c(8:18,21:23),function(x) cor(as.numeric(ref_data),c(as.matrix(as.data.frame(LK_dat_proc_filtered[,x])))))

plot(c(as.matrix(as.data.frame(LK_dat_proc_filtered[,17]))),as.numeric(ref_data),pch=20,cex=0.2)
lines(c(as.matrix(as.data.frame(LK_dat_proc_filtered[,17]))),predict(lm(y~x,data.frame(x=c(as.matrix(as.data.frame(LK_dat_proc_filtered[,17]))),y=ref_data))))

plot(c(as.matrix(as.data.frame(LK_dat_proc_filtered[,15]))),as.numeric(ref_data),pch=20,cex=0.2)
lines(c(as.matrix(as.data.frame(LK_dat_proc_filtered[,15]))),predict(lm(y~x,data.frame(x=c(as.matrix(as.data.frame(LK_dat_proc_filtered[,15]))),y=ref_data))))

colnames(LK_dat_proc_filtered[c(8:18,21:23)])

kreis_predictornames <- colnames(LK_dat_proc_filtered[c(8:18,21:23)])
kreis_predictors_fallzahlen <- tmp_lasso$beta
kreis_cor_regression_fallzahlen <- cor(as.numeric(ref_data),predict(tmp_lasso,newx = as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors]))))
kreis_predictors_gr <- tmp_lasso$beta
kreis_cor_regression_gr <- cor(as.numeric(ref_data),predict(tmp_lasso,newx = as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors]))))
land_predictornames <- colnames(regression_data_land[,6:8])
land_predictors_fallzahlen <- tmp_lasso$beta
land_cor_regression_fallzahlen <- cor(as.numeric(regression_data_land$fallzahl),predict(tmp_lasso,newx = as.matrix(regression_data_land[,6:8])))
land_predictors_gr <- tmp_lasso$beta
land_cor_regression_gr <- cor(as.numeric(regression_data_land$wachstum),predict(tmp_lasso,newx = as.matrix(regression_data_land[,6:8])))

save(regression_data_land,file="regression_data_laender.RData")
save(kreis_predictornames,land_predictornames,land_predictors_fallzahlen,land_cor_regression_fallzahlen,land_predictors_gr,land_cor_regression_gr,kreis_predictors_fallzahlen,kreis_cor_regression_fallzahlen,kreis_predictors_fallzahlen,kreis_predictors_gr,kreis_cor_regression_gr,file="glasso_results_03_26.RData")

# ----------- TEST LANDATLAS ---------
landatlas <- read.csv("landatlas_byGenderAndAgeGroup.csv")
landatlas$IdLandkreis <- landatlas$KreisID
LK_dat_proc <- LK_dat_proc %>%
   inner_join(landatlas, by = 'IdLandkreis') %>%
   ungroup()

predictors <- c(8:18,21:23,28:32,34:38,40:47,49:60,62:64,66:72,75:77,79,81,83:118)
ref_data <- LK_dat_proc_filtered$csum_LK_pro_1kEinwohner*100
ref_data <- LK_dat_proc_filtered$gr
tmp_cv <- cv.glmnet(as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors])),as.numeric(ref_data))
tmp_best_lamda <- tmp_cv$lambda.min
tmp_lasso <- glmnet(as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors])),as.numeric(ref_data),alpha = 1, lambda = tmp_best_lamda)
sel_predictors <- which(as.matrix(tmp_lasso$beta) != 0)
plot(as.numeric(ref_data),pch=20,col="red")
points(predict(tmp_lasso,newx = as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors]))),pch=20,col="blue")
#plot(regression_data_land$altenquotient,regression_data_land$fallzahl,pch=20)
#cor(regression_data_land$altenquotient,regression_data_land$fallzahl)
cor(as.numeric(ref_data),predict(tmp_lasso,newx = as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors]))))
tmp_lasso$beta
cor(as.numeric(ref_data),as.numeric(LK_dat_proc_filtered$Einkommen))


Soziooekonomische_Lage
Schulabg_Hochschulreife
Schulabg_ohne
Laendlichkeit
SuV_Veraenderung
Breitband_GemVB
Wohnungsleerstand
Natuerliche_Bev_Entwicklung
Grundschule

#33,39,48,61,65,73,74,78,80

test_cor <- t(as.matrix(sapply(c(8:18,21:23,28:32,34:38,40:47,49:60,62:64,66:72,75:77,79,81,83:118),function(x) cor(as.numeric(ref_data),c(as.matrix(as.data.frame(LK_dat_proc_filtered[,x])))))))
colnames(test_cor) <- colnames(LK_dat_proc_filtered[,predictors])
test_cor

# --------- Temporal evoluation of predictors ---------------

#ref_dates <- c(as.Date('2020-03-12'),as.Date('2020-03-19'),as.Date('2020-03-26'),as.Date('2020-04-02'))
ref_dates <- as.Date('2020-03-12'):as.Date('2020-04-02')
predictors <- c(8:18,21:23)
cor_zahlen <- cor_gr <- 0
lasso_zahlen <- lasso_gr <- array(0,dim=c(length(ref_dates),length(predictors)))
mean_gr <- 0

for (i in 1:length(ref_dates)) {
  end_date <- ref_dates[i]
  LK_dat_csum_filt <- LK_dat_csum_ma %>% filter(Meldedatum <= end_date) #%>% filter(Meldedatum > end_date-14)
  LK_dat_csum_filt_gr <- LK_dat_csum_filt %>%
    group_by(IdLandkreis) %>%
    nest() %>%
    mutate(lm = purrr::map(.$data, function(d) lm(y ~ x, rename(d, x = Meldedatum, y = csum_ma) %>% mutate(y = log(y))))) %>% 
    mutate(gr = purrr::map(.$lm, function(l) coef(l)[2])) %>% 
    select(-data,-lm) %>% 
    unnest(gr)
  # add to processed data
  LK_dat_proc_tmp <- LK_dat_proc %>% 
    inner_join(LK_dat_csum_filt_gr) %>% filter(csum_LK_pro_1kEinwohner > 0.05) %>% filter(gr > 0)
  LK_dat_proc_filtered <- LK_dat_proc_tmp %>% filter(Meldedatum == end_date)
  
  mean_gr[i] <- mean(LK_dat_proc_filtered$gr)
  
  ref_data <- LK_dat_proc_filtered$csum_LK_pro_1kEinwohner*100
  tmp_cv <- cv.glmnet(as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors])),as.numeric(ref_data))
  tmp_best_lamda <- tmp_cv$lambda.min
  tmp_lasso <- glmnet(as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors])),as.numeric(ref_data),alpha = 1, lambda = tmp_best_lamda)
  sel_predictors <- which(as.matrix(tmp_lasso$beta) != 0)
  # plot(as.numeric(ref_data),pch=20,col="red",main=paste(end_date,"Fallzahlen"))
  # points(predict(tmp_lasso,newx = as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors]))),pch=20,col="blue")
  cor_zahlen[i] <- cor(as.numeric(ref_data),predict(tmp_lasso,newx = as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors]))))
  lasso_zahlen[i,] <- sign(as.matrix(tmp_lasso$beta))
  
  ref_data <- exp(LK_dat_proc_filtered$gr)-1
  tmp_cv <- cv.glmnet(as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors])),as.numeric(ref_data))
  tmp_best_lamda <- tmp_cv$lambda.min
  tmp_lasso <- glmnet(as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors])),as.numeric(ref_data),alpha = 1, lambda = tmp_best_lamda)
  sel_predictors <- which(as.matrix(tmp_lasso$beta) != 0)
  # plot(as.numeric(ref_data),pch=20,col="red",main=paste(end_date,"GR"))
  # points(predict(tmp_lasso,newx = as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors]))),pch=20,col="blue")
  cor_gr[i] <- cor(as.numeric(ref_data),predict(tmp_lasso,newx = as.matrix(as.data.frame(LK_dat_proc_filtered[,predictors]))))
  lasso_gr[i,] <- sign(as.matrix(tmp_lasso$beta))
}

plot(cor_gr)
plot(cor_zahlen)

par(mar=c(3,12,2,0.5))
image(ref_dates,1:length(predictors),lasso_gr,col=c("green","white","red"),yaxt="n",ylab="",xlab="",main="Wachstumsraten")
axis(side=2,at=1:length(predictors),labels = colnames(LK_dat_proc_filtered[,predictors]),las=1)

image(ref_dates,1:length(predictors),lasso_zahlen,col=c("darkgreen","white","darkred"),yaxt="n",ylab="",xlab="",main="Fallzahlen")
axis(side=2,at=1:length(predictors),labels = colnames(LK_dat_proc_filtered[,predictors]),las=1)

cor()

plot(mean_gr)

