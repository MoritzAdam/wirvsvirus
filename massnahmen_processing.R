library(googlesheets4)
library(googledrive)
library(tidyverse)
library(readr)
library(readxl)
#drive_auth()
#sheets_auth(token = drive_token())

if(drive_has_token()==FALSE){
  print("No authentification with googledrive set up so far, please use drive_auth() and sheets_auth(token = drive_token())")
}

if (drive_has_token()){

setwd("/home/bea/Documents/WirVsVirus") 

response <- read_sheet("https://docs.google.com/spreadsheets/d/1J8KhzH5UwtVdXEViRASh1SVCpJSbicCpLb6Bedvf4JA/edit?usp=sharing") %>% 
  select(matches("Wann wurden die Maßnahmen|Postleitzahl|Um welche Maßnahme|Erzähle uns mehr|wieder aufgehoben|Wann wurde die Maßnahme aufgehoben")) %>% rename(wann=1, plz=2, was=3, info=4, aufgehoben=5, wann_aufgehoben=6)

gverzeichnis.lookup <- read_delim(paste0(getwd(), "/wirvsvirus/data_landkreise/gemeindeverzeichnis.csv"), ";", escape_double = FALSE, trim_ws = TRUE)  %>% 
  select(matches("Amtl.Gemeindeschlüssel|PLZ Ort")) %>% rename(schlüssel=1, plz_ort=2)

gverzeichnis <- tibble(
  plz = as.numeric(unlist(regmatches(gverzeichnis.lookup$plz_ort, gregexpr("[[:digit:]]+", gverzeichnis.lookup$plz_ort)))),
  IdLandkreis = gverzeichnis.lookup$schlüssel %>% substr(1, 5)
)

response %>% left_join(gverzeichnis) %>% filter(was != "sonstiges")

}


