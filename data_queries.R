library(httr)
library(rlist)
library(jsonlite)
library(tidyverse)

#' helper function to download daily RKI data for "Landkreise" and format them as a tibble
#'
#' @param n_entries check n_entries here https://www.arcgis.com/home/item.html?id=dd4580c810204019a7b8eb3e0b329dd6&view=list#data for the current moment
#' @param batch_size check batch size here https://services7.arcgis.com/mOBPykOjAyBO2ZKk/ArcGIS/rest/services/RKI_COVID19/FeatureServer/0
#' @param dir path to save data
#'
#' @return tibble
#' @export
#'
#' @examples dat <- query_arcgis_all()
query_arcgis_all <- function(n_entries = 8919,
                             batch_size = 2000,
                             force_download = FALSE,
                             dir = './data_landkreise') {
  n_batch <- seq(0, plyr::round_any(n_entries, 1e3), by = batch_size)
  # check if data already queried
  time <- Sys.time() %>% 
    as.Date()
  time <- time - 1
  file <- file.path(dir,paste0('data_landkreise','_',time,'.csv'))
  
  if(file.exists(file) & !force_download) {
    print('read from file')
    dat <- readr::read_csv(file, 
                           col_types = list(
                             X1 = col_double(),
                             id = col_double(),
                             IdBundesland = col_double(),
                             Bundesland = col_character(),
                             Landkreis = col_character(),
                             Altersgruppe = col_character(),
                             Geschlecht = col_character(),
                             AnzahlFall = col_double(),
                             AnzahlTodesfall = col_double(),
                             ObjectId = col_double(),
                             Meldedatum = col_datetime(format = ""),
                             IdLandkreis = col_character()
                           )
                           ) %>% 
      as_tibble()
  } else {
    print('query database')
    arcgis_url <- 'https://services7.arcgis.com/mOBPykOjAyBO2ZKk/ArcGIS/rest/services/RKI_COVID19/FeatureServer/0/'
  
    dat <- lapply(n_batch, function(n_b){
      qloc <- paste0(arcgis_url,
                     'query?where=ObjectId+>+0&objectIds=&time=&resultType=none&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=',
                     as.character(n_b),'&resultRecordCount=&sqlFormat=standard&f=pgeojson&token='
                     )
      resp <- GET(qloc)
      if (http_error(resp)) stop('http error')
      #resp <- content(resp, as = 'parsed', type = 'application/json')
      resp <- content(resp, as = 'text', type = 'application/json', encoding = 'UTF-8') %>% 
        fromJSON(.)
      resp <- resp$features %>% 
        as_tibble()
      resp <- bind_cols(tibble(id = resp$id), resp$properties)
    }) %>% 
      bind_rows()
    dat$Meldedatum <- as.POSIXct(dat$Meldedatum/1000, origin = "1970-01-01")
    date <- max(dat$Meldedatum) %>% 
      as.Date()
    if (!dir.exists(dir)) dir.create(dir)
    file <- file.path(dir,paste0('data_landkreise','_',date,'.csv'))
    write.csv(x = dat, file = file)
  }
  return(dat)
}

