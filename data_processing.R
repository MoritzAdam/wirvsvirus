source('data_queries.R')

LK_dat <- query_arcgis_all()

LK_dat_csum <- LK_dat %>%
  group_by(Landkreis, Meldedatum, IdLandkreis) %>% 
  summarise(sum_LK = sum(AnzahlFall)) %>% 
  group_by(Landkreis) %>% 
  arrange(Meldedatum, .by_group = T) %>% 
  mutate(csum = cumsum(sum_LK)) %>% 
  mutate_at(vars(IdLandkreis), as.numeric) %>%
  mutate_at(vars(Meldedatum), as.Date) %>% 
  ungroup()
  #filter(max(csum) > 120 | Landkreis == 'LK Düren')

LK_pred <- read_delim('predictors_landkreise_mit_mobilitaet.csv', delim = ",") %>% 
  as_tibble() %>% 
  select(-Name) %>% 
  rename(IdLandkreis = Schluessel) %>% 
  mutate(!!sym('Bevölkerung(2018)') := str_replace_all(!!sym('Bevölkerung(2018)'),' ','')) %>% 
  mutate_at(vars(IdLandkreis, !!sym('Bevölkerung(2018)')), as.numeric) %>% 
  mutate(!!sym('männlich') := str_replace_all(!!sym('männlich'),' ','')) %>% 
  mutate_at(vars(IdLandkreis, !!sym('männlich')), as.numeric) %>% 
  mutate(!!sym('weiblich') := str_replace_all(!!sym('weiblich'),' ','')) %>% 
  mutate_at(vars(IdLandkreis, !!sym('weiblich')), as.numeric)

LK_dat_proc <- LK_dat_csum %>% 
  group_by(IdLandkreis, Meldedatum) %>% 
  inner_join(LK_pred, by = 'IdLandkreis') %>% 
  mutate(csum_LK_pro_1kEinwohner = 1e3*csum/!!sym('Bevölkerung(2018)')) %>% 
  ungroup()

# filtering for specific date
LK_dat_proc_filtered <- LK_dat_proc %>% 
  filter(Meldedatum == as.Date('2020-03-19')) 
# cases per Einwohner
LK_dat_proc_filtered$csum_LK_pro_1kEinwohner


# growth rates based on 3-day-rolling mean and excluding first and last 3 days
LK_dat_con <- LK_dat_csum %>% 
  select(Landkreis, Meldedatum) %>% 
  group_by(Landkreis) %>% 
  summarise(datmn = min(Meldedatum), datmx = max(Meldedatum)) %>% 
  mutate(datsq = purrr::map2(.$datmn, .$datmx, function(x,y) seq.Date(as.Date(x), as.Date(y), 1))) %>% 
  mutate(datmn = datmn + 3, datmx = datmx - 3) %>% 
  unnest() %>% 
  mutate(use = if_else(datsq >= datmn & datsq <= datsq, T, F)) %>% 
  rename(Meldedatum = datsq)

LK_dat_csum_ma <- LK_dat_csum %>% 
  right_join(., LK_dat_con) %>% 
  mutate(csum_ma = zoo::rollapply(csum,3,mean,fill = NA,na.rm=T)) %>% 
  filter(!is.na(csum_ma))

LK_dat_csum_ma_gr <- LK_dat_csum_ma %>%
  group_by(IdLandkreis) %>%
  nest() %>%
  mutate(lm = purrr::map(.$data, function(d) lm(y ~ x, rename(d, x = Meldedatum, y = csum_ma) %>% mutate(y = log(y))))) %>% 
  mutate(gr = purrr::map(.$lm, function(l) coef(l)[2])) %>% 
  select(-data,-lm) %>% 
  unnest(gr)

# add to processed data
LK_dat_proc <- LK_dat_proc %>% 
  inner_join(LK_dat_csum_ma_gr)
# growth rates filtering
 LK_dat_proc %>% filter(gr > 0) %>% .$gr %>% hist()
 LK_dat_proc %>% filter(csum_LK_pro_1kEinwohner > 0.05) %>% filter(gr > 0) %>% .$gr %>% hist()
 LK_dat_proc_filtered <- LK_dat_proc %>% filter(Meldedatum == as.Date('2020-03-19')) 
 