source('data_queries.R')

# plot and fit cummulative growth of Covid19 infections
# for Bundesländer & Landkreise
LK_dat <- query_arcgis_all()

BL_dat_csum <- LK_dat %>% 
  group_by(Bundesland, Meldedatum) %>% 
  summarise(sum_BL = sum(AnzahlFall)) %>% 
  arrange(Meldedatum, .by_group = T) %>% 
  mutate(csum = cumsum(sum_BL))

ggplot(BL_dat_csum, 
       aes(x = Meldedatum, y = csum)) + 
  geom_line() + geom_point() + 
  geom_smooth(data = BL_dat %>% filter(Meldedatum < as.Date('2020-03-19'))) +
  facet_wrap(~ Bundesland, scales = 'free') + 
  scale_y_log10()

LK_selection <- c('LK Heinsberg', 'LK Tirschenreuth', 'LK Starnberg')

LK_dat_sel_csum <- LK_dat %>%
  #filter(Landkreis %in% LK_selection) %>% 
  group_by(Landkreis, Meldedatum) %>% 
  summarise(sum_LK = sum(AnzahlFall)) %>% 
  arrange(Meldedatum, .by_group = T) %>% 
  group_by(Landkreis) %>% 
  mutate(csum = cumsum(sum_LK)) %>% 
  filter(max(csum) > 120 | Landkreis == 'LK Düren')

LK_dat_con <- LK_dat_sel_csum %>% 
  select(Landkreis, Meldedatum) %>% 
  group_by(Landkreis) %>% 
  summarise(datmn = min(Meldedatum), datmx = max(Meldedatum)) %>% 
  mutate(datsq = purrr::map2(.$datmn, .$datmx, function(x,y) seq.Date(as.Date(x), as.Date(y), 1))) %>% 
  mutate(datmn = datmn + 3, datmx = datmx - 3) %>% 
  unnest() %>% 
  mutate(use = if_else(datsq >= datmn & datsq <= datsq, T, F)) %>% 
  rename(Meldedatum = datsq)

LK_dat_sel_csum_ma <- LK_dat_sel_csum %>% 
  mutate_at(vars(Meldedatum), as.Date) %>% 
  right_join(., LK_dat_con) %>% 
  mutate(csum_ma = zoo::rollapply(csum,3,mean,fill = NA,na.rm=T)) %>% 
  filter(!is.na(csum_ma))

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

ggplot(LK_dat_sel_csum_ma,
       aes(x = Meldedatum, y = csum_ma)) + 
  geom_line() + geom_point() + 
  geom_smooth(data = LK_dat_sel_csum_ma %>% filter(use == T)) + #, method = 'lm') + #filter(Meldedatum < as.Date('2020-03-18'))) +
  #geom_text(x = 0, y = 100, mapping = aes(label = lm_lab), parse = T, 
  #          data = LK_dat_sel_csum_ma %>% group_by(Landkreis) %>% nest() %>% mutate(lm_lab = purrr::map(.$data, function(d) lm_eqn(rename(d, x = Meldedatum, y = csum_ma) %>% mutate(y = log(y))))) %>% unnest(lm_lab) %>% select(-data)) + 
  facet_wrap(~ Landkreis, scales = 'free') + 
  scale_y_log10()

