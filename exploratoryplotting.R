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
  filter(Landkreis %in% c('LK Düren', 'SK Köln', 'LK Heinsberg', 'StadtRegion Aachen'))
  #filter(max(csum) > 120 | Landkreis == 'LK Düren')

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

source('data_processing.R')
ref_data <- LK_dat_proc_filtered$csum_LK_pro_1kEinwohner*100
ref_data <- LK_dat_proc_filtered$gr
sapply(c(8:18,21:23),function(x) cor(as.numeric(ref_data),c(as.matrix(as.data.frame(LK_dat_proc_filtered[,x])))))
load('glasso_results.RData')

# arrow plot
dat <- kreis_predictors_fallzahlen[,1] %>% as.list() %>% as_tibble() %>% gather() %>% 
  filter(key %in% c('Bevölkerung(2018)','Anteil Alter18-24 in (%)','Anteil Alter65-alt in (%)','Einkommen','Personenkilometer pro Einwohner pro Tag','Krankenhausbetten/1000 Einwohner')) %>% 
  mutate(sgn = case_when(value > 0 ~ 1, value == 0 ~ 0, value < 0 ~ -1))
dat$key <- c('Einwohner', 'Alter\n18-24', 'Alter\n> 65', 'Krankenhaus-\nbetten\npro 1000\nEinwohner', 'Einkommen', 'Personen-\nkilometer\npro Tag')

ggplot(dat) + 
  geom_segment(aes(y = 0, yend = sgn, x = key, xend = key, color = as.character(sgn)), arrow = arrow(angle = 45, type = 'closed', length = unit(1.2,'cm')), size = 10) + 
  scale_color_manual(values = c('grey50', '#ED3537', '#1494E9'), guide = FALSE) + 
  theme_bw() + 
  theme(axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid = element_blank(),
        text = element_text(size = 20, face = 'bold'))

ggsave('plots/arrow_plot_lasso.pdf', width = 25, height = 12, units = 'cm')
ggsave('plots/arrow_plot_lasso.png', width = 25, height = 12, units = 'cm')

load('regression_data_laender.RData')

# scatter plot
dat <- regression_data_land %>% 
  as_tibble() %>% 
  select(fallzahl, altenquotient)

plt1 <- ggplot(dat, aes(x = altenquotient, y = fallzahl)) + 
  geom_point(size = 7, color = '#0D68C3', shape = '\u2716') + 
  geom_smooth(method = 'lm', formula = y~x) + 
  labs(x = 'Altenquotient', y = 'Fallzahl pro\n100.000 Einwohner') + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        text = element_text(size = 20, face = 'bold'))

dat <- LK_dat_proc_filtered %>% 
  select(csum_LK_pro_1kEinwohner, Einkommen)

plt2 <- ggplot(dat, aes(x = Einkommen/1000, y = csum_LK_pro_1kEinwohner * 100)) + 
  geom_point(size = 2, color = '#0D68C3', shape = '\u2716') + 
  geom_smooth(method = 'lm', formula = y~x) + 
  labs(x = 'Einkommen/\n1000 Euro') + 
  theme_bw() + 
  theme(panel.grid = element_blank(), axis.title.y = element_blank(),
        text = element_text(size = 20, face = 'bold'))

plt <- cowplot::plot_grid(plt1,plt2, align = 'hv', axis = 'tblr')
ggsave(plot = plt, filename = 'plots/scatter_plot_predictors.png', width = 25, height = 12, units = 'cm')
  
