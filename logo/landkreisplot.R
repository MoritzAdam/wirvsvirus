library(tidyverse)
library(sf)
# data from https://public.opendatasoft.com/explore/dataset/landkreise-in-germany/information/

lk_sf <- read_sf(dsn = 'logo/landkreise-in-germany.shp') %>% 
  filter(name_2 %in% c('Rhein-Neckar-Kreis', 'Heidelberg'))
lk_sf$fill <- c('grey85', 'grey65')

plt <- ggplot(lk_sf) + 
  geom_sf(aes(fill = fill), color = 'grey30', size = 0.5) + 
  scale_fill_manual(values = lk_sf$fill, guide = FALSE) + 
  coord_sf() + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.background = element_blank(), panel.background = element_blank())
plt
ggsave('logo/RN_HD.pdf', width = 15, height = 15, units = 'cm')
