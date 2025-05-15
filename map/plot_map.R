library(tidyverse)
library(sf)
library(here)
library(ggplot2)
library(cshapes)
library(magrittr)


worldmap <- here('data', 'map', 'cntry1880.shp') %>% st_read()


# ggplot() + geom_sf(data = europe_cropped) + theme_bw()

# ggplot() + geom_sf(data = worldmap) +
#   coord_sf(xlim = c(-20, 45), ylim = c(35, 73), expand = FALSE) +
#   theme_bw()


#Make sure country codes are consistent with LMU data
countrycodes = c("AUTHUN", "BEL", "DNK", "FIN", "FRA", "DEU", "GRC", "ITA", "NLD", "NOR", "PRT", "ESP", "SWE", "CHE", "GBR")

countrynames = c("Austria Hungary", "Belgium", "Denmark", "Finland", "France", 
                 "Germany", "Greece", "Italy", "Netherlands", "Norway", "Portugal",
                 "Spain", "Sweden", "Switzerland", "United Kingdom")

for (i in 1:length(countrynames)){
  worldmap[worldmap$NAME == countrynames[i], "WB_CNTRY"] = countrycodes[i]
}

worldmap %<>% rename(iso_o = WB_CNTRY)

lmu = read_csv(here::here("causal", "replicating_timini_paper", "timini_tradehist_currencydummies.csv")) %>% 
  filter(iso_d == "GBR", year == 1870) %>% select(iso_o, start_o, end_o, LMU_o)




worldmap %<>% left_join(lmu, by="iso_o")

Bulgaria = worldmap %>% filter(NAME == 'Bulgaria')
Bulgaria$NAME = 'Bulg'
worldmap %<>% rbind(Bulgaria)
worldmap[[9]][[140]][[1]] = NULL # Crete was not part of Bulgaria at the time!


country = c('Lombardy', "Bulgaria", 'Romania', 'Serbia', 'Norway-Sweden')

start = c(1865, 1880, 1867, 1878, 1868)

# country = c('Lombardy','Romania', 'Serbia', 'Norway-Sweden')
# # 
# start = c(1865, 1880, 1867, 1878, 1868)



for (i in 1:length(countrynames)){
  worldmap[worldmap$NAME == country[i], "start_o"] = start[i]
}

worldmap$start_o = as.factor(worldmap$start_o)
ggplot() + geom_sf(data = worldmap, colour = "black") + aes(fill = start_o) + theme_void(base_size = 20) + 
  theme(legend.position="bottom", legend.direction = "horizontal",
          axis.line = element_blank(), 
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(), # optional - remove gridlines
          panel.grid.minor = element_blank() # optional - remove gridlines
        ) + 
  coord_sf(xlim = c(-13, 45), ylim = c(30, 73), expand = FALSE) +
  labs(fill="") +  
  scale_fill_grey(start = 0.8, end = 0.4, na.value = "white", na.translate=FALSE) + 
  guides(fill = guide_legend(nrow = 1))

ggsave(here('data', 'map', "europe_1880.png"), width = 5000, height = 3000, units = "px")

# worldmap = cshp(date=as.Date("1886/1/1"), useGW = FALSE)
# 
# ggplot() + geom_sf(data = worldmap) +
#     coord_sf(xlim = c(-20, 45), ylim = c(35, 73), expand = FALSE) +
#     theme_bw()