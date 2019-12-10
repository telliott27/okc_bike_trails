library(tidyverse)
library(here)
library(osmdata)

city <- "Oklahoma City Oklahoma"

cycle_tags <- available_tags("cycleway")

streets <- getbb(city) %>% 
  opq() %>% 
  add_osm_feature(
    key = "highway",
    value = c("motorway", "primary", "secondary", "tertiary")
  ) %>% 
  osmdata_sf()

small_streets <- getbb(city) %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

highway_cycles <- getbb(city) %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("cycleway")) %>%
  osmdata_sf()
cycleways <- getbb(city) %>% 
  opq() %>% 
  add_osm_feature(key = "cycleway",
                  value = cycle_tags) %>% 
  osmdata_sf()
bicycle <- getbb(city) %>% 
  opq() %>% 
  add_osm_feature(key = "bicycle") %>% 
  osmdata_sf()
bike_roads <- c(highway_cycles, cycleways, bicycle)

save(bike_roads, streets, small_streets,
     file = here("road_data.Rdata"))

gp <- ggplot() +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#949DA5",
          size = .3,
          alpha = .6) +
  geom_sf(
    data = streets$osm_lines,
    inherit.aes = FALSE,
    color = "#6A737D",
    size = 0.4,
    alpha = 0.8
  ) +
  geom_sf(data = bike_roads$osm_lines,
          inherit.aes = FALSE,
          color = "#0366D6",
          size = 0.9,
          alpha = 0.9) +
  coord_sf(xlim = c(-97.8, -97.25), 
           ylim = c(35.3, 35.63),
           expand = FALSE) +
  annotate("label", x = -97.525, y = 35.34, 
           label = "Oklahoma City Bike Trails",
           family = "Copperplate Light",
           color = "#054289",
           size = 16) +
  theme_void() +
  nowt()
gp
ggsave(here(str_c(city, ".png")), plot = gp, width = 10, height = 8, dpi = 400)
