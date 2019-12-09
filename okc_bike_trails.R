library(tidyverse)
library(hubbeR)
library(lubridate)
library(here)
library(osmdata)

quartzFonts(
  baskerville = c("Baskerville Regular", "Baskerville Bold", "Baskerville Italic", "Baskerville Bold Italic")
)

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
     file = here("streets", "road_data.Rdata"))

gp <- ggplot() +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = ghColorSelect("grey-400"),
          size = .3,
          alpha = .6) +
  geom_sf(
    data = streets$osm_lines,
    inherit.aes = FALSE,
    color = ghColorSelect("grey"),
    size = 0.4,
    alpha = 0.8
  ) +
  geom_sf(data = bike_roads$osm_lines,
          inherit.aes = FALSE,
          color = ghColorSelect("blue"),
          size = 0.9,
          alpha = 0.9) +
  coord_sf(xlim = c(-97.75, -97.3), 
           ylim = c(35.3, 35.63),
           expand = FALSE) +
  annotate("text", x = -97.525, y = 35.34, 
           label = "Oklahoma City Bike Trails",
           family = "Copperplate Light",
           size = 16) +
  theme_void() +
  nowt()
gp
ggsave(here("streets", str_c(city, ".png")), plot = gp, width = 10, height = 10, dpi = 400)
