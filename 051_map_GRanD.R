library(tidyverse)
library(sf)
library(mapview)

#### Load data ----
existing_dams <- readRDS("outputs/rds/existing_dams_scenarios.rds") %>%
  mutate(CapacityCex = abs(sqrt(Capacity))/10) # for visualization purposes only


# Maps ----
mapviewOptions(legend.pos = "bottomleft")
mapviewOptions(fgb = FALSE)

my_col.regions <- colorRampPalette(scales::brewer_pal(type = "div", palette = "RdYlBu", direction = -1)(9))
legendBreaks <- c(-1.6, -1.2, -0.8, -0.4, -0.2, 0.2, 0.4, 0.8, 1.2, 1.6)


#### 1. GRanD & Change in Scarcity (Pessimistic 2050) ----
mapview(existing_dams, layer.name = 
          "Existing dams under<br/>pessimistic scenario for 2050<br/>–––––––––––––––––––––––––––<br/>Change in scarcity risk (color)<br/>Reservoir capacity (size)",
        zcol = "RC1_P50rc", cex = existing_dams$CapacityCex, label = existing_dams$Name,  
        col.regions = my_col.regions,
        at = legendBreaks,
        alpha.regions = 0.8,
        lwd = 0.1,
        viewer.suppress = FALSE,
        popup = leafpop::popupTable(existing_dams, zcol = c("Name", "Capacity", "Country", "Basin", "RC1", "RC1_P50", "RC1_P50rc"))
) %>% 
  leafem::addLogo( 
    #"https://panda.maps.arcgis.com/sharing/rest/content/items/eaf9bbe5243d4dca8073e3517517bd71/data", src = "remote", # Black
    "https://panda.maps.arcgis.com/sharing/rest/content/items/203af4ae947245948ad0958bc2c7ea9c/data", src = "remote", # White
    position = "bottomleft",
    offset.x = 20,
    offset.y = 330,
    width = 200,
    height = 40.5) %>%
    leaflet::setView(10, 20, 3)


#### 2. GRanD & Change in Flooding (Pessimistic 2050) ----
mapview(existing_dams, layer.name = 
          "Existing dams under<br/>pessimistic scenario for 2050<br/>–––––––––––––––––––––––––––<br/>Change in flood risk (color)<br/>Reservoir capacity (size)",
        zcol = "RC2_P50rc", cex = existing_dams$CapacityCex, label = existing_dams$Name,  
        col.regions = my_col.regions,
        at = legendBreaks,
        alpha.regions = 0.8,
        lwd = 0.1,
        viewer.suppress = FALSE,
        popup = leafpop::popupTable(existing_dams, zcol = c("Name", "Capacity", "Country", "Basin", "RC2", "RC2_P50", "RC2_P50rc"))
) %>% 
  leafem::addLogo( 
  #"https://panda.maps.arcgis.com/sharing/rest/content/items/eaf9bbe5243d4dca8073e3517517bd71/data", src = "remote", # Black
  "https://panda.maps.arcgis.com/sharing/rest/content/items/203af4ae947245948ad0958bc2c7ea9c/data", src = "remote", # White
  position = "bottomleft",
  offset.x = 20,
  offset.y = 330,
  width = 200,
  height = 40.5) %>%
  leaflet::setView(10, 20, 3)


#### 6a. GRanD & Biodiversity (2020) ----
mapview(existing_dams, layer.name = 
          "Existing dams risks to<br/>freshwater biodiversity<br/>–––––––––––––––––––––––––––<br/>Biodiversity risk in 2020 (color)<br/>Reservoir capacity (size)",
        zcol = "RC10", cex = existing_dams$CapacityCex, label = existing_dams$Name,  
        col.regions = colorRampPalette(c("#e9ffbe", "#ffd700", "#e60000")), 
        at = c(1, 1.4, 1.8, 2.2, 2.6, 3, 3.4, 3.8, 4.2, 4.6, 5),
        alpha.regions = 0.7,
        lwd = 0.1,
        viewer.suppress = FALSE,
        popup = leafpop::popupTable(existing_dams, zcol = c("Name", "Capacity", "Country", "Basin", "Basin", "RC10"))
) %>% 
  leafem::addLogo( 
    #"https://panda.maps.arcgis.com/sharing/rest/content/items/eaf9bbe5243d4dca8073e3517517bd71/data", src = "remote", # Black
    "https://panda.maps.arcgis.com/sharing/rest/content/items/203af4ae947245948ad0958bc2c7ea9c/data", src = "remote", # White
    position = "bottomleft",
    offset.x = 20,
    offset.y = 350,
    width = 200,
    height = 40.5) %>%
  leaflet::setView(10, 20, 3)
