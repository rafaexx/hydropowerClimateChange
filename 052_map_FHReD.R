library(tidyverse)
library(sf)
library(mapview)

#### Load data ----
projected_dams <- readRDS("outputs/rds/projected_dams_scenarios.rds") %>%
  mutate(CapacityCex = abs(sqrt(Capacity))/5) # for visualization purposes only


# Maps ----
mapviewOptions(legend.pos = "bottomleft")
mapviewOptions(fgb = FALSE)

my_col.regions <- colorRampPalette(scales::brewer_pal(type = "div", palette = "RdYlBu", direction = -1)(9))
legendBreaks <- c(-1.6, -1.2, -0.8, -0.4, -0.2, 0.2, 0.4, 0.8, 1.2, 1.6)


#### 3. FHReD & Change in Scarcity (Pessimistic 2050) ----
mapview(projected_dams, layer.name = 
          "Projected dams under<br/>pessimistic scenario for 2050<br/>–––––––––––––––––––––––––––<br/>Change in scarcity risk (color)<br/>Hydropower capacity (size)",
        zcol = "RC1_P50rc", cex = projected_dams$CapacityCex, label = projected_dams$Name,  
        col.regions = my_col.regions,
        at = legendBreaks,
        alpha.regions = 0.8,
        lwd = 0.1,
        viewer.suppress = FALSE,
        popup = leafpop::popupTable(projected_dams, zcol = c("Name", "Capacity", "Country", "Basin", "RC1", "RC1_P50", "RC1_P50rc"))
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


#### 4. FHReD & Change in Flooding (Pessimistic 2050) ----
mapviewOptions(basemaps = c("CartoDB.DarkMatter", "CartoDB.Positron", "CartoDB.Voyager", "Esri.WorldImagery", "Esri.WorldTopoMap"))
mapview(projected_dams, layer.name = 
          "Projected dams under<br/>pessimistic scenario for 2050<br/>–––––––––––––––––––––––––––<br/>Change in flood risk (color)<br/>Hydropower capacity (size)",
        zcol = "RC2_P50rc", cex = projected_dams$CapacityCex, label = projected_dams$Name,  
        col.regions = my_col.regions,
        at = legendBreaks,
        alpha.regions = 0.8,
        lwd = 0.1,
        viewer.suppress = FALSE,
        popup = leafpop::popupTable(projected_dams, zcol = c("Name", "Capacity", "Country", "Basin", "RC2", "RC2_P50", "RC2_P50rc"))
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

                   
#### 6b. FHReD + Biodiversity (2020) ----
# restart R using command/ctrl + shift + F10
library(tidyverse)
library(sf)
library(mapview)

mapviewOptions(legend.pos = "bottomleft")
mapviewOptions(fgb = FALSE)

mapview(projected_dams, layer.name = 
          "Projected dams risks to<br/>freshwater biodiversity<br/>–––––––––––––––––––––––––––<br/>Biodiversity risk in 2020 (color)<br/>Hydropower capacity (size)",
        zcol = "RC10", cex = projected_dams$CapacityCex, label = projected_dams$Name,  
        col.regions = colorRampPalette(c("#e9ffbe", "#ffd700", "#e60000")), 
        at = c(1, 1.4, 1.8, 2.2, 2.6, 3, 3.4, 3.8, 4.2, 4.6, 5),
        alpha.regions = 0.7,
        lwd = 0.1,
        viewer.suppress = FALSE,
        popup = leafpop::popupTable(projected_dams, zcol = c("Name", "Capacity", "Country", "Basin", "RC10"))
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
