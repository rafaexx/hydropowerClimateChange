library(tidyverse)
library(sf)
library(mapview)

#### Load data ----
current_dams <- readRDS("outputs/rds/current_dams_scenarios.rds") 


# Organise data ----
data <- current_dams %>%
  select(GRAND_ID, DAM_NAME, RES_NAME, RIVER, CAP_MCM, MAIN_USE, 
         Basin:Province, 
         Overa = Overall, Phy, RC1:RC4, Reg, RC5:RC8, Rep, RC9:RC12, 
         Overa_O30:Overa_P50, Overa_O30c:Overa_P50c,
         Phy_O30:Phy_P50, Phy_O30c:Phy_P50c,
         RC1_O30:RC4_P50,
         Reg_O30:Reg_P50, Reg_O30c:Reg_P50c,
         RC5_O30:RC8_P50,
         Rep_O30:Rep_P50, Rep_O30c:Rep_P50c,
         RC9_O30:RC12_P50,
         RC1_O3rc:RC12_P5rc) %>%
  rename_at(vars(contains("Overa")), ~ str_replace(., "Overa", "Overall")) %>%
  rename_at(vars(contains("3rc")), ~ str_replace(., "3rc", "30rc")) %>%
  rename_at(vars(contains("5rc")), ~ str_replace(., "5rc", "50rc")) %>%
  mutate_at(vars(Overall:RC12_P50), ~ round(., 2))


# Maps ----
mapviewOptions(legend.pos = "bottomleft")

data <- data %>%
  mutate(Capacity = abs(sqrt(CAP_MCM))/10) # for visualisation purposes only

my_col.regions <- colorRampPalette(scales::brewer_pal(type = "div", palette = "RdYlBu", direction = -1)(9))
legendBreaks <- c(-1.6, -1.2, -0.8, -0.4, -0.2, 0.2, 0.4, 0.8, 1.2, 1.6)


#### 1. GRanD + Change in Scarcity (Pessimistic 2050) ----
mapview(data, layer.name = 
          "Existing dams under<br/>pessimistic scenario for 2050<br/>–––––––––––––––––––––––––––<br/>Change in scarcity risk (color)<br/>Reservoir capacity (size)",
        zcol = "RC1_P50rc", cex = data$Capacity, label = data$DAM_NAME,  
        col.regions = my_col.regions,
        at = legendBreaks,
        alpha.regions = 0.8,
        lwd = 0.1,
        viewer.suppress = FALSE,
        popup = leafpop::popupTable(data, zcol = c("DAM_NAME", "CAP_MCM", "Country", "Basin", "RC1", "RC1_P50", "RC1_P50rc"))
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


#### 2. GRanD + Change in Flooding (Pessimistic 2050) ----
mapviewOptions(basemaps = c("CartoDB.DarkMatter", "CartoDB.Positron", "CartoDB.Voyager", "Esri.WorldImagery", "Esri.WorldTopoMap"))
mapview(data, layer.name = 
          "Existing dams under<br/>pessimistic scenario for 2050<br/>–––––––––––––––––––––––––––<br/>Change in flood risk (color)<br/>Reservoir capacity (size)",
        zcol = "RC2_P50rc", cex = data$Capacity, label = data$DAM_NAME,  
        col.regions = my_col.regions,
        at = legendBreaks,
        alpha.regions = 0.8,
        lwd = 0.1,
        viewer.suppress = FALSE,
        popup = leafpop::popupTable(data, zcol = c("DAM_NAME", "CAP_MCM", "Country", "Basin", "RC2", "RC2_P50", "RC2_P50rc"))
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


#### 5a. GRanD + Biodiversity Importance (2020) ----
# restart R using command/ctrl + shift + F10
library(tidyverse)
library(sf)
library(mapview)

mapviewOptions(legend.pos = "bottomleft")

mapview(data, layer.name = 
          "Existing dams risks to<br/>freshwater biodiversity<br/>–––––––––––––––––––––––––––<br/>Biodiversity risk in 2020 (color)<br/>Reservoir capacity (size)",
        zcol = "RC10", cex = data$Capacity, label = data$DAM_NAME,  
        col.regions = colorRampPalette(c("#e9ffbe", "#ffd700", "#e60000")), 
        at = c(1, 1.4, 1.8, 2.2, 2.6, 3, 3.4, 3.8, 4.2, 4.6, 5),
        alpha.regions = 0.7,
        lwd = 0.1,
        viewer.suppress = FALSE,
        popup = leafpop::popupTable(data, zcol = c("DAM_NAME", "CAP_MCM", "Country", "Basin", "Basin", "RC10"))
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
