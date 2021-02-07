library(tidyverse)
library(sf)
library(mapview)

# Load and organise data ----
all_dams <- readRDS("outputs/rds/all_dams_scenarios.rds") %>%
  mutate(
    Capacity_perc = sqrt(Capacity_perc)*15, Capacity_perc = if_else(Capacity_perc < 0.1, 0.1, Capacity_perc), # for visualisation purposes only
    `Risk combination` = case_when(
      RC10 > 3.4 & RC1_P50 > 3.4 ~ "1-1 Biodiversity High, Scarcity High",
      RC10 > 3.4 & RC1_P50 > 2.6 & RC1_P50 <= 3.4 ~ "1-2 Biodiversity High, Scarcity Medium",
      RC10 > 3.4 & RC1_P50 <= 2.6 ~ "1-3 Biodiversity High, Scarcity Low",
      RC10 > 2.6 & RC10 <= 3.4 & RC1_P50 > 3.4 ~ "2-1 Biodiversity Medium, Scarcity High",
      RC10 > 2.6 & RC10 <= 3.4 & RC1_P50 > 2.6 & RC1_P50 <= 3.4 ~ "2-2 Biodiversity Medium, Scarcity Medium",
      RC10 > 2.6 & RC10 <= 3.4 & RC1_P50 <= 2.6 ~ "2-3 Biodiversity Medium, Scarcity Low",
      RC10 <= 2.6 & RC1_P50 > 3.4 ~ "3-1 Biodiversity Low, Scarcity High",
      RC10 <= 2.6 & RC1_P50 > 2.6 & RC1_P50 <= 3.4 ~ "3-2 Biodiversity Low, Scarcity Medium",
      RC10 <= 2.6 & RC1_P50 <= 2.6 ~ "3-3 Biodiversity Low, Scarcity Low"
    ) 
  ) %>%
  select(Status:Basin, `Scarcity risk in 2050` = RC1_P50, `Biodiversity risk in 2020` = RC10, `Risk combination`)


# Maps ----
mapviewOptions(legend.pos = "bottomleft")

#### 6. GRanD + FHReD + Scarcity (2050) + Biodiversity Importance (2020) ----
mapview(all_dams, layer.name = 
          "Dams under scarcity risk and with risks to freshwater biodiversity",
        zcol = "Risk combination", cex = all_dams$Capacity_perc, label = all_dams$Name,  
        col.regions = colorRampPalette(biscale::bi_pal("Brown", preview = FALSE)), 
        #at = c(1, 1.4, 1.8, 2.2, 2.6, 3, 3.4, 3.8, 4.2, 4.6, 5),
        alpha.regions = 0.8,
        lwd = 0.1,
        viewer.suppress = FALSE,
        legend = FALSE,
        popup = leafpop::popupTable(all_dams, zcol = c("Status", "Name", "Capacity", "Country", "Province", "Basin", "Scarcity risk in 2050", "Biodiversity risk in 2020", "Risk combination"))
) %>% 
  leafem::addLogo( 
    "https://panda.maps.arcgis.com/sharing/rest/content/items/2b114801659048d1a9876ab14e838976/data", src = "remote",
    position = "bottomleft",
    offset.x = 10,
    offset.y = 50,
    width = 230,
    height = 318.7817) %>%
  leaflet::setView(10, 20, 3)
