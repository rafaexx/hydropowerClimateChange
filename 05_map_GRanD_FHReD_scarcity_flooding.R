library(tidyverse)
library(sf)
library(mapview)

# Load and organise data ----
all_dams <- readRDS("outputs/rds/all_dams_scenarios.rds") %>%
  mutate(
    Capacity_perc = sqrt(Capacity_perc)*15, Capacity_perc = if_else(Capacity_perc < 0.1, 0.1, Capacity_perc), # for visualisation purposes only
    `Risk combination` = case_when(
      RC1_P50 > 3.4 & RC2_P50 > 3.4 ~ "1-1 Scarcity High, Flooding High",
      RC1_P50 > 3.4 & RC2_P50 > 2.6 & RC2_P50 <= 3.4 ~ "1-2 Scarcity High, Flooding Medium",
      RC1_P50 > 3.4 & RC2_P50 <= 2.6 ~ "1-3 Scarcity High, Flooding Low",
      RC1_P50 > 2.6 & RC1_P50 <= 3.4 & RC2_P50 > 3.4 ~ "2-1 Scarcity Medium, Flooding High",
      RC1_P50 > 2.6 & RC1_P50 <= 3.4 & RC2_P50 > 2.6 & RC2_P50 <= 3.4 ~ "2-2 Scarcity Medium, Flooding Medium",
      RC1_P50 > 2.6 & RC1_P50 <= 3.4 & RC2_P50 <= 2.6 ~ "2-3 Scarcity Medium, Flooding Low",
      RC1_P50 <= 2.6 & RC2_P50 > 3.4 ~ "3-1 Scarcity Low, Flooding High",
      RC1_P50 <= 2.6 & RC2_P50 > 2.6 & RC2_P50 <= 3.4 ~ "3-2 Scarcity Low, Flooding Medium",
      RC1_P50 <= 2.6 & RC2_P50 <= 2.6 ~ "3-3 Scarcity Low, Flooding Low"
    ) 
  ) %>%
  select(Status:Basin, `Flooding risk in 2050` = RC2_P50, `Scarcity risk in 2050` = RC1_P50, `Risk combination`)


# Maps ----
mapviewOptions(legend.pos = "bottomleft")

# invert biscale::bi_pal("GrPink", preview = FALSE)
biscale_inverted <- c("#574249", "#985356", "#C85A5A", "#627F8C", "#AD9EA5", "#E4ACAC", "#64ACBE", "#B0D5DF", "#E8E8E8")
names(biscale_inverted) <- c("3-3", "2-3", "1-3", "3-2", "2-2", "1-2", "3-1", "2-1", "1-1")

#### 7. GRanD + FHReD + Scarcity (2050) + Flooding (2050) ----
mapview(all_dams, layer.name = 
          "Dams under scarcity and flooding risk",
        zcol = "Risk combination", cex = all_dams$Capacity_perc, label = all_dams$Name,  
        col.regions = colorRampPalette(biscale_inverted), 
        alpha.regions = 0.8,
        lwd = 0.1,
        viewer.suppress = FALSE,
        legend = FALSE,
        popup = leafpop::popupTable(all_dams, zcol = c("Status", "Name", "Capacity", "Country", "Province", "Basin", "Scarcity risk in 2050", "Flooding risk in 2050", "Risk combination"))
) %>% 
  leafem::addLogo( 
    "https://panda.maps.arcgis.com/sharing/rest/content/items/e8c499616d334affabd9b250c7a0d76e/data", src = "remote",
    position = "bottomleft",
    offset.x = 10,
    offset.y = 50,
    width = 230,
    height = 318.7817) %>%
  leaflet::setView(10, 20, 3)
