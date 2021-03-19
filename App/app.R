library(tidyverse)
library(sf)
library(scales)
library(mapview)
library(shiny)
library(leaflet)
library(leafpop)
library(biscale)
library(leafem)
library(shinythemes)


#setwd("App")

#### Organise datasets ----
current_dams <- readRDS("data/current_dams_scenarios.rds") %>%
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
  mutate_at(vars(Overall:RC12_P50), ~ round(., 2)) %>%
  st_set_crs(4326)

future_dams <- readRDS("data/future_dams_scenarios.rds") %>%
  select(DAM_ID, Project_na, Main_river, Capacity__, Stage, 
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
  mutate_at(vars(Overall:RC12_P50), ~ round(., 2)) %>%
  st_set_crs(4326)

all_dams1 <- readRDS("data/all_dams_scenarios.rds") %>%
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
  select(Status:Basin, `Scarcity risk in 2050` = RC1_P50, `Biodiversity risk in 2020` = RC10, `Risk combination`) %>%
  st_set_crs(4326)

all_dams2 <- readRDS("data/all_dams_scenarios.rds") %>%
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
  select(Status:Basin, `Flooding risk in 2050` = RC2_P50, `Scarcity risk in 2050` = RC1_P50, `Risk combination`) %>%
  st_set_crs(4326)


# Maps ----
mapviewOptions(legend.pos = "bottomleft")

current_dams <- current_dams %>%
  mutate(Capacity = abs(sqrt(CAP_MCM))/10) # for visualisation purposes only

future_dams <- future_dams %>%
  mutate(Capacity = abs(sqrt(Capacity__))/5) # for visualisation purposes only

my_col.regions <- colorRampPalette(scales::brewer_pal(type = "div", palette = "RdYlBu", direction = -1)(9))
legendBreaks <- c(-1.6, -1.2, -0.8, -0.4, -0.2, 0.2, 0.4, 0.8, 1.2, 1.6)


#### 1. GRanD + Change in Scarcity (Pessimistic 2050) ----
map1 <- mapview(current_dams, layer.name = 
                  "Existing dams under<br/>pessimistic scenario for 2050<br/>–––––––––––––––––––––––––––<br/>Change in scarcity risk (color)<br/>Reservoir capacity (size)",
                zcol = "RC1_P50rc", cex = current_dams$Capacity, label = current_dams$DAM_NAME,  
                col.regions = my_col.regions,
                at = legendBreaks,
                alpha.regions = 0.8,
                lwd = 0.1,
                viewer.suppress = FALSE,
                popup = leafpop::popupTable(current_dams, zcol = c("DAM_NAME", "CAP_MCM", "Country", "Basin", "RC1", "RC1_P50", "RC1_P50rc"))
) 


#### 3. FHReD + Change in Scarcity (Pessimistic 2050) ----
map3 <- mapview(future_dams, layer.name = 
                  "Projected dams under<br/>pessimistic scenario for 2050<br/>–––––––––––––––––––––––––––<br/>Change in scarcity risk (color)<br/>Hydropower capacity (size)",
                zcol = "RC1_P50rc", cex = future_dams$Capacity, label = future_dams$Project_na,  
                col.regions = my_col.regions,
                at = legendBreaks,
                alpha.regions = 0.8,
                lwd = 0.1,
                viewer.suppress = FALSE,
                popup = leafpop::popupTable(future_dams, zcol = c("Project_na", "Capacity__", "Country", "Basin", "RC1", "RC1_P50", "RC1_P50rc"))
) 


#### 5a. GRanD + Biodiversity Importance (2020) ----
map5a <- mapview(current_dams, layer.name = 
                   "Existing dams risks to<br/>freshwater biodiversity<br/>–––––––––––––––––––––––––––<br/>Biodiversity risk in 2020 (color)<br/>Reservoir capacity (size)",
                 zcol = "RC10", cex = current_dams$Capacity, label = current_dams$DAM_NAME,  
                 col.regions = colorRampPalette(c("#e9ffbe", "#ffd700", "#e60000")), 
                 at = c(1, 1.4, 1.8, 2.2, 2.6, 3, 3.4, 3.8, 4.2, 4.6, 5),
                 alpha.regions = 0.7,
                 lwd = 0.1,
                 viewer.suppress = FALSE,
                 popup = leafpop::popupTable(current_dams, zcol = c("DAM_NAME", "CAP_MCM", "Country", "Basin", "Basin", "RC10"))
) 


#### 5b. FHReD + Biodiversity Importance (2020) ----
map5b <- mapview(future_dams, layer.name = 
                   "Projected dams risks to<br/>freshwater biodiversity<br/>–––––––––––––––––––––––––––<br/>Biodiversity risk in 2020 (color)<br/>Hydropower capacity (size)",
                 zcol = "RC10", cex = future_dams$Capacity, label = future_dams$Project_na,  
                 col.regions = colorRampPalette(c("#e9ffbe", "#ffd700", "#e60000")), 
                 at = c(1, 1.4, 1.8, 2.2, 2.6, 3, 3.4, 3.8, 4.2, 4.6, 5),
                 alpha.regions = 0.7,
                 lwd = 0.1,
                 viewer.suppress = FALSE,
                 popup = leafpop::popupTable(future_dams, zcol = c("Project_na", "Capacity__", "Country", "Basin", "RC10"))
) 


#### 6. GRanD + FHReD + Scarcity (2050) + Biodiversity Importance (2020) ----
map6 <- mapview(all_dams1, layer.name = 
                  "Dams under scarcity and biodiversity risk",
                zcol = "Risk combination", cex = all_dams1$Capacity_perc, label = all_dams1$Name,  
                col.regions = colorRampPalette(biscale::bi_pal("Brown", preview = FALSE)), 
                alpha.regions = 0.8,
                lwd = 0.1,
                viewer.suppress = FALSE,
                legend = FALSE,
                popup = leafpop::popupTable(all_dams1, zcol = c("Status", "Name", "Capacity", "Country", "Province", "Basin", "Scarcity risk in 2050", "Biodiversity risk in 2020", "Risk combination"))
) 


#### 7. GRanD + FHReD + Scarcity (2050) + Flooding (2050) ----
biscale_inverted <- c("#574249", "#985356", "#C85A5A", "#627F8C", "#AD9EA5", "#E4ACAC", "#64ACBE", "#B0D5DF", "#E8E8E8")
names(biscale_inverted) <- c("3-3", "2-3", "1-3", "3-2", "2-2", "1-2", "3-1", "2-1", "1-1")

map7 <- mapview(all_dams2, layer.name = 
                  "Dams under scarcity and flooding risk",
                zcol = "Risk combination", cex = all_dams2$Capacity_perc, label = all_dams2$Name,  
                col.regions = colorRampPalette(biscale_inverted), 
                alpha.regions = 0.8,
                lwd = 0.1,
                viewer.suppress = FALSE,
                legend = FALSE,
                popup = leafpop::popupTable(all_dams2, zcol = c("Status", "Name", "Capacity", "Country", "Province", "Basin", "Scarcity risk in 2050", "Flooding risk in 2050", "Risk combination"))
)


#### 2. GRanD + Change in Flooding (Pessimistic 2050) ----
mapviewOptions(basemaps = c("CartoDB.DarkMatter", "CartoDB.Positron", "CartoDB.Voyager", "Esri.WorldImagery", "Esri.WorldTopoMap"))

map2 <- mapview(current_dams, layer.name = 
                  "Existing dams under<br/>pessimistic scenario for 2050<br/>–––––––––––––––––––––––––––<br/>Change in flood risk (color)<br/>Reservoir capacity (size)",
                zcol = "RC2_P50rc", cex = current_dams$Capacity, label = current_dams$DAM_NAME,  
                col.regions = my_col.regions,
                at = legendBreaks,
                alpha.regions = 0.8,
                lwd = 0.1,
                viewer.suppress = FALSE,
                popup = leafpop::popupTable(current_dams, zcol = c("DAM_NAME", "CAP_MCM", "Country", "Basin", "RC2", "RC2_P50", "RC2_P50rc"))
) 


map4 <- mapview(future_dams, layer.name = 
                  "Projected dams under<br/>pessimistic scenario for 2050<br/>–––––––––––––––––––––––––––<br/>Change in flood risk (color)<br/>Hydropower capacity (size)",
                zcol = "RC2_P50rc", cex = future_dams$Capacity, label = future_dams$Project_na,  
                col.regions = my_col.regions,
                at = legendBreaks,
                alpha.regions = 0.8,
                lwd = 0.1,
                viewer.suppress = FALSE,
                popup = leafpop::popupTable(future_dams, zcol = c("Project_na", "Capacity__", "Country", "Basin", "RC2", "RC2_P50", "RC2_P50rc"))
) 


# Define UI ----
ui <- navbarPage("Using the WWF Water Risk Filter to Screen Existing and Projected Hydropower Projects for Climate and Biodiversity Risks",
                 theme = shinythemes::shinytheme("simplex"), collapsible = FALSE,
                 tabPanel("GRanD+FHReD+Scarcity+Flooding",
                          bootstrapPage(leafletOutput("map7", width="100%", height="840px"),
                                        p(a("© WWF Water Risk Filter 2021", href = "https://waterriskfilter.panda.org/"), " | This product incorporates data from the GRanD v1.3 and FHReD databases, available at ", a("globaldamwatch.org", href = "http://globaldamwatch.org/"), align = "center", style = "font-size:11px")
                          )
                 ),
                 tabPanel("GRanD+Scarcity",
                          bootstrapPage(leafletOutput("map1", width="100%", height="840px"),
                                        p(a("© WWF Water Risk Filter 2021", href = "https://waterriskfilter.panda.org/"), " | This product incorporates data from the GRanD v1.3 and FHReD databases, available at ", a("globaldamwatch.org", href = "http://globaldamwatch.org/"), align = "center", style = "font-size:11px")
                          )
                 ),
                 tabPanel("GRanD+Floods",
                          bootstrapPage(leafletOutput("map2", width="100%", height="840px"),
                                        p(a("© WWF Water Risk Filter 2021", href = "https://waterriskfilter.panda.org/"), " | This product incorporates data from the GRanD v1.3 and FHReD databases, available at ", a("globaldamwatch.org", href = "http://globaldamwatch.org/"), align = "center", style = "font-size:11px")
                          )
                 ),
                 tabPanel("FHReD+Scarcity",
                          bootstrapPage(leafletOutput("map3", width="100%", height="840px"),
                                        p(a("© WWF Water Risk Filter 2021", href = "https://waterriskfilter.panda.org/"), " | This product incorporates data from the GRanD v1.3 and FHReD databases, available at ", a("globaldamwatch.org", href = "http://globaldamwatch.org/"), align = "center", style = "font-size:11px")
                          )
                 ),
                 tabPanel("FHReD+Floods",
                          bootstrapPage(leafletOutput("map4", width="100%", height="840px"),
                                        p(a("© WWF Water Risk Filter 2021", href = "https://waterriskfilter.panda.org/"), " | This product incorporates data from the GRanD v1.3 and FHReD databases, available at ", a("globaldamwatch.org", href = "http://globaldamwatch.org/"), align = "center", style = "font-size:11px")
                          )
                 ),
                 tabPanel("GRanD+Biodiversity",
                          bootstrapPage(leafletOutput("map5a", width="100%", height="840px"),
                                        p(a("© WWF Water Risk Filter 2021", href = "https://waterriskfilter.panda.org/"), " | This product incorporates data from the GRanD v1.3 and FHReD databases, available at ", a("globaldamwatch.org", href = "http://globaldamwatch.org/"), align = "center", style = "font-size:11px")
                          )
                 ),
                 tabPanel("FHReD+Biodiversity",
                          bootstrapPage(leafletOutput("map5b", width="100%", height="840px"),
                                        p(a("© WWF Water Risk Filter 2021", href = "https://waterriskfilter.panda.org/"), " | This product incorporates data from the GRanD v1.3 and FHReD databases, available at ", a("globaldamwatch.org", href = "http://globaldamwatch.org/"), align = "center", style = "font-size:11px")
                          )
                 ),
                 tabPanel("GRanD+FHReD+Scarcity+Biodiversity",
                          bootstrapPage(leafletOutput("map6", width="100%", height="840px"),
                                        p(a("© WWF Water Risk Filter 2021", href = "https://waterriskfilter.panda.org/"), " | This product incorporates data from the GRanD v1.3 and FHReD databases, available at ", a("globaldamwatch.org", href = "http://globaldamwatch.org/"), align = "center", style = "font-size:11px")
                          )
                 ),
                 tabPanel("About", icon = icon("info-circle"),
                          fluidPage(
                            fluidRow(
                              column(8, offset = 2,
                                     h4("About it"),
                                     p("These maps are some of the outputs of an analysis using the ",  a("WWF Water Risk Filter", href = "https://waterriskfilter.panda.org/"), " to demonstrate how one such tool can be used to screen for a variety of risks at a global scale, including risks to riverine ecosystems from both climate change and hydropower as well as risks to hydropower projects — and operators, owners, and investors — from climate change and potential regulatory or reputational risk arising from negative impacts to ecosystems. The study is under review in the special issue of the journal Water: ",  a("Water Management to Protect Aquatic Ecosystems Function and Health in the Face of Climate Change", href = "https://www.mdpi.com/si/water/freshwater_management_ecosystems_aquatic"), ".", align = "justify"), 
                                     p("The complete dataset — that is, the GRanD v1.3 and FHReD datasets from ", a("globaldamwatch.org", href = "http://globaldamwatch.org/"), ", added with the Water Risk Filter risk scores and risk changes — is available at ",  a("https://zenodo.org/record/4516604", href = "https://zenodo.org/record/4516604"), ". The source code used in this study is available at ",  a("https://github.com/rafaelatwwf/hydropowerClimateChange", href = "https://github.com/rafaelatwwf/hydropowerClimateChange"), ".", align = "justify"),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     p(a("© WWF Water Risk Filter 2021", href = "https://waterriskfilter.panda.org/"), align = "center", style = "font-size:11px")
                              )
                            )
                          )
                 )
)

# Define server logic ----
server <- function(input, output, session) {
  # Render map1
  output$map1 <- renderLeaflet({
    map1@map %>% 
      leafem::addLogo( 
        #"https://panda.maps.arcgis.com/sharing/rest/content/items/eaf9bbe5243d4dca8073e3517517bd71/data", src = "remote", # Black
        "https://panda.maps.arcgis.com/sharing/rest/content/items/203af4ae947245948ad0958bc2c7ea9c/data", src = "remote", # White
        position = "bottomleft",
        offset.x = 20,
        offset.y = 330,
        width = 200,
        height = 40.5) %>%
      leaflet::setView(10, 20, 3)
  })
  
  # Render map2
  output$map2 <- renderLeaflet({
    map2@map %>% 
      leafem::addLogo( 
        #"https://panda.maps.arcgis.com/sharing/rest/content/items/eaf9bbe5243d4dca8073e3517517bd71/data", src = "remote", # Black
        "https://panda.maps.arcgis.com/sharing/rest/content/items/203af4ae947245948ad0958bc2c7ea9c/data", src = "remote", # White
        position = "bottomleft",
        offset.x = 20,
        offset.y = 330,
        width = 200,
        height = 40.5) %>%
      leaflet::setView(10, 20, 3)
  })
  
  # Render map3
  output$map3 <- renderLeaflet({
    map3@map %>% 
      leafem::addLogo( 
        #"https://panda.maps.arcgis.com/sharing/rest/content/items/eaf9bbe5243d4dca8073e3517517bd71/data", src = "remote", # Black
        "https://panda.maps.arcgis.com/sharing/rest/content/items/203af4ae947245948ad0958bc2c7ea9c/data", src = "remote", # White
        position = "bottomleft",
        offset.x = 20,
        offset.y = 330,
        width = 200,
        height = 40.5) %>%
      leaflet::setView(10, 20, 3)
  })
  
  # Render map4
  output$map4 <- renderLeaflet({
    map4@map %>% 
      leafem::addLogo( 
        #"https://panda.maps.arcgis.com/sharing/rest/content/items/eaf9bbe5243d4dca8073e3517517bd71/data", src = "remote", # Black
        "https://panda.maps.arcgis.com/sharing/rest/content/items/203af4ae947245948ad0958bc2c7ea9c/data", src = "remote", # White
        position = "bottomleft",
        offset.x = 20,
        offset.y = 330,
        width = 200,
        height = 40.5) %>%
      leaflet::setView(10, 20, 3)
  })
  
  # Render map5a
  output$map5a <- renderLeaflet({
    map5a@map %>% 
      leafem::addLogo( 
        #"https://panda.maps.arcgis.com/sharing/rest/content/items/eaf9bbe5243d4dca8073e3517517bd71/data", src = "remote", # Black
        "https://panda.maps.arcgis.com/sharing/rest/content/items/203af4ae947245948ad0958bc2c7ea9c/data", src = "remote", # White
        position = "bottomleft",
        offset.x = 20,
        offset.y = 350,
        width = 200,
        height = 40.5) %>%
      leaflet::setView(10, 20, 3)
  })
  
  # Render map5b
  output$map5b <- renderLeaflet({
    map5b@map %>% 
      leafem::addLogo( 
        #"https://panda.maps.arcgis.com/sharing/rest/content/items/eaf9bbe5243d4dca8073e3517517bd71/data", src = "remote", # Black
        "https://panda.maps.arcgis.com/sharing/rest/content/items/203af4ae947245948ad0958bc2c7ea9c/data", src = "remote", # White
        position = "bottomleft",
        offset.x = 20,
        offset.y = 350,
        width = 200,
        height = 40.5) %>%
      leaflet::setView(10, 20, 3)
  })
  
  # Render map6
  output$map6 <- renderLeaflet({
    map6@map %>% 
      leafem::addLogo( 
        "https://panda.maps.arcgis.com/sharing/rest/content/items/2b114801659048d1a9876ab14e838976/data", src = "remote",
        position = "bottomleft",
        offset.x = 10,
        offset.y = 50,
        width = 230,
        height = 318.7817) %>%
      leaflet::setView(10, 20, 3)
  })
  
  # Render map7
  output$map7 <- renderLeaflet({
    map7@map %>% 
      leafem::addLogo( 
        "https://panda.maps.arcgis.com/sharing/rest/content/items/e8c499616d334affabd9b250c7a0d76e/data", src = "remote",
        position = "bottomleft",
        offset.x = 10,
        offset.y = 50,
        width = 230,
        height = 318.7817) %>%
      leaflet::setView(10, 20, 3)
  })
  
}

# Run the app ----
shinyApp(ui, server)
