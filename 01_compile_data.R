library(tidyverse)
library(sf)
`%notin%` <- Negate(`%in%`)

#### Load data ----
current_dams <- st_read("inputData/GRanD_v1_3/GRanD_dams_v1_3.shp") %>%
  drop_na(USE_ELEC) %>%
  filter(TIMELINE %notin% c("Destroyed", "Removed", "Replaced", "Subsumed"))

future_dams <- st_read("inputData/FHReD_2015_future_dams_Zarfl_et_al_beta_version/FHReD2015.shp") %>%
  rename(CountryDataset = Country)

major_basins <- st_read("inputData/basins_grdc_rbd_gadm/basins_grdc_rbd_gadm_h7.shp") %>%
  select(Basin = BasinProv)
  
adm_boundaries <- st_read("inputData/gadm36/gadm36_countryProv.shp", stringsAsFactors = FALSE) %>%
  select(ISO3 = GID_0, Country = NAME_0, GID = GID_1, Province = NAME_1) %>%
  mutate(Country = if_else(Country %in% c("Australia", "Brazil", "Canada", "China", "Russia", "United States"), glue::glue("{Country}-{Province}"), Country))

wrf <- st_read("inputData/0_WRF_20201116/WRF_masterdata_lv7_20201116.shp") %>%
  select(RC1:RC12)

wrf_scenarios <- st_read("inputData/WRF_Scenarios/WRF_Scenarios_riskChanges.shp") %>%
  select(-c(LV_7_ID:ID_String))


#### Define industry weighting  ----
# Electric Energy Production - Hydropower
iwRC1 <- 0.50
iwRC2 <- 0.25
iwRC3 <- 0.10
iwRC4 <- 0.15
iwRC5 <- 0.30
iwRC6 <- 0.30
iwRC7 <- 0.25
iwRC8 <- 0.15
iwRC9 <- 0.20
iwRC10 <- 0.10
iwRC11 <- 0.30
iwRC12 <- 0.40
iwPhy <- 0.65
iwReg <- 0.20
iwRep <- 0.15


#### Build portfolio ----
build_potfolio <- function(input){
  
  input %>% 
    st_join(major_basins) %>%
    st_join(adm_boundaries) %>%
    st_join(wrf) %>%
    mutate(
      Phy = RC1*iwRC1 + RC2*iwRC2 + RC3*iwRC3 + RC4*iwRC4,
      Reg = RC5*iwRC5 + RC6*iwRC6 + RC7*iwRC7 + RC8*iwRC8,
      Rep = RC9*iwRC9 + RC10*iwRC10 + RC11*iwRC11 + RC12*iwRC12,
      Overall = Phy*iwPhy + Reg*iwReg + Rep*iwRep,
    ) %>%
    st_join(wrf_scenarios) %>%
    
    # Compute scenarios for the RCs
    mutate(
      RC1_O30 = RC1 + RC1_O3rc,
      RC1_O50 = RC1 + RC1_O5rc,
      RC1_C30 = RC1 + RC1_C3rc,
      RC1_C50 = RC1 + RC1_C5rc,
      RC1_P30 = RC1 + RC1_P3rc,
      RC1_P50 = RC1 + RC1_P5rc,
      
      RC2_O30 = RC2 + RC2_O3rc,
      RC2_O50 = RC2 + RC2_O5rc,
      RC2_C30 = RC2 + RC2_C3rc,
      RC2_C50 = RC2 + RC2_C5rc,
      RC2_P30 = RC2 + RC2_P3rc,
      RC2_P50 = RC2 + RC2_P5rc,
      
      RC3_O30 = RC3 + RC3_O3rc,
      RC3_O50 = RC3 + RC3_O5rc,
      RC3_C30 = RC3 + RC3_C3rc,
      RC3_C50 = RC3 + RC3_C5rc,
      RC3_P30 = RC3 + RC3_P3rc,
      RC3_P50 = RC3 + RC3_P5rc,
      
      RC4_O30 = RC4 + RC4_O3rc,
      RC4_O50 = RC4 + RC4_O5rc,
      RC4_C30 = RC4 + RC4_C3rc,
      RC4_C50 = RC4 + RC4_C5rc,
      RC4_P30 = RC4 + RC4_P3rc,
      RC4_P50 = RC4 + RC4_P5rc,
      
      RC5_O30 = RC5 + RC5_O3rc,
      RC5_O50 = RC5 + RC5_O5rc,
      RC5_C30 = RC5 + RC5_C3rc,
      RC5_C50 = RC5 + RC5_C5rc,
      RC5_P30 = RC5 + RC5_P3rc,
      RC5_P50 = RC5 + RC5_P5rc,
      
      RC6_O30 = RC6 + RC6_O3rc,
      RC6_O50 = RC6 + RC6_O5rc,
      RC6_C30 = RC6 + RC6_C3rc,
      RC6_C50 = RC6 + RC6_C5rc,
      RC6_P30 = RC6 + RC6_P3rc,
      RC6_P50 = RC6 + RC6_P5rc,
      
      RC7_O30 = RC7 + RC7_O3rc,
      RC7_O50 = RC7 + RC7_O5rc,
      RC7_C30 = RC7 + RC7_C3rc,
      RC7_C50 = RC7 + RC7_C5rc,
      RC7_P30 = RC7 + RC7_P3rc,
      RC7_P50 = RC7 + RC7_P5rc,
      
      RC8_O30 = RC8 + RC8_O3rc,
      RC8_O50 = RC8 + RC8_O5rc,
      RC8_C30 = RC8 + RC8_C3rc,
      RC8_C50 = RC8 + RC8_C5rc,
      RC8_P30 = RC8 + RC8_P3rc,
      RC8_P50 = RC8 + RC8_P5rc,
      
      RC9_O30 = RC9,
      RC9_O50 = RC9,
      RC9_C30 = RC9,
      RC9_C50 = RC9,
      RC9_P30 = RC9,
      RC9_P50 = RC9,
      
      RC10_O30 = RC10 + RC10_O3rc,
      RC10_O50 = RC10 + RC10_O5rc,
      RC10_C30 = RC10 + RC10_C3rc,
      RC10_C50 = RC10 + RC10_C5rc,
      RC10_P30 = RC10 + RC10_P3rc,
      RC10_P50 = RC10 + RC10_P5rc,
      
      RC11_O30 = RC11,
      RC11_O50 = RC11,
      RC11_C30 = RC11,
      RC11_C50 = RC11,
      RC11_P30 = RC11,
      RC11_P50 = RC11,
      
      RC12_O30 = RC12 + RC12_O3rc,
      RC12_O50 = RC12 + RC12_O5rc,
      RC12_C30 = RC12 + RC12_C3rc,
      RC12_C50 = RC12 + RC12_C5rc,
      RC12_P30 = RC12 + RC12_P3rc,
      RC12_P50 = RC12 + RC12_P5rc
    ) %>%
    
    # Compute Phy, Reg, Rep and Overall, applying the industry weighting
    mutate(
      Phy_O30 = RC1_O30*iwRC1 +  RC2_O30*iwRC2 +  RC3_O30*iwRC3 +  RC4_O30*iwRC4,
      Phy_O50 = RC1_O50*iwRC1 +  RC2_O50*iwRC2 +  RC3_O50*iwRC3 +  RC4_O50*iwRC4,
      Phy_C30 = RC1_C30*iwRC1 +  RC2_C30*iwRC2 +  RC3_C30*iwRC3 +  RC4_C30*iwRC4,
      Phy_C50 = RC1_C50*iwRC1 +  RC2_C50*iwRC2 +  RC3_C50*iwRC3 +  RC4_C50*iwRC4,
      Phy_P30 = RC1_P30*iwRC1 +  RC2_P30*iwRC2 +  RC3_P30*iwRC3 +  RC4_P30*iwRC4,
      Phy_P50 = RC1_P50*iwRC1 +  RC2_P50*iwRC2 +  RC3_P50*iwRC3 +  RC4_P50*iwRC4,
      
      Reg_O30 = RC5_O30*iwRC5 +  RC6_O30*iwRC6 +  RC7_O30*iwRC7 +  RC8_O30*iwRC8,
      Reg_O50 = RC5_O50*iwRC5 +  RC6_O50*iwRC6 +  RC7_O50*iwRC7 +  RC8_O50*iwRC8,
      Reg_C30 = RC5_C30*iwRC5 +  RC6_C30*iwRC6 +  RC7_C30*iwRC7 +  RC8_C30*iwRC8,
      Reg_C50 = RC5_C50*iwRC5 +  RC6_C50*iwRC6 +  RC7_C50*iwRC7 +  RC8_C50*iwRC8,
      Reg_P30 = RC5_P30*iwRC5 +  RC6_P30*iwRC6 +  RC7_P30*iwRC7 +  RC8_P30*iwRC8,
      Reg_P50 = RC5_P50*iwRC5 +  RC6_P50*iwRC6 +  RC7_P50*iwRC7 +  RC8_P50*iwRC8,
      
      Rep_O30 = RC9_O30*iwRC9 + RC10_O30*iwRC10 + RC11_O30*iwRC11 + RC12_O30*iwRC12, # Uses current RC9 and RC11 due to unavailability of projection data
      Rep_O50 = RC9_O50*iwRC9 + RC10_O50*iwRC10 + RC11_O50*iwRC11 + RC12_O50*iwRC12,
      Rep_C30 = RC9_C30*iwRC9 + RC10_C30*iwRC10 + RC11_C30*iwRC11 + RC12_C30*iwRC12,
      Rep_C50 = RC9_C50*iwRC9 + RC10_C50*iwRC10 + RC11_C50*iwRC11 + RC12_C50*iwRC12,
      Rep_P30 = RC9_P30*iwRC9 + RC10_P30*iwRC10 + RC11_P30*iwRC11 + RC12_P30*iwRC12,
      Rep_P50 = RC9_P50*iwRC9 + RC10_P50*iwRC10 + RC11_P50*iwRC11 + RC12_P50*iwRC12,
      
      Overa_O30 = Phy_O30*iwPhy + Reg_O30*iwReg + Rep_O30*iwRep,
      Overa_O50 = Phy_O50*iwPhy + Reg_O50*iwReg + Rep_O50*iwRep,
      Overa_C30 = Phy_C30*iwPhy + Reg_C30*iwReg + Rep_C30*iwRep,
      Overa_C50 = Phy_C50*iwPhy + Reg_C50*iwReg + Rep_C50*iwRep,
      Overa_P30 = Phy_P30*iwPhy + Reg_P30*iwReg + Rep_P30*iwRep,
      Overa_P50 = Phy_P50*iwPhy + Reg_P50*iwReg + Rep_P50*iwRep
    ) %>% 
    
    # Correct risk scores range 1 to 6.6
    mutate_at(vars(RC1_O30:Overa_P50), ~if_else(.>6.6,6.6,.)) %>%
    mutate_at(vars(RC1_O30:Overa_P50), ~if_else(.<1,1,.)) %>%
    
    # Compute percentage changes, relative to the WRF risk score range from 1 to 5
    mutate(
      Phy_O30c = (Phy_O30 - Phy) / 4 * 100,
      Phy_O50c = (Phy_O50 - Phy) / 4 * 100,
      Phy_C30c = (Phy_C30 - Phy) / 4 * 100,
      Phy_C50c = (Phy_C50 - Phy) / 4 * 100,
      Phy_P30c = (Phy_P30 - Phy) / 4 * 100,
      Phy_P50c = (Phy_P50 - Phy) / 4 * 100,
      
      Reg_O30c = (Reg_O30 - Reg) / 4 * 100,
      Reg_O50c = (Reg_O50 - Reg) / 4 * 100,
      Reg_C30c = (Reg_C30 - Reg) / 4 * 100,
      Reg_C50c = (Reg_C50 - Reg) / 4 * 100,
      Reg_P30c = (Reg_P30 - Reg) / 4 * 100,
      Reg_P50c = (Reg_P50 - Reg) / 4 * 100,
      
      Rep_O30c = (Rep_O30 - Rep) / 4 * 100,
      Rep_O50c = (Rep_O50 - Rep) / 4 * 100,
      Rep_C30c = (Rep_C30 - Rep) / 4 * 100,
      Rep_C50c = (Rep_C50 - Rep) / 4 * 100,
      Rep_P30c = (Rep_P30 - Rep) / 4 * 100,
      Rep_P50c = if_else(Rep_P50 - Rep > 1.6, 1.6, Rep_P50 - Rep) / 4 * 100,
      
      Overa_O30c = (Overa_O30 - Overall) / 4 * 100,
      Overa_O50c = (Overa_O50 - Overall) / 4 * 100,
      Overa_C30c = (Overa_C30 - Overall) / 4 * 100,
      Overa_C50c = (Overa_C50 - Overall) / 4 * 100,
      Overa_P30c = (Overa_P30 - Overall) / 4 * 100,
      Overa_P50c = (Overa_P50 - Overall) / 4 * 100
    )
}


#### Compile datasets ----
current_dams <- build_potfolio(current_dams)
future_dams <- build_potfolio(future_dams)

# Merge all
current_dams_temp <- current_dams %>%
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
  mutate(Capacity_perc = round(CAP_MCM/5746118*100, 2),
         Status = "Existing") %>%
  select(Status, Id = GRAND_ID, Name = DAM_NAME, Capacity = CAP_MCM, Capacity_perc, Use = MAIN_USE, Country, Province, Basin, Overall:RC12_P50rc)

future_dams_temp <- future_dams %>%
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
  mutate_at(vars(Overall:RC12_P50), ~ round(., 2))  %>%
  mutate(Capacity_perc = round(Capacity__/724646.5*100, 2), 
         Status = case_when(Stage == "P" ~ "Planned", Stage == "U" ~ "Under construction"),
         Use = "Hydroelectricity") %>%
  select(Status, Id = DAM_ID, Name = Project_na, Capacity = Capacity__, Capacity_perc, Use, Country, Province, Basin, Overall:RC12_P50rc)

all_dams <- rbind(current_dams_temp, future_dams_temp)


#### Save RDSs ----
saveRDS(current_dams, "outputs/rds/current_dams_scenarios.rds")
saveRDS(future_dams, "outputs/rds/future_dams_scenarios.rds")
saveRDS(all_dams, "outputs/rds/all_dams_scenarios.rds")


#### Export ----
# Current dams
current_damsExport <- current_dams %>%
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
  st_drop_geometry()
skimr::skim(current_damsExport)

# Future dams
future_damsExport <- future_dams %>%
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
  st_drop_geometry()
skimr::skim(future_damsExport)

# Descriptions
columnsDescription <- tribble(
  ~Column, ~Description,
  "Overall",   "WRF 2020 risk score in Overall basin risk, composed of three risk types: Physical (Phy), Regulatory (Reg), and Reputational (Reg)",
  "Phy",   "WRF 2020 risk score in Physical risk, composed of four risk categories: Scarcity (RC1), Flooding (RC2), Water quality (RC3), and Ecosystem Services Status (RC4)",
  "RC1",   "WRF 2020 risk score in the risk category 1. Scarcity",
  "RC2",   "WRF 2020 risk score in the risk category 2. Flooding",
  "RC3",   "WRF 2020 risk score in the risk category 3. Water Quality",
  "RC4",   "WRF 2020 risk score in the risk category 4. Ecosystem Services Status",
  "Reg",   "WRF 2020 risk score in Regulatory risk, composed of four risk categories: Enabling Environment (RC5), Institutions & Governance (RC6), Management Instruments (RC7), and Infrastructure & Finance (RC8)",
  "RC5",   "WRF 2020 risk score in the risk category 5. Enabling Environment",
  "RC6",   "WRF 2020 risk score in the risk category 6. Institutions & Governance",
  "RC7",   "WRF 2020 risk score in the risk category 7. Management Instruments",
  "RC8",   "WRF 2020 risk score in the risk category 8. Infrastructure & Finance",
  "Rep",   "WRF 2020 risk score in Reputational risk, composed of four risk categories: Cultural Importance (RC9), Biodiversity Importance (RC10), Media Scrutiny (RC11), and Conflict (RC12)",
  "RC9",   "WRF 2020 risk score in the risk category 9. Cultural Importance",
  "RC10",   "WRF 2020 risk score in the risk category 10. Biodiversity Importance",
  "RC11",   "WRF 2020 risk score in the risk category 11.Media Scrutiny",
  "RC12",   "WRF 2020 risk score in the risk category 12. Conflict",
  "[RiskLayer]_O30", "Scenario of [RiskLayer] in the Optimistic pathway, for the year 2030",
  "[RiskLayer]_O50", "Scenario of [RiskLayer] in the Optimistic pathway, for the year 2050",
  "[RiskLayer]_C30", "Scenario of [RiskLayer] in the Current trend pathway, for the year 2030",
  "[RiskLayer]_C50", "Scenario of [RiskLayer] in the Current trend pathway, for the year 2050",
  "[RiskLayer]_P30", "Scenario of [RiskLayer] in the Pessimistic pathway, for the year 2030",
  "[RiskLayer]_P50", "Scenario of [RiskLayer] in the Pessimistic pathway, for the year 2050",
  "[RiskLayer]_O30c", "Percentage change in risk score between today's risk and scenario of [RiskLayer] in the Optimistic pathway, for the year 2030",
  "[RiskLayer]_O50c", "Percentage change in risk score between today's risk and scenario of [RiskLayer] in the Optimistic pathway, for the year 2050",
  "[RiskLayer]_C30c", "Percentage change in risk score between today's risk and scenario of [RiskLayer] in the Current trend pathway, for the year 2030",
  "[RiskLayer]_C50c", "Percentage change in risk score between today's risk and scenario of [RiskLayer] in the Current trend pathway, for the year 2050",
  "[RiskLayer]_P30c", "Percentage change in risk score between today's risk and scenario of [RiskLayer] in the Pessimistic pathway, for the year 2030",
  "[RiskLayer]_P50c", "Percentage change in risk score between today's risk and scenario of [RiskLayer] in the Pessimistic pathway, for the year 2050",
  "[RiskLayer]_[PathwayYear]rc", "Change in risk score between today's risk and scenario of [RiskLayer] in the [PathwayYear, e.g. O30,O50,C30,C50,P30,P50]"
)

openxlsx::write.xlsx(list(
  "GRanD+WRF Scenarios" = current_damsExport,
  "Columns description" = columnsDescription),
  file = "outputs/xlsx/GRanD_WRFscenarios.xlsx", row.names = FALSE)

openxlsx::write.xlsx(list(
  "FHReD+WRF Scenarios" = future_damsExport,
  "Columns description" = columnsDescription),
  file = "outputs/xlsx/FHReD_WRFscenarios.xlsx", row.names = FALSE)
