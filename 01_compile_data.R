library(tidyverse)
library(sf)
`%notin%` <- Negate(`%in%`)

#### Subset from GRanD hydropower dams only ----
st_read("inputData/GRanD_v1_3/GRanD_dams_v1_3.shp") %>%
  drop_na(USE_ELEC) %>%
  filter(TIMELINE %notin% c("Destroyed", "Removed", "Replaced", "Subsumed")) %>%
  st_write("inputData/GRanD_v1_3/GRanD_dams_v1_3_selection.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)


#### Iterate over points of dams to compute dam's catchments (watersheds) and mean WRF risk scores in the watersheds (weighted by area) 
## Contributions from Mira Anand

# reticulate::source_python("py_scripts/catchments.py") # run manually
# reticulate::source_python("py_scripts/zonal_stats.py") # run manually

# Workflow for GRanD (existing dams):
#   run grand_catchments 
#   run make_pickle to calculate results 
#   check results in Jupyter notebook dam_catchments.ipynb
#   correct the few watersheds which produced errors 
#   run grand_catchments on corrected watersheds
#   run make_pickle to calculate new statistics for corrected watersheds
#   update grand results dictionary with corrected watersheds
#   run update points to add attributes to point file
#   write results in /intermediate_output
# 
# Workflow for FHReD (projected dams):
#   run fhred_catchments 
#   run make_pickle to calculate results
#   open results in Jupyter notebook dam_catchments.ipynb
#   list dam IDs with errors
#   run fhred_catchments_part2 on points with errors
#   run make_pickle to calculate new statistics for corrected watersheds 
#   update fhred results dictionary with corrected watersheds 
#   check results and correct remaining incorrect watersheds 
#   run make_pickle on corrected watersheds
#   update fhred results dictionary with corrected watersheds
#   run update points to add attributes to point file
#   write results in /intermediate_output


#### Load data ----
major_basins <- st_read("inputData/basins_grdc_rbd_gadm/basins_grdc_rbd_gadm_h7.shp") %>%
  select(Basin = BasinProv)
  
adm_boundaries <- st_read("inputData/gadm36/gadm36_countryProv.shp", stringsAsFactors = FALSE) %>%
  select(ISO3 = GID_0, Country = NAME_0, GID = GID_1, Province = NAME_1) %>%
  mutate(Country = if_else(Country %in% c("Australia", "Brazil", "Canada", "China", "Russia", "United States"), glue::glue("{Country}-{Province}"), Country))

existing_dams <- st_read("intermediate_output/dam_results.gdb", layer = "GRanD_v1_3_selection_stats") %>%
  st_join(major_basins) %>%
  st_join(adm_boundaries) %>%
  select(GRAND_ID, DAM_NAME, RES_NAME, RIVER, CAP_MCM, MAIN_USE, area_calc,
         Basin:Province, 
         RC1:RC2_P5rc) %>%
  rename_at(vars(contains("3rc")), ~ str_replace(., "3rc", "30rc")) %>%
  rename_at(vars(contains("5rc")), ~ str_replace(., "5rc", "50rc")) %>%
  mutate_at(vars(RC1:RC2_P50rc), ~ round(., 2)) %>%
  mutate(Capacity_perc = round(CAP_MCM/5746118*100, 2),
         Status = "Existing") %>%
  select(Status, Id = GRAND_ID, Name = DAM_NAME, Capacity = CAP_MCM, Capacity_perc, Use = MAIN_USE, CatchArea = area_calc, Country, Province, Basin, RC1:RC2_P50rc)

projected_dams <- st_read("intermediate_output/dam_results.gdb", layer = "FHReD2015_withGOID") %>%
  rename(CountryDataset = Country) %>% 
  st_join(major_basins) %>%
  st_join(adm_boundaries) %>%
  select(DAM_ID, Project_na, Main_river, Capacity__, Stage, area_calc,
         Basin:Province, 
         RC1:RC2_P5rc) %>%
  rename_at(vars(contains("3rc")), ~ str_replace(., "3rc", "30rc")) %>%
  rename_at(vars(contains("5rc")), ~ str_replace(., "5rc", "50rc")) %>%
  mutate_at(vars(RC1:RC2_P50rc), ~ round(., 2)) %>%
  mutate(Capacity_perc = round(Capacity__/724646.5*100, 2), 
         Status = case_when(Stage == "P" ~ "Planned", Stage == "U" ~ "Under construction"),
         Use = "Hydroelectricity") %>%
  select(Status, Id = DAM_ID, Name = Project_na, Capacity = Capacity__, Capacity_perc, Use, CatchArea = area_calc, Country, Province, Basin, RC1:RC2_P50rc)

    
#### Save RDSs ----
saveRDS(existing_dams, "outputs/rds/existing_dams_scenarios.rds")
saveRDS(projected_dams, "outputs/rds/projected_dams_scenarios.rds")
rbind(existing_dams, projected_dams) %>%
  saveRDS("outputs/rds/all_dams_scenarios.rds")


#### Export ----
# Shapefile
st_write(existing_dams, "outputs/shp/GRanD_WRFscenarios.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)
st_write(projected_dams, "outputs/shp/FHReD_WRFscenarios.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)


# Excel
columnsDescriptionGRanD <- tribble(
  ~Column, ~Description,
  "Status",   "Status of the project",
  "Id",   "Dam ID, same as 'GRAND_ID' in GRanD",
  "Name", "Name of dam, same as 'DAM_NAME' in GRanD",
  "Capacity", "Reservoir capacity in million m3, same as 'CAP_MCM' in GRanD",
  "Capacity_perc", "Reservoir capacity in percentage, relative to the global existing capacity",
  "Use", "Main purpose of reservoir, same as 'MAIN_USE' in GRanD",
  "CatchArea", "Area of the catchment in km2, based on HydroSHEDS (https://www.hydrosheds.org)",
  "Country", "Name of country, same as 'NAME_0' in GADM 3.6, or name of country and province in case of large countries, same as 'NAME_0'-'NAME_1' (https://gadm.org)",
  "Province", "Name of province (or first level of sub-national administrative division), same as 'NAME_1' in GADM 3.6 (https://gadm.org)",
  "Basin", "Name of major river basin of the world, based primarily on GRDC 2020 (https://www.bafg.de/GRDC/EN/02_srvcs/22_gslrs/221_MRB/riverbasins_node.html)",
  "RC1",   "WRF 2020 risk score in the risk category 1. Scarcity",
  "RC2",   "WRF 2020 risk score in the risk category 2. Flooding",
  "RC10",   "WRF 2020 risk score in the risk category 10. Biodiversity Importance",
  "[RiskLayer]_O30", "Scenario of [RiskLayer] in the Optimistic pathway, for the year 2030",
  "[RiskLayer]_O50", "Scenario of [RiskLayer] in the Optimistic pathway, for the year 2050",
  "[RiskLayer]_C30", "Scenario of [RiskLayer] in the Current trend pathway, for the year 2030",
  "[RiskLayer]_C50", "Scenario of [RiskLayer] in the Current trend pathway, for the year 2050",
  "[RiskLayer]_P30", "Scenario of [RiskLayer] in the Pessimistic pathway, for the year 2030",
  "[RiskLayer]_P50", "Scenario of [RiskLayer] in the Pessimistic pathway, for the year 2050",
  "[RiskLayer]_[PathwayYear]rc", "Change in risk score between today's risk and scenario of [RiskLayer] in the [PathwayYear, e.g., O30,O50,C30,C50,P30,P50]"
)

columnsDescriptionFHReD <- tribble(
  ~Column, ~Description,
  "Status",   "Status of the project",
  "Id",   "Dam ID, same as 'DAM_ID' in FHReD",
  "Name", "Name of project, same as 'Project_na' in FHReD",
  "Capacity", "Hydropower capacity in MW, same as 'Capacity__' in FHReD",
  "Capacity_perc", "Hydropower capacity in percentage, relative to the global projected capacity",
  "Use", "Main purpose of reervoir",
  "CatchArea", "Area of the catchment in km2, based on HydroSHEDS (https://www.hydrosheds.org)",
  "Country", "Name of country, same as 'NAME_0' in GADM 3.6, or name of country and province in case of large countries, same as 'NAME_0'-'NAME_1' (https://gadm.org)",
  "Province", "Name of province (or first level of sub-national administrative division), same as 'NAME_1' in GADM 3.6 (https://gadm.org)",
  "Basin", "Name of major river basin of the world, based primarily on GRDC 2020 (https://www.bafg.de/GRDC/EN/02_srvcs/22_gslrs/221_MRB/riverbasins_node.html)",
  "RC1",   "WRF 2020 risk score in the risk category 1. Scarcity",
  "RC2",   "WRF 2020 risk score in the risk category 2. Flooding",
  "RC10",   "WRF 2020 risk score in the risk category 10. Biodiversity Importance",
  "[RiskLayer]_O30", "Scenario of [RiskLayer] in the Optimistic pathway, for the year 2030",
  "[RiskLayer]_O50", "Scenario of [RiskLayer] in the Optimistic pathway, for the year 2050",
  "[RiskLayer]_C30", "Scenario of [RiskLayer] in the Current trend pathway, for the year 2030",
  "[RiskLayer]_C50", "Scenario of [RiskLayer] in the Current trend pathway, for the year 2050",
  "[RiskLayer]_P30", "Scenario of [RiskLayer] in the Pessimistic pathway, for the year 2030",
  "[RiskLayer]_P50", "Scenario of [RiskLayer] in the Pessimistic pathway, for the year 2050",
  "[RiskLayer]_[PathwayYear]rc", "Change in risk score between today's risk and scenario of [RiskLayer] in the [PathwayYear, e.g., O30,O50,C30,C50,P30,P50]"
)

openxlsx::write.xlsx(list(
  "GRanD+WRF Scenarios" = st_drop_geometry(existing_dams),
  "Columns description" = columnsDescriptionGRanD),
  file = "outputs/xlsx/GRanD_WRFscenarios.xlsx", row.names = FALSE)

openxlsx::write.xlsx(list(
  "FHReD+WRF Scenarios" = st_drop_geometry(projected_dams),
  "Columns description" = columnsDescriptionFHReD),
  file = "outputs/xlsx/FHReD_WRFscenarios.xlsx", row.names = FALSE)
