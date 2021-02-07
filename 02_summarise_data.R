library(tidyverse)
library(sf)

#### Load data ----
current_dams <- readRDS("outputs/rds/current_dams_scenarios.rds") %>%
  st_drop_geometry()

future_dams <- readRDS("outputs/rds/future_dams_scenarios.rds") %>%
  st_drop_geometry()


#### Summary current dams ----
# By basin
summaryBasin_grand <- current_dams %>%
  group_by(Basin) %>%
  summarise(
    Count = n(),
    Capacity = sum(CAP_MCM),
    # Today
    ScarcityToday = round(mean(RC1),2),
    FloodToday = round(mean(RC2),2),
    EcosystemToday = round(mean(RC4),2),
    BiodiversityToday = round(mean(RC10),2),
    ConflictToday = round(mean(RC12),2),
    # Optimistic
    ScarcityChange_O50 = round(mean(RC1_O5rc),2),
    FloodChange_O50 = round(mean(RC2_O5rc),2),
    EcosystemChange_O50 = round(mean(RC4_O5rc),2),
    BiodiversityChange_O50 = round(mean(RC10_O5rc),2),
    ConflictChange_O50 = round(mean(RC12_O5rc),2),
    # Current trend
    ScarcityChange_C50 = round(mean(RC1_C5rc),2),
    FloodChange_C50 = round(mean(RC2_C5rc),2),
    EcosystemChange_C50 = round(mean(RC4_C5rc),2),
    BiodiversityChange_C50 = round(mean(RC10_C5rc),2),
    ConflictChange_C50 = round(mean(RC12_C5rc),2),
    # Pessimistic
    ScarcityChange_P50 = round(mean(RC1_P5rc),2),
    FloodChange_P50 = round(mean(RC2_P5rc),2),
    EcosystemChange_P50 = round(mean(RC4_P5rc),2),
    BiodiversityChange_P50 = round(mean(RC10_P5rc),2),
    ConflictChange_P50 = round(mean(RC12_P5rc),2)
  ) %>%
  arrange(desc(Count))

# By country
summaryCountry_grand <- current_dams %>%
  group_by(Country) %>%
  summarise(
    Count = n(),
    Capacity = sum(CAP_MCM),
    # Today
    ScarcityToday = round(mean(RC1),2),
    FloodToday = round(mean(RC2),2),
    EcosystemToday = round(mean(RC4),2),
    BiodiversityToday = round(mean(RC10),2),
    ConflictToday = round(mean(RC12),2),
    # Optimistic
    ScarcityChange_O50 = round(mean(RC1_O5rc),2),
    FloodChange_O50 = round(mean(RC2_O5rc),2),
    EcosystemChange_O50 = round(mean(RC4_O5rc),2),
    BiodiversityChange_O50 = round(mean(RC10_O5rc),2),
    ConflictChange_O50 = round(mean(RC12_O5rc),2),
    # Current trend
    ScarcityChange_C50 = round(mean(RC1_C5rc),2),
    FloodChange_C50 = round(mean(RC2_C5rc),2),
    EcosystemChange_C50 = round(mean(RC4_C5rc),2),
    BiodiversityChange_C50 = round(mean(RC10_C5rc),2),
    ConflictChange_C50 = round(mean(RC12_C5rc),2),
    # Pessimistic
    ScarcityChange_P50 = round(mean(RC1_P5rc),2),
    FloodChange_P50 = round(mean(RC2_P5rc),2),
    EcosystemChange_P50 = round(mean(RC4_P5rc),2),
    BiodiversityChange_P50 = round(mean(RC10_P5rc),2),
    ConflictChange_P50 = round(mean(RC12_P5rc),2)
  ) %>%
  arrange(desc(Count))


#### Summary future dams ----
# By basin
summaryBasin_fhred <- future_dams %>%
  group_by(Basin) %>%
  summarise(
    Count = n(),
    Capacity = sum(Capacity__),
    # Today
    ScarcityToday = round(mean(RC1),2),
    FloodToday = round(mean(RC2),2),
    EcosystemToday = round(mean(RC4),2),
    BiodiversityToday = round(mean(RC10),2),
    ConflictToday = round(mean(RC12),2),
    # Optimistic
    ScarcityChange_O50 = round(mean(RC1_O5rc),2),
    FloodChange_O50 = round(mean(RC2_O5rc),2),
    EcosystemChange_O50 = round(mean(RC4_O5rc),2),
    BiodiversityChange_O50 = round(mean(RC10_O5rc),2),
    ConflictChange_O50 = round(mean(RC12_O5rc),2),
    # Current trend
    ScarcityChange_C50 = round(mean(RC1_C5rc),2),
    FloodChange_C50 = round(mean(RC2_C5rc),2),
    EcosystemChange_C50 = round(mean(RC4_C5rc),2),
    BiodiversityChange_C50 = round(mean(RC10_C5rc),2),
    ConflictChange_C50 = round(mean(RC12_C5rc),2),
    # Pessimistic
    ScarcityChange_P50 = round(mean(RC1_P5rc),2),
    FloodChange_P50 = round(mean(RC2_P5rc),2),
    EcosystemChange_P50 = round(mean(RC4_P5rc),2),
    BiodiversityChange_P50 = round(mean(RC10_P5rc),2),
    ConflictChange_P50 = round(mean(RC12_P5rc),2)
  ) %>%
  arrange(desc(Count))

# By country
summaryCountry_fhred <- future_dams %>%
  group_by(Country) %>%
  summarise(
    Count = n(),
    Capacity = sum(Capacity__),
    # Today
    ScarcityToday = round(mean(RC1),2),
    FloodToday = round(mean(RC2),2),
    EcosystemToday = round(mean(RC4),2),
    BiodiversityToday = round(mean(RC10),2),
    ConflictToday = round(mean(RC12),2),
    # Optimistic
    ScarcityChange_O50 = round(mean(RC1_O5rc),2),
    FloodChange_O50 = round(mean(RC2_O5rc),2),
    EcosystemChange_O50 = round(mean(RC4_O5rc),2),
    BiodiversityChange_O50 = round(mean(RC10_O5rc),2),
    ConflictChange_O50 = round(mean(RC12_O5rc),2),
    # Current trend
    ScarcityChange_C50 = round(mean(RC1_C5rc),2),
    FloodChange_C50 = round(mean(RC2_C5rc),2),
    EcosystemChange_C50 = round(mean(RC4_C5rc),2),
    BiodiversityChange_C50 = round(mean(RC10_C5rc),2),
    ConflictChange_C50 = round(mean(RC12_C5rc),2),
    # Pessimistic
    ScarcityChange_P50 = round(mean(RC1_P5rc),2),
    FloodChange_P50 = round(mean(RC2_P5rc),2),
    EcosystemChange_P50 = round(mean(RC4_P5rc),2),
    BiodiversityChange_P50 = round(mean(RC10_P5rc),2),
    ConflictChange_P50 = round(mean(RC12_P5rc),2)
  ) %>%
  arrange(desc(Count))


#### Save RDSs ----
saveRDS(summaryBasin_grand, "outputs/rds/summaryBasin_grand.rds")
saveRDS(summaryBasin_fhred, "outputs/rds/summaryBasin_fhred.rds")
saveRDS(summaryCountry_grand, "outputs/rds/summaryCountry_grand.rds")
saveRDS(summaryCountry_fhred, "outputs/rds/summaryCountry_fhred.rds")


#### Export ----
columnsDescription <- tribble(
  ~Column, ~Description,
  "Count",   "Number of dams",
  "Capacity",   "Sum capacity",
  "ScarcityToday",   "Mean risk score in the WRF risk category 1. Scarcity, as the 2020 data.",
  "FloodToday",   "Mean risk score in the WRF risk category 2. Flooding, as the 2020 data.",
  "EcosystemToday",   "Mean risk score in the WRF risk category 4. Ecosystem Services Status, as the 2020 data. Not used in the analysis.",
  "BiodiversityToday",   "Mean risk score in the WRF risk category 10. Biodiversity Importance, as the 2020 data.",
  "ConflictToday",   "Mean risk score in the WRF risk category 12. Conflict, as the 2020 data. Not used in the analysis.",
  "ScarcityChange_O50",   "Mean risk change in the WRF risk category 1. Scarcity, between 2020 and  2050 optimistic scenario.",
  "FloodChange_O50",   "Mean risk change in the WRF risk category 2. Flooding, between 2020 and  2050 optimistic scenario.",
  "EcosystemChange_O50",   "Mean risk change in the WRF risk category 4. Ecosystem Services Status, between 2020 and  2050 optimistic scenario. Not used in the analysis.",
  "BiodiversityChange_O50",   "Mean risk change in the WRF risk category 10. Biodiversity Importance, between 2020 and  2050 optimistic scenario.",
  "ConflictChange_O50",   "Mean risk change in the WRF risk category 12. Conflict, between 2020 and  2050 optimistic scenario. Not used in the analysis.",
  "ScarcityChange_C50",   "Mean risk change in the WRF risk category 1. Scarcity, between 2020 and  2050 current trend scenario.",
  "FloodChange_C50",   "Mean risk change in the WRF risk category 2. Flooding, between 2020 and  2050 current trend scenario.",
  "EcosystemChange_C50",   "Mean risk change in the WRF risk category 4. Ecosystem Services Status, between 2020 and  2050 current trend scenario. Not used in the analysis.",
  "BiodiversityChange_C50",   "Mean risk change in the WRF risk category 10. Biodiversity Importance, between 2020 and  2050 current trend scenario.",
  "ConflictChange_C50",   "Mean risk change in the WRF risk category 12. Conflict, between 2020 and  2050 current trend scenario. Not used in the analysis.",
  "ScarcityChange_P50",   "Mean risk change in the WRF risk category 1. Scarcity, between 2020 and  2050 pessimistic scenario.",
  "FloodChange_P50",   "Mean risk change in the WRF risk category 2. Flooding, between 2020 and  2050 pessimistic scenario.",
  "EcosystemChange_P50",   "Mean risk change in the WRF risk category 4. Ecosystem Services Status, between 2020 and  2050 pessimistic scenario. Not used in the analysis.",
  "BiodiversityChange_P50",   "Mean risk change in the WRF risk category 10. Biodiversity Importance, between 2020 and  2050 pessimistic scenario.",
  "ConflictChange_P50",   "Mean risk change in the WRF risk category 12. Conflict, between 2020 and  2050 pessimistic scenario. Not used in the analysis."
)

openxlsx::write.xlsx(list(
  "GRanD by Basin" = summaryBasin_grand, 
  "GRanD by Country " = summaryCountry_grand, 
  "FHReD by Basin" = summaryBasin_fhred, 
  "FHReD by Country" = summaryCountry_fhred,
  "Columns description" = columnsDescription),
  file = "outputs/xlsx/Hydropower_WRFscenarios_Country_Basin_Summaries.xlsx", row.names = FALSE)
