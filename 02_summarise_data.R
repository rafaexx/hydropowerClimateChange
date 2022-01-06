library(tidyverse)
library(sf)

#### Load data ----
existing_dams <- readRDS("outputs/rds/existing_dams_scenarios.rds") %>%
  st_drop_geometry()

projected_dams <- readRDS("outputs/rds/projected_dams_scenarios.rds") %>%
  st_drop_geometry()


#### Summarise dams by Country or by Basin ----
summariseDams <- function(input, region){
  
  region <- enquo(region)
  
  input %>%
    group_by(!!region) %>%
    summarise(
      Count = n(),
      Capacity = sum(Capacity),
      # Today
      ScarcityToday = round(mean(RC1),2),
      FloodToday = round(mean(RC2),2),
      BiodiversityToday = round(mean(RC10),2),
      # Optimistic
      ScarcityChange_O50 = round(mean(RC1_O50rc),2),
      FloodChange_O50 = round(mean(RC2_O50rc),2),
      # Current trend
      ScarcityChange_C50 = round(mean(RC1_C50rc),2),
      FloodChange_C50 = round(mean(RC2_C50rc),2),
      # Pessimistic
      ScarcityChange_P50 = round(mean(RC1_P50rc),2),
      FloodChange_P50 = round(mean(RC2_P50rc),2)
    ) %>%
    arrange(desc(Count))
}

summaryCountry_grand <- summariseDams(existing_dams, Country)
summaryBasin_grand <- summariseDams(existing_dams, Basin)
summaryCountry_fhred <- summariseDams(projected_dams, Country)
summaryBasin_fhred <- summariseDams(projected_dams, Basin)


#### Save RDSs ----
saveRDS(summaryCountry_grand, "outputs/rds/summaryCountry_grand.rds")
saveRDS(summaryBasin_grand, "outputs/rds/summaryBasin_grand.rds")
saveRDS(summaryCountry_fhred, "outputs/rds/summaryCountry_fhred.rds")
saveRDS(summaryBasin_fhred, "outputs/rds/summaryBasin_fhred.rds")


#### Export ----
columnsDescription <- tribble(
  ~Column, ~Description,
  "Count",   "Number of dams",
  "Capacity",   "Sum capacity",
  "ScarcityToday",   "Mean risk score in the WRF risk category 1. Scarcity, as the 2020 data.",
  "FloodToday",   "Mean risk score in the WRF risk category 2. Flooding, as the 2020 data.",
  "BiodiversityToday",   "Mean risk score in the WRF risk category 10. Biodiversity Importance, as the 2020 data.",
  "ScarcityChange_O50",   "Mean risk change in the WRF risk category 1. Scarcity, between 2020 and  2050 optimistic scenario.",
  "FloodChange_O50",   "Mean risk change in the WRF risk category 2. Flooding, between 2020 and  2050 optimistic scenario.",
  "ScarcityChange_C50",   "Mean risk change in the WRF risk category 1. Scarcity, between 2020 and  2050 current trend scenario.",
  "FloodChange_C50",   "Mean risk change in the WRF risk category 2. Flooding, between 2020 and  2050 current trend scenario.",
  "ScarcityChange_P50",   "Mean risk change in the WRF risk category 1. Scarcity, between 2020 and  2050 pessimistic scenario.",
  "FloodChange_P50",   "Mean risk change in the WRF risk category 2. Flooding, between 2020 and  2050 pessimistic scenario."
)

openxlsx::write.xlsx(list(
  "GRanD by Basin" = summaryBasin_grand, 
  "GRanD by Country " = summaryCountry_grand, 
  "FHReD by Basin" = summaryBasin_fhred, 
  "FHReD by Country" = summaryCountry_fhred,
  "Columns description" = columnsDescription),
  file = "outputs/xlsx/Hydropower_WRFscenarios_Country_Basin_Summaries.xlsx", row.names = FALSE)
