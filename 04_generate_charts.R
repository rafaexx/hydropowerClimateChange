library(tidyverse)
library(sf)

#### Load data ----
current_dams <- readRDS("outputs/rds/current_dams_scenarios.rds")
future_dams <- readRDS("outputs/rds/future_dams_scenarios.rds")
summaryCountry_grand <- readRDS("outputs/rds/summaryCountry_grand.rds")
summaryCountry_fhred <- readRDS("outputs/rds/summaryCountry_fhred.rds")


#### Boxplot of risks by Country (Analyses 1 to 4)----
# X = Country (number of dams)
# Y = Change in Risk by 2050 (pessimistic scenario)
# Color = Current risk
# Width = Sum of dam capacity 

makeBoxplot1to4 <- function(data, summary, riskChange, n, RCx_P5rc, RCx, AxisLabel, title, fileName){
  
  riskChange <- enquo(riskChange)
  RCx_P5rc <- enquo(RCx_P5rc)
  RCx <- enquo(RCx)
  
  temp <- summary %>%
    arrange(desc(!!riskChange)) %>%
    slice(seq_len(n)) %>%
    mutate(CountryCount = glue::glue("{Country} ({Count})"),
           Weight = Capacity/Count) %>%
    select(Country, Count, CountryCount, Weight)
  
  mydata <- data %>% 
    inner_join(temp) %>% 
    group_by(CountryCount) %>% 
    mutate(`Median risk (2020)` = median(!!RCx)) 
  
  b <- ggplot(mydata, aes(x = forcats::fct_reorder(CountryCount, !!RCx_P5rc, .fun = mean, .desc = TRUE),
                          y = !!RCx_P5rc)) +
    
    geom_boxplot(aes(fill = `Median risk (2020)`, weight = Weight), varwidth = TRUE, outlier.size=0.5) +
    
    scale_fill_gradient2(midpoint = 3, low = "#e9ffbe", mid = "#ffd700", high = "#e60000",
                         breaks = c(1,2,3,4,5), labels = c(1,2,3,4,5), limits = c(1,5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.6)) +
    labs(x = NULL, y = AxisLabel) +
    
    ggtitle(title) +                                                   
    
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          #legend.position="none",
          plot.margin=unit(c(0.5,0.5,0.5,1),"cm")) #top, right, bottom, left
  #b
  ggsave(sprintf("outputs/boxplots/%s.jpeg", fileName), b,
         width = 35, height = 15, dpi = 300, units = "cm", device='jpeg')
  #plotly::ggplotly(b)
}

makeBoxplot1to4(current_dams, summaryCountry_grand, ScarcityChange_P50, 25, RC1_P5rc, RC1, "Change in scarcity risk by 2050", "Existing dams  |  Scarcity risk per country", "Boxplot1_GRanD_ChangeScarcityP50")
makeBoxplot1to4(current_dams, summaryCountry_grand, FloodChange_P50, 25, RC2_P5rc, RC2, "Change in flood risk by 2050", "Existing dams  |  Flood risk per country", "Boxplot2_GRanD_ChangeFloodingP50")
makeBoxplot1to4(future_dams, summaryCountry_fhred, ScarcityChange_P50, 25, RC1_P5rc, RC1, "Change in scarcity risk by 2050", "Potential dams  |  Scarcity risk per country", "Boxplot3_FHReD_ChangeScarcityP50")
makeBoxplot1to4(future_dams, summaryCountry_fhred, FloodChange_P50, 25, RC2_P5rc, RC2, "Change in flood risk by 2050", "Potential dams  |  Flood risk per country", "Boxplot4_FHReD_ChangeFloodingP50")


#### Bar charts by risk class (Analyses 1 to 4) ----
makeBarchart1to4 <- function(data, RC, RC_O, RC_C, RC_P, fileName){
  RC <- enquo(RC)
  RC_O <- enquo(RC_O)
  RC_C <- enquo(RC_C)
  RC_P <- enquo(RC_P)
  
  b <- data %>%
    mutate(
      `2020` = case_when(
        !!RC <= 1.8 ~ "Very low",
        !!RC > 1.8 & !!RC <= 2.6 ~ "Low",
        !!RC > 2.6 & !!RC <= 3.4 ~ "Medium",
        !!RC > 3.4 & !!RC <= 4.2 ~ "High",
        !!RC > 4.2 & !!RC <= 5.0 ~ "Very high"
      ),
      Optimistic = case_when(
        !!RC_O <= 1.8 ~ "Very low",
        !!RC_O > 1.8 & !!RC_O <= 2.6 ~ "Low",
        !!RC_O > 2.6 & !!RC_O <= 3.4 ~ "Medium",
        !!RC_O > 3.4 & !!RC_O <= 4.2 ~ "High",
        !!RC_O > 4.2 & !!RC_O <= 5.0 ~ "Very high",
        !!RC_O > 5.0 & !!RC_O <= 6.6 ~ "Extreme"
      ),
      `Current trend` = case_when(
        !!RC_C <= 1.8 ~ "Very low",
        !!RC_C > 1.8 & !!RC_C <= 2.6 ~ "Low",
        !!RC_C > 2.6 & !!RC_C <= 3.4 ~ "Medium",
        !!RC_C > 3.4 & !!RC_C <= 4.2 ~ "High",
        !!RC_C > 4.2 & !!RC_C <= 5.0 ~ "Very high",
        !!RC_C > 5.0 & !!RC_C <= 6.6 ~ "Extreme"
      ),
      Pessimistic = case_when(
        !!RC_P <= 1.8 ~ "Very low",
        !!RC_P > 1.8 & !!RC_P <= 2.6 ~ "Low",
        !!RC_P > 2.6 & !!RC_P <= 3.4 ~ "Medium",
        !!RC_P > 3.4 & !!RC_P <= 4.2 ~ "High",
        !!RC_P > 4.2 & !!RC_P <= 5.0 ~ "Very high",
        !!RC_P > 5.0 & !!RC_P <= 6.6 ~ "Extreme"
      )
    ) %>%
    select(`2020`:Pessimistic) %>%
    gather(Group, Risk, `2020`:Pessimistic) %>%
    mutate(
      Group = factor(.$Group, levels = c("2020", "Optimistic", "Current trend", "Pessimistic")),
      Risk = factor(.$Risk, levels = c("Extreme", "Very high", "High", "Medium","Low" , "Very low"))
    ) %>%
    ggplot(aes(x = Group, fill = Risk)) +
    geom_bar(position="fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("#8c0000", "#e60000", "#ff5500", "#ffaa00", "#ffff73", "#e9ffbe")) +
    labs(x= "           Scenarios for 2050", y= " ") +
    theme_classic() +
    coord_flip()
  ggsave(sprintf("outputs/barcharts/%s.jpeg", fileName), b,
         width = 12, height = 6, dpi = 300, units = "cm", device='jpeg')
}

makeBarchart1to4(current_dams, RC1, RC1_O50, RC1_C50, RC1_P50, "Barchart1_GRanD_Scenarios_Scarcity")
makeBarchart1to4(current_dams, RC2, RC2_O50, RC2_C50, RC2_P50, "Barchart2_GRanD_Scenarios_Flooding")
makeBarchart1to4(future_dams, RC1, RC1_O50, RC1_C50, RC1_P50, "Barchart3_FHReD_Scenarios_Scarcity")
makeBarchart1to4(future_dams, RC2, RC2_O50, RC2_C50, RC2_P50, "Barchart4_FHReD_Scenarios_Flooding")


#### Bar charts by risk class (Analysis 5) ----
b5a <- current_dams %>%
  mutate(
    RC10 = case_when(
      RC10 <= 1.8 ~ 1.0,
      RC10 > 1.8 & RC10 <= 2.6 ~ 1.8,
      RC10 > 2.6 & RC10 <= 3.4 ~ 2.6,
      RC10 > 3.4 & RC10 <= 4.2 ~ 3.4,
      RC10 > 4.2 & RC10 <= 5.0 ~ 4.2
    )
  ) %>%
  ggplot(aes(x = RC10)) +
  geom_bar(fill = c("#e9ffbe", "#ffff73", "#ffaa00", "#ff5500", "#e60000")) +
  scale_x_continuous(breaks = c(1, 1.8, 2.6, 3.4, 4.2, 5), limits = c(0.5, 5)) +
  scale_y_continuous(limits = c(0,1900)) +
  geom_text(stat='count', aes(label=paste0(round((..count..)/sum(..count..)*100), "%")), vjust=-1, color="black",
            position = position_dodge(0.9), size=3.5) +
  theme_classic() +
  labs(x= "Biodiversity risk in 2020", y= "Number of existing dams")
ggsave("outputs/barcharts/Barchart5a_GRanD_BiodiversityImportance2020.jpeg", b5a,
       width = 10, height = 6, dpi = 300, units = "cm", device='jpeg')


b5b <- future_dams %>%
  mutate(
    RC10 = case_when(
      RC10 <= 1.8 ~ 1.0,
      RC10 > 1.8 & RC10 <= 2.6 ~ 1.8,
      RC10 > 2.6 & RC10 <= 3.4 ~ 2.6,
      RC10 > 3.4 & RC10 <= 4.2 ~ 3.4,
      RC10 > 4.2 & RC10 <= 5.0 ~ 4.2
    )
  ) %>%
  ggplot(aes(x = RC10)) +
  geom_bar(fill = c("#e9ffbe", "#ffff73", "#ffaa00", "#ff5500", "#e60000")) +
  scale_x_continuous(breaks = c(1, 1.8, 2.6, 3.4, 4.2, 5), limits = c(0.5, 5)) +
  scale_y_continuous(limits = c(0,1900)) +
  geom_text(stat='count', aes(label=paste0(round((..count..)/sum(..count..)*100), "%")), vjust=-1, color="black",
            position = position_dodge(0.9), size=3.5) +
  theme_classic() +
  labs(x= "Biodiversity risk in 2020", y= "Number of future dams")
ggsave("outputs/barcharts/Barchart5b_FHReD_BiodiversityImportance2020.jpeg", b5b,
       width = 10, height = 6, dpi = 300, units = "cm", device='jpeg')
