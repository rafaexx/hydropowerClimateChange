library(tidyverse)
library(sf)
library(networkD3)


#### Load data ----
existing_dams <- readRDS("outputs/rds/existing_dams_scenarios.rds") %>% st_drop_geometry()
projected_dams <- readRDS("outputs/rds/projected_dams_scenarios.rds") %>% st_drop_geometry()
summaryCountry_grand <- readRDS("outputs/rds/summaryCountry_grand.rds")
summaryCountry_fhred <- readRDS("outputs/rds/summaryCountry_fhred.rds")


#### Boxplot of risks by Country (Analyses 1 to 4)----
# X = Country (number of dams)
# Y = Change in Risk by 2050 (pessimistic scenario)
# Color = Current risk
# Width = Sum of dam capacity 

makeBoxplot1to4 <- function(data, summary, riskChange, n, RCx_P50rc, RCx, AxisLabel, title, fileName){
  
  riskChange <- enquo(riskChange)
  RCx_P50rc <- enquo(RCx_P50rc)
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
  
  b <- ggplot(mydata, aes(x = forcats::fct_reorder(CountryCount, !!RCx_P50rc, .fun = mean, .desc = TRUE),
                          y = !!RCx_P50rc)) +
    
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

makeBoxplot1to4(existing_dams, summaryCountry_grand, ScarcityChange_P50, 25, RC1_P50rc, RC1, "Change in scarcity risk by 2050", "Existing dams  |  Scarcity risk per country", "Boxplot1_GRanD_ChangeScarcityP50")
makeBoxplot1to4(existing_dams, summaryCountry_grand, FloodChange_P50, 25, RC2_P50rc, RC2, "Change in flood risk by 2050", "Existing dams  |  Flood risk per country", "Boxplot2_GRanD_ChangeFloodingP50")
makeBoxplot1to4(projected_dams, summaryCountry_fhred, ScarcityChange_P50, 25, RC1_P50rc, RC1, "Change in scarcity risk by 2050", "Projected dams  |  Scarcity risk per country", "Boxplot3_FHReD_ChangeScarcityP50")
makeBoxplot1to4(projected_dams, summaryCountry_fhred, FloodChange_P50, 25, RC2_P50rc, RC2, "Change in flood risk by 2050", "Projected dams  |  Flood risk per country", "Boxplot4_FHReD_ChangeFloodingP50")


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

makeBarchart1to4(existing_dams, RC1, RC1_O50, RC1_C50, RC1_P50, "Barchart1_GRanD_Scenarios_Scarcity")
makeBarchart1to4(existing_dams, RC2, RC2_O50, RC2_C50, RC2_P50, "Barchart2_GRanD_Scenarios_Flooding")
makeBarchart1to4(projected_dams, RC1, RC1_O50, RC1_C50, RC1_P50, "Barchart3_FHReD_Scenarios_Scarcity")
makeBarchart1to4(projected_dams, RC2, RC2_O50, RC2_C50, RC2_P50, "Barchart4_FHReD_Scenarios_Flooding")


#### Sankey diagrams by risk class (Analyses 1 to 4) ----
#### Classify risk scores
scoresToClasses <- function(variable){
  variable <- rlang::eval_tidy(variable)
  case_when(
    !!variable <= 1.8 ~ "Very_low",
    !!variable > 1.8 & !!variable <= 2.6 ~ "Low",
    !!variable > 2.6 & !!variable <= 3.4 ~ "Medium",
    !!variable > 3.4 & !!variable <= 4.2 ~ "High",
    !!variable > 4.2 & !!variable <= 5.0 ~ "Very_high",
    !!variable > 5.0 & !!variable <= 6.6 ~ "Extreme"
  )
}

makeSankey <- function(data, RCx, RCx_Xxx){
  
  #### Transform to connections
  RCx <- enquo(RCx)
  RCx_Xxx <- enquo(RCx_Xxx)
  
  links <- data %>%
    mutate(
      source = scoresToClasses(!!RCx),
      target = scoresToClasses(!!RCx_Xxx)
    ) %>%
    group_by(source, target) %>%
    summarise(value = n())
  
  links$target <- paste(links$target, "_", sep="")
  
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(
    name=c("Very_high", "High", "Medium", "Low", "Very_low", "Extreme_", "Very_high_", "High_", "Medium_", "Low_", "Very_low_")
  )
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  # prepare colour scale
  myColor <- 'd3.scaleOrdinal() 
  .domain(["Very_high", "High", "Medium", "Low", "Very_low", "Extreme_", "Very_high_", "High_", "Medium_", "Low_", "Very_low_"]) 
  .range(["#e60000", "#ff5500", "#ffaa00", "#ffff73", "#e9ffbe", "#8c0000", "#e60000", "#ff5500", "#ffaa00", "#ffff73", "#e9ffbe"])'
  
  # Make the Network
  sankeyNetwork(Links = links, Nodes = nodes,
                Source = "IDsource", Target = "IDtarget",
                Value = "value", NodeID = "name", 
                iterations = 0, sinksRight=FALSE, 
                colourScale = myColor,
                nodeWidth = 40, nodePadding = 10, 
                fontFamily = "Arial Nova", fontSize = 13)
}

makeSankey(existing_dams, RC1, RC1_P50) 
makeSankey(existing_dams, RC2, RC2_P50)
makeSankey(projected_dams, RC1, RC1_P50)
makeSankey(projected_dams, RC2, RC2_P50)


#### Bar charts by risk class (Analysis 6) ----
b6a <- existing_dams %>%
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
ggsave("outputs/barcharts/Barchart6a_GRanD_Biodiversity2020.jpeg", b5a,
       width = 10, height = 6, dpi = 300, units = "cm", device='jpeg')


b6b <- projected_dams %>%
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
  labs(x= "Biodiversity risk in 2020", y= "Number of projected dams")
ggsave("outputs/barcharts/Barchart6b_FHReD_Biodiversity2020.jpeg", b5b,
       width = 10, height = 6, dpi = 300, units = "cm", device='jpeg')
