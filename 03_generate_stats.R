library(tidyverse)
library(sf)

#### Load data ----
current_dams <- readRDS("outputs/rds/current_dams_scenarios.rds") %>%
  st_drop_geometry()

future_dams <- readRDS("outputs/rds/future_dams_scenarios.rds") %>%
  st_drop_geometry()

all_dams <- readRDS("outputs/rds/all_dams_scenarios.rds") %>%
  st_drop_geometry()


#### 1. GRanD + Change in Scarcity (Pessimistic 2050) ----

## Number of dams
# ...exposed to medium to very high risk over time
tibble(
  time = c(2020, 2030,2050),
  optimistic = c(tally(filter(current_dams, RC1 > 2.6))[[1]], tally(filter(current_dams, RC1_O30 > 2.6))[[1]], tally(filter(current_dams, RC1_O50 > 2.6))[[1]]),
  current = c(tally(filter(current_dams, RC1 > 2.6))[[1]], tally(filter(current_dams, RC1_C30 > 2.6))[[1]], tally(filter(current_dams, RC1_C50 > 2.6))[[1]]),
  pessimistic = c(tally(filter(current_dams, RC1 > 2.6))[[1]], tally(filter(current_dams, RC1_P30 > 2.6))[[1]], tally(filter(current_dams, RC1_P50 > 2.6))[[1]])
) %>%
  mutate_at(vars(optimistic, current, pessimistic), .funs = list(perc = ~ ./tally(current_dams)[[1]])) #%>%
  #openxlsx::write.xlsx("C:/Users/Rafael.Camargo/OneDrive - wwfgermany/WRF/Reports/Hydropower/Summary1_GRanD_ChangeScarcity.xlsx")
  
# ...likely to face increase in risk by 2050 under a pessimistic scenario
tally(filter(current_dams, RC1_P5rc > 0.2))/tally(current_dams) # 692/2488 (28%)
# and already exposed to medium to very high risk today
tally(filter(current_dams, RC1_P5rc > 0.2 & RC1 > 2.6))/tally(filter(current_dams, RC1_P5rc > 0.2)) # 399/692 (58%)


## Dams capacity (million m3)
# ...exposed to medium to very high risk over time
tibble(
  time = c(2020, 2030,2050),
  optimistic = c(sum(current_dams %>% filter(RC1 > 2.6) %>% select(CAP_MCM)), sum(current_dams %>% filter(RC1_O30 > 2.6) %>% select(CAP_MCM)), sum(current_dams %>% filter(RC1_O50 > 2.6) %>% select(CAP_MCM))),
  current = c(sum(current_dams %>% filter(RC1 > 2.6) %>% select(CAP_MCM)), sum(current_dams %>% filter(RC1_C30 > 2.6) %>% select(CAP_MCM)), sum(current_dams %>% filter(RC1_C50 > 2.6) %>% select(CAP_MCM))),
  pessimistic = c(sum(current_dams %>% filter(RC1 > 2.6) %>% select(CAP_MCM)), sum(current_dams %>% filter(RC1_P30 > 2.6) %>% select(CAP_MCM)), sum(current_dams %>% filter(RC1_P50 > 2.6) %>% select(CAP_MCM)))
) %>%
  mutate_at(vars(optimistic, current, pessimistic), .funs = list(perc = ~ ./sum(current_dams %>% select(CAP_MCM))))

# ...likely to face increase in risk by 2050 under a pessimistic scenario
sum(current_dams %>% filter(RC1_P5rc > 0.2) %>% select(CAP_MCM))/sum(current_dams %>% select(CAP_MCM)) # 1.07/5.75 trillion m3 (19%)
# and already exposed to medium to very high risk today
sum(current_dams %>% filter(RC1_P5rc > 0.2 & RC1 > 2.6) %>% select(CAP_MCM))/sum(current_dams %>% filter(RC1_P5rc > 0.2) %>% select(CAP_MCM)) # 0.58/1.07 trillion m3 (54%)


#### 2. GRanD + Change in Flooding (Pessimistic 2050) ----

## Number of dams
# ...exposed to medium to very high risk over time
tibble(
  time = c(2020, 2030,2050),
  optimistic = c(tally(filter(current_dams, RC2 > 2.6))[[1]], tally(filter(current_dams, RC2_O30 > 2.6))[[1]], tally(filter(current_dams, RC2_O50 > 2.6))[[1]]),
  current = c(tally(filter(current_dams, RC2 > 2.6))[[1]], tally(filter(current_dams, RC2_C30 > 2.6))[[1]], tally(filter(current_dams, RC2_C50 > 2.6))[[1]]),
  pessimistic = c(tally(filter(current_dams, RC2 > 2.6))[[1]], tally(filter(current_dams, RC2_P30 > 2.6))[[1]], tally(filter(current_dams, RC2_P50 > 2.6))[[1]])
) %>%
  mutate_at(vars(optimistic, current, pessimistic), .funs = list(perc = ~ ./tally(current_dams)[[1]]))

# ...likely to face increase in risk by 2050 under a pessimistic scenario
tally(filter(current_dams, RC2_P5rc > 0.2))/tally(current_dams) # 906/2488 (36%)
# and already exposed to medium to very high risk today
tally(filter(current_dams, RC2_P5rc > 0.2 & RC2 > 2.6))/tally(filter(current_dams, RC2_P5rc > 0.2)) # 806/906 (89%)


## Dams capacity (million m3)
# ...exposed to medium to very high risk over time
tibble(
  time = c(2020, 2030,2050),
  optimistic = c(sum(current_dams %>% filter(RC2 > 2.6) %>% select(CAP_MCM)), sum(current_dams %>% filter(RC2_O30 > 2.6) %>% select(CAP_MCM)), sum(current_dams %>% filter(RC2_O50 > 2.6) %>% select(CAP_MCM))),
  current = c(sum(current_dams %>% filter(RC2 > 2.6) %>% select(CAP_MCM)), sum(current_dams %>% filter(RC2_C30 > 2.6) %>% select(CAP_MCM)), sum(current_dams %>% filter(RC2_C50 > 2.6) %>% select(CAP_MCM))),
  pessimistic = c(sum(current_dams %>% filter(RC2 > 2.6) %>% select(CAP_MCM)), sum(current_dams %>% filter(RC2_P30 > 2.6) %>% select(CAP_MCM)), sum(current_dams %>% filter(RC2_P50 > 2.6) %>% select(CAP_MCM)))
) %>%
  mutate_at(vars(optimistic, current, pessimistic), .funs = list(perc = ~ ./sum(current_dams %>% select(CAP_MCM))))

# ...likely to face increase in risk by 2050 under a pessimistic scenario
sum(current_dams %>% filter(RC2_P5rc > 0.2) %>% select(CAP_MCM))/sum(current_dams %>% select(CAP_MCM)) # 3.25/5.75 trillion m3 (57%)
# and already exposed to medium to very high risk today
sum(current_dams %>% filter(RC2_P5rc > 0.2 & RC2 > 2.6) %>% select(CAP_MCM))/sum(current_dams %>% filter(RC2_P5rc > 0.2) %>% select(CAP_MCM)) # 2.46/3.25 trillion m3 (76%)


#### 3. FHReD + Change in Scarcity (Pessimistic 2050) ----

## Number of dams
# ...exposed to medium to very high risk over time
tibble(
  time = c(2020, 2030,2050),
  optimistic = c(tally(filter(future_dams, RC1 > 2.6))[[1]], tally(filter(future_dams, RC1_O30 > 2.6))[[1]], tally(filter(future_dams, RC1_O50 > 2.6))[[1]]),
  current = c(tally(filter(future_dams, RC1 > 2.6))[[1]], tally(filter(future_dams, RC1_C30 > 2.6))[[1]], tally(filter(future_dams, RC1_C50 > 2.6))[[1]]),
  pessimistic = c(tally(filter(future_dams, RC1 > 2.6))[[1]], tally(filter(future_dams, RC1_P30 > 2.6))[[1]], tally(filter(future_dams, RC1_P50 > 2.6))[[1]])
) %>%
  mutate_at(vars(optimistic, current, pessimistic), .funs = list(perc = ~ ./tally(future_dams)[[1]]))

# ...likely to face increase in risk by 2050 under a pessimistic scenario
tally(filter(future_dams, RC1_P5rc > 0.2))/tally(future_dams) # 617/3700 (17%)
# and already exposed to medium to very high risk today
tally(filter(future_dams, RC1_P5rc > 0.2 & RC1 > 2.6))/tally(filter(future_dams, RC1_P5rc > 0.2)) # 439/617 (71%)


## Dams capacity (MW)
# ...exposed to medium to very high risk over time
tibble(
  time = c(2020, 2030,2050),
  optimistic = c(sum(future_dams %>% filter(RC1 > 2.6) %>% select(Capacity__)), sum(future_dams %>% filter(RC1_O30 > 2.6) %>% select(Capacity__)), sum(future_dams %>% filter(RC1_O50 > 2.6) %>% select(Capacity__))),
  current = c(sum(future_dams %>% filter(RC1 > 2.6) %>% select(Capacity__)), sum(future_dams %>% filter(RC1_C30 > 2.6) %>% select(Capacity__)), sum(future_dams %>% filter(RC1_C50 > 2.6) %>% select(Capacity__))),
  pessimistic = c(sum(future_dams %>% filter(RC1 > 2.6) %>% select(Capacity__)), sum(future_dams %>% filter(RC1_P30 > 2.6) %>% select(Capacity__)), sum(future_dams %>% filter(RC1_P50 > 2.6) %>% select(Capacity__)))
) %>%
  mutate_at(vars(optimistic, current, pessimistic), .funs = list(perc = ~ ./sum(future_dams %>% select(Capacity__))))

# ...likely to face increase in risk by 2050 under a pessimistic scenario
sum(future_dams %>% filter(RC1_P5rc > 0.2) %>% select(Capacity__))/sum(future_dams %>% select(Capacity__)) # 80.82/724.65 GW (11%)
# and already exposed to medium to very high risk today
sum(future_dams %>% filter(RC1_P5rc > 0.2 & RC1 > 2.6) %>% select(Capacity__))/sum(future_dams %>% filter(RC1_P5rc > 0.2) %>% select(Capacity__)) # 53.73/80.82 GW (66%)


#### 4. FHReD + Change in Flooding (Pessimistic 2050) ----

## Number of dams
# ...exposed to medium to very high risk over time
tibble(
  time = c(2020, 2030,2050),
  optimistic = c(tally(filter(future_dams, RC2 > 2.6))[[1]], tally(filter(future_dams, RC2_O30 > 2.6))[[1]], tally(filter(future_dams, RC2_O50 > 2.6))[[1]]),
  current = c(tally(filter(future_dams, RC2 > 2.6))[[1]], tally(filter(future_dams, RC2_C30 > 2.6))[[1]], tally(filter(future_dams, RC2_C50 > 2.6))[[1]]),
  pessimistic = c(tally(filter(future_dams, RC2 > 2.6))[[1]], tally(filter(future_dams, RC2_P30 > 2.6))[[1]], tally(filter(future_dams, RC2_P50 > 2.6))[[1]])
) %>%
  mutate_at(vars(optimistic, current, pessimistic), .funs = list(perc = ~ ./tally(future_dams)[[1]]))

# ...likely to face increase in risk by 2050 under a pessimistic scenario
tally(filter(future_dams, RC2_P5rc > 0.2))/tally(future_dams) # 2270/3700 (61%)
# and already exposed to medium to very high risk today
tally(filter(future_dams, RC2_P5rc > 0.2 & RC2 > 2.6))/tally(filter(future_dams, RC2_P5rc > 0.2)) # 1963/2270 (86%)


## Dams capacity (MW)
# ...exposed to medium to very high risk over time
tibble(
  time = c(2020, 2030,2050),
  optimistic = c(sum(future_dams %>% filter(RC2 > 2.6) %>% select(Capacity__)), sum(future_dams %>% filter(RC2_O30 > 2.6) %>% select(Capacity__)), sum(future_dams %>% filter(RC2_O50 > 2.6) %>% select(Capacity__))),
  current = c(sum(future_dams %>% filter(RC2 > 2.6) %>% select(Capacity__)), sum(future_dams %>% filter(RC2_C30 > 2.6) %>% select(Capacity__)), sum(future_dams %>% filter(RC2_C50 > 2.6) %>% select(Capacity__))),
  pessimistic = c(sum(future_dams %>% filter(RC2 > 2.6) %>% select(Capacity__)), sum(future_dams %>% filter(RC2_P30 > 2.6) %>% select(Capacity__)), sum(future_dams %>% filter(RC2_P50 > 2.6) %>% select(Capacity__)))
) %>%
  mutate_at(vars(optimistic, current, pessimistic), .funs = list(perc = ~ ./sum(future_dams %>% select(Capacity__))))

# ...likely to face increase in risk by 2050 under a pessimistic scenario
sum(future_dams %>% filter(RC2_P5rc > 0.2) %>% select(Capacity__))/sum(future_dams %>% select(Capacity__)) # 578.10/724.65 GW (80%)
# and already exposed to medium to very high risk today
sum(future_dams %>% filter(RC2_P5rc > 0.2 & RC2 > 2.6) %>% select(Capacity__))/sum(future_dams %>% filter(RC2_P5rc > 0.2) %>% select(Capacity__)) # 478.88/578.10 GW (83%)


#### 5. GRanD/FHReD + Biodiversity Importance (2020) ----

## Number of dams
# ...already exposed to medium to very high risk today
tally(filter(current_dams, RC10 > 2.6))/tally(current_dams) # 1874/2488 (75%)
tally(filter(future_dams, RC10 > 2.6))/tally(future_dams) # 3431/3700 (93%)


## Dams capacity
# ...already exposed to medium to very high risk today
sum(current_dams %>% filter(RC10 > 2.6) %>% select(CAP_MCM))/sum(current_dams %>% select(CAP_MCM)) # 3.37/5.75 trillion m3 (59%)
sum(future_dams %>% filter(RC10 > 2.6) %>% select(Capacity__))/sum(future_dams %>% select(Capacity__)) # 664.86/724.65 GW (92%)


#### 6. GRanD + FHReD + Scarcity (2050) + Biodiversity Importance (2020) ----

## Number of dams
# ...already exposed to medium to very high risk today
tally(filter(all_dams, RC1_P50 > 2.6 & RC10 > 2.6))/tally(all_dams) # 1612/6188 (26%)


# Geographic distribution
all_dams %>%
  filter(RC1_P50 > 2.6 & RC10 > 2.6) %>%
  group_by(Country) %>%
  summarise(n=n()) %>%
  arrange(desc(n))


# Risk combination
temp_all_dams <- all_dams %>%
  mutate(
    Capacity_perc = sqrt(Capacity_perc)*15, Capacity_perc = if_else(Capacity_perc < 0.1, 0.1, Capacity_perc), #only for visualization purposes!
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

temp_all_dams %>%
  group_by(`Risk combination`) %>%
  summarise(
    n=n(),
    perc=round(n()/6188*100)
  )


#### 7. GRanD + FHReD + Scarcity (2020) + Flooding (2050) ----

## Number of dams
# ...already exposed to medium to very high risk today
tally(filter(all_dams, RC1_P50 > 2.6 & RC2_P50 > 2.6))/tally(all_dams) # 1096/6188 (18%)


# Geographic distribution
all_dams %>%
  filter(RC1_P50 > 2.6 & RC2_P50 > 2.6) %>%
  group_by(Country) %>%
  summarise(n=n()) %>%
  arrange(desc(n))


# Risk combination
temp_all_dams <- all_dams %>%
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

temp_all_dams %>%
  group_by(`Risk combination`) %>%
  summarise(
    n=n(),
    perc=round(n()/6188*100)
  )


#### Management bins (Analyses 1 to 4) ----
temp <- current_dams %>%
  mutate(
    Group = case_when(
      RC1 <= 2.6 & RC1_P50 <= 2.6 ~ "Low risk in 2020 & keep low",
      RC1 > 2.6 & RC1_P50 <= 2.6 ~ "Medium-high risk in 2020 & decrease to low",
      RC1 <= 2.6 & RC1_P50 > 2.6 ~ "Low risk in 2020 & increase to medium-high",
      between(RC1, 2.600001, 5) & between(RC1_P5rc, -0.2, 0.2) ~ "Medium-very high risk in 2020 & minimal change",
      between(RC1, 2.600001, 5) & RC1_P5rc > 0.2 & RC1_P50 <= 5 ~ "Medium-very high risk in 2020 & increase",
      RC1 > 3.4 & RC1_P50 > 5 ~ "High-very high risk in 2020 & increase to extreme",
    )
  ) %>%
  #filter(is.na(Group)) %>%
  #select(RC2, RC2_P50, RC2_P5rc)
  group_by(Group) %>%
  summarise(
    n=n(),
    perc=n()/2488,
    cap=sum(CAP_MCM),
    cap_perc=sum(CAP_MCM)/5746118 #sum(Capacity__)/724646.5
  )

current_dams %>%
  mutate(
    Group = case_when(
      RC1 <= 2.6 & RC1_P50 <= 2.6 ~ "Low risk in 2020 & keep low",
      RC1 > 2.6 & RC1_P50 <= 2.6 ~ "Medium-high risk in 2020 & decrease to low",
      RC1 <= 2.6 & RC1_P50 > 2.6 ~ "Low risk in 2020 & increase to medium-high",
      between(RC1, 2.600001, 5) & between(RC1_P5rc, -0.2, 0.2) ~ "Medium-very high risk in 2020 & minimal change",
      between(RC1, 2.600001, 5) & RC1_P5rc > 0.2 & RC1_P50 <= 5 ~ "Medium-very high risk in 2020 & increase",
      RC1 > 3.4 & RC1_P50 > 5 ~ "High-very high risk in 2020 & increase to extreme",
    )
  ) %>%
  filter(Group == "High-very high risk in 2020 & increase to extreme") %>%
  group_by(Country) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
