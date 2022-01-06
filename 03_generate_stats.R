library(tidyverse)
library(sf)

#### Load data ----
existing_dams <- readRDS("outputs/rds/existing_dams_scenarios.rds") %>%
  st_drop_geometry()

projected_dams <- readRDS("outputs/rds/projected_dams_scenarios.rds") %>%
  st_drop_geometry()

all_dams <- readRDS("outputs/rds/all_dams_scenarios.rds") %>%
  st_drop_geometry()


#### 1. GRanD & Change in Scarcity (Pessimistic 2050) ----

## Number of dams
# ...exposed to medium to very high risk over time
tibble(
  time = c(2020, 2030,2050),
  optimistic = c(tally(filter(existing_dams, RC1 > 2.6))[[1]], tally(filter(existing_dams, RC1_O30 > 2.6))[[1]], tally(filter(existing_dams, RC1_O50 > 2.6))[[1]]),
  current = c(tally(filter(existing_dams, RC1 > 2.6))[[1]], tally(filter(existing_dams, RC1_C30 > 2.6))[[1]], tally(filter(existing_dams, RC1_C50 > 2.6))[[1]]),
  pessimistic = c(tally(filter(existing_dams, RC1 > 2.6))[[1]], tally(filter(existing_dams, RC1_P30 > 2.6))[[1]], tally(filter(existing_dams, RC1_P50 > 2.6))[[1]])
) %>%
  mutate_at(vars(optimistic, current, pessimistic), .funs = list(perc = ~ ./tally(existing_dams)[[1]]))

# ...likely to face increase in risk
tally(filter(existing_dams, RC1_P50rc > 0.2))/tally(existing_dams) # 806/2488 (32%)


#### 2. GRanD & Change in Flooding (Pessimistic 2050) ----

## Number of dams
# ...exposed to medium to very high risk over time
tibble(
  time = c(2020, 2030,2050),
  optimistic = c(tally(filter(existing_dams, RC2 > 2.6))[[1]], tally(filter(existing_dams, RC2_O30 > 2.6))[[1]], tally(filter(existing_dams, RC2_O50 > 2.6))[[1]]),
  current = c(tally(filter(existing_dams, RC2 > 2.6))[[1]], tally(filter(existing_dams, RC2_C30 > 2.6))[[1]], tally(filter(existing_dams, RC2_C50 > 2.6))[[1]]),
  pessimistic = c(tally(filter(existing_dams, RC2 > 2.6))[[1]], tally(filter(existing_dams, RC2_P30 > 2.6))[[1]], tally(filter(existing_dams, RC2_P50 > 2.6))[[1]])
) %>%
  mutate_at(vars(optimistic, current, pessimistic), .funs = list(perc = ~ ./tally(existing_dams)[[1]]))

# ...likely to face decrease in risk
tally(filter(existing_dams, RC2_P50rc < -0.2))/tally(existing_dams) # 1056/2488 (42%)

# ...likely to face increase in risk
tally(filter(existing_dams, RC2_P50rc > 0.2))/tally(existing_dams) # 906/2488 (36%)


#### 3. FHReD & Change in Scarcity (Pessimistic 2050) ----

## Number of dams
# ...exposed to medium to very high risk over time
tibble(
  time = c(2020, 2030,2050),
  optimistic = c(tally(filter(projected_dams, RC1 > 2.6))[[1]], tally(filter(projected_dams, RC1_O30 > 2.6))[[1]], tally(filter(projected_dams, RC1_O50 > 2.6))[[1]]),
  current = c(tally(filter(projected_dams, RC1 > 2.6))[[1]], tally(filter(projected_dams, RC1_C30 > 2.6))[[1]], tally(filter(projected_dams, RC1_C50 > 2.6))[[1]]),
  pessimistic = c(tally(filter(projected_dams, RC1 > 2.6))[[1]], tally(filter(projected_dams, RC1_P30 > 2.6))[[1]], tally(filter(projected_dams, RC1_P50 > 2.6))[[1]])
) %>%
  mutate_at(vars(optimistic, current, pessimistic), .funs = list(perc = ~ ./tally(projected_dams)[[1]]))

# ...very low/low today and same risk level in the future
tally(filter(projected_dams, RC1 <= 2.6 & RC1_P50 <= 2.6))/tally(projected_dams) # 2632/3700 (71%)

# ...likely to face increase in risk
tally(filter(projected_dams, RC1_P50rc > 0.2))/tally(projected_dams) # 730/3700 (20%)


#### 4. FHReD & Change in Flooding (Pessimistic 2050) ----

## Number of dams
# ...exposed to medium to very high risk over time
tibble(
  time = c(2020, 2030,2050),
  optimistic = c(tally(filter(projected_dams, RC2 > 2.6))[[1]], tally(filter(projected_dams, RC2_O30 > 2.6))[[1]], tally(filter(projected_dams, RC2_O50 > 2.6))[[1]]),
  current = c(tally(filter(projected_dams, RC2 > 2.6))[[1]], tally(filter(projected_dams, RC2_C30 > 2.6))[[1]], tally(filter(projected_dams, RC2_C50 > 2.6))[[1]]),
  pessimistic = c(tally(filter(projected_dams, RC2 > 2.6))[[1]], tally(filter(projected_dams, RC2_P30 > 2.6))[[1]], tally(filter(projected_dams, RC2_P50 > 2.6))[[1]])
) %>%
  mutate_at(vars(optimistic, current, pessimistic), .funs = list(perc = ~ ./tally(projected_dams)[[1]]))

# ...likely to face increase in risk
tally(filter(projected_dams, RC2_P50rc > 0.2))/tally(projected_dams) # 2315/3700 (63%)


#### 5. GRanD + FHReD & Scarcity (2050) + Flooding (2050) ----

## Number of dams
# ...projected to face medium to very high risk
tally(filter(all_dams, RC1_P50 > 2.6 & RC2_P50 > 2.6))/tally(all_dams) # 1157/6188 (19%)

# Risk combination
all_dams %>%
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
  group_by(`Risk combination`) %>%
  summarise(
    n=n(),
    perc=round(n()/6188*100)
  )

# Geographic distribution
all_dams %>%
  filter(RC1_P50 > 2.6 & RC2_P50 > 2.6) %>%
  group_by(Country) %>%
  summarise(n=n()) %>%
  arrange(desc(n))

all_dams %>%
  filter(RC1_P50 > 2.6 & RC2_P50 > 2.6) %>%
  arrange(desc(Capacity_perc)) %>%
  select(Status, Name, Capacity, Capacity_perc, Use, Country, RC1_P50, RC2_P50)


#### 6. GRanD / FHReD & Biodiversity (2020) ----
mean(existing_dams$RC10);plotrix::std.error(existing_dams$RC10)
mean(projected_dams$RC10);plotrix::std.error(projected_dams$RC10)


## Number of dams
# ...already exposed to medium to very high risk today
tally(filter(existing_dams, RC10 > 2.6))/tally(existing_dams) # 1889/2488 (76%)
tally(filter(projected_dams, RC10 > 2.6))/tally(projected_dams) # 3431/3700 (93%)

existing_dams %>%
  filter(RC10 > 2.6) %>%
  group_by(Country) %>%
  summarise(n=n(), cap=sum(Capacity)) %>%
  arrange(desc(cap))

projected_dams %>%
  filter(RC10 > 2.6) %>%
  group_by(Country) %>%
  summarise(n=n(), cap=sum(Capacity)) %>%
  arrange(desc(cap))


#### 7. GRanD + FHReD & Scarcity (2050) + Biodiversity (2020) ----

## Number of dams
# ...already exposed to medium to very high risk today
tally(filter(all_dams, RC1_P50 > 2.6 & RC10 > 2.6))/tally(all_dams) # 1648/6188 (27%)


# Risk combination
all_dams %>%
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
  select(Status:Basin, `Scarcity risk in 2050` = RC1_P50, `Biodiversity risk in 2020` = RC10, `Risk combination`) %>%
  group_by(`Risk combination`) %>%
  summarise(
    n=n(),
    perc=round(n()/6188*100)
  )

# Geographic distribution
all_dams %>%
  filter(RC1_P50 > 2.6 & RC10 > 2.6) %>%
  group_by(Country) %>%
  summarise(n=n()) %>%
  arrange(desc(n))

all_dams %>%
  filter(RC1_P50 > 2.6 & RC10 > 2.6) %>%
  arrange(desc(Capacity_perc)) %>%
  select(Status, Name, Capacity, Capacity_perc, Use, Country, RC1_P50, RC2_P50)
