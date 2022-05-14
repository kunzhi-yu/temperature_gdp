library(tidyverse)
library(knitr)

# load data sets for states analysis
state_gdp <- read_csv('bea_download.csv')
state_temps <- read_csv('statetemps.csv')
state_pop <- read_csv('nst-est2019-alldata.csv')

# tidy data to only include variables that will be used. Rename columns.
state_gdp <- state_gdp %>% 
  rename(
    State = GeoName,
    gdp = `2019`
  ) %>% 
  select(
    State,
    gdp
  )

state_pop <- state_pop %>% 
  rename(
    State = NAME,
    pop = POPESTIMATE2019
  ) %>% 
  select(
    State,
    pop
  )

# left merge
state_gdp_per_cap <- merge(x = state_gdp, y = state_pop, by = "State")

# change gdp into gdp instead of gdp millions of dollars and calculate gdp per cap
state_gdp_per_cap <- state_gdp_per_cap %>%
  mutate(gdp = gdp * 1000000,
         gdp_per_cap = gdp / pop)

# left merge.
state_temps_gdppc <- merge(x = state_gdp_per_cap, y = state_temps, by = "State")

# tidy data to only include variables that will be used. Rename columns.
state_temps_gdppc <- state_temps_gdppc %>%
  rename(
    state = State,
    temp = `Avg C`
  ) %>% 
  select(
    state,
    gdp_per_cap,
    temp
  )

# plot regression
state_temps_gdppc %>%
  ggplot(aes(x = temp, y = gdp_per_cap)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) +
  theme_classic()

# print regression l
states_lm <- lm(gdp_per_cap ~ temp, data = state_temps_gdppc)
summary(states_lm)$coefficients
cor(x = state_temps_gdppc$temp, y = state_temps_gdppc$gdp_per_cap)
summary(states_lm)$r.squared

# read data for world
world_gdp <- read_csv('API_NY.GDP.PCAP.CD_DS2_en_csv_v2_4019678.csv')
world_temp <- read_csv('table-1.csv')

world_gdp <- world_gdp %>%
  rename(
    gdp = `2019`,
    country = `...1`
  ) %>% 
  select(
    country,
    gdp
  ) %>% 
  na.omit(world_gdp)

world_temp <- world_temp %>% 
  rename(
    country = Country,
    temp = `Average yearly temperature (1961–1990 Celsius)`
  )

# left merge
global_temps_gdppc <- merge(x = world_gdp, y = world_temp, by = "country")

global_temps_gdppc %>% ggplot(aes(x = temp, y = gdp)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) +
  theme_classic()

global_lm <- lm(gdp ~ temp, data = global_temps_gdppc)
summary(global_lm)$coefficients
cor(x = global_temps_gdppc$temp, y = global_temps_gdppc$gdp)
summary(global_lm)$r.squared
