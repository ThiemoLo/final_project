# 1. Is there an observable correlation between generational/age brackets and
# type of mortality (for example, what is the highest rate for people in baby
# boomers, generation x, etc.) Using generational brackets in which people were
# born into, and the ages provided we could place people’s age into a generation
# (which would change every year in the study as the ages of these generations
# change)

library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)

# General birth date ranges for generations used
generation_silent <- (1925:1945)
generation_baby_boomer <- (1946:1964)
generation_x <- (1965:1979)
generation_y <- (1980:1994)
generation_z <- (1995:2012)

# Load in state in question
state = "WASHINGTON"
state_data <- read.csv(paste0("./data/IHME_USA_COUNTY_USE_INJ_MORTALITY_1980_2014_", state, "_Y2018M03D13.CSV"))

## Find birth year and generational bucket
state_data <- state_data %>%
  mutate(birth_year = year_id - age_id) %>%
  mutate(generation =
    case_when(
      birth_year %in% generation_silent ~ "Silent",
      birth_year %in% generation_baby_boomer ~ "Baby Boomer",
      birth_year %in% generation_x ~ "X",
      birth_year %in% generation_y ~ "Y",
      birth_year %in% generation_silent ~ "Z"
    ))

## For overall statistics
silent <- state_data %>%
  filter(generation == "Silent") %>% 
  group_by(cause_name) %>%
  summarize(
    silent_lower_bound = mean(lower),
    silent_upper_bound = mean(upper)
    )
baby <- state_data %>%
  filter(generation == "Baby Boomer") %>%
  group_by(cause_name) %>%
  summarize(
    baby_lower_bound = mean(lower),
    baby_upper_bound = mean(upper)
  )
x <- state_data %>%
  filter(generation == "X") %>%
  group_by(cause_name) %>%
  summarize(
    x_lower_bound = mean(lower),
    x_upper_bound = mean(upper)
  )
y <- state_data %>%
  filter(generation == "Y") %>%
  group_by(cause_name) %>%
  summarize(
    y_lower_bound = mean(lower),
    y_upper_bound = mean(upper)
  )
z <- state_data %>%
  filter(generation == "Z") %>%
  group_by(cause_name) %>%
  summarize(
    z_lower_bound = mean(lower),
    z_interval_upper_bound = mean(upper)
  )

# Combining different generations
state_combined <- right_join(silent, baby, by = "cause_name")
state_combined <- left_join(state_combined, x, by = "cause_name")
state_combined <- left_join(state_combined, y, by = "cause_name")
state_combined <- left_join(state_combined, z, by = "cause_name")


state_combined_long <- gather(state_combined, key = data_type, value = mortality_percentage, silent_lower_bound,
                silent_upper_bound, baby_lower_bound, baby_upper_bound, x_lower_bound, x_upper_bound,
                y_lower_bound, y_upper_bound, z_lower_bound, z_interval_upper_bound)

state_combined_long <- state_combined_long %>%
  mutate(generation =  gsub("_.*","", state_combined_long$data_type))

ggplot(data = state_combined_long) +
  geom_point(mapping = aes(x = cause_name, y = mortality_percentage, color = generation)) +
  facet_grid(. ~ generation)


## For specific year


year = 1990
state_data_year <- state_data %>%
  filter(year_id == year)

silent_year <- state_data_year %>%
  filter(generation == "Silent") %>% 
  group_by(cause_name) %>%
  summarize(
    silent_lower_bound = mean(lower),
    silent_upper_bound = mean(upper)
  )
baby_year <- state_data_year %>%
  filter(generation == "Baby Boomer") %>%
  group_by(cause_name) %>%
  summarize(
    baby_lower_bound = mean(lower),
    baby_upper_bound = mean(upper)
  )
x_year <- state_data_year %>%
  filter(generation == "X") %>%
  group_by(cause_name) %>%
  summarize(
    x_lower_bound = mean(lower),
    x_upper_bound = mean(upper)
  )
y_year <- state_data_year %>%
  filter(generation == "Y") %>%
  group_by(cause_name) %>%
  summarize(
    y_lower_bound = mean(lower),
    y_upper_bound = mean(upper)
  )
z_year <- state_data_year %>%
  filter(generation == "Z") %>%
  group_by(cause_name) %>%
  summarize(
    z_lower_bound = mean(lower),
    z_interval_upper_bound = mean(upper)
  )

# Combining different generations
state_combined_year <- right_join(silent_year, baby_year, by = "cause_name")
state_combined_year <- left_join(state_combined_year, x_year, by = "cause_name")
state_combined_year <- left_join(state_combined_year, y_year, by = "cause_name")
state_combined_year <- left_join(state_combined_year, z_year, by = "cause_name")

state_combined_year_long <- gather(state_combined_year, key = data_type, value = mortality_percentage, silent_lower_bound,
                              silent_upper_bound, baby_lower_bound, baby_upper_bound, x_lower_bound, x_upper_bound,
                              y_lower_bound, y_upper_bound, z_lower_bound, z_interval_upper_bound)

## Difference from year to overall average (to pull for inline analysis)
state_combined_long_delta <- state_combined_long %>%
  mutate(overall_average = state_combined_year_long$mortality_percentage) %>%
  mutate(delta = mortality_percentage - overall_average)



## Year over year changes for specific type
type <- "Alcohol use disorders"

silent_change <- state_data %>%
  filter(generation == "Silent") %>%
  filter(cause_name == type) %>%
  group_by(year_id) %>%
  summarize(
    x_lower_bound = mean(lower),
    x_upper_bound = mean(upper)
  )
baby_change <- state_data %>%
  filter(generation == "Baby Boomer") %>%
  filter(cause_name == type) %>%
  group_by(year_id) %>%
  summarize(
    x_lower_bound = mean(lower),
    x_upper_bound = mean(upper)
  )
x_change <- state_data %>%
  filter(generation == "X") %>%
  filter(cause_name == type) %>%
  group_by(year_id) %>%
  summarize(
    x_lower_bound = mean(lower),
    x_upper_bound = mean(upper)
  )
y_change <- state_data %>%
  filter(generation == "Y") %>%
  filter(cause_name == type) %>%
  group_by(year_id) %>%
  summarize(
    x_lower_bound = mean(lower),
    x_upper_bound = mean(upper)
  )
z_change <- state_data %>%
  filter(generation == "Z") %>%
  filter(cause_name == type) %>%
  group_by(year_id) %>%
  summarize(
    x_lower_bound = mean(lower),
    x_upper_bound = mean(upper)
  )


ggplot(silent_change, aes(x = year_id, y = x_lower_bound)) +
  geom_line() +
  geom_line(data = baby_change, color = "red") +
  geom_line(data = x_change, color = "blue") +
  geom_line(data = y_change, color = "green") +
  geom_line(data = z_change, color = "purple")
