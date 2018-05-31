library(dplyr)
library(tidyr)
library(ggplot2)

#source("apikeys.R")

#4. Over the past 20 years, with increases in mental illness/disorder awareness and treatment,
# is there a decrease in self-harm mortality? This aims to see if whether certain state governments
# have been successful in dealing with these societal problems. How many health bills have been
# passed in a given year and how much on average has each bill changed the rate. Given some bills
# need time to ramp up, what this overall change based on current overall number of bills?

# Get self-harm mortality data

self_harm_data <- read.csv("data_4/self_harm_state_data.csv", stringsAsFactors = FALSE)

self_harm_data[3:10] <- lapply(self_harm_data[3:10], substr, 1, 5)

self_harm_data <- self_harm_data %>%
  filter(FIPS < 60 & Location != "District of Columbia")
colnames(self_harm_data)[3:10] <- substr(colnames(self_harm_data[3:10]), 16, 19)

self_harm_data_long <- gather(
  self_harm_data, key = Year, value = Mortality_Rate,
  "1980", "1985", "1990", "1995", "2000", "2005", "2010", "2014"
)

self_harm_data_long$Mortality_Rate <- as.numeric(self_harm_data_long$Mortality_Rate)

avg_self_harm_data <- self_harm_data_long %>%
  group_by(Year) %>%
  summarize(Average_Rate = mean(Mortality_Rate))

avg_self_harm_data$Year <- as.numeric

self_harm_boxplot <- ggplot(data = self_harm_data_long) +
  geom_boxplot(mapping = aes(x = Year, y = Mortality_Rate))

avg_self_harm_line_graph <- ggplot(data = avg_self_harm_data) +
  geom_line(mapping = aes(x = as.numeric(Year), y = Average_Rate))
