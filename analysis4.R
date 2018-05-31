#library(dplyr)
#library(tidyr)
#library(ggplot2)

# Get self-harm mortality data

self_harm_data <- read.csv("data/self_harm_state_data.csv", stringsAsFactors = FALSE)

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

avg_self_harm_line_graph <- ggplot(data = avg_self_harm_data) +
  geom_line(mapping = aes(x = as.numeric(Year), y = Average_Rate), size = 5, color = "RED") +
  labs(
    title = "Average Yearly Self-Harm Mortality Rates",
    x = "Year",
    y = "Mortality Rate"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Get bill count and rate difference data

mental_health_data <- read.csv("data/mental_health_bills_data.csv", stringsAsFactors = FALSE)

mental_health_data <- mutate(mental_health_data, Rate_Change_per_Bill = Rate.Change/Bill.Count)
overall_avg_rate_change <- summarize(mental_health_data, overall_change = sum(Rate.Change)/sum(Bill.Count))$overall_change
overall_avg_rate_change <- round(overall_avg_rate_change, digits = 6)
mental_health_data$Rate_Change_per_Bill <- round(mental_health_data$Rate_Change_per_Bill, digits = 6)
colnames(mental_health_data) <- c("Year Interval", "Health Bill Count", "Rate Change", "Avg Rate Change per Bill")
