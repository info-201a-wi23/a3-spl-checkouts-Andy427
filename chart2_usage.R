library("ggplot2")
library("plotly")
library("dplyr")
library("scales")

# 1. Creating the data frame
spl_df <- read.csv("Desktop/INFO201/week-7-exercises-Andy427/2017-2023-10-Checkouts-SPL-Data.csv")

## Chart Two: UsageClass over time
usage_over_time <- spl_df %>% group_by(CheckoutYear) %>% summarise(PhysicalUsage = sum(str_detect(UsageClass, "Physical")))

ggplot(data = usage_over_time, aes(x = CheckoutYear, y = PhysicalUsage, group = 1)) + 
  geom_point(aes(color = "Actual")) +
  geom_smooth(aes(color = "Estimate / Trend")) +
  labs(title = "Physical Usage Over Time",
       x = "Checkout Year",
       y = "Usage Numbers",
       color = "Lines") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) +
  scale_y_continuous(breaks = seq(-200000, 300000, 50000))