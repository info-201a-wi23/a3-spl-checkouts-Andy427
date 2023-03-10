library("ggplot2")
library("plotly")
library("dplyr")
library("scales")
library("webshot")

# 1. Creating the data frame
spl_df <- read.csv("/Users/Andy/Desktop/INFO201/week-7-exercises-Andy427/2017-2023-10-Checkouts-SPL-Data.csv")

## Chart One: Checkouts over time
spl_df <- spl_df %>% mutate(Date = as.Date(paste0("1", "/", CheckoutMonth, "/", CheckoutYear), format = "%d/%m/%Y"))

checkout_over_time <- spl_df %>% group_by(Date) %>% summarise(Checkouts = sum(Checkouts)) %>% arrange(Date) 

ggplot(data = checkout_over_time, aes(x = Date, y = Checkouts, group = 1)) + 
  geom_point(aes(color = "Actual")) +
  geom_smooth(aes(color = "Estimate / Trend")) +
  labs(title = "Checkouts Over Time",
       x = "Date",
       y = "Number of Checkouts",
       color = "Lines") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = "year", date_minor_breaks = "month") +
  scale_y_discrete(breaks = seq(0, 350000, 50000))
