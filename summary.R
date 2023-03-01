library("ggplot2")
library("plotly")
library("dplyr")
library("scales")

# 1. Creating the data frame
spl_df <- read.csv("Desktop/INFO201/week-7-exercises-Andy427/2017-2023-10-Checkouts-SPL-Data.csv")

# 2. 5 Summary Statistics
avg_checkouts <- spl_df %>% summarise(Checkouts = mean(Checkouts, na.rm = T)) %>% pull(Checkouts)

most_common_type <- spl_df %>% summarise(MaterialType = max(MaterialType, na.rm = T)) %>% pull(MaterialType)

most_read_author <- spl_df %>% filter(Checkouts == max(Checkouts, na.rm = T)) %>% pull(Creator)

most_read_book <- spl_df %>% filter(Checkouts == max(Checkouts, na.rm = T)) %>% pull(Title)

most_common_usageclass <- spl_df %>% summarize(UsageClass = max(UsageClass, na.rm = T)) %>% pull(UsageClass)

# 3. Charts
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

## Chart Three: Bar Chart of Most Read Authors
remove_empty <- filter(spl_df, Creator != "")

top_authors <- remove_empty %>% group_by(Creator) %>% summarise(Checkouts = sum(Checkouts, na.rm = T)) %>% arrange(desc(Checkouts)) %>% head(25)

ggplot(data=top_authors, aes(x=Creator, y=Checkouts)) + 
  geom_bar(stat = "identity", width = 0.75) +
  labs(title = "Most Read Authors",
       x = "Authors",
       y = "Number of Checkouts",
       color = "Lines") +
theme(axis.text.x = element_text(angle = 90))