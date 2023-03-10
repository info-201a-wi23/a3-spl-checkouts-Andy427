library("ggplot2")
library("plotly")
library("dplyr")
library("scales")

# 1. Creating the data frame
spl_df <- read.csv("/Users/Andy/Desktop/INFO201/week-7-exercises-Andy427/2017-2023-10-Checkouts-SPL-Data.csv")

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