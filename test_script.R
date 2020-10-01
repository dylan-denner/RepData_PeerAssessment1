# just a list of files inside master.zip
#master <- as.character(unzip("activity.zip", list = TRUE)$Name)
# load the first file "file1.csv"


library("dplyr") 
library("ggplot2")
library("scales")

data <- read.csv(unz("activity.zip", "activity.csv"), header = TRUE,
                 sep = ",") 


data$date <- as.Date(data$date)

dates_by_day <- seq(from = min(data$date), to = max(data$date), by = "day")

daily_steps <- data %>% group_by(date) %>% summarise(sum_steps = sum(steps))

p <- ggplot(daily_steps, aes(x=daily_steps$date, y=daily_steps$sum_steps, group=1)) + geom_bar(stat = "identity") +labs(title = "Total Steps by Date", x = "Date", y = "Total Steps")


print(p)


