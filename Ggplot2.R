#----------------------------#
#   PDSG: R Shiny Tutorial   #
#----------------------------#

#----------------------------#
#          GGPLOT            #
#----------------------------#

#Load libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(lubridate)
library(tidyr)
library(rlang)

#Read dataset
dataset <- read.csv("data/hour.csv")
head(dataset)

#Summary of dataset
summary(dataset)
nrow(dataset)
ncol(dataset)
colnames(dataset)

#-------------------- HeatMap ----------------------#

#Assign colors for our map
col1 = "#d8e1cf" 
col2 = "#438484"

#Check format fo the date column
class(dataset$dteday)
class(dataset$hr)
class(dataset$mnth)

#Use Lubridate to create a date column
dataset$dteday <- as.Date(dataset$dteday)

#Create date fields as factors so we can have labels
dataset$mnth <- factor(dataset$mnth, levels = c(1:12),
                       labels = c("Jan", "Feb", "Mar", "Apr", "May",
                                  "Jun", "Jul", "Aug", "Sep", "Oct",
                                  "Nov", "Dec"))
dataset$weekday <- factor(dataset$weekday, levels = c(0:6),
                          labels = c("Sun","Mon", "Tue", "Wed",
                                     "Thur", "Fri", "Sat"))

dayHour <- dataset %>%
  group_by( hr, weekday) %>%
  summarise(
    Cas = sum(casual),
    Reg = sum(registered),
    Total = sum(cnt))


ggplot(dayHour, aes(hr, weekday)) + geom_tile(aes(fill = Total), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Total Bike Sharing Users")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Heatmap of Bike Sharing Users by Day of Week and Hour of Day",
       x = "Hour of Day", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#--------------------Plotting the Time Series----------------------#

#Group data by season
df <- dataset %>%
  group_by(dteday)%>%
  summarise(
    Cas = sum(casual),
    Reg = sum(registered),
    Total = sum(cnt)
  )%>%
  gather(key = "riders", value = "value", -dteday)

df$dteday <- as.Date(df$dteday)

ggplot(df, aes(x = dteday, y = value)) +
  geom_line(aes(color = riders), size = 0.2) +
  scale_color_manual(values = c("#3f78bf", "#bf73cc", "#1ebbd7")) +
  theme_minimal() +
  xlab("Date") +
  ylab("Number of Riders")







#--------------------Exploring Users by Season----------------------#

#Let's add labels to season
class(dataset$season)

dataset$season <- factor(dataset$season, levels = c(1,2,3,4),
                                 labels = c("spring", "summer", "fall", "winter"))

#Group data by season
users_by_season <- dataset %>% 
  group_by(season) %>% 
  summarise(casual_count = sum(casual),
            registered_count = sum(registered),
            total_count = sum(cnt))%>%
  gather(key = 'variable', value = 'value', -season)


#Create a triple bar chart
ggplot(data = users_by_season) +
  geom_bar(aes(x = season, y = value, fill = variable), 
           stat = "identity",
           position = "dodge") +
  ggtitle("Bike Sharing Count by Season") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=14)) +
  scale_fill_brewer(palette="Oranges")



#-------------- OPTIONAL EXERCISE -----------------------------#
# Now let's repeat the above steps to create a monthDay heat map















monthDay <- dataset %>% 
  group_by( weekday, mnth) %>%
  summarise(
    Cas = sum(casual),
    Reg = sum(registered),
    Total = sum(cnt)
)

ggplot(monthDay, aes(mnth, weekday)) + geom_tile(aes(fill = Total),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Total Bike Sharing Users")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Heatmap of Bike Sharing Users by Day of Week and Month",
       x = "Month", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())








