#####loading the dataset and converting to data table
cyclist_data <- read.csv("C:\\Users\\acer\\Desktop\\case study1\\combined_data.csv")
cyclist_data <- data.table(cyclist_data)
######checking and cleaning
head(cyclist_data)
View(cyclist_data)
cyclist_data <- na.omit(cyclist_data)
dim(cyclist_data)
cyclist_data <- cyclist_data[!duplicated(cyclist_data$ride_id)]
clean_names(cyclist_data)
cyclist_data$started_at <-as.POSIXct(cyclist_data$started_at,format="%Y-%m-%d %H:%M:%S")
cyclist_data$ended_at <- as.POSIXct(cyclist_data$ended_at,format="%Y-%m-%d %H:%M:%S")
cyclist_data <- subset(cyclist_data,ended_at>started_at)

unique(cyclist_data$start_station_name)
unique(cyclist_data$end_station_name)

########## adding weekdays wrt starting and ending date
cyclist_data <- cyclist_data %>%
  mutate(starting_day=weekdays(cyclist_data$started_at),endidng_day=weekdays(cyclist_data$ended_at))

head(cyclist_data[cyclist_data$starting_day != cyclist_data$ending_day])


cyclist_data <- cyclist_data %>%
  mutate(duration_in_min=as.numeric(difftime(ended_at,started_at,units="mins")))

cyclist_data <- cyclist_data %>%
  select(-c(start_station_id,end_station_id))
boxplot(cyclist_data$duration_in_min,title(main="Box plot for duration"))
data <- cyclist_data %>%
  filter(duration_in_min>600  )

cyclist_data <- cyclist_data %>%
  filter(duration_in_min>5 & duration_in_min<600 )

cyclist_data <- cyclist_data %>%
  mutate(month=format(started_at,"%B"))

dim(cyclist_data[cyclist_data$start_station_name==''])
woo_data <- subset(cyclist_data,start_station_name!='')
fwrite(woo_data,file="C:\\Users\\acer\\Desktop\\case study1\\refined_data.csv")



######creating plots

ggplot(data = cyclist_data)+
  geom_bar(mapping = aes(x=starting_day,color=starting_day,fill=starting_day))+
  labs(title = "Weekly rental trends",subtitle = "Analysing peak days for cycle rentals",x="Bikes rented on",y="Frequency")

ggplot(data=cyclist_data)+
  geom_bar(mapping=aes(x=starting_day,fill=member_casual))+
  labs(title = "Type of customer contributing to renting on weekdays",x="Bikes rented on",y="Frequency")

ggplot(data=cyclist_data)+
  geom_bar(mapping=aes(x=rideable_type,fill=member_casual))+
  labs(title = "Bike type used trends",x="Bike type",y="Frequency")

pie(radius = 1,(rideable_count <- table(cyclist_data$rideable_type)),main = "Pie chart showing the type of bikes used")

ggplot(data=cyclist_data)+
  geom_bar(mapping=aes(x=starting_day,fill=member_casual))+facet_wrap(~month)+
  labs(title = "Monthly trends of bike usage",x="Weekday",y="frequency")

ggplot(data=cyclist_data)+
  geom_bar(mapping=aes(x=starting_day,fill=rideable_type))+facet_wrap(~month)+
  labs(title = "Monthly trends of bike usage based on bike type",x="Weekday",y="frequency")

ggplot(data=cyclist_data)+
  geom_bar(mapping=aes(x=month,fill=rideable_type))+facet_wrap(~member_casual)+
  labs(title="Monthly bike usage trends based on the customer type",x='',y="frequency")
           

ggplot(data = data)+
  geom_bar(mapping=aes(x=month,fill=rideable_type))+
  labs(title="montly trends of Bikes being inappropriately returned",subtitle = "Type of Bikes being taken by customers for more than 25 days",x="Month",y="frequency")+
  facet_wrap(~member_casual)

your_datatable_top_10 <- woo_data %>%
  count(start_station_name, sort = TRUE) %>%
  head(10) %>%
  inner_join(woo_data, by = "start_station_name")



# Create a bar plot using ggplot2
ggplot(your_datatable_top_10, aes(x = start_station_name)) +
  geom_bar(color="blue",fill="lightblue") +
  ggtitle("Top 10 Stations") +
  xlab("Station") +
  ylab("Frequency")
