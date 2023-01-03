#dplyr is a grammar of data manipulation, providing a consistent set of 
#verbs that help you solve the most common data manipulation challenges:
# mutate() adds new variables that are functions of existing variables
# select() picks variables based on their names.
# filter() picks cases based on their values.
# summarise() reduces multiple values down to a single summary.
# arrange() changes the ordering of the rows.
#data manipulaton
install.packages('dplyr')
library(dplyr)

#install hflights package which contains the hflights data
install.packages("hflights")

#package call for hflights
library(hflights)

View(hflights)

#creating flight1 which contains the hflights data from hflight package
flight1<-hflights::hflights
View(flight1)

#select using dplyr (Projection)
syntax= var_name <- select(data,col_list)
flight_data<- select(flight1,FlightNum,ArrTime,DepTime)

head(flight_data)
View(flight_data)
#using column index in select statement
flight_data<-select(flight1,1,2,3,4)
#retriving all columns that contains Time, this is doing a pattern search on the columns
flight_data<-select(flight1, contains("time"))
View(flight_data)
#using select with a range
flight_data<-select(flight1, Year:ArrTime)
head(flight_data)

#using column index range in select
flight_data<-select(flight1, 2:10)
View(flight_data)
#using start_with and end_with function in dplyr with select statement
flight_data<-select(flight1, starts_with("Day"), ends_with("Time"))
head(flight_data)

library(dplyr)
View(flight_data)
summary(flight1$Year)

flight_data <- select(flight1, -Year)
head(flight1)
View(flight_data)
#Using Piping
flight_data <- select(flight1, Year, ArrTime, DepTime)
xx <- flight1 %>% select(Year, ArrTime, DepTime)
head(xx)
#mutate function: it is used to manipulate columns in table to get new columns
#mutate generates a psuedocolumn for the expression
str(flight1)
flight_data<- mutate(flight1, ActualGroundTime=ActualElapsedTime-AirTime)
View(flight_data)
str(flight_data)
flight_data<- mutate(flight1, Averagespeed=Distance/AirTime*60)
flight_data <-flight1 %>% mutate(Averagespeed=Distance/AirTime*60)
flight_data<- mutate(flight1, TotalTaxi=TaxiIn+TaxiOut)
flight_data<- mutate(flight1, TimeLoss=ArrDelay+DepDelay)

#filter: this function helps to restrict or filter data based on conditions
flight_data<- filter(flight1, Distance>3000)
View(flight_data)
range(flight_data$Distance)

table(flight1$UniqueCarrier)
flight_data<- filter(flight1, UniqueCarrier %in% c("OO", "US", "AA"))
View(flight_data)
#Use table to know the unique values in a dataset
table(flight1$UniqueCarrier)
table(flight_data$UniqueCarrier)

flight_data<- filter(flight1, TaxiIn+TaxiOut>AirTime)
flight_data<- filter(flight1, Distance>3000)
flight_data<- filter(flight1, DepTime<500|ArrTime>2200) #filter flights that arrived later than 10pm(2200)
flight_data<- filter(flight1, Dest=="JFK", Cancelled==1)
View(flight_data)
#Arrange help order the dataset in ascending or descending order
#Arrange the flight1 dataset with respect to DepDelay column
flight_data<- arrange(flight1, desc(DepDelay))
flight_data<- arrange(flight1, -DepDelay) #orderby descending
View(flight_data)
range(flight_data$DepDelay)
flight_data<- arrange(flight1, DepDelay+ArrDelay)

#Summarize-------aggregate function

flight_data<- summarise(flight1, min_dist=min(Distance), 
                        max_dist=max(Distance))
flight_data
flight_data<- summarise(flight1, earliest=min(ArrDelay, na.rm = T), 
                        average= mean(ArrDelay, na.rm = T), 
                        latest= max(ArrDelay, na.rm = T), 
                        SD= sd(ArrDelay, na.rm = T))

#Pipe Operator (%>%): allow combining of dplyr package and helps to connect objects

flight_data<- flight1 %>% select(contains("Time")) %>% filter(AirTime>60)
