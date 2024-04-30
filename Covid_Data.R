#Task 1
#----------------------------------------------------------------------------------------------------
#load dataset
dataset <- read.csv("covid_19_india.csv")

#replace "-" with NA
dataset[dataset == "-"] <- NA

#load dplyr
library(dplyr)

#check data types
str(dataset)

#convert ConfirmedIndianNational and ConfirmedForeignNational into integer, and Date into Date format
dataset$ConfirmedIndianNational <- as.integer(dataset$ConfirmedIndianNational)
dataset$ConfirmedForeignNational <- as.integer(dataset$ConfirmedForeignNational)
dataset$Date <- as.Date(dataset$Date, format="%Y-%m-%d")

# generate a random whole number between 0 to "Confirmed" to replace NA in ConfirmedIndianNational column
dataset <- mutate(dataset,
                    ConfirmedIndianNational = ifelse(is.na(ConfirmedIndianNational),
                                                     round(runif(length(ConfirmedIndianNational), 0, Confirmed)),
                                                     ConfirmedIndianNational))

# replace NA values in ConfirmedForeignNational with difference between Confirmed and ConfirmedIndianNational
dataset <- mutate(dataset,
                    ConfirmedForeignNational = ifelse(is.na(ConfirmedForeignNational),
                                                      Confirmed - ConfirmedIndianNational,
                                                      ConfirmedForeignNational))

#remove rows that are unassigned and Cases being reassigned to states
dataset <- subset(dataset, !grepl("unassigned", State.UnionTerritory, ignore.case = TRUE))
dataset <- subset(dataset, !grepl("Cases being reassigned to states", State.UnionTerritory, ignore.case = TRUE))

#Correction of state names typo 
dataset <- dataset %>%
  mutate(State.UnionTerritory = gsub("Bihar\\*\\*\\*\\*", "Bihar", State.UnionTerritory))  %>%
  mutate(State.UnionTerritory = gsub("Daman & Diu", "Dadra and Nagar Haveli and Daman and Diu", State.UnionTerritory)) %>%
  mutate(State.UnionTerritory = gsub("Himanchal Pradesh", "Himachal Pradesh", State.UnionTerritory))  %>%
  mutate(State.UnionTerritory = gsub("Karanataka", "Karnataka", State.UnionTerritory))  %>%
  mutate(State.UnionTerritory = gsub("Madhya Pradesh\\*\\*\\*", "Madhya Pradesh", State.UnionTerritory))  %>%
  mutate(State.UnionTerritory = gsub("Maharashtra\\*\\*\\*", "Maharashtra", State.UnionTerritory))  %>%
  mutate(State.UnionTerritory = gsub("Telengana", "Telangana", State.UnionTerritory))

#------------------------------------------------------------------------------------------------------

#Task 2

#-------------------------------------------------------------------------------------------------------

#summary of dataset (Shown mean, median and maximum values of columns)
summary(dataset)

#Total number of states
T_State.UnionTerritory <- length(unique(dataset$State.UnionTerritory))
print(T_State.UnionTerritory)

#Total number and SD of ConfirmedIndianNational
T_ConfirmedIndianNational <- sum(dataset$ConfirmedIndianNational)
print(T_ConfirmedIndianNational)

SD_ConfirmedIndianNational <- round(sd(dataset$ConfirmedIndianNational))
print(SD_ConfirmedIndianNational)

#Total number and SD of ConfirmedForeignNational
T_ConfirmedForeignNational <- sum(dataset$ConfirmedForeignNational)
print(T_ConfirmedForeignNational)

SD_ConfirmedForeignNational <- round(sd(dataset$ConfirmedForeignNational))
print(SD_ConfirmedForeignNational)

#Total number and SD of Cured
T_Cured <- sum(dataset$Cured)
print(T_Cured)

SD_Cured <- round(sd(dataset$Cured))
print(SD_Cured)

#Total number and SD of Deaths
T_Deaths <- sum(dataset$Deaths)
print(T_Deaths)

SD_Deaths <- round(sd(dataset$Deaths))
print(SD_Deaths)

#Total number and SD of Confirmed
T_Confirmed <- sum(dataset$Confirmed)
print(T_Confirmed)

SD_Confirmed <- round(sd(dataset$Confirmed))
print(SD_Confirmed)

#Calcualte total number of confirmed case in each state
Confirmed_State <- dataset %>%
  group_by(State.UnionTerritory) %>%
  summarise(TotalConfirmedCases = sum(Confirmed))

print(Confirmed_State)

#Create a bar chart for total number of confirmed case in each state
library(ggplot2)

ggplot(Confirmed_State, aes(x = State.UnionTerritory, y = TotalConfirmedCases)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = TotalConfirmedCases), angle = 90, vjust = 0.5,hjust=-0.1, size = 3) +
  theme_minimal() +
  labs(title = "Total Confirmed Cases by State",
       x = "State.UnionTerritory",
       y = "Total Confirmed Cases") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = scales::comma)

#-------------------------------------------------------------------------------------------------------

#Task 3

#---------------------------------------------------------------------------------------------------------
#Calculate the average number of confirmed cases of each month.
library(lubridate)

Date_Confirmed <- data.frame(Date = as.Date(dataset$Date, format = "%Y/%m/%d"), Confirmed = dataset$Confirmed) 

Date_Confirmed <- mutate(Date_Confirmed, Month = month(Date), Year = year(Date))

options(scipen = 999) #Disable scientific notation

AveragePerMonth <- Date_Confirmed %>%
  group_by(Year, Month) %>%
  summarize(AverageCases = round(mean(Confirmed)))


AveragePerMonth$YearMonth <- paste(AveragePerMonth$Year, AveragePerMonth$Month, sep = "-")

#---------------------------------------------------------------------------------------------------------------

#Task 4

#------------------------------------------------------------------------------------------------------------------
#Create a line plot to display average number of confirmed cases in each month.
AveragePerMonth$YearMonth <- factor(AveragePerMonth$YearMonth, levels = unique(AveragePerMonth$YearMonth)) #convert the "YearMonth" column to a factor, specifying the levels as the unique values in the original order

ggplot(AveragePerMonth, aes(x = YearMonth, y = AverageCases, group = Year, color = as.factor(Year))) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Average Confirmed") +
  theme_minimal()
