library(tidyverse)
library(ggrepel)

#read in data
data <- read.csv("StudentPerformanceFactors.csv")
data <- as.data.frame(data)
data |> head()

#check NA
sum(is.na(data))

data |> distinct(Teacher_Quality) #Seems to be secret missing values in this column
data |> filter(Teacher_Quality == "") |> nrow() #78 rows
#Given the distribution of this data (explored later), it would be reasonable to impute a "Medium" value to begin
data <- data |> 
  mutate(Teacher_Quality = case_when(
    Teacher_Quality == "" ~ "Medium",
    TRUE ~ Teacher_Quality
  ))

data |> filter(Distance_from_Home == "") |> nrow() #67 rows
#It also makes sense to impute "Near" here
data <- data |> 
  mutate(Distance_from_Home = case_when(
    Distance_from_Home == "" ~ "Near",
    TRUE ~ Distance_from_Home
  ))

data |> filter(Parental_Education_Level == "") |> nrow()
#Might be wrong to impute high school since distribution is more spread out, but fine for now
data <- data |> 
  mutate(Parental_Education_Level = case_when(
    Parental_Education_Level == "" ~ "High School",
    TRUE ~ Parental_Education_Level
  ))

#
summary(data)

ggplot(data, aes(x = Hours_Studied)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Hours Studied", y = "Count", title = "Hours of studying seems partly normal")

ggplot(data, aes(x = Hours_Studied)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Hours Studied", y = "Count", title = "Motivation level has strangely little effect on hours of study") +
  facet_wrap(~Motivation_Level, scales = "free_y")


ggplot(data, aes(x = Attendance)) +
  geom_histogram(binwidth = 2) +
  labs(x = "% Attendance", y = "Count", 
       title = "Pupil attendance seems fairly evenly spread out, except for the beginning and end")

#Bar Plots -----
ggplot(data, aes(x = Parental_Involvement)) +
  geom_bar() +
  labs(x = "Parental Involvement", y = "Count", title = "Distribution of parental involvement",
       subtitle = "There seem to be fewer students who report low parental involvement")

ggplot(data, aes(x = Access_to_Resources)) +
  geom_bar() +
  labs(x = "Access to resources", y = "Count", title = "Distribution of Resource Access",
       subtitle = "There seem to be fewer students who report low access to resources too")

ggplot(data, aes(x = Motivation_Level)) +
  geom_bar() +
  labs(x = "Motivation Level", y = "Count", title = "Distribution of Motivation Levels",
       subtitle = "This has a different distribution to the previous two")

ggplot(data, aes(x = Family_Income)) +
  geom_bar() +
  labs(x = "Family Income", y = "Count", title = "Much fewer high family incomes than anything else")

ggplot(data, aes(x = Teacher_Quality)) +
  geom_bar() +
  labs(x = "Teacher Quality", y = "Count", title = "Relatively few poor teachers, mostly medium quality")

ggplot(data, aes(x = Peer_Influence)) +
  geom_bar() +
  labs(x = "Peer Influence", y = "Count")

ggplot(data, aes(x = Distance_from_Home)) +
  geom_bar() +
  labs(x = "Distance From Home", y = "Count")

ggplot(data, aes(x = Parental_Education_Level)) +
  geom_bar() +
  labs(x = "Parental Education Level", y = "Count")

data |> 
  select(Extracurricular_Activities) |> 
  group_by(Extracurricular_Activities) |> 
  summarise(num = n()) |> 
  ggplot(aes(x = Extracurricular_Activities, y = num)) +
  geom_bar(stat = "identity") +
  labs(x = "Extracurricular Activities", y = "Count", title = "Much more people do extracurricular activies than not") +
  geom_text(aes(label = num),
            vjust = 1.5,
            colour = "white",
            size = 6)

data |> 
  select(Internet_Access) |> 
  group_by(Internet_Access) |> 
  summarise(num = n()) |> 
  ggplot(aes(x = Internet_Access, y = num)) +
  geom_bar(stat = "identity") +
  labs(x = "Internet Access", y = "Count", title = "A very low percent do not have internet access") +
  geom_text(aes(label = num),
            vjust = 1.5,
            colour = "white",
            size = 6)

data |> 
  select(School_Type) |> 
  group_by(School_Type) |> 
  summarise(num = n()) |> 
  ggplot(aes(x = School_Type,  y = num)) +
  geom_bar(stat = "identity") +
  labs(x = "School Type", y = "Count") +
  geom_text(aes(label = num),
            vjust = 1.5,
            colour = "white",
            size = 6)

data |> 
  select(Learning_Disabilities) |> 
  group_by(Learning_Disabilities) |> 
  summarise(num = n()) |> 
  ggplot(aes(x = Learning_Disabilities, y = num)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = num),
            vjust = 1.5,
            colour = "white",
            size = 6)

data |> 
  select(Gender) |> 
  group_by(Gender) |> 
  summarise(num = n()) |> 
  ggplot(aes(x = Gender, y = num)) +
  geom_bar(stat = "identity") + 
  labs(y = "Count", title = "The data seems to be imbalanced in terms of gender") +
  geom_text(aes(label = num),
            vjust = 1.5,
            colour = "white",
            size = 6)

 #access to resources is not parent dependent but rather environment dependent, so this isnt useful to graph.
#ggplot(data, aes(x = Access_to_Resources, fill = Parental_Involvement)) +
#  geom_bar(position = "fill") +
#  labs(x = "Access to Resources", y = "Proportion", 
#       title = "There doesnt seem to be much difference in parental involvement across different 
#       access to resources")



data |> 
  ggplot(aes(x = Sleep_Hours)) +
  geom_histogram(binwidth = 1, )

#Does sleep affect attendance? Inconclusive.

data |> 
  mutate(sleep_cat = case_when(
    Sleep_Hours > 9 ~ "More",
    Sleep_Hours > 7 ~ "Expected",
    TRUE ~ "Less"
  )) |> 
  ggplot(aes(x = Attendance)) +
  geom_histogram(binwidth = 3,) +
  facet_wrap(~sleep_cat, scales = "free_y")

data |> 
  ggplot(aes(x = Previous_Scores)) +
  geom_histogram(binwidth = 3) +
  labs(x = "Previous Scores", y = "Count", 
  title = "Previous exam scores were generally evenly spread, except for the beginning and end")

data |> 
  ggplot(aes(x = Previous_Scores, y = Exam_Score)) +
  geom_jitter(alpha = .3) +
  labs(x = "Previous Exam Score", y = "Final Exam Score", 
       title = "There seems to be very little correlation between previous and final exam scores",
       subtitle = "Although the distribution of points does slightly change between lowester and highest previous exam scores")
cor(data$Previous_Scores, data$Exam_Score)

data |> 
  ggplot(aes(x = Exam_Score)) +
  geom_histogram(binwidth = 5) +
  labs(x = "Exam Score", y = "Count", title = "This exam score distribution brings a lot of difficulty",
       subtitle = "If almost all exam scores are about the same, there isnt much to predict")

#What influences exam score?

data |> 
  ggplot(aes(x = Hours_Studied, y = Exam_Score)) +
  geom_jitter(alpha = .4) +
  labs(x = "Hours Studied", y = "Exam Score",
       title = "Hours studied has some influence on exam results")

data |> 
  ggplot(aes(x = Exam_Score)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~Internet_Access, scales = "free_y") +
  labs(x = "Exam Score", y = "Count", title = "Internet access doesnt strongly influence the exam score distribution")

data |> 
  ggplot(aes(x = Exam_Score)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~Gender, scales = "free_y") +
  labs(x = "Exam Score", y = "Count", title = "Neither does gender")

data |> 
  ggplot(aes(x = Exam_Score)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~Learning_Disabilities, scales = "free_y") +
  labs(x = "Exam Score", y = "Count", title = "Somehow learning difficulty has little influence")

data |> 
  ggplot(aes(x = Exam_Score)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~Access_to_Resources, scales = "free_y") +
  labs(x = "Exam Score", y = "Count", title = "High access to resources might influence exam results")

