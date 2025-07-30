library(tidyverse)
library(ggrepel)

#read in data
data <- read.csv("StudentPerformanceFactors.csv")
data <- as.data.frame(data)
data |> head()

#check NA
sum(is.na(data))

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

#The above two are incredibly similar, possible correlation to explore later


#access to resources is not parent dependent but rather environment dependent, so this isnt useful to graph.
#ggplot(data, aes(x = Access_to_Resources, fill = Parental_Involvement)) +
#  geom_bar(position = "fill") +
#  labs(x = "Access to Resources", y = "Proportion", 
#       title = "There doesnt seem to be much difference in parental involvement across different 
#       access to resources")

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
       title = "There seems to be very little correlation between previous and final exam scores")
cor(data$Previous_Scores, data$Exam_Score)
