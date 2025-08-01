library(rsample)
library(tidyverse)
library(caret)
library(vip)
library(ggplot2)

#Using models to try and find some ability to predict performance despite difficult distribution

split <- initial_split(data, prop = .7)
data_train <- training(split)
data_test <- testing(split)

#Converting catergorical to numeric columns

convert_cat <- function(x){
  recode(x, "Low" = 1, "Medium" = 2, "High" = 3,
            "Near" = 1, "Moderate" = 2, "Far" = 3,
            "Negative" = 1, "Neutral" = 2, "Positive" = 3,
            "No" = 0, "Yes" = 1,
            "Public" = 1, "Private" = 2,
            "High School" = 1, "College" = 2, "Postgraduate" = 3,
            "Male" = 1, "Female" = 2
         )
}

num_train <- data_train |> 
  mutate(across(c(Parental_Involvement, Access_to_Resources, Extracurricular_Activities,
                  Motivation_Level, Internet_Access, Family_Income, Teacher_Quality,
                  School_Type, Peer_Influence, Learning_Disabilities, 
                  Parental_Education_Level, Distance_from_Home, Gender), convert_cat))
num_test <- data_test |> 
  mutate(across(c(Parental_Involvement, Access_to_Resources, Extracurricular_Activities,
                  Motivation_Level, Internet_Access, Family_Income, Teacher_Quality,
                  School_Type, Peer_Influence, Learning_Disabilities, 
                  Parental_Education_Level, Distance_from_Home, Gender), convert_cat))


set.seed(123) 
cv_model1 <- train(
  Exam_Score ~ .,
  data = num_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
summary(cv_model1)

#linear model checks
df <- broom::augment(cv_model1$finalModel, data = num_train)
df$.resid <- resid(cv_model1$finalModel)  #.resid doesnt seem to be created for some reason

ggplot(df, aes(.fitted, .resid)) +
  geom_point(alpha = .2, size = 1) +
  labs(x = "Fitted", y = "Residual", title = "Mostly constant, but many outliers",
       subtitle = "Probably due to model limitations in not being able to predict highest exam scores")
df |> 
  filter(.resid < 10) |> 
  ggplot(aes(.fitted, .resid)) +
  geom_point(alpha = .2, size = 1) +
  labs(x = "Fitted", y = "Residual", title = "Patterns emerge on closer inspection",
       subtitle = "Line pattern suggests linear model is bad for this data, possibly emerges due to variable encoding")

#Decision Tree
library(rpart)
library(rpart.plot)
library(vip)
library(pdp)

#dont need to convert to numeric for this. 

dt1 <- rpart(
  formula = Exam_Score ~ .,
  data = data_train,
  method = "anova"
)
rpart.plot(dt1)

ames_dt2 <- train(
  Exam_Score ~ .,
  data = num_train,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 20
)
ggplot(ames_dt2)

#It seems the effort put in is the most important thing
vip(ames_dt2, num_features = 10, bar = FALSE) +
  labs(x = "Variable", title = "There a few very relevant variables, the rest are less useful",
       subtitle = "Hours studying and Attendance are intuitive in their importance, so this seems reasonable")

