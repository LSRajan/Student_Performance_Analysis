library(rsample)
library(tidyverse)
library(caret)
library(vip)
library(ggplot2)

#Using models to try and find some ability to predict performance despite difficult distribution
#Important note: this is primarily me applying what I've learnt from: 
#Hands-On Machine Learning with R - Bradley Boehmke & Brandon Greenwell -2020-02-01
#There are models that are mostly the same, this is more me applying and trying to understand better how they work and how to use them




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

dt2 <- train(
  Exam_Score ~ .,
  data = num_train,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 20
)
ggplot(dt2)

#It seems the effort put in is the most important thing
vip(dt2, num_features = 10, bar = FALSE) +
  labs(x = "Variable", title = "There a few very relevant variables, the rest are less useful",
       subtitle = "Hours studying and Attendance are intuitive in their importance, so this seems reasonable")

ggplot(data, aes(x = Attendance, y = Exam_Score)) +
  geom_jitter(alpha = .4) +
  labs(y = "Exam Score", title = "The impact of attendance is evident")
ggplot(data, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_jitter(alpha = .4) +
  labs(x = "Hours Studied", y = "Exam Score", title = "The impact of study hours is also evident")
ggplot(data, aes(x = Exam_Score)) +
  geom_histogram(binwdith = 2) +
  facet_wrap(~Access_to_Resources, scales = "free_y") +
  labs(x = "Exam Score", y = "Count", title = "A high access to resources does seem to have higher exam scores")

#Bagging the decision tree
library(ipred)  

set.seed(123)
bag_1 <- bagging(
  formula = Exam_Score ~ .,
  data = num_train,
  nbagg = 100,
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = 0)
)

#Comparing decision tree RMSE vs bagged tree RMSE, bagging improved performance.
#This is expected, decision trees have higher variance, bagging is a way of reducing this variance
min(dt2$results$RMSE)
bag_1$err


#notes for RF model
#mtry needs to be higher due to less relevant parameters
#n_trees: A good rule of thumb is to start with 10 times the number of features


#

library(ranger)
library(h2o)

h2o.no_progress()
h2o.init(max_mem_size = "5g")

train_h2o <- as.h2o(num_train)

predictors <- setdiff(colnames(num_train), "Exam_Score")

num_features <- length(predictors)

h2o_rf_baseline <- h2o.randomForest(
  x = predictors,
  y = "Exam_Score",
  training_frame = train_h2o,
  ntrees = num_predictors * 10,
  seed = 123
)
h2o_rf_baseline


#Used a higher set of values for mtries, there are seemingly fewer relevant predictors as seen before so
#a higher mtries value is needed. (Without this, its more likely to get a selection of variables that arent relevant)
hyper_grid <- list(
  mtries = floor(num_features * c(.15, .25, .333, .4, .5)),
  min_rows = c(1, 3, 5, 10),
  max_depth = c(10, 20, 30),
  sample_rate = c(.55, .632, .70, .80)
)

search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.001,
  stopping_rounds = 10,    
  max_runtime_secs = 60*5  
)

random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_random_grid",
  x = predictors, 
  y = "Exam_Score", 
  training_frame = train_h2o,
  hyper_params = hyper_grid,
  ntrees = num_features * 10,
  seed = 123,
  stopping_metric = "RMSE",   
  stopping_rounds = 10,          
  stopping_tolerance = 0.005,     
  search_criteria = search_criteria
)
random_grid_perf <- h2o.getGrid(
  grid_id = "rf_random_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)

#train a model with the best performaing hyperparameters

h2o_rf_best <- h2o.randomForest(
  x = predictors,
  y = "Exam_Score",
  training_frame = train_h2o,
  ntrees = num_predictors * 10,
  mtries = 9,
  min_rows = 3,
  max_depth = 30,
  sample_rate = 0.632,
  seed = 123
)

#very similar results between baseline and model after gridsearch.
h2o_rf_baseline@model$training_metrics@metrics$RMSE
h2o_rf_best@model$training_metrics@metrics$RMSE

#This uses MDI for variable importance, not quite the same as the measure before
h2o_rf_best@model$variable_importances |> 
  ggplot(aes(x = scaled_importance, y = fct_reorder(variable, scaled_importance))) +
  geom_bar(stat = "identity") +
  labs(y = "Variable", x = "Scaled Importance", title = "The same predictors seem to be as important again",
       subtitle = "Attendance, hours of study and previous exam score are consistently the more important variables")
