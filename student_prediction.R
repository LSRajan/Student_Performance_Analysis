#Using models to try and find some ability to predict performance despite difficult distribution

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

numeric_data <- data |> 
  mutate(across(c(Parental_Involvement, Access_to_Resources, Extracurricular_Activities,
                  Motivation_Level, Internet_Access, Family_Income, Teacher_Quality,
                  School_Type, Peer_Influence, Learning_Disabilities, 
                  Parental_Education_Level, Distance_from_Home, Gender), convert_cat))

model1 <- lm(Exam_Score ~ ., numeric_data)
summary(model1)
