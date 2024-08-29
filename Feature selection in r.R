library(ggplot2)
library(dplyr)
library(corrplot)
library(MASS)

data <- read.csv("C:/Users/User/Downloads/individual project dataset/credit_risk_dataset.csv")

head(df)

clean_data <- na.omit(data)

clean_data$person_home_ownership <- as.factor(clean_data$person_home_ownership)
clean_data$loan_intent <- as.factor(clean_data$loan_intent)
clean_data$loan_grade <- as.factor(clean_data$loan_grade)
clean_data$cb_person_default_on_file <- as.factor(clean_data$cb_person_default_on_file)

full_model <- lm(loan_status ~ ., data = clean_data)

# Perform stepwise regression using AIC
stepwise_model <- stepAIC(full_model,direction = "both")
summary(stepwise_model)

mul_reg <- lm(loan_status ~ person_age + person_income + person_home_ownership + 
  loan_intent + loan_grade + loan_amnt + loan_int_rate + loan_percent_income + 
  cb_person_cred_hist_length, data= clean_data)
summary(mul_reg)
