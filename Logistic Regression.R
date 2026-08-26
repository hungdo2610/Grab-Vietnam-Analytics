"""
Logistic Regression: Predicting High-Value GrabFood/GrabMart Transactions
============================================================================
Target: high_value (1 = top 20% of basket_value_vnd, 0 = remaining 80%)
Threshold is calculated on TRAIN only, then applied unchanged to TEST
(no leakage).

Files:
  train_cleaned.csv (739 rows) -- used to fit the model
  test_cleaned.csv  (184 rows) -- held out, used only for final evaluation
"""
library(tidyverse)
library(broom)
library(pROC)
library(car)

# ============================================================
# 0. LOAD DATA
# ============================================================

train <- read.csv("data/processed/train_cleaned.csv")
test <- read.csv("data/processed/test_cleaned.csv")

# ============================================================
# 1. Recode data 
# ============================================================


# Remove booking ID if present
train <- train %>% select(-any_of("booking_id"))
test  <- test %>% select(-any_of("booking_id"))

recode_data <- function(df) {
  
  df %>%
    mutate(
      promo_code_used = replace_na(promo_code_used, "No Promo"),
      
      payment_method = if_else(
        payment_method == "Unknown",
        "Cash",
        payment_method
      ),
      
      traffic_level = if_else(
        traffic_level == "Unknown",
        "Medium",
        traffic_level
      ),
      
      promo_code_used = if_else(
        promo_code_used == "Unknown",
        "No Promo",
        promo_code_used
      ),
      
      weather_condition = case_when(
        weather_condition == "Unknown" ~ "Clear",
        weather_condition == "Heavy Rain" ~ "Rain",
        TRUE ~ weather_condition
      ),
      
      city = if_else(
        city %in% c("Ho Chi Minh City", "Hanoi"),
        city,
        "Other City"
      )
    )
}

train <- recode_data(train)
test  <- recode_data(test)

# ============================================================
# 2. Create the high-value target 
# ============================================================


threshold <- quantile(
  train$basket_value_vnd,
  probs = 0.80,
  na.rm = TRUE
)

train <- train %>%
  mutate(
    high_value = if_else(basket_value_vnd >= threshold, 1, 0)
  )

test <- test %>%
  mutate(
    high_value = if_else(basket_value_vnd >= threshold, 1, 0)
  )

# ============================================================
# 3. Check Target
# ============================================================
table(train$high_value)
prop.table(table(train$high_value))

table(test$high_value)
prop.table(table(test$high_value))
# ============================================================
# 4. Model
# ============================================================

logit_model <- glm(
  high_value ~
    customer_segment +
    service_type +
    customer_age +
    city +
    payment_method +
    booking_channel +
    distance_km_clean +
    estimated_duration_min +
    time_period +
    is_weekend +
    promo_code_used +
    discount_amount_vnd +
    traffic_level +
    weather_condition,
  data = train,
  family = binomial
)

summary(logit_model)

#Test Predictors
test_prob <- predict(
  logit_model,
  newdata = test,
  type = "response"
)
# ============================================================
# 4. Testing 
# ============================================================
anova(logit_model, test = "Chisq")

null_model <- glm(
  high_value ~ 1,
  data = train,
  family = binomial
)

anova(null_model, logit_model, test = "Chisq")



test_prob <- predict(
  logit_model,
  newdata = test,
  type = "response"
)

test_pred <- ifelse(test_prob >= 0.5, 1, 0)

# Confusion matrix
table(
  Actual = test$high_value,
  Predicted = test_pred
)

# Accuracy
mean(test_pred == test$high_value)

# ROC-AUC
roc_model <- pROC::roc(
  test$high_value,
  test_prob
)

pROC::auc(roc_model)

