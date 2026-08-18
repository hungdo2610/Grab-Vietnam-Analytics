# ============================================================
# GRAB VIETNAM ANALYTICS
# DATA PREPARATION
# ============================================================


# ============================================================
# 1. LOAD PACKAGES
# ============================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(e1071)
library(tidyr)
library(stringr)
library(lubridate)
library(caret)
library(randomForest)
library(pROC)


# ============================================================
# 2. LOAD RAW DATA
# ============================================================

grab <- read_excel("/Users/hungdo/Downloads/Grab Vietnam.xlsx")


# ============================================================
# 3. CONVERT NUMERICAL VARIABLES
# ============================================================

numeric_vars <- c(
  "customer_age",
  "driver_experience_years",
  "distance_km",
  "estimated_duration_min",
  "actual_duration_min",
  "waiting_time_min",
  "fare_amount_vnd",
  "basket_value_vnd",
  "discount_amount_vnd"
)

grab <- grab %>%
  mutate(
    across(
      all_of(numeric_vars),
      as.numeric
    )
  )


# ============================================================
# 4. CREATE BASKET DATASET
# ============================================================

# Keep only GrabFood and GrabMart transactions
# with a valid basket value.

basket <- grab %>%
  filter(
    service_type %in% c("GrabFood", "GrabMart"),
    !is.na(basket_value_vnd)
  )


# Check number of observations by service type

table(basket$service_type)


# ============================================================
# 5. DATA QUALITY CHECKS
# ============================================================


# ------------------------------------------------------------
# 5.1 Missing values
# ------------------------------------------------------------

missing_summary <- data.frame(
  variable = names(basket),
  missing = sapply(basket, function(x) sum(is.na(x))),
  percentage = sapply(
    basket,
    function(x) mean(is.na(x)) * 100
  )
) %>%
  arrange(desc(missing))

missing_summary


# ------------------------------------------------------------
# 5.2 Duplicate bookings
# ------------------------------------------------------------

sum(duplicated(basket$booking_id))

n_distinct(basket$booking_id)

nrow(basket)


# Identify duplicated booking IDs

basket %>%
  group_by(booking_id) %>%
  filter(n() > 1) %>%
  arrange(booking_id)


# Remove duplicated bookings

basket <- basket %>%
  distinct(
    booking_id,
    .keep_all = TRUE
  )


# Confirm duplicates have been removed

sum(duplicated(basket$booking_id))

n_distinct(basket$booking_id)

nrow(basket)


# ------------------------------------------------------------
# 5.3 Variable types
# ------------------------------------------------------------

str(basket)


# ============================================================
# 6. CLEAN BOOKING DATETIME
# ============================================================

basket <- basket %>%
  mutate(
    booking_datetime = as.POSIXct(
      booking_datetime,
      format = "%Y-%m-%d %H:%M"
    )
  )


# ============================================================
# 7. CREATE TIME VARIABLES
# ============================================================

basket <- basket %>%
  mutate(
    booking_hour = hour(booking_datetime),
    
    booking_day = weekdays(booking_datetime),
    
    is_weekend = ifelse(
      booking_day %in% c("Saturday", "Sunday"),
      "Weekend",
      "Weekday"
    ),
    
    time_period = case_when(
      booking_hour >= 0 & booking_hour < 6 ~ "Early Morning",
      booking_hour >= 6 & booking_hour < 11 ~ "Morning",
      booking_hour >= 11 & booking_hour < 14 ~ "Lunch",
      booking_hour >= 14 & booking_hour < 17 ~ "Afternoon",
      booking_hour >= 17 & booking_hour < 21 ~ "Evening",
      TRUE ~ "Night"
    )
  )


# ============================================================
# 8. CHECK IMPOSSIBLE VALUES
# ============================================================

basket %>%
  summarise(
    negative_age = sum(customer_age < 0, na.rm = TRUE),
    zero_distance = sum(distance_km == 0, na.rm = TRUE),
    negative_distance = sum(distance_km < 0, na.rm = TRUE),
    negative_duration = sum(
      estimated_duration_min < 0,
      na.rm = TRUE
    ),
    negative_basket = sum(
      basket_value_vnd < 0,
      na.rm = TRUE
    ),
    zero_basket = sum(
      basket_value_vnd == 0,
      na.rm = TRUE
    )
  )


# ============================================================
# 9. CHECK AND CLEAN DISTANCE
# ============================================================

# Calculate estimated speed

basket <- basket %>%
  mutate(
    estimated_speed_kmh =
      distance_km / estimated_duration_min * 60
  )


# Flag potentially problematic observations

basket <- basket %>%
  mutate(
    distance_invalid = estimated_speed_kmh > 50
  )


# Check problematic observations

basket %>%
  filter(distance_invalid) %>%
  select(
    booking_id,
    service_type,
    distance_km,
    estimated_duration_min,
    estimated_speed_kmh,
    basket_value_vnd
  )


# Replace invalid distance values with NA

basket <- basket %>%
  mutate(
    distance_km_clean = ifelse(
      distance_invalid,
      NA,
      distance_km
    )
  )


# Check cleaned distance variable

summary(basket$distance_km_clean)

sum(is.na(basket$distance_km_clean))


# ============================================================
# 10. CONVERT CATEGORICAL VARIABLES TO FACTORS
# ============================================================

basket <- basket %>%
  mutate(
    customer_segment = factor(customer_segment),
    service_type = factor(service_type),
    city = factor(city),
    payment_method = factor(payment_method),
    booking_channel = factor(booking_channel),
    time_period = factor(time_period),
    is_weekend = factor(is_weekend),
    promo_code_used = factor(promo_code_used),
    traffic_level = factor(traffic_level),
    weather_condition = factor(weather_condition)
  )


# ============================================================
# 11. BASKET VALUE DESCRIPTIVE ANALYSIS
# ============================================================


# Summary by service type

basket %>%
  group_by(service_type) %>%
  summarise(
    n = n(),
    mean_basket = mean(
      basket_value_vnd,
      na.rm = TRUE
    ),
    median_basket = median(
      basket_value_vnd,
      na.rm = TRUE
    ),
    sd_basket = sd(
      basket_value_vnd,
      na.rm = TRUE
    ),
    minimum = min(
      basket_value_vnd,
      na.rm = TRUE
    ),
    Q1 = quantile(
      basket_value_vnd,
      0.25,
      na.rm = TRUE
    ),
    Q3 = quantile(
      basket_value_vnd,
      0.75,
      na.rm = TRUE
    ),
    maximum = max(
      basket_value_vnd,
      na.rm = TRUE
    )
  )


# ------------------------------------------------------------
# 11.1 High-value threshold
# ------------------------------------------------------------

basket_threshold <- quantile(
  basket$basket_value_vnd,
  0.80,
  na.rm = TRUE
)

basket_threshold


# ------------------------------------------------------------
# 11.2 Skewness
# ------------------------------------------------------------

basket_skewness <- skewness(
  basket$basket_value_vnd,
  na.rm = TRUE
)

basket_skewness


# ------------------------------------------------------------
# 11.3 Distribution
# ------------------------------------------------------------

ggplot(
  basket,
  aes(x = basket_value_vnd)
) +
  geom_histogram(bins = 30) +
  geom_vline(
    xintercept = basket_threshold,
    linetype = "dashed"
  ) +
  labs(
    title = "Distribution of Basket Value",
    x = "Basket Value (VND)",
    y = "Number of Orders",
    subtitle = "Dashed line represents the 80th percentile"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 11.4 IQR and upper outlier boundary
# ------------------------------------------------------------

Q1_basket <- quantile(
  basket$basket_value_vnd,
  0.25,
  na.rm = TRUE
)

Q3_basket <- quantile(
  basket$basket_value_vnd,
  0.75,
  na.rm = TRUE
)

IQR_basket <- Q3_basket - Q1_basket

upper_basket <- Q3_basket + 1.5 * IQR_basket

IQR_basket

upper_basket


# ============================================================
# 12. ADDITIONAL EDA VARIABLES
# ============================================================


# ------------------------------------------------------------
# 12.1 Promo code
# ------------------------------------------------------------

table(
  basket$promo_code_used,
  useNA = "ifany"
)

basket %>%
  group_by(promo_code_used) %>%
  summarise(
    n = n(),
    mean_basket = mean(
      basket_value_vnd,
      na.rm = TRUE
    ),
    median_basket = median(
      basket_value_vnd,
      na.rm = TRUE
    ),
    sd_basket = sd(
      basket_value_vnd,
      na.rm = TRUE
    ),
    minimum = min(
      basket_value_vnd,
      na.rm = TRUE
    ),
    Q1 = quantile(
      basket_value_vnd,
      0.25,
      na.rm = TRUE
    ),
    Q3 = quantile(
      basket_value_vnd,
      0.75,
      na.rm = TRUE
    ),
    maximum = max(
      basket_value_vnd,
      na.rm = TRUE
    )
  ) %>%
  arrange(desc(mean_basket))


# ------------------------------------------------------------
# 12.2 Discount
# ------------------------------------------------------------

basket %>%
  summarise(
    n = sum(!is.na(discount_amount_vnd)),
    missing = sum(is.na(discount_amount_vnd)),
    mean_discount = mean(
      discount_amount_vnd,
      na.rm = TRUE
    ),
    median_discount = median(
      discount_amount_vnd,
      na.rm = TRUE
    ),
    sd_discount = sd(
      discount_amount_vnd,
      na.rm = TRUE
    ),
    minimum = min(
      discount_amount_vnd,
      na.rm = TRUE
    ),
    Q1 = quantile(
      discount_amount_vnd,
      0.25,
      na.rm = TRUE
    ),
    Q3 = quantile(
      discount_amount_vnd,
      0.75,
      na.rm = TRUE
    ),
    maximum = max(
      discount_amount_vnd,
      na.rm = TRUE
    )
  )


# Relationship between discount and basket value

ggplot(
  basket %>%
    filter(
      !is.na(discount_amount_vnd),
      !is.na(basket_value_vnd)
    ),
  aes(
    x = discount_amount_vnd,
    y = basket_value_vnd
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Relationship Between Discount and Basket Value",
    x = "Discount Amount (VND)",
    y = "Basket Value (VND)"
  ) +
  theme_minimal()


cor(
  basket$discount_amount_vnd,
  basket$basket_value_vnd,
  use = "complete.obs"
)


# ------------------------------------------------------------
# 12.3 Traffic level
# ------------------------------------------------------------

table(
  basket$traffic_level,
  useNA = "ifany"
)

basket %>%
  group_by(traffic_level) %>%
  summarise(
    n = n(),
    mean_basket = mean(
      basket_value_vnd,
      na.rm = TRUE
    ),
    median_basket = median(
      basket_value_vnd,
      na.rm = TRUE
    ),
    sd_basket = sd(
      basket_value_vnd,
      na.rm = TRUE
    )
  ) %>%
  arrange(desc(mean_basket))


ggplot(
  basket %>%
    filter(
      !is.na(traffic_level),
      !is.na(basket_value_vnd)
    ),
  aes(
    x = traffic_level,
    y = basket_value_vnd
  )
) +
  geom_boxplot() +
  labs(
    title = "Basket Value by Traffic Level",
    x = "Traffic Level",
    y = "Basket Value (VND)"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 12.4 Weather condition
# ------------------------------------------------------------

table(
  basket$weather_condition,
  useNA = "ifany"
)

basket %>%
  group_by(weather_condition) %>%
  summarise(
    n = n(),
    mean_basket = mean(
      basket_value_vnd,
      na.rm = TRUE
    ),
    median_basket = median(
      basket_value_vnd,
      na.rm = TRUE
    ),
    sd_basket = sd(
      basket_value_vnd,
      na.rm = TRUE
    )
  ) %>%
  arrange(desc(mean_basket))


ggplot(
  basket %>%
    filter(
      !is.na(weather_condition),
      !is.na(basket_value_vnd)
    ),
  aes(
    x = weather_condition,
    y = basket_value_vnd
  )
) +
  geom_boxplot() +
  labs(
    title = "Basket Value by Weather Condition",
    x = "Weather Condition",
    y = "Basket Value (VND)"
  ) +
  theme_minimal()


# ============================================================
# 13. CREATE MODEL DATASET
# ============================================================

model_variables <- c(
  
  # Outcome
  "basket_value_vnd",
  
  # Customer
  "customer_segment",
  "customer_age",
  
  # Service and booking
  "service_type",
  "city",
  "payment_method",
  "booking_channel",
  
  # Operational
  "distance_km_clean",
  "estimated_duration_min",
  
  # Time
  "time_period",
  "is_weekend",
  
  # Promotion
  "promo_code_used",
  "discount_amount_vnd",
  
  # Environment
  "traffic_level",
  "weather_condition"
)


model_data <- basket %>%
  select(all_of(model_variables))


# ============================================================
# 14. TRAIN / TEST SPLIT
# ============================================================

set.seed(123)

train_index <- createDataPartition(
  model_data$basket_value_vnd,
  p = 0.80,
  list = FALSE
)

train <- model_data[train_index, ]
test <- model_data[-train_index, ]


# Check number of observations

nrow(model_data)
nrow(train)
nrow(test)


# Check proportions

nrow(train) / nrow(model_data)
nrow(test) / nrow(model_data)


# ============================================================
# 15. HANDLE MISSING VALUES
# ============================================================

# ------------------------------------------------------------
# 15.1 Numerical variables
# ------------------------------------------------------------

numeric_model_vars <- c(
  "customer_age",
  "distance_km_clean"
)


# Calculate medians using TRAINING data only

train_medians <- sapply(
  train[numeric_model_vars],
  median,
  na.rm = TRUE
)


# Apply training medians to both datasets

for (var in numeric_model_vars) {
  
  train[[var]][is.na(train[[var]])] <- train_medians[var]
  
  test[[var]][is.na(test[[var]])] <- train_medians[var]
}


# ------------------------------------------------------------
# 15.2 Categorical variables
# ------------------------------------------------------------

categorical_model_vars <- c(
  "payment_method",
  "traffic_level",
  "weather_condition",
  "promo_code_used"
)


# Replace missing values with "Unknown"

for (var in categorical_model_vars) {
  
  train[[var]] <- as.character(train[[var]])
  test[[var]] <- as.character(test[[var]])
  
  train[[var]][is.na(train[[var]])] <- "Unknown"
  test[[var]][is.na(test[[var]])] <- "Unknown"
  
  train[[var]] <- factor(train[[var]])
  test[[var]] <- factor(
    test[[var]],
    levels = levels(train[[var]])
  )
}

# ============================================================
# 16. VALIDATE TRAIN / TEST DATA
# ============================================================

# ------------------------------------------------------------
# 16.1 Check observations and split proportions
# ------------------------------------------------------------

nrow(model_data)
nrow(train)
nrow(test)

# Confirm no observations were lost
nrow(train) + nrow(test) == nrow(model_data)

# Check train/test proportions
round(
  c(
    Train = nrow(train) / nrow(model_data),
    Test = nrow(test) / nrow(model_data)
  ) * 100,
  2
)


# ------------------------------------------------------------
# 16.2 Check variable consistency
# ------------------------------------------------------------

# Check that train and test contain the same variables
identical(names(train), names(test))

# Check dimensions
dim(train)
dim(test)


# ------------------------------------------------------------
# 16.3 Check missing values
# ------------------------------------------------------------

colSums(is.na(train))

colSums(is.na(test))


# ------------------------------------------------------------
# 16.4 Compare target variable distribution
# ------------------------------------------------------------

train %>%
  summarise(
    n = n(),
    mean = mean(basket_value_vnd),
    median = median(basket_value_vnd),
    sd = sd(basket_value_vnd),
    min = min(basket_value_vnd),
    Q1 = quantile(basket_value_vnd, 0.25),
    Q3 = quantile(basket_value_vnd, 0.75),
    max = max(basket_value_vnd)
  )

test %>%
  summarise(
    n = n(),
    mean = mean(basket_value_vnd),
    median = median(basket_value_vnd),
    sd = sd(basket_value_vnd),
    min = min(basket_value_vnd),
    Q1 = quantile(basket_value_vnd, 0.25),
    Q3 = quantile(basket_value_vnd, 0.75),
    max = max(basket_value_vnd)
  )


# ------------------------------------------------------------
# 16.5 Compare categorical variable distributions
# ------------------------------------------------------------

prop.table(table(train$service_type))
prop.table(table(test$service_type))

prop.table(table(train$customer_segment))
prop.table(table(test$customer_segment))

prop.table(table(train$city))
prop.table(table(test$city))

prop.table(table(train$payment_method))
prop.table(table(test$payment_method))

prop.table(table(train$booking_channel))
prop.table(table(test$booking_channel))


# ------------------------------------------------------------
# 16.6 Compare numerical variable distributions
# ------------------------------------------------------------

numeric_vars <- c(
  "customer_age",
  "distance_km_clean",
  "estimated_duration_min",
  "discount_amount_vnd"
)

train %>%
  summarise(
    across(
      all_of(numeric_vars),
      list(
        mean = ~mean(.x),
        median = ~median(.x),
        sd = ~sd(.x)
      )
    )
  )

test %>%
  summarise(
    across(
      all_of(numeric_vars),
      list(
        mean = ~mean(.x),
        median = ~median(.x),
        sd = ~sd(.x)
      )
    )
  )


# ------------------------------------------------------------
# 16.7 Visual comparison of target variable
# ------------------------------------------------------------

comparison <- bind_rows(
  train %>% mutate(dataset = "Train"),
  test %>% mutate(dataset = "Test")
)

ggplot(
  comparison,
  aes(
    x = dataset,
    y = basket_value_vnd
  )
) +
  geom_boxplot() +
  labs(
    title = "Basket Value Distribution: Train vs Test",
    x = "Dataset",
    y = "Basket Value (VND)"
  ) +
  theme_minimal()


# ============================================================
# 17. SAVE MODEL DATASETS
# ============================================================

write.csv(
  train,
  "data/train.csv",
  row.names = FALSE
)

write.csv(
  test,
  "data/test.csv",
  row.names = FALSE
)

write.csv(
  model_data,
  "data/model_data.csv",
  row.names = FALSE
)



