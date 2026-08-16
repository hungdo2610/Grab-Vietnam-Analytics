install.packages(c(
  "readxl",
  "dplyr",
  "ggplot2",
  "e1071",
  "tidyr",
  "stringr",
  "lubridate",
  "caret",
  "randomForest",
  "pROC"
))

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

grab <- read_excel("/Users/hungdo/Downloads/Grab Vietnam.xlsx")

#convert numerical data from text to numerical form. 
numeric_vars <- c(
  "customer_age",
  "driver_experience_years",
  "distance_km",
  "estimated_duration_min",
  "actual_duration_min",
  "waiting_time_min",
  "fare_amount_vnd",
  "basket_value_vnd"
)

grab <- grab %>%
  mutate(
    across(
      all_of(numeric_vars),
      ~ as.numeric(.)
    )
  )


#check missing values

missing_values <- sapply(
  grab[numeric_vars],
  function(x) sum(is.na(x))
)

missing_values



# Compare basket value by service type
basket %>%
  group_by(service_type) %>%
  summarise(
    n = n(),
    mean_basket = mean(basket_value_vnd),
    median_basket = median(basket_value_vnd),
    sd_basket = sd(basket_value_vnd),
    minimum = min(basket_value_vnd),
    Q1 = quantile(basket_value_vnd, 0.25),
    Q3 = quantile(basket_value_vnd, 0.75),
    maximum = max(basket_value_vnd)
  )

#Basket 80th percentile
basket_threshold <- quantile(
  basket$basket_value_vnd,
  0.80,
  na.rm = TRUE
)

basket_threshold


#skewness of basket_value_vnd 
basket_skewness <- skewness(
  basket$basket_value_vnd,
  na.rm = TRUE
)

basket_skewness

table(basket$service_type)


#distribution of basket value histogram
summary(basket$basket_value_vnd)
ggplot(basket, aes(x = basket_value_vnd)) +
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

#Percentiles and IQR for Basket size. 
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


#Split services into GrabFood and GrabMart
basket <- grab %>%
  filter(
    service_type %in% c("GrabFood", "GrabMart"),
    !is.na(basket_value_vnd)
  )
table(basket$service_type)

#check duplicates
sum(duplicated(basket$booking_id))

n_distinct(basket$booking_id)
nrow(basket)

#check duplicated booking_id observations
basket %>%
  group_by(booking_id) %>%
  filter(n() > 1) %>%
  arrange(booking_id)

#remove duplicated bookings
basket <- basket %>%
distinct(booking_id, .keep_all = TRUE)

nrow(basket)
n_distinct(basket$booking_id)
sum(duplicated(basket$booking_id))

#check variable types
str(basket)
sapply(basket, class)

#check missing values
missing_summary <- data.frame(
  variable = names(basket),
  missing = sapply(basket, function(x) sum(is.na(x))),
  percentage = sapply(
    basket,
    function(x) mean(is.na(x)) * 100
  )
)

missing_summary %>%
  
  arrange(desc(missing))


#convert booking_datetime to datetime
basket <- basket %>%
  mutate(
    booking_datetime = as.POSIXct(
      booking_datetime,
      format = "%Y-%m-%d %H:%M"
    )
  )
class(basket$booking_datetime)


#create time variable
basket <- basket %>%
  mutate(
    booking_hour = as.integer(
      format(booking_datetime, "%H")
    ),
    booking_day = weekdays(booking_datetime),
    is_weekend = ifelse(
      booking_day %in% c("Saturday", "Sunday"),
      "Weekend",
      "Weekday"
    )
  )
basket$is_weekend <- factor(basket$is_weekend)

head(
  basket %>%
    select(
      booking_datetime,
      booking_hour,
      booking_day,
      is_weekend
    )
)

table(basket$booking_hour)
table(basket$is_weekend)
#check impossible values
summary(basket %>%
          select(
            customer_age,
            distance_km,
            estimated_duration_min,
            basket_value_vnd
          ))

basket %>%
  summarise(
    negative_age = sum(customer_age < 0, na.rm = TRUE),
    zero_distance = sum(distance_km == 0, na.rm = TRUE),
    negative_distance = sum(distance_km < 0, na.rm = TRUE),
    negative_duration = sum(estimated_duration_min < 0, na.rm = TRUE),
    negative_basket = sum(basket_value_vnd < 0, na.rm = TRUE),
    zero_basket = sum(basket_value_vnd == 0, na.rm = TRUE)
  )
#check distances
basket %>%
  arrange(desc(distance_km)) %>%
  select(
    booking_id,
    city,
    service_type,
    distance_km,
    estimated_duration_min,
    basket_value_vnd
  ) %>%
  head(15)

#Filter observations with distance >15km and calculate speed
basket %>%
  filter(distance_km > 15) %>%
  arrange(desc(distance_km)) %>%
  select(
    booking_id,
    city,
    service_type,
    distance_km,
    estimated_duration_min,
    basket_value_vnd
  )

basket <- basket %>%
  mutate(
    estimated_speed_kmh =
      distance_km / estimated_duration_min * 60
  )

basket %>%
  arrange(desc(estimated_speed_kmh)) %>%
  select(
    booking_id,
    service_type,
    distance_km,
    estimated_duration_min,
    estimated_speed_kmh,
    basket_value_vnd
  ) %>%
  head(20)
#flag problematic observations
basket <- basket %>%
  mutate(
    distance_invalid = estimated_speed_kmh > 50
  )
table(basket$distance_invalid)

#List of 9 problematic observations 
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


#Turn problematic data into n/a
basket <- basket %>%
  mutate(
    distance_km_clean = ifelse(
      distance_invalid,
      NA,
      distance_km
    )
  )


summary(basket$distance_km_clean)
sum(is.na(basket$distance_km_clean))

#
missing_summary <- data.frame(
  variable = names(basket),
  missing = sapply(
    basket,
    function(x) sum(is.na(x))
  ),
  percentage = sapply(
    basket,
    function(x) mean(is.na(x)) * 100
  )
)

missing_summary %>%
  arrange(desc(missing))

#
model_variables <- c(
  "basket_value_vnd",
  "customer_segment",
  "customer_age",
  "service_type",
  "city",
  "payment_method",
  "booking_channel",
  "distance_km_clean",
  "estimated_duration_min"
)

model_data <- basket %>%
  select(all_of(model_variables)) %>%
  drop_na()

dim(model_data)


#Check categorical variables
table(basket$customer_segment)
table(basket$service_type)
table(basket$city)
table(basket$payment_method)
table(basket$booking_channel)
unique(basket$customer_segment)
unique(basket$city)
unique(basket$payment_method)
unique(basket$booking_channel)




#create model data
model_variables <- c(
  "basket_value_vnd",
  "customer_segment",
  "customer_age",
  "service_type",
  "city",
  "payment_method",
  "booking_channel",
  "distance_km_clean",
  "estimated_duration_min",
  "booking_hour",
  "is_weekend"
)

model_data <- basket %>%
  select(all_of(model_variables)) %>%
  drop_na()

dim(model_data)

# EDA: Promo Code Usage for Food/Mart Orders

table(basket$promo_code_used, useNA = "ifany")

basket %>%
  group_by(promo_code_used) %>%
  summarise(
    n = n(),
    mean_basket = mean(basket_value_vnd, na.rm = TRUE),
    median_basket = median(basket_value_vnd, na.rm = TRUE),
    sd_basket = sd(basket_value_vnd, na.rm = TRUE),
    minimum = min(basket_value_vnd, na.rm = TRUE),
    Q1 = quantile(basket_value_vnd, 0.25, na.rm = TRUE),
    Q3 = quantile(basket_value_vnd, 0.75, na.rm = TRUE),
    maximum = max(basket_value_vnd, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_basket))

ggplot(
  basket %>%
    filter(
      !is.na(promo_code_used),
      !is.na(basket_value_vnd)
    ),
  aes(
    x = promo_code_used,
    y = basket_value_vnd
  )
) +
  geom_boxplot() +
  labs(
    title = "Basket Value by Promo Code",
    x = "Promo Code",
    y = "Basket Value (VND)"
  ) +
  theme_minimal()

# EDA: Discount Amount for Food/Mart Orders
basket <- basket %>%
  mutate(
    discount_amount_vnd = as.numeric(discount_amount_vnd)
  )

basket %>%
  summarise(
    n = sum(!is.na(discount_amount_vnd)),
    missing = sum(is.na(discount_amount_vnd)),
    mean_discount = mean(discount_amount_vnd, na.rm = TRUE),
    median_discount = median(discount_amount_vnd, na.rm = TRUE),
    sd_discount = sd(discount_amount_vnd, na.rm = TRUE),
    minimum = min(discount_amount_vnd, na.rm = TRUE),
    Q1 = quantile(discount_amount_vnd, 0.25, na.rm = TRUE),
    Q3 = quantile(discount_amount_vnd, 0.75, na.rm = TRUE),
    maximum = max(discount_amount_vnd, na.rm = TRUE)
  )

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
  geom_smooth(method = "lm", se = TRUE) +
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

# EDA: Traffic Level

# Frequency
table(basket$traffic_level, useNA = "ifany")

# Basket value by traffic level
basket %>%
  group_by(traffic_level) %>%
  summarise(
    n = n(),
    mean_basket = mean(basket_value_vnd, na.rm = TRUE),
    median_basket = median(basket_value_vnd, na.rm = TRUE),
    sd_basket = sd(basket_value_vnd, na.rm = TRUE),
    minimum = min(basket_value_vnd, na.rm = TRUE),
    Q1 = quantile(basket_value_vnd, 0.25, na.rm = TRUE),
    Q3 = quantile(basket_value_vnd, 0.75, na.rm = TRUE),
    maximum = max(basket_value_vnd, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_basket))

# Visualization
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

# EDA: Weather Condition

# Frequency
table(basket$weather_condition, useNA = "ifany")

# Basket value by weather condition
basket %>%
  group_by(weather_condition) %>%
  summarise(
    n = n(),
    mean_basket = mean(basket_value_vnd, na.rm = TRUE),
    median_basket = median(basket_value_vnd, na.rm = TRUE),
    sd_basket = sd(basket_value_vnd, na.rm = TRUE),
    minimum = min(basket_value_vnd, na.rm = TRUE),
    Q1 = quantile(basket_value_vnd, 0.25, na.rm = TRUE),
    Q3 = quantile(basket_value_vnd, 0.75, na.rm = TRUE),
    maximum = max(basket_value_vnd, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_basket))

# Visualization
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
# FINAL MODELLING DATASET
# ============================================================

# 1. Create business-friendly time periods
basket <- basket %>%
  mutate(
    time_period = case_when(
      booking_hour >= 0 & booking_hour < 6 ~ "Early Morning",
      booking_hour >= 6 & booking_hour < 11 ~ "Morning",
      booking_hour >= 11 & booking_hour < 14 ~ "Lunch",
      booking_hour >= 14 & booking_hour < 17 ~ "Afternoon",
      booking_hour >= 17 & booking_hour < 21 ~ "Evening",
      TRUE ~ "Night"
    )
  )

# Convert categorical variables to factors
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


# 2. Define all candidate modelling variables

model_variables <- c(
  
  # Outcome
  "basket_value_vnd",
  
  # Core customer variables
  "customer_segment",
  "customer_age",
  
  # Service/order variables
  "service_type",
  "city",
  "payment_method",
  "booking_channel",
  
  # Operational variables
  "distance_km_clean",
  "estimated_duration_min",
  
  # Time variables
  "time_period",
  "is_weekend",
  
  # Promotional variables
  "promo_code_used",
  "discount_amount_vnd",
  
  # Environmental variables
  "traffic_level",
  "weather_condition"
)


# 3. Create modelling dataset

model_data <- basket %>%
  select(all_of(model_variables)) %>%
  drop_na()


# 4. Check dimensions

dim(model_data)


# 5. Check variable types

str(model_data)


# 6. Check missing values

missing_model <- data.frame(
  variable = names(model_data),
  missing = sapply(
    model_data,
    function(x) sum(is.na(x))
  )
)

missing_model


# 7. Check categorical variables

table(model_data$customer_segment)
table(model_data$service_type)
table(model_data$city)
table(model_data$payment_method)
table(model_data$booking_channel)
table(model_data$time_period)
table(model_data$is_weekend)
table(model_data$promo_code_used)
table(model_data$traffic_level)
table(model_data$weather_condition)


# 8. Check numerical variables

summary(
  model_data %>%
    select(
      basket_value_vnd,
      customer_age,
      distance_km_clean,
      estimated_duration_min,
      discount_amount_vnd
    )
)