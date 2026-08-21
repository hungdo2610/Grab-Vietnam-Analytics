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



basket <- grab %>%
  filter(
    service_type %in% c("GrabFood", "GrabMart")
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

# ============================================================
# 5.3 CATEGORICAL DATA QUALITY
# ============================================================

categorical_vars <- c(
  "customer_segment",
  "service_type",
  "city",
  "payment_method",
  "booking_channel",
  "promo_code_used",
  "traffic_level",
  "weather_condition"
)

lapply(
  basket[categorical_vars],
  unique
)

lapply(
  basket[categorical_vars],
  function(x) unique(str_trim(as.character(x)))
)

lapply(
  basket[categorical_vars],
  function(x) table(x, useNA = "ifany")
)

# ============================================================
# 5.4 CATEGORICAL VALUE CONSISTENCY CHECK
# ============================================================

categorical_vars <- c(
  "customer_segment",
  "service_type",
  "city",
  "payment_method",
  "booking_channel",
  "promo_code_used",
  "traffic_level",
  "weather_condition"
)


# ------------------------------------------------------------
# Check unique values before cleaning
# ------------------------------------------------------------

categorical_unique_values <- lapply(
  basket[categorical_vars],
  function(x) {
    sort(
      unique(
        as.character(x)
      )
    )
  }
)

categorical_unique_values


# ------------------------------------------------------------
# Check for leading/trailing whitespace
# ------------------------------------------------------------

whitespace_check <- lapply(
  basket[categorical_vars],
  function(x) {
    
    x <- as.character(x)
    
    x[
      !is.na(x) &
        x != str_trim(x)
    ]
  }
)

whitespace_check


# ------------------------------------------------------------
# Remove leading/trailing whitespace
# ------------------------------------------------------------

basket <- basket %>%
  mutate(
    across(
      all_of(categorical_vars),
      ~ str_trim(as.character(.x))
    )
  )


# ------------------------------------------------------------
# Check for empty strings
# ------------------------------------------------------------

empty_string_check <- lapply(
  basket[categorical_vars],
  function(x) {
    
    x[
      !is.na(x) &
        x == ""
    ]
  }
)

empty_string_check


# ------------------------------------------------------------
# Convert empty strings to NA
# ------------------------------------------------------------

basket <- basket %>%
  mutate(
    across(
      all_of(categorical_vars),
      ~ na_if(.x, "")
    )
  )


# ------------------------------------------------------------
# Re-check unique values after cleaning
# ------------------------------------------------------------

categorical_unique_values_clean <- lapply(
  basket[categorical_vars],
  function(x) {
    sort(
      unique(
        as.character(x)
      )
    )
  }
)

categorical_unique_values_clean

# ============================================================
# 5.5 REVIEW CATEGORICAL LEVELS FOR SPELLING / TYPO ERRORS
# ============================================================

for (var in categorical_vars) {
  
  cat("\n====================================\n")
  cat("VARIABLE:", var, "\n")
  cat("====================================\n")
  
  print(
    sort(
      table(
        basket[[var]],
        useNA = "ifany"
      ),
      decreasing = TRUE
    )
  )
}

# ============================================================
# 5.6 MISSINGNESS HEATMAP
# ============================================================

# ------------------------------------------------------------
# Variables retained for MLR modelling
# Excludes feature-engineered variables that contain no
# independent missing values
# ------------------------------------------------------------

analysis_vars <- c(
  
  
  "customer_segment",
  "service_type",
  "customer_age",
  
  
  "city",
  "payment_method",
  "booking_channel",
  "distance_km",
  "estimated_duration_min",
  
  
  "promo_code_used",
  "discount_amount_vnd",
  
  
  "traffic_level",
  "weather_condition",
  
  
  "basket_value_vnd"
)


# ------------------------------------------------------------
# Calculate missing counts and percentages
# ------------------------------------------------------------

missing_counts <- basket %>%
  summarise(
    across(
      all_of(analysis_vars),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing"
  ) %>%
  mutate(
    percentage = missing / nrow(basket) * 100
  ) %>%
  arrange(
    desc(missing),
    variable
  )



missing_counts


variable_order <- missing_counts$variable


missing_map <- basket %>%
  mutate(
    row_id = row_number()
  ) %>%
  select(
    row_id,
    all_of(analysis_vars)
  ) %>%
  mutate(
    across(
      -row_id,
      ~ ifelse(
        is.na(.),
        "Missing",
        "Present"
      )
    )
  ) %>%
  pivot_longer(
    cols = -row_id,
    names_to = "variable",
    values_to = "status"
  )


missing_map <- missing_map %>%
  mutate(
    variable = factor(
      variable,
      levels = variable_order
    )
  )


# ------------------------------------------------------------
# Create readable labels with percentages
# ------------------------------------------------------------

label_lookup <- missing_counts %>%
  mutate(
    label = case_when(
      
      variable == "customer_segment" ~
        "Customer\nsegment",
      
      variable == "service_type" ~
        "Service\ntype",
      
      variable == "customer_age" ~
        "Customer\nage",
      
      variable == "city" ~
        "City",
      
      variable == "payment_method" ~
        "Payment\nmethod",
      
      variable == "booking_channel" ~
        "Booking\nchannel",
      
      variable == "distance_km" ~
        "Distance",
      
      variable == "estimated_duration_min" ~
        "Estimated\nduration",
      
      variable == "promo_code_used" ~
        "Promo\ncode",
      
      variable == "discount_amount_vnd" ~
        "Discount",
      
      variable == "traffic_level" ~
        "Traffic\nlevel",
      
      variable == "weather_condition" ~
        "Weather\ncondition",
      
      variable == "basket_value_vnd" ~
        "Basket\nvalue",
      
      TRUE ~ variable
    ),
    
    label = paste0(
      label,
      "\n(",
      round(percentage, 1),
      "%)"
    )
  )



label_vector <- label_lookup$label

names(label_vector) <- label_lookup$variable


# ------------------------------------------------------------
# Create heatmap
# ------------------------------------------------------------

missing_plot <- ggplot(
  missing_map,
  aes(
    x = variable,
    y = row_id,
    fill = status
  )
) +
  
  geom_tile(
    width = 0.95,
    height = 1
  ) +
  
  scale_fill_manual(
    values = c(
      "Present" = "#D9D9D9",
      "Missing" = "#2A9D8F"
    )
  ) +
  
  scale_x_discrete(
    limits = variable_order,
    labels = label_vector
  ) +
  
  labs(
    title = "Missingness Heatmap",
    subtitle = "Distribution of missing values across observations",
    x = "Variable",
    y = "Observation",
    fill = "Data status"
  ) +
  
  theme_minimal() +
  
  theme(
    
    # --------------------------------------------------------
    # Title
    # --------------------------------------------------------
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      family = "sans"
    ),
    
    plot.subtitle = element_text(
      size = 11,
      family = "sans"
    ),
    
    
    # --------------------------------------------------------
    # X-axis labels
    # --------------------------------------------------------
    
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 1,
      size = 9.5,
      family = "sans",
      lineheight = 0.9,
      margin = ggplot2::margin(t = 4)
    ),
    
    axis.title.x = element_text(
      size = 11,
      family = "sans",
      margin = ggplot2::margin(t = 20)
    ),
    
    
    # --------------------------------------------------------
    # Y-axis
    # --------------------------------------------------------
    
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    
    # --------------------------------------------------------
    # Remove grid
    # --------------------------------------------------------
    
    panel.grid = element_blank(),
    
    
    # --------------------------------------------------------
    # White space
    # --------------------------------------------------------
    
    plot.margin = ggplot2::margin(
      t = 10,
      r = 25,
      b = 10,
      l = 20
    ),
    
    
    # --------------------------------------------------------
    # Legend
    # --------------------------------------------------------
    
    legend.position = "right",
    
    legend.title = element_text(
      size = 11,
      family = "sans"
    ),
    
    legend.text = element_text(
      size = 10,
      family = "sans"
    )
  )


missing_plot


# ------------------------------------------------------------
# Save heatmap
# ------------------------------------------------------------

ggsave(
  "figures/00_missingness_heatmap.png",
  missing_plot,
  width = 16,
  height = 9,
  dpi = 300
)

# ------------------------------------------------------------
# Variables included in the missingness assessment
# ------------------------------------------------------------

missing_analysis_vars <- c(
  
  
  "customer_segment",
  "service_type",
  "customer_age",
  
  
  "city",
  "payment_method",
  "booking_channel",
  "distance_km",
  "estimated_duration_min",
  
  
  "promo_code_used",
  "discount_amount_vnd",
  
  
  "traffic_level",
  "weather_condition",
  
  
  "basket_value_vnd"
)


# ------------------------------------------------------------
# Create complete / incomplete indicator
# ------------------------------------------------------------

basket <- basket %>%
  mutate(
    has_missing = ifelse(
      rowSums(
        is.na(
          select(
            .,
            all_of(missing_analysis_vars)
          )
        )
      ) > 0,
      "Has Missing Data",
      "Complete"
    )
  )


# ------------------------------------------------------------
# Convert to factor
# ------------------------------------------------------------

basket <- basket %>%
  mutate(
    has_missing = factor(
      has_missing,
      levels = c(
        "Complete",
        "Has Missing Data"
      )
    )
  )


# ============================================================
# 5.5.1 MISSINGNESS GROUP SUMMARY
# ============================================================

missing_group_summary <- basket %>%
  count(has_missing) %>%
  mutate(
    percentage = n / nrow(basket) * 100
  )


missing_group_summary


# ============================================================
# 5.5.2 DESCRIPTIVE COMPARISON
# ============================================================

missing_impact <- basket %>%
  group_by(has_missing) %>%
  summarise(
    
    # Number of observations
    n = n(),
    
    # Percentage of observations
    percentage = n() / nrow(basket) * 100,
    
    # Basket value
    mean_basket_value = mean(
      basket_value_vnd,
      na.rm = TRUE
    ),
    
    median_basket_value = median(
      basket_value_vnd,
      na.rm = TRUE
    ),
    
    sd_basket_value = sd(
      basket_value_vnd,
      na.rm = TRUE
    ),
    
    # Customer age
    mean_customer_age = mean(
      customer_age,
      na.rm = TRUE
    ),
    
    median_customer_age = median(
      customer_age,
      na.rm = TRUE
    ),
    
    # Distance
    mean_distance = mean(
      distance_km,
      na.rm = TRUE
    ),
    
    median_distance = median(
      distance_km,
      na.rm = TRUE
    ),
    
    # Estimated duration
    mean_estimated_duration = mean(
      estimated_duration_min,
      na.rm = TRUE
    ),
    
    median_estimated_duration = median(
      estimated_duration_min,
      na.rm = TRUE
    ),
    
    # Discount
    mean_discount = mean(
      discount_amount_vnd,
      na.rm = TRUE
    ),
    
    median_discount = median(
      discount_amount_vnd,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# Display comparison
missing_impact


# ============================================================
# 5.5.3 BASKET VALUE COMPARISON
# ============================================================

basket_value_comparison <- basket %>%
  group_by(has_missing) %>%
  summarise(
    
    n = n(),
    
    mean = mean(
      basket_value_vnd,
      na.rm = TRUE
    ),
    
    median = median(
      basket_value_vnd,
      na.rm = TRUE
    ),
    
    sd = sd(
      basket_value_vnd,
      na.rm = TRUE
    ),
    
    min = min(
      basket_value_vnd,
      na.rm = TRUE
    ),
    
    max = max(
      basket_value_vnd,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


basket_value_comparison


# ============================================================
# 5.5.4 DIFFERENCE IN MEAN BASKET VALUE
# ============================================================

mean_complete <- basket %>%
  filter(
    has_missing == "Complete"
  ) %>%
  summarise(
    mean_value = mean(
      basket_value_vnd,
      na.rm = TRUE
    )
  ) %>%
  pull(mean_value)


mean_missing <- basket %>%
  filter(
    has_missing == "Has Missing Data"
  ) %>%
  summarise(
    mean_value = mean(
      basket_value_vnd,
      na.rm = TRUE
    )
  ) %>%
  pull(mean_value)


mean_difference <- mean_missing - mean_complete

percentage_difference <- (
  mean_missing - mean_complete
) / mean_complete * 100


mean_difference

percentage_difference


# ============================================================
# 5.5.5 CATEGORICAL DISTRIBUTION COMPARISON
# ============================================================

categorical_missing_vars <- c(
  "customer_segment",
  "service_type",
  "city",
  "payment_method",
  "booking_channel",
  "promo_code_used",
  "traffic_level",
  "weather_condition"
)


categorical_missing_summary <- basket %>%
  select(
    has_missing,
    all_of(categorical_missing_vars)
  ) %>%
  pivot_longer(
    cols = all_of(categorical_missing_vars),
    names_to = "variable",
    values_to = "category"
  ) %>%
  group_by(
    variable,
    has_missing,
    category
  ) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(
    variable,
    has_missing
  ) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()


categorical_missing_summary

# ============================================================
# 5.5.5b CATEGORICAL DISTRIBUTION DIFFERENCES
# ============================================================

categorical_difference <- categorical_missing_summary %>%
  select(
    variable,
    has_missing,
    category,
    percentage
  ) %>%
  pivot_wider(
    names_from = has_missing,
    values_from = percentage,
    values_fill = 0
  ) %>%
  mutate(
    percentage_difference =
      `Has Missing Data` - Complete
  ) %>%
  arrange(
    desc(abs(percentage_difference))
  )

categorical_difference

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
# 11. EXPLORATORY DATA ANALYSIS
# ============================================================


# ============================================================
# 11.1 TARGET VARIABLE: BASKET VALUE
# ============================================================

# ------------------------------------------------------------
# Descriptive statistics
# ------------------------------------------------------------

basket_value_summary <- basket %>%
  summarise(
    
    n = sum(
      !is.na(basket_value_vnd)
    ),
    
    mean = mean(
      basket_value_vnd,
      na.rm = TRUE
    ),
    
    median = median(
      basket_value_vnd,
      na.rm = TRUE
    ),
    
    sd = sd(
      basket_value_vnd,
      na.rm = TRUE
    ),
    
    min = min(
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
    
    max = max(
      basket_value_vnd,
      na.rm = TRUE
    )
  )


basket_value_summary


# ============================================================
# 11.2 BASKET VALUE SKEWNESS
# ============================================================

basket_value_skewness <- skewness(
  basket$basket_value_vnd,
  na.rm = TRUE
)

basket_value_skewness


# ============================================================
# 11.3 BASKET VALUE OUTLIER ASSESSMENT
# ============================================================

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

lower_basket <- Q1_basket -
  1.5 * IQR_basket

upper_basket <- Q3_basket +
  1.5 * IQR_basket


# Display IQR boundaries

Q1_basket
Q3_basket
IQR_basket
lower_basket
upper_basket


# Count potential outliers

basket_outlier_summary <- basket %>%
  summarise(
    
    lower_outliers = sum(
      basket_value_vnd < lower_basket,
      na.rm = TRUE
    ),
    
    upper_outliers = sum(
      basket_value_vnd > upper_basket,
      na.rm = TRUE
    ),
    
    total_outliers = sum(
      basket_value_vnd < lower_basket |
        basket_value_vnd > upper_basket,
      na.rm = TRUE
    )
  )


basket_outlier_summary


# ============================================================
# 11.4 BASKET VALUE DISTRIBUTION
# ============================================================

basket_histogram <- ggplot(
  basket,
  aes(
    x = basket_value_vnd
  )
) +
  
  geom_histogram(
    bins = 30,
    na.rm = TRUE
  ) +
  
  labs(
    title = "Distribution of Basket Value",
    subtitle = "Basket value across GrabFood and GrabMart transactions",
    x = "Basket Value (VND)",
    y = "Number of Orders"
  ) +
  
  scale_x_continuous(
    labels = scales::label_number(
      big.mark = ","
    )
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      family = "sans"
    ),
    
    plot.subtitle = element_text(
      size = 11,
      family = "sans"
    ),
    
    axis.title.x = element_text(
      size = 11,
      family = "sans"
    ),
    
    axis.title.y = element_text(
      size = 11,
      family = "sans"
    ),
    
    axis.text.x = element_text(
      size = 10,
      family = "sans"
    ),
    
    axis.text.y = element_text(
      size = 10,
      family = "sans"
    ),
    
    panel.grid.minor = element_blank(),
    
    plot.margin = ggplot2::margin(
      t = 10,
      r = 15,
      b = 15,
      l = 15
    )
  )


basket_histogram


# ------------------------------------------------------------
# Save histogram
# ------------------------------------------------------------

ggsave(
  "figures/01_basket_value_histogram.png",
  basket_histogram,
  width = 10,
  height = 7,
  dpi = 300
)


# ============================================================
# 11.5 NUMERICAL PREDICTOR SUMMARY
# ============================================================

numerical_eda_vars <- c(
  "customer_age",
  "distance_km",
  "estimated_duration_min",
  "discount_amount_vnd"
)


numerical_summary <- basket %>%
  summarise(
    across(
      all_of(numerical_eda_vars),
      list(
        
        mean = ~ mean(
          .x,
          na.rm = TRUE
        ),
        
        median = ~ median(
          .x,
          na.rm = TRUE
        ),
        
        sd = ~ sd(
          .x,
          na.rm = TRUE
        ),
        
        min = ~ min(
          .x,
          na.rm = TRUE
        ),
        
        max = ~ max(
          .x,
          na.rm = TRUE
        )
      ),
      .names = "{.col}_{.fn}"
    )
  )


numerical_summary


# ============================================================
# 11.6 CATEGORICAL PREDICTOR DISTRIBUTIONS
# ============================================================



categorical_eda_vars <- c(
  "customer_segment",
  "service_type",
  "city",
  "payment_method",
  "booking_channel",
  "promo_code_used",
  "traffic_level",
  "weather_condition"
)


categorical_summary <- basket %>%
  select(
    all_of(categorical_eda_vars)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "category"
  ) %>%
  group_by(
    variable,
    category
  ) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(
    variable
  ) %>%
  mutate(
    percentage =
      n / sum(n) * 100
  ) %>%
  ungroup()


categorical_summary


# ============================================================
# 11.7 NUMERICAL PREDICTOR RELATIONSHIPS
# ============================================================


numerical_relationships <- basket %>%
  summarise(
    
    age_correlation = cor(
      customer_age,
      basket_value_vnd,
      use = "complete.obs"
    ),
    
    distance_correlation = cor(
      distance_km,
      basket_value_vnd,
      use = "complete.obs"
    ),
    
    duration_correlation = cor(
      estimated_duration_min,
      basket_value_vnd,
      use = "complete.obs"
    ),
    
    discount_correlation = cor(
      discount_amount_vnd,
      basket_value_vnd,
      use = "complete.obs"
    )
  )


numerical_relationships


# ============================================================
# 11.8 BASKET VALUE BY CATEGORICAL PREDICTORS
# ============================================================

categorical_basket_summary <- basket %>%
  select(
    all_of(categorical_eda_vars),
    basket_value_vnd
  ) %>%
  pivot_longer(
    cols = all_of(categorical_eda_vars),
    names_to = "variable",
    values_to = "category"
  ) %>%
  group_by(
    variable,
    category
  ) %>%
  summarise(
    
    n = n(),
    
    mean_basket_value =
      mean(
        basket_value_vnd,
        na.rm = TRUE
      ),
    
    median_basket_value =
      median(
        basket_value_vnd,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  arrange(
    variable,
    desc(mean_basket_value)
  )


categorical_basket_summary


# ============================================================
# 11.9 MISSINGNESS CHECK BEFORE MODELLING
# ============================================================


model_predictors_current <- c(
  
  # MLR 1
  "customer_segment",
  "service_type",
  "customer_age",
  
  # MLR 2
  "city",
  "payment_method",
  "booking_channel",
  "distance_km",
  "estimated_duration_min",
  
  # MLR 4
  "promo_code_used",
  "discount_amount_vnd",
  
  # MLR 5
  "traffic_level",
  "weather_condition"
)


model_missing_summary <- basket %>%
  summarise(
    across(
      all_of(model_predictors_current),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing"
  ) %>%
  mutate(
    percentage =
      missing / nrow(basket) * 100
  ) %>%
  arrange(
    desc(missing)
  )


model_missing_summary


# ============================================================
# 11.10 FINAL EDA CHECK
# ============================================================

# Number of observations

nrow(basket)


# Number of unique bookings

n_distinct(
  basket$booking_id
)


# Service type distribution

table(
  basket$service_type,
  useNA = "ifany"
)


# Basket value summary

basket_value_summary


# Basket value skewness

basket_value_skewness


# Basket value outlier summary

basket_outlier_summary

# ============================================================
# 12. FEATURE ENGINEERING AND MODELLING PREPARATION
# ============================================================


# ============================================================
# 12.1 CREATE TIME VARIABLES
# ============================================================

# Convert booking_datetime to POSIXct if necessary

basket <- basket %>%
  mutate(
    booking_datetime = as.POSIXct(
      booking_datetime
    )
  )


# ------------------------------------------------------------
# Create booking hour
# ------------------------------------------------------------

basket <- basket %>%
  mutate(
    booking_hour = hour(
      booking_datetime
    )
  )


# ------------------------------------------------------------
# Create weekend indicator
# ------------------------------------------------------------

basket <- basket %>%
  mutate(
    is_weekend = ifelse(
      weekdays(booking_datetime) %in%
        c("Saturday", "Sunday"),
      "Weekend",
      "Weekday"
    )
  )


# ------------------------------------------------------------
# Create time period
# ------------------------------------------------------------

basket <- basket %>%
  mutate(
    time_period = case_when(
      
      booking_hour >= 0 &
        booking_hour < 6 ~
        "Early Morning",
      
      booking_hour >= 6 &
        booking_hour < 11 ~
        "Morning",
      
      booking_hour >= 11 &
        booking_hour < 14 ~
        "Lunch",
      
      booking_hour >= 14 &
        booking_hour < 17 ~
        "Afternoon",
      
      booking_hour >= 17 &
        booking_hour < 21 ~
        "Evening",
      
      booking_hour >= 21 &
        booking_hour <= 23 ~
        "Night"
    )
  )


# Check feature engineering

table(
  basket$time_period,
  useNA = "ifany"
)

table(
  basket$is_weekend,
  useNA = "ifany"
)


# ============================================================
# 12.2 CHECK DISTANCE / DURATION PLAUSIBILITY
# ============================================================

# Estimated speed in km/h

basket <- basket %>%
  mutate(
    estimated_speed_kmh =
      distance_km /
      estimated_duration_min *
      60
  )


# Examine potentially implausible observations

basket %>%
  filter(
    estimated_speed_kmh > 50
  ) %>%
  select(
    booking_id,
    distance_km,
    estimated_duration_min,
    estimated_speed_kmh
  )


# ============================================================
# 12.3 CREATE CLEAN DISTANCE VARIABLE
# ============================================================

basket <- basket %>%
  mutate(
    distance_km_clean = ifelse(
      estimated_speed_kmh > 50,
      NA,
      distance_km
    )
  )


# Check the effect of cleaning

distance_cleaning_summary <- basket %>%
  summarise(
    
    original_missing =
      sum(
        is.na(distance_km)
      ),
    
    cleaned_missing =
      sum(
        is.na(distance_km_clean)
      ),
    
    newly_flagged =
      sum(
        is.na(distance_km_clean) &
          !is.na(distance_km)
      )
  )


distance_cleaning_summary




# ============================================================
# 12.4 CONVERT MODELLING VARIABLES TO FACTORS
# ============================================================

basket <- basket %>%
  mutate(
    
    customer_segment =
      factor(customer_segment),
    
    service_type =
      factor(service_type),
    
    city =
      factor(city),
    
    payment_method =
      factor(payment_method),
    
    booking_channel =
      factor(booking_channel),
    
    time_period =
      factor(time_period),
    
    is_weekend =
      factor(is_weekend),
    
    promo_code_used =
      factor(promo_code_used),
    
    traffic_level =
      factor(traffic_level),
    
    weather_condition =
      factor(weather_condition)
  )


# Check structure

str(
  basket[
    c(
      "customer_segment",
      "service_type",
      "customer_age",
      "city",
      "payment_method",
      "booking_channel",
      "distance_km_clean",
      "estimated_duration_min",
      "time_period",
      "is_weekend",
      "promo_code_used",
      "discount_amount_vnd",
      "traffic_level",
      "weather_condition",
      "basket_value_vnd"
    )
  ]
)

# ============================================================
# 13. FINAL MODELLING DATASET
# ============================================================


model_vars <- c(
  

  "customer_segment",
  "service_type",
  "customer_age",

  "city",
  "payment_method",
  "booking_channel",
  "distance_km_clean",
  "estimated_duration_min",
  

  "time_period",
  "is_weekend",
  
 
  "promo_code_used",
  "discount_amount_vnd",
  
  
  "traffic_level",
  "weather_condition",
  

  "basket_value_vnd"
)


# ------------------------------------------------------------
# Create modelling dataset
# ------------------------------------------------------------

model_data <- basket %>%
  select(
    booking_id,
    all_of(model_vars)
  )


# ------------------------------------------------------------
# Check dimensions
# ------------------------------------------------------------

dim(model_data)


# ------------------------------------------------------------
# Check structure
# ------------------------------------------------------------

str(model_data)


# ------------------------------------------------------------
# Check missing values
# ------------------------------------------------------------

model_missing <- model_data %>%
  summarise(
    across(
      all_of(model_vars),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing"
  ) %>%
  mutate(
    percentage =
      missing / nrow(model_data) * 100
  ) %>%
  arrange(
    desc(missing)
  )


model_missing

# ============================================================
# 15.1 CHECK PROMOTION MISSINGNESS
# ============================================================

table(
  model_data$promo_code_used,
  useNA = "ifany"
)
# ------------------------------------------------------------
# Check relationship between promo code and discount
# ------------------------------------------------------------

table(
  model_data$promo_code_used,
  is.na(model_data$discount_amount_vnd),
  useNA = "ifany"
)

# ============================================================
# 15.2 TRAIN / TEST SPLIT
# ============================================================

set.seed(123)

train_index <- createDataPartition(
  model_data$basket_value_vnd,
  p = 0.80,
  list = FALSE
)

train <- model_data[
  train_index,
]

test <- model_data[
  -train_index,
]


# Check dataset sizes

nrow(train)

nrow(test)


# Check proportions

nrow(train) / nrow(model_data)

nrow(test) / nrow(model_data)

# ============================================================
# 15.3 VARIABLE-SPECIFIC MISSING VALUE IMPUTATION
# ============================================================

# ------------------------------------------------------------
# Numerical variables
# ------------------------------------------------------------


numeric_impute_vars <- c(
  "customer_age",
  "distance_km_clean"
)


# Calculate training-set medians

train_medians <- sapply(
  train[numeric_impute_vars],
  median,
  na.rm = TRUE
)


train_medians


# Apply training medians to TRAIN

for (var in numeric_impute_vars) {
  
  train[[var]][
    is.na(train[[var]])
  ] <- train_medians[var]
}


# Apply the SAME training medians to TEST

for (var in numeric_impute_vars) {
  
  test[[var]][
    is.na(test[[var]])
  ] <- train_medians[var]
}


# ------------------------------------------------------------
# Categorical variables
# ------------------------------------------------------------
# Missing values are retained as an explicit "Unknown"
# category rather than being replaced by the most common
# category.
# ------------------------------------------------------------

categorical_impute_vars <- c(
  "payment_method",
  "traffic_level",
  "weather_condition",
  "promo_code_used"
)


for (var in categorical_impute_vars) {
  
  # Convert to character before adding "Unknown"
  
  train[[var]] <- as.character(
    train[[var]]
  )
  
  test[[var]] <- as.character(
    test[[var]]
  )
  
  
  # Replace missing values
  
  train[[var]][
    is.na(train[[var]])
  ] <- "Unknown"
  
  test[[var]][
    is.na(test[[var]])
  ] <- "Unknown"
}


# ------------------------------------------------------------
# Convert categorical variables back to factors
# ------------------------------------------------------------

for (var in categorical_impute_vars) {
  
  # Combine levels from train and test so that any legitimate
  # test-set category is not accidentally lost.
  
  factor_levels <- union(
    unique(train[[var]]),
    unique(test[[var]])
  )
  
  
  train[[var]] <- factor(
    train[[var]],
    levels = factor_levels
  )
  
  test[[var]] <- factor(
    test[[var]],
    levels = factor_levels
  )
}


# ============================================================
# 15.4 VERIFY IMPUTATION
# ============================================================

# ------------------------------------------------------------
# Check remaining missing values in TRAIN
# ------------------------------------------------------------

train_missing_after <- train %>%
  summarise(
    across(
      all_of(model_vars),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing"
  ) %>%
  arrange(
    desc(missing)
  )


train_missing_after


# ------------------------------------------------------------
# Check remaining missing values in TEST
# ------------------------------------------------------------

test_missing_after <- test %>%
  summarise(
    across(
      all_of(model_vars),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing"
  ) %>%
  arrange(
    desc(missing)
  )


test_missing_after


# ============================================================
# 15.5 CHECK IMPUTED VALUES
# ============================================================

train_medians

# ============================================================
# 15.6.1 NUMERICAL IMPUTATION COMPARISON
# ============================================================

# ------------------------------------------------------------
# Combine final TRAIN and TEST data
# ------------------------------------------------------------

final_model_data <- bind_rows(
  train,
  test
)


# ------------------------------------------------------------
# Calculate BEFORE-imputation means
# ------------------------------------------------------------

before_numeric <- model_data %>%
  summarise(
    
    customer_age = mean(
      customer_age,
      na.rm = TRUE
    ),
    
    distance_km_clean = mean(
      distance_km_clean,
      na.rm = TRUE
    )
    
  ) %>%
  
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "mean_value"
  ) %>%
  
  mutate(
    stage = "Before imputation"
  )


# ------------------------------------------------------------
# Calculate AFTER-imputation means
# ------------------------------------------------------------

after_numeric <- final_model_data %>%
  summarise(
    
    customer_age = mean(
      customer_age,
      na.rm = TRUE
    ),
    
    distance_km_clean = mean(
      distance_km_clean,
      na.rm = TRUE
    )
    
  ) %>%
  
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "mean_value"
  ) %>%
  
  mutate(
    stage = "After imputation"
  )


# ------------------------------------------------------------
# Combine BEFORE and AFTER
# ------------------------------------------------------------

numeric_imputation_comparison <- bind_rows(
  before_numeric,
  after_numeric
) %>%
  
  mutate(
    variable = case_when(
      
      variable == "customer_age" ~
        "Customer age",
      
      variable == "distance_km_clean" ~
        "Distance (km)",
      
      TRUE ~ variable
    )
  )


# ------------------------------------------------------------
# Display comparison
# ------------------------------------------------------------

numeric_imputation_comparison




# ============================================================
# 16. CHECK FINAL TRAIN / TEST DATA
# ============================================================

dim(train)
dim(test)

summary(train$basket_value_vnd)
summary(test$basket_value_vnd)

str(train)

# ============================================================
# 16.1 EXPORT FINAL CLEANED DATASETS
# ============================================================

# ------------------------------------------------------------
# Combine TRAIN and TEST
# ------------------------------------------------------------

final_cleaned_data <- bind_rows(
  train,
  test
)


# ------------------------------------------------------------
# Check dimensions
# ------------------------------------------------------------

dim(train)
dim(test)
dim(final_cleaned_data)


# ------------------------------------------------------------
# Create output folder
# ------------------------------------------------------------

dir.create(
  "data/processed",
  showWarnings = FALSE,
  recursive = TRUE
)


# ------------------------------------------------------------
# Export TRAIN dataset
# ------------------------------------------------------------

write.csv(
  train,
  "data/processed/train_cleaned.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# Export TEST dataset
# ------------------------------------------------------------

write.csv(
  test,
  "data/processed/test_cleaned.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# Export FULL dataset
# ------------------------------------------------------------

write.csv(
  final_cleaned_data,
  "data/processed/full_cleaned_dataset_DONT_USE.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# Confirm files were exported
# ------------------------------------------------------------

file.exists(
  "data/processed/train_cleaned.csv"
)

file.exists(
  "data/processed/test_cleaned.csv"
)

file.exists(
  "data/processed/full_cleaned_dataset.csv"
)


# ============================================================
# GRAB VIETNAM ANALYTICS
# Visualizations 
# ============================================================

# ------------------------------------------------------------
# Figure 1: Distribution of Basket Value
# ------------------------------------------------------------
basket_threshold <- quantile(
  basket$basket_value_vnd,
  0.80,
  na.rm = TRUE
)

basket_threshold

p1 <- ggplot(
  basket,
  aes(x = basket_value_vnd)
) +
  
  geom_histogram(
    aes(
      fill = after_stat(
        ifelse(
          x >= basket_threshold,
          "High-value (Top 20%)",
          "Below 80th Percentile"
        )
      )
    ),
    bins = 30,
    color = "white",
    linewidth = 0.2
  ) +
  
  geom_vline(
    xintercept = basket_threshold,
    linetype = "dashed",
    linewidth = 1
  ) +
  
  annotate(
    "label",
    x = basket_threshold + 15000,
    y = Inf,
    label = paste0(
      "80th percentile\n",
      format(
        basket_threshold,
        big.mark = ",",
        scientific = FALSE
      ),
      " VND"
    ),
    vjust = 1.5,
    hjust = 0,
    size = 4
  ) +
  
  labs(
    title = "Distribution of Basket Value",
    subtitle = paste0(
      "Transactions above ",
      format(
        basket_threshold,
        big.mark = ",",
        scientific = FALSE
      ),
      " VND represent the top 20% of orders"
    ),
    x = "Basket Value (VND)",
    y = "Number of Orders",
    fill = "Transaction Group"
  ) +
  
  scale_x_continuous(
    labels = scales::label_number(
      big.mark = ","
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Below 80th Percentile" = "grey60",
      "High-value (Top 20%)" = "#2A9D8F"
    )
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    plot.subtitle = element_text(
      size = 12
    ),
    axis.title = element_text(
      face = "bold"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

p1


ggsave(
  "figures/01_basket_value_distribution.png",
  p1,
  width = 10,
  height = 7
)

