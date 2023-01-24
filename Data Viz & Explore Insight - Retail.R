#' Load packages -----------------------------------------------------
library(tidyverse)
library(scales)

#' Import Data -------------------------------------------------------
retail <- read_csv("data/response_retail_v2.csv")

retail |> 
  glimpse()

#' Convert Varable Type ----------------------------------------------
retail_transform <- retail |> 
  mutate(
    # Character to factor
    gender = factor(gender), 
    marital_status = factor(marital_status), 
    payment_channel = factor(payment_channel), 
    # Double to integer
    visit_last_1mo = as.integer(visit_last_1mo), 
    visit_last_2mo = as.integer(visit_last_2mo), 
    visit_last_3mo = as.integer(visit_last_3mo), 
    # Binary numeric to factor (# only for exploration purpose)
    groceries = factor(groceries, levels = c(0, 1), 
                       labels = c("No", "Yes")), 
    toiletries = factor(toiletries, levels = c(0, 1), 
                        labels = c("No", "Yes")), 
    food = factor(food, levels = c(0, 1), 
                  labels = c("No", "Yes")), 
    electronic = factor(electronic, levels = c(0, 1), 
                        labels = c("No", "Yes")), 
    clothes = factor(clothes, levels = c(0, 1), 
                     labels = c("No", "Yes")), 
    home_appliances = factor(home_appliances, levels = c(0, 1), 
                             labels = c("No", "Yes")), 
    others = factor(others, levels = c(0, 1), 
                    labels = c("No", "Yes")), 
    # Target Variable
    response = factor(response, levels = c(0, 1), 
                      labels = c("No", "Yes"))
  )

# "Compact" way  ----------------------------------------------------

retail_transform <- retail |> 
  mutate(across(.cols = c(gender, marital_status, payment_channel), 
                .fns = factor)) |> 
  mutate(across(.cols = starts_with("visit"), 
                .fns = as.integer)) |> 
  mutate(across(.cols = c(groceries:others, response), 
                .fns = ~factor(.x, levels = c(0, 1), 
                               labels = c("No", "Yes")))
         ) 

skimr::skim(retail_transform)
  
#' Create New Derived Variables --------------------------------------
retail_transform <- retail_transform |> 
  rowwise() |> # for row-wise calculation
  mutate(
    total_visit = sum(c_across(starts_with("visit"))), 
    month_visit_count = sum(c_across(starts_with("visit")) > 0), 
    total_spending = sum(c_across(starts_with("spending"))), 
    month_spending_count = sum(c_across(starts_with("spending")) > 0), 
    monthly_visit = case_when(total_visit == 0 ~ 0, 
                              TRUE ~ round(total_visit/month_visit_count, 2)),
    monthly_spending = case_when(
      total_spending == 0 ~ 0, 
      TRUE ~ round(total_spending/month_spending_count, 2)
      ), 
    avg_spending_per_visit = case_when(
      total_visit == 0 ~ 0, 
      TRUE ~ round(total_spending/total_visit, 2)
      ), 
    .after = spending_last_3mo
    ) |> 
  ungroup()

retail_transform |> 
  glimpse()

# Data Exploration ---------------------------------------------------

skimr::skim(retail_transform)

# Response Distribution ----------------------------------------------

retail_transform |> 
  count(response) |> 
  mutate(pct = n/sum(n))

library(scales)

retail_transform |> 
  count(response) |> 
  mutate(pct = n/sum(n)) |> 
  ggplot(aes(x = response, y = n)) + 
  geom_col(fill = "skyblue") + 
  geom_text(aes(label = percent(pct)), vjust = -0.5) + 
  scale_y_continuous(labels = number_format(big.mark = ","), 
                     limits = c(0, 3000)) + 
  labs(y = "Count") + 
  theme_light()


# Gender Distribution ------------------------------------------------

retail_transform |> 
  count(gender)

retail_transform |> 
  count(gender) |> 
  mutate(pct = n/sum(n))

retail_transform |> 
  count(gender) |> 
  mutate(pct = n/sum(n)) |> 
  ggplot(aes(x = gender, y = n)) + 
  geom_col(fill = "skyblue") + 
  geom_text(aes(label = percent(pct, accuracy = 0.01)), vjust = -0.25) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  labs(y = "Count") + 
  theme_light()

# Marital Status Distribution ----------------------------------------

retail_transform |> 
  count(marital_status) |> 
  mutate(pct = n/sum(n))

retail_transform |> 
  count(marital_status) |> 
  mutate(pct = n/sum(n)) |> 
  ggplot(aes(x = marital_status, y = n)) + 
  geom_col(fill = "skyblue") + 
  geom_text(aes(label = percent(pct, accuracy = 0.01)), vjust = -0.25) + 
  labs(y = "Count") + 
  theme_light()

# Payment Channel Distribution ---------------------------------------

retail_transform |> 
  count(payment_channel) |> 
  mutate(pct = n/sum(n))

retail_transform |> 
  count(payment_channel) |> 
  mutate(pct = n/sum(n)) |> 
  ggplot(aes(x = payment_channel, y = pct)) + 
  geom_col()

# Home Appliances Distribution ---------------------------------------

retail_transform |> 
  count(home_appliances) |> 
  mutate(pct = n/sum(n))

retail_transform |> 
  count(home_appliances) |> 
  mutate(pct = n/sum(n)) |> 
  ggplot(aes(x = home_appliances, y = n)) + 
  geom_col(fill = "skyblue") + 
  geom_text(aes(label = percent(pct, accuracy = 0.01)), 
            vjust = -0.25) + 
  labs(y = "Count") + 
  theme_light() + 
  theme(panel.grid = element_blank())

# Categorical By Response --------------------------------------------

retail_transform |> 
  ggplot(aes(x = gender, fill = response)) + 
  geom_bar(position = position_fill()) + 
  scale_y_continuous(labels = percent_format())

#' Fisher's Exact Test -----------------------------------------------
# Hypothesis:
# H0: There are no significant association
# H1: There are significant association
# Inference: Reject H0 if p-value < alpha

retail_transform |> 
  select(gender, response) |> 
  table() |> 
  fisher.test(conf.int = FALSE)

fisher.test(x = retail_transform[["gender"]], 
            y = retail_transform[["response"]]) |> 
  pluck("p.value")

source("Retail/fct_category_by_response.R")

category_by_response("gender")
category_by_response("marital_status")
category_by_response("payment_channel")
category_by_response("groceries")
category_by_response("toiletries")
category_by_response("food")
category_by_response("electronic")
category_by_response("clothes")
category_by_response("home_appliances")
category_by_response("others")

#' Numeric Variable by Response --------------------------------------

retail_transform |> 
  select(where(is.numeric)) |> 
  summary()

retail_transform |> 
  ggplot(aes(x = visit_last_1mo)) + 
  geom_bar() + 
  theme_light()

theme_set(theme_light())

retail_transform |> 
  ggplot(aes(x = visit_last_1mo)) + 
  geom_bar() + 
  scale_x_continuous(breaks = 0:10)

retail_transform |> 
  ggplot(aes(x = visit_last_1mo)) + 
  geom_boxplot() + 
  scale_x_continuous(breaks = 0:10)

library(gridExtra)

grid.arrange(
  retail_transform |> 
    ggplot(aes(x = visit_last_1mo)) + 
    geom_bar() + 
    theme(
      axis.text = element_blank(), 
      axis.title = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.grid.major.y = element_blank()
    ),
  retail_transform |> 
    ggplot(aes(x = visit_last_1mo)) + 
    geom_boxplot() + 
    scale_x_continuous(labels = number_format(big.mark = ",")) + 
    theme(
      axis.text.y = element_blank(), 
      axis.title.y = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.grid.major.y = element_blank()
    ),
  ncol = 1, 
  heights = c(0.7, 0.3)
)

source("Retail/fct_discrit_distribution.R")

discrit_distribution("visit_last_1mo")
discrit_distribution("visit_last_2mo")
discrit_distribution("visit_last_3mo")

source("Retail/fct_discrit_by_response.R")

discrit_by_response("visit_last_1mo")
discrit_by_response("visit_last_2mo")
discrit_by_response("visit_last_3mo")

source("Retail/fct_numeric_by_response.R")

#' Ideal significant variable distribution by target/response
data.frame(
  response = rep(c("Yes", "No"), each = 10000), 
  ideal = c(rnorm(10000, 0), rnorm(10000, 6))
) |> 
  numeric_by_response("ideal", .data = _)

#' Fair significant variable distribution by target/response
data.frame(
  response = rep(c("Yes", "No"), each = 1000), 
  fair = c(rnorm(1000, 0), rnorm(1000, 1))
) |> 
  numeric_by_response("fair", .data = _)

#' Not Ideal significant variable distribution by target/response
data.frame(
  response = rep(c("Yes", "No"), each = 1000), 
  not_ideal = c(rnorm(1000, 0), rnorm(1000, 0))
) |> 
  numeric_by_response("not_ideal", .data = _)

retail_transform |> 
  ggplot(aes(x = spending_last_1mo)) + 
  geom_density(fill = "skyblue") + 
  theme_light() + 
  theme(panel.grid = element_blank())
numeric_by_response("spending_last_1mo")

retail_transform |> 
  ggplot(aes(x = spending_last_2mo)) + 
  geom_density(fill = "skyblue") + 
  theme_light() + 
  theme(panel.grid = element_blank())
numeric_by_response("spending_last_2mo")

numeric_by_response("spending_last_3mo")
numeric_by_response("age")
numeric_by_response("monthly_income")
numeric_by_response("recency_last_visit")
numeric_by_response("monthly_visit")
numeric_by_response("total_visit")
numeric_by_response("monthly_spending")
numeric_by_response("avg_spending_per_visit")

# Correlation --------------------------------------------------------

library(ggcorrplot)

pmat <- retail_transform |> 
  select(where(is.numeric), -member_id) |> 
  cor_pmat(conf.level = 0.95)

retail_transform |> 
  select(where(is.numeric), -member_id) |> 
  cor() |>  
  ggcorrplot(type = "lower", method = "square", show.diag = TRUE, 
           p.mat = pmat, sig.level = 0.05, insig = "pch", 
           colors = c("red", "white", "red"), 
           hc.order = TRUE, ggtheme = theme_light)









