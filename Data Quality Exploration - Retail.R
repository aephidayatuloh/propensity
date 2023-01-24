library(tidyverse)
library(scales)

retail <- read_csv("data/response_retail_v2.csv")

retail |> 
  glimpse()

# Data EXploration ----------------------------------------------------

skimr::skim(retail)

# This should be Categorical ------------------------------------------

retail |> 
  count(response) 

retail |> 
  count(gender)

retail |> 
  count(marital_status)

retail |> 
  count(payment_channel)

# This should be Discrit/Integer --------------------------------------

retail |> 
  ggplot(aes(x = visit_last_1mo)) + 
  geom_bar() + 
  theme_light()

retail |> 
  ggplot(aes(x = visit_last_2mo)) + 
  geom_bar() + 
  theme_light()

retail |> 
  ggplot(aes(x = visit_last_3mo)) + 
  geom_bar() + 
  theme_light()
