## Bakery items model

library(tidyverse)
library(lubridate)
library(here)
library(tidymodels)
library(xgboost)
library(ranger)


# Read in data ------------------------------------------------------------

items <- read_csv(here("data/items.csv")) %>% 
  filter(family == "BREAD/BAKERY")
stores <- read_csv(here("data/stores.csv"))
holidays_events <- read_csv(here("data/holidays_events.csv"))

# Create test/train split
groceries_split <- 
  read_csv(here("data/train.csv")) %>%  
  # Join item information - family, class, perishable
  inner_join(items, by = "item_nbr") %>% 
  # Join store information - city, state, type, cluster
  left_join(stores, by = "store_nbr") %>% 
  initial_split(prop = 0.9, strata = store_nbr)

train <- training(groceries_split)
test <- testing(groceries_split)

sales_cv <- rsample::vfold_cv(train, v = 10, strata = store_nbr)


## Pre-processing & feature selection --------------------------------------

prep_juice <- function(x) juice(prep(x))

# Feature engineering - trial multiple recipes ----------------------------

sales_recipe_full <- recipe(unit_sales ~ store_nbr + date + type + cluster + 
                              state + id,
                            data = train) %>% 
  update_role(id, new_role = "id") %>% 
  step_date(date, keep_original_cols = F) %>% 
  step_mutate(date_year = as.factor(date_year),
              # Force negative values to zero
              unit_sales = if_else(unit_sales < 0,0,unit_sales)) %>% 
  step_dummy(all_nominal_predictors()) 

sales_recipe_full %>% 
  prep_juice()


## Model specification -----------------------------------------------------

# XGBoost workflow
xgb_wf <- workflow() %>% 
  add_model(boost_tree() %>% 
              set_mode("regression")) %>% 
  add_recipe(sales_recipe_full)

## Load custom metrics -----------------------------------------------------

# Two metric functions - rmsle and nwrmsle
source(here("R/metric_sets.R"))


## Fit resamples - first model XGBoost -------------------------------------

grid_control <- control_grid(save_pred = TRUE, verbose = TRUE)

mset <- metric_set(rmsle)

xgb_resampled <- xgb_wf %>%
  fit_resamples(resamples = sales_cv,
                metrics = metric_set(rmsle),
                control = grid_control)


collect_metrics(xgb_resampled)

# rmse = 0.385