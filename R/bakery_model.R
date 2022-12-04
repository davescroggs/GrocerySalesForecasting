## Bakery items model

library(tidyverse)
library(lubridate)
library(here)
library(tidymodels)
library(xgboost)
library(ranger)


# Read in data ------------------------------------------------------------

# Select on the bread/bakery family
items <- read_csv(here("data/items.csv")) %>% 
  filter(family == "BREAD/BAKERY")
stores <- read_csv(here("data/stores.csv"))
holidays_events <- read_csv(here("data/holidays_events.csv"))

full_data <- read_csv(here("data/train.csv"))

# Create test/train split
groceries_split <- 
  full_data %>%  
  # Join item information - family, class, perishable
  inner_join(items, by = "item_nbr") %>% 
  # Join store information - city, state, type, cluster
  left_join(stores, by = "store_nbr") %>% 
  # Split with items as strata
  initial_split(prop = 0.9, strata = item_nbr)

train <- training(groceries_split)
test <- testing(groceries_split)

sales_cv <- rsample::vfold_cv(train, v = 5, strata = item_nbr)


## Pre-processing & feature selection --------------------------------------

prep_juice <- function(x) juice(prep(x))

# Feature engineering - trial multiple recipes ----------------------------

sales_recipe_full <- recipe(unit_sales ~ item_nbr + store_nbr + date + type + cluster + 
                              state + id,
                            data = train) %>% 
  update_role(id, new_role = "id") %>% 
  step_date(date, keep_original_cols = F) %>% 
  step_mutate(date_year = as.factor(date_year),
              # Force negative values to zero
              unit_sales = if_else(unit_sales < 0,0,unit_sales)) %>% 
  step_dummy(all_nominal_predictors()) 

sales_recipe_full %>% 
  prep_juice() %>% 
  filter(unit_sales == 0)


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

collect_notes(xgb_resampled)
# rmsle = 0.693

xgb_resampled %>% 
  select(.predictions) %>% 
  unnest(.predictions) %>% 
  left_join(train %>% transmute(.row = 1:n(), item_nbr)) %>% 
  left_join(items %>% 
              mutate(weight = perishable * 1.5)) %>% 
  summarise(nwrmsle = nwrmsle_vec(unit_sales, .pred, weight))

## nwrmsle = 0.890 - Same as rmsle because of the same weight, without item_nbr


# Hyperparameter tuning ---------------------------------------------------

# XGBoost workflow
xgb_wf_tune <- workflow() %>% 
  add_model(boost_tree(mode = "regression",
                       trees = 1000, 
                       tree_depth = tune(),
                       min_n = tune(), 
                       mtry = tune(),
                       learn_rate = 0.02)) %>%  
  add_recipe(sales_recipe_full)

xgb_grid <- 
  grid_regular(
    tree_depth(range = c(1, 15)),
    min_n(range = c(2, 40)), 
    mtry(range = c(1, 13)),
  levels = 3
)


cv_small <- train %>% 
  group_by(item_nbr,wday = wday(date),store_nbr) %>% 
  sample_frac(0.005) %>% 
  ungroup() %>% 
  vfold_cv(v = 4, strata = item_nbr)

library(finetune)
set.seed(345)

# Use rmse as metric so parallel processing is possible
doParallel::registerDoParallel(core = 4)


xgb_rs <- tune_race_anova(
  xgb_wf_tune,
  resamples = cv_small,
  grid = xgb_grid,
  metrics = metric_set(rmse),
  control = control_race(verbose = TRUE,verbose_elim = TRUE)
)

xgb_rs %>% 
  plot_race()


# Best result -------------------------------------------------------------

xgb_rs %>% 
  show_best()

# A tibble: 4 × 9
# mtry min_n tree_depth .metric .estimator  mean     n std_err .config              
# <int> <int>      <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
#   1    13     2          8 rmse    standard    8.67     4   0.177 Preprocessor1_Model20
# 2    13    21         15 rmse    standard    8.69     4   0.169 Preprocessor1_Model24
# 3    13    21          8 rmse    standard    8.80     4   0.202 Preprocessor1_Model23
# 4    13    40         15 rmse    standard    8.82     4   0.171 Preprocessor1_Model27

# Finalise model ----------------------------------------------------------

# Revert to single core to run on rmsle
doParallel::registerDoParallel(core = 1)

xgb_last <- xgb_wf %>%
  finalize_workflow(select_best(xgb_rs, "rmse")) %>%
  last_fit(groceries_split,
           metrics = metric_set(rmsle),
           control = grid_control)

xgb_last %>% 
  collect_metrics() 
  

# Residuals ---------------------------------------------------------------

# Most of the predictions are underestimating the true result
# It looks like there's lot

xgb_last %>%
  select(.predictions) %>% 
  unnest(.predictions) %>% 
  mutate(residual = abs(.pred - unit_sales) %>% percent_rank()) %>% 
  filter(residual > 0.999) %>% 
  left_join(train %>% transmute(.row = 1:n(), date)) %>% 
  left_join(holidays_events, by = "date") %>% 
  ggplot(aes(unit_sales, .pred, col = type)) +
  geom_abline(color = "gray80", size = 1) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Truth",
    y = "Predicted unit sales",
    color = NULL
  )

