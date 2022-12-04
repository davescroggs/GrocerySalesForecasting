## Metric sets

# Create two metric sets
# 1 - root mean square logarithmic error for fitting models
# 2 - weighted root mean square logarithmic error for predicting kaggle score

# Resources
## Create a custom metric
# https://yardstick.tidymodels.org/articles/custom-metrics.html#numeric-example-mean-squared-error

# Why parallel computing won't work
# https://github.com/tidymodels/yardstick/issues/195

library(rlang)

rmsle_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  rmsle_impl <- function(truth, estimate) {
    estimate <- if_else(estimate < 0, 0, estimate)
    sqrt(mean((log(truth + 1) - log(estimate + 1))^2))
  }
  
  metric_vec_template(
    metric_impl = rmsle_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

rmsle <- function(data, ...) {
  UseMethod("rmsle")
}
rmsle <- new_numeric_metric(rmsle, direction = "minimize")

rmsle.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  metric_summarizer(
    metric_nm = "rmsle",
    metric_fn = rmsle_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ...
  )
}

nwrmsle_vec <- function(truth, estimate, weight, na_rm = TRUE, ...) {
  estimate <- if_else(estimate < 0, 0, estimate)
  res <- sqrt(sum(weight *(log(truth + 1) - log(estimate + 1))^2)/sum(weight))
  return(res)
}

## Example 
# tibble(
#   truth = runif(10),
#   estimate = runif(10),
#   weight = rep(c(0.5, 1.5), each = 5)
# ) %$%
#   wrmsle_vec(truth, estimate, weight)
