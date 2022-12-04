library(tidyverse)
library(lubridate)
library(here)

# Read in data
train <- read_csv(here("data/train.csv")) %>%  
  filter(item_nbr == 502331)
test <- read_csv(here("data/test.csv")) %>% 
  filter(item_nbr == 502331)
items <- read_csv(here("data/items.csv")) %>% 
  filter(family == "BREAD/BAKERY")
stores <- read_csv(here("data/stores.csv"))
transactions <- read_csv(here("data/transactions.csv"))
oil <- read_csv(here("data/oil.csv"))
holidays <- read_csv(here("data/holidays_events.csv"))

# Train -------------------------------------------------------------------


# Date continuity and missing values --------------------------------------

train %>% 
  mutate(day = lubridate::yday(date)) %>% 
  group_by(year = year(date)) %>% 
  summarise(n = n(),
            min = min(day),
            max = max(day),
            distinct_days = n_distinct(day),
            NAs = sum(is.na(day)),
            missing_days = paste(setdiff(1:366,day), collapse = ", "))

# The 359th (360th on a leap year) day is Christmas day and there is no data for this day

train %>% 
  distinct(yday(date), .keep_all = T) %>% 
  filter(between(date,as.Date("2013-12-20"),as.Date("2013-12-31")))

# Time series -------------------------------------------------------------


train %>% 
  mutate(dom = ymd(paste(year(date),month(date),1, sep = "-"))) %>% 
  count(dom, store_nbr, wt = unit_sales) %>% 
  ggplot(aes(x = dom, y = n, col = store_nbr, group = store_nbr)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month") +
  theme(axis.text.x = element_text(angle = 90))

train %>% 
  mutate(store_nbr = factor(store_nbr),
         store_nbr = fct_reorder(store_nbr, unit_sales, median)) %>% 
  ggplot(aes(x = unit_sales, y = store_nbr)) +
  geom_boxplot()

train %>% 
  group_by(store_nbr, onpromotion) %>% 
  summarise(median_units = median(unit_sales)) %>%  
  mutate(store_nbr = factor(store_nbr),
         store_nbr = fct_reorder(store_nbr, median_units, mean)) %>% 
  ggplot(aes(x = median_units, y = store_nbr, col = onpromotion)) +
  geom_point()

## Autocorrelation

train %>% 
  filter(store_nbr == 5) %>% 
  ggplot(aes(x = date, y = unit_sales, col = wday(date,label = T))) +
  geom_point() +
  geom_line()

train %>% 
  mutate(dow = wday(date)) %>% 
  arrange(store_nbr,date) %>% 
  arrange(store_nbr,dow) %>% 
  group_by(store_nbr,dow) %>% 
  mutate(prev_week = lag(unit_sales))


# Promotions --------------------------------------------------------------

train %>% 
  left_join(stores, by = "store_nbr") %>% 
  group_by(store_nbr, type, onpromotion) %>% 
  summarise(median_units = median(unit_sales),
            .groups = "drop") %>% 
  mutate(onpromotion = case_when(
    onpromotion ~ "Promotoion",
    !onpromotion ~ "Standard",
    is.na(onpromotion) ~ "Missing",
    TRUE ~ "Err")) %>% 
  pivot_wider(names_from = onpromotion, values_from = median_units) %>% 
  pivot_longer(cols = c(Promotoion, Missing)) %>% 
  mutate(difference = value - Standard,
         store_nbr = fct_reorder(factor(store_nbr), difference, sum)) %>% 
  ggplot(aes(x = difference, y = store_nbr, col = name)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  facet_grid(type~., scales = "free_y")


# Store location ----------------------------------------------------------


train %>% 
  left_join(stores, by = "store_nbr") %>% 
  mutate(state = factor(state),
         state = fct_reorder(state, unit_sales, median)) %>% 
  ggplot(aes(x = unit_sales, y = state)) +
  geom_boxplot()


# Differencing ------------------------------------------------------------

train %>% 
  arrange(store_nbr, date) %>% 
  group_by(store_nbr) %>% 
  mutate(prev = lag(unit_sales))


# Dates - Year, month, day ------------------------------------------------

train %>% 
  arrange(store_nbr, date) %>% 
  mutate(dow = lubridate::wday(date, label = TRUE)) %>% 
  group_by(store_nbr, dow) %>% 
  summarise(unit_sales_total = sum(unit_sales),
            n = n(),
            .groups = "drop") %>% 
  mutate(store_nbr = factor(store_nbr),
         store_nbr = fct_reorder(store_nbr, unit_sales_total, sum)) %>% 
  ggplot(aes(x = unit_sales_total, y = store_nbr, col = dow)) + 
  geom_point() +
  ggrepel::geom_label_repel(data = ~group_by(., dow) %>% sample_n(size = 1),
                            aes(label = dow),
                            min.segment.length = 0,nudge_x = -5000)

train %>% 
  filter(store_nbr %in% sample(1:54,20)) %>% 
  arrange(store_nbr, date) %>% 
  mutate(dow = lubridate::wday(date, label = TRUE, week_start = 1),
         store_nbr = factor(store_nbr),
         store_nbr = fct_reorder(store_nbr, unit_sales, sum)) %>% 
  ggplot(aes(x = unit_sales, y = store_nbr, fill = dow)) + 
  geom_boxplot()
  


# Holidays ----------------------------------------------------------------


holidays %>% 
  skimr::skim()

holidays %>% 
  mutate(locale = if_else(locale == "Local", locale_name,locale)) %>% 
  ggplot(aes(x = date, y = locale,col = type)) +
  geom_point() +
  facet_grid(.~year(date),scales = "free_x")

holidays %>% 
  mutate(locale = if_else(locale == "Local", locale_name,locale)) %>% 
  ggplot(aes(x = yday(date), y = locale,col = factor(year(date)))) +
  geom_point()

holidays %>% 
  count(locale)
