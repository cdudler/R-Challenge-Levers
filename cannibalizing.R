library(data.table)
library(tidyverse)
library(lubridate)
library(tidymodels)
library(dplyr)
# Data loading
transacd  <- fread('/Users/xinyangdu/Group A4 Dropbox/Du Xinyang/Mac/Downloads/final_data/transactional_data.csv')
machined  <- fread('/Users/xinyangdu/Group A4 Dropbox/Du Xinyang/Mac/Downloads/final_data/machine_data.csv')

# Model data creation
daily_sales <- transacd %>% 
  group_by(machine) %>% 
  summarise(daily_sales = n() / uniqueN(date))

data <- left_join(x = daily_sales, y = machined,  by = "machine")

#Sort by sales and if there're nearby machines (ascending)
data <-data[order(data$daily_sales,data$num_vendex_nearby_300),]
# Feature engineering
data <- data %>% 
  mutate(
    log_transport = log10(total_number_of_routes_600),
    log_transport = replace_na(log_transport, mean(log_transport, na.rm = TRUE)),
    isna_train_AvgDailyPassengers = ifelse(is.na(train_AvgDailyPassengers), yes = 1, no = 0),
    no_income = ifelse(test = is.na(income_average), yes = 1, no = 0),
    income_average = replace_na(as.numeric(income_average), mean(as.numeric(income_average), na.rm = TRUE))
  )

# Compute cannibalizing with linear regression
# We will model sales taking into account location factors but EXCLUDING any machines around (num_vendex_nearby_300)
# variable to see how do we overestimate or understimate sales because of that. 
# The relative jump will be the cannibalization in sales

# Selection of variables to use in the model
model_lm_vars <- c('daily_sales', 'isna_train_AvgDailyPassengers',
                   'num_hotels_45', 'log_transport',
                   'income_average','small_machine')
model_lm_data <- data[, model_lm_vars]

#train-test split 7:3
set.seed(2022)
model_lm_split <- initial_split(data = model_lm_data,
                                prop = 0.7)
model_lm_train <- training(model_lm_split)
model_lm_test <- testing(model_lm_split)
#linear regression model
model_lm <- linear_reg() %>% set_engine('lm')
#fit the model using training data
model_lm_fit <- model_lm %>% 
  fit(daily_sales ~ ., data = model_lm_train)
model_lm_fit %>% extract_fit_engine() %>% summary()
tidy(model_lm_fit)
#predictions from training
model_lm_train_preds <- bind_cols(
  model_lm_train,
  predict(object = model_lm_fit, new_data = model_lm_train %>% select(-daily_sales))
)
#evaluate
rmse(model_lm_train_preds, truth = daily_sales, estimate = .pred)
#predictions from test set
model_lm_test_preds <- bind_cols(
  model_lm_test,
  predict(object = model_lm_fit, new_data = model_lm_test %>% select(-daily_sales))
)
#evaluate
rmse(model_lm_test_preds, truth = daily_sales, estimate = .pred)

ggplot(model_lm_test_preds, aes(x = daily_sales, y = .pred)) +
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) +
  labs(y = "Predicted", x = "Actual") +
  coord_obs_pred()

# Not the best model (remember we didn't include information of if ther're other machines around), but no overfitting
#(training 4.74 vs test 4.44)
#apply the model to the whole dataset
model_lm_delta <- bind_cols(
  data, 
  predict(object = model_lm_fit, new_data = model_lm_data %>% select(-daily_sales))
)

model_lm_delta %>% 
  group_by(num_vendex_nearby_300) %>% 
  summarise(ratio_sales_with_vs_without = mean(daily_sales / .pred)) %>% 
  ungroup() %>% 
  mutate(gap = ratio_sales_with_vs_without / shift(ratio_sales_with_vs_without, -1))
#-----------------------------------------------------------
#!!!gap when num_vendex_nearby_300=0 is 0.898<1
#That means if we move a machine to a place where other conditions remain the same but there're no other machines 
#it will decrease its sales. Probably because there's a strong correlation between the number of machines placed
#and for example the people flow, transportation, hotels nearby...


# Business case -----------------------------------------------------------
# We will calculate how much would each machine sell if they were moved the machine that don't have other machines around
#to places where there're other machines around

happy_machines <- data %>% 
  filter(num_vendex_nearby_300 != 0) %>% 
  mutate(sales_if_move = daily_sales * 0.898,
         decrease = daily_sales-sales_if_move)
lonely_machines <- data %>% 
  filter(num_vendex_nearby_300 == 0) %>% 
  mutate(sales_if_move = daily_sales * (1 / 0.898),
         increase = sales_if_move-daily_sales)

# Paring happy (with other machines around) and lonely machines (has no other machine around)
nrow(happy_machines)
nrow(lonely_machines)
#1291 vs 1204, need to get rid of 87 rows of data from happy_machines

a <- happy_machines %>% 
  # Ordering machines loosing the most
  arrange(decrease) %>% 
  select(machine, decrease) %>% 
  head(nrow(happy_machines))

b <- lonely_machines %>% 
  # Ordering machines winning
  arrange(desc(increase)) %>% 
  select(machine, increase)

#get rid of extra rows
a<-a[-c(1205:1291),]
cost_moving <- 500
c <- bind_cols(a, b) %>% 
  mutate(profit = increase - decrease,
         yearly_profit = profit * 365 * 0.9) %>% # Assuming 10% of the days inactive
  filter(yearly_profit > cost_moving)

sum(c$yearly_profit) / 1e3
