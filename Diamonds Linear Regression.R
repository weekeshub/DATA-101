library(tidymodels)
view(diamonds)

set.seed(1234)
diamonds_split <- initial_split(diamonds, prop = 0.60)
diamonds_train <- training(diamonds_split)
diamonds_test2 <- testing(diamonds_split)

dim(diamonds_train)
dim(diamonds_test2)



set.seed(845)
diamonds_val <- initial_split(diamonds_test2, prop = 0.50)
diamonds_validate <- training(diamonds_val)
diamonds_test <- testing(diamonds_val)

dim(diamonds_train)
dim(diamonds_validate)
dim(diamonds_test)


diamonds <- mutate(diamonds, carat2 = carat^2)

diamondstrain_rec <-
  recipe(price ~., data = diamonds_train) %>%
  step_log(all_outcomes()) %>%
  step_unorder(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_poly(carat, degree = 2)

diamondstest_rec <-
  recipe(price ~., data = diamonds_test) %>%
  step_log(all_outcomes()) %>%
  step_unorder(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_poly(carat, degree = 2)

prep(diamondstrain_rec)
prep(diamondstest_rec)

diamondstrain_juiced <- juice(prep(diamondstrain_rec))
dim(diamondstrain_juiced)
names(diamondstrain_juiced)

diamondstest_juiced <- juice(prep(diamondstest_rec))
dim(diamondstest_juiced)
names(diamondstest_juiced)

view(diamondstrain_juiced)
view(diamondstest_juiced)

lm_model <- 
  linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

lm_model

lm_fit <- fit(lm_model, price ~ carat_poly_1 + carat_poly_2, diamondstrain_juiced)
lm_fit

names(lm_fit)

summary(lm_fit$fit)

tidy(lm_fit)
glance(lm_fit)

predict(lm_fit, new_data = diamondstest_juiced)

diamonds_test_results <- predict(lm_fit, new_data = diamondstest_juiced) %>%
  bind_cols(diamondstest_juiced)

rmse(diamonds_test_results,
    truth = price,
    estimate = .pred)

rsq(diamonds_test_results,
     truth = price,
     estimate = .pred)

ggplot(data = diamonds_test_results,
       mapping = aes(x = .pred, y = price)) +
  geom_point(color = 'red') +
  geom_abline(intercept = 0, slope = 1, color = 'black') +
  labs(title = 'Linear Regression - Diamonds Test Set',
       x = 'Predicted Price',
       y = 'Actual Price')
