library(fastDummies)
library(tidymodels)
view(diamonds)


#dummy variables
results1 <- fastDummies::dummy_cols(diamonds, select_columns = "cut")
knitr::kable(results1)
view(results1)
results2 <- fastDummies::dummy_cols(results1, select_columns = "color")
knitr::kable(results2)
view(results2)
results3 <- fastDummies::dummy_cols(results2, select_columns = "clarity")
knitr::kable(results3)
view(results3)


#add carat squared variable
diamonds <- mutate(results3, carat2 = carat^2)


#split data 75/25 into train(75) and test(25) data set
set.seed(156)
diamonds_split <- initial_split(diamonds, prop = 0.75)
diamonds_train <- training(diamonds_split)
diamonds_test <- testing(diamonds_split)
dim(diamonds_train)
dim(diamonds_test)


#tell R I'm doing a linear model
lm_model <-
  linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")
lm_model


#fit linear models for train data set
model1 <- lm(`price` ~ `carat` + `carat2` + `x` + `y`, data = diamonds_train)
summary(model1)
lm_fit1 <- fit(lm_model, price ~ carat + carat2 + x + y, diamonds_train)
lm_fit1

model2 <- lm(`price` ~ `carat` + `carat2` + `x` + `y` + `z`, data = diamonds_train)
summary(model2)
lm_fit2 <- fit(lm_model, price ~ carat + carat2 + x + y + z, diamonds_train)
lm_fit2



model3 <- lm(`price` ~ `depth` + `table` + `carat` + `carat2`, data = diamonds_train)
summary(model3)
lm_fit3 <- fit(lm_model, price ~ depth + table + carat + carat2, diamonds_train)
lm_fit3

model4 <- lm(`price` ~ `depth` + `table` + `carat` + `carat2` + `x`, data = diamonds_train)
summary(model4)
lm_fit4 <- fit(lm_model, price ~ depth + table + carat + carat2 + x, diamonds_train)
lm_fit4

model5 <- lm(`price` ~ `depth` + `table` + `carat` + `carat2` + `x` + `z`, data = diamonds_train)
summary(model5)
lm_fit5 <- fit(lm_model, price ~ depth + table + carat + carat2 + x + z, diamonds_train)
lm_fit5


#predict price of train data set
predict(lm_fit1, new_data = diamonds_train)
predict(lm_fit2, new_data = diamonds_train)
predict(lm_fit3, new_data = diamonds_train)
predict(lm_fit4, new_data = diamonds_train)
predict(lm_fit5, new_data = diamonds_train)


#add predicted price column to train data set
diamonds_train_results1 <- predict(lm_fit1, new_data = diamonds_train) %>%
  bind_cols(diamonds_train)
diamonds_train_results1

diamonds_train_results2 <- predict(lm_fit2, new_data = diamonds_train) %>%
  bind_cols(diamonds_train)
diamonds_train_results2

diamonds_train_results3 <- predict(lm_fit3, new_data = diamonds_train) %>%
  bind_cols(diamonds_train)
diamonds_train_results3

diamonds_train_results4 <- predict(lm_fit4, new_data = diamonds_train) %>%
  bind_cols(diamonds_train)
diamonds_train_results4

diamonds_train_results5 <- predict(lm_fit5, new_data = diamonds_train) %>%
  bind_cols(diamonds_train)
diamonds_train_results5


#calculate root mean square error for train data set
rmse(diamonds_train_results1,
     truth = price,
     estimate = .pred)
rmse(diamonds_train_results2,
     truth = price,
     estimate = .pred)
rmse(diamonds_train_results3,
     truth = price,
     estimate = .pred)
rmse(diamonds_train_results4,
     truth = price,
     estimate = .pred)
rmse(diamonds_train_results5,
     truth = price,
     estimate = .pred)


#calculate r squared for train data set
rsq(diamonds_train_results1,
    truth = price,
    estimate = .pred)
rsq(diamonds_train_results2,
    truth = price,
    estimate = .pred)
rsq(diamonds_train_results3,
    truth = price,
    estimate = .pred)
rsq(diamonds_train_results4,
    truth = price,
    estimate = .pred)
rsq(diamonds_train_results5,
    truth = price,
    estimate = .pred)


#create linear regression plot of train data set
ggplot(data = diamonds_train_results1,
       mapping = aes(x = .pred, y = price))+
  geom_point(color = 'green')+
  geom_abline(intercept = 0, slope = 1, color = 'black')+
  labs(title = 'Linear Regression - Diamonds Train Set',
       x = 'Predicted Price',
       y = 'Actual Price')

ggplot(data = diamonds_train_results2,
       mapping = aes(x = .pred, y = price))+
  geom_point(color = 'red')+
  geom_abline(intercept = 0, slope = 1, color = 'black')+
  labs(title = 'Linear Regression - Diamonds Train Set',
       x = 'Predicted Price',
       y = 'Actual Price')

ggplot(data = diamonds_train_results3,
       mapping = aes(x = .pred, y = price))+
  geom_point(color = 'blue')+
  geom_abline(intercept = 0, slope = 1, color = 'black')+
  labs(title = 'Linear Regression - Diamonds Train Set',
       x = 'Predicted Price',
       y = 'Actual Price')

#BEST MODEL
ggplot(data = diamonds_train_results4,
       mapping = aes(x = .pred, y = price))+
  geom_point(color = 'purple')+
  geom_abline(intercept = 0, slope = 1, color = 'black')+
  labs(title = 'Linear Regression - Diamonds Train Set',
       x = 'Predicted Price',
       y = 'Actual Price')

ggplot(data = diamonds_train_results5,
       mapping = aes(x = .pred, y = price))+
  geom_point(color = 'orange')+
  geom_abline(intercept = 0, slope = 1, color = 'black')+
  labs(title = 'Linear Regression - Diamonds Train Set',
       x = 'Predicted Price',
       y = 'Actual Price')





#TEST DATA SET


#fit linear model for test data set
model44 <- lm(`price` ~ `depth` + `table` + `carat` + `carat2` + `x`, data = diamonds_test)
summary(model44)
lm_fit44 <- fit(lm_model, price ~ depth + table + carat + carat2 + x, diamonds_test)
lm_fit44


#predict price of test data set
predict(lm_fit44, new_data = diamonds_test)


#add predicted price column to test data set
diamonds_test_results <- predict(lm_fit44, new_data = diamonds_test) %>%
  bind_cols(diamonds_test)
diamonds_test_results


#calculate root mean square error for test data set
rmse(diamonds_test_results,
     truth = price,
     estimate = .pred)


#calculate r squared for test data set
rsq(diamonds_test_results,
    truth = price,
    estimate = .pred)


#create linear regression plot for test data set
ggplot(data = diamonds_test_results,
       mapping = aes(x = .pred, y = price))+
  geom_point(color = 'brown')+
  geom_abline(intercept = 0, slope = 1, color = 'black')+
  labs(title = 'Linear Regression - Diamonds Test Set',
       x = 'Predicted Price',
       y = 'Actual Price')
