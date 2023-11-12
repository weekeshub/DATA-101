install.packages("rpart.plot")
library(rpart.plot)
library(rpart)
library(tidymodels)

fit1 <- rpart(more_than_20 ~ mileage + mpg, data = audi, method = 'class')
rpart.plot(fit1)

fit2 <- rpart(more_than_20 ~ tax + mileage + mpg + engineSize, data = audi, method = 'class')
rpart.plot(fit2)

set.seed(1234)
audi_split <- initial_split(audi, prop = 0.80)
audi_train <- training(audi_split)
audi_test <- testing(audi_split)

dim(audi_train)
dim(audi_test)

audi_rec_train <-
  recipe(price ~., data = audi_train)

audi_juiced_trained <- juice(prep(audi_rec_train))
names(audi_juiced_trained)

audi_rec_test <-
  recipe(price ~., data = audi_test)

audi_juiced_test <- juice(prep(audi_rec_test))
names(audi_juiced_test)

fit3 <- rpart(more_than_20 ~ mileage + mpg, data = audi_juiced_trained, method = 'class')
rpart.plot(fit3)

predict_unseen <- predict(fit3, audi_juiced_test, type = 'class')

table_mat <- table(audi_juiced_test$more_than_20, predict_unseen)
table_mat

accuracy_test <- sum(diag(table_mat))/sum(table_mat)
accuracy_test

