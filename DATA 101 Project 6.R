library(rpart.plot)
library(rpart)
library(tidymodels)

fit4 <- rpart(Purchased ~ Age + EstimatedSalary, data = Decision_tree_data, method = 'class')
rpart.plot(fit4)