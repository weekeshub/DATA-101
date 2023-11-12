library(tidyverse)
library(fastDummies)
library(tidymodels)
library(pscl)
library(caret)
library(car)
library(aod)

#data cleaning
CV_Dataset <- mutate(Cardiovascular_Dataset, `age_years` = `age` / 365)
CV_Dataset

#LOGISTIC REGRESSION

#dummy variables
results1 <- fastDummies::dummy_cols(CV_Dataset, select_columns = "gender")
knitr::kable(results1)
view(results1)
results2 <- fastDummies::dummy_cols(results1, select_columns = "cholesterol")
knitr::kable(results2)
view(results2)
results3 <- fastDummies::dummy_cols(results2, select_columns = "gluc")
knitr::kable(results3)
view(results3)
results4 <- fastDummies::dummy_cols(results3, select_columns = "smoke")
knitr::kable(results4)
view(results4)
results5 <- fastDummies::dummy_cols(results4, select_columns = "alco")
knitr::kable(results5)
view(results5)
results6 <- fastDummies::dummy_cols(results5, select_columns = "active")
knitr::kable(results6)
view(results6)
CV_Data <- fastDummies::dummy_cols(results6, select_columns = "cardio")
knitr::kable(CV_Data)
view(CV_Data)

#summary
summary(CV_Data)

#create training and test samples
set.seed(1156)
CV_split <- initial_split(CV_Data, prop = 0.80)
CV_train <- training(CV_split)
CV_test <- testing(CV_split)

dim(CV_train)
dim(CV_test)

#TRAIN DATA SET

#logit model
mylogit_train1 <- glm(cardio ~ age_years + ap_lo + gender_1 + cholesterol_1 + gluc_1 + smoke_0 + alco_0 + active_0, data = CV_train, family = 'binomial')
summary(mylogit_train)
mylogit_train2 <- glm(cardio ~ age_years + height + ap_lo + gender_1 + cholesterol_1 + gluc_1 + smoke_0 + alco_0 + active_0, data = CV_train, family = 'binomial')
summary(mylogit_train)
mylogit_train3 <- glm(cardio ~ age_years + height + weight + ap_lo + gender_1 + cholesterol_1 + gluc_1 + smoke_0 + alco_0 + active_0, data = CV_train, family = 'binomial')
summary(mylogit_train)

#confidence interval estimates for coefficients
confint(mylogit_train1)
confint(mylogit_train2)
confint(mylogit_train3)

#confidence interval estimates for coefficients using standard errors
confint.default(mylogit_train1)
confint.default(mylogit_train2)
confint.default(mylogit_train3)

#Wald's test
wald.test(b = coef(mylogit_train1), Sigma = vcov(mylogit_train1), Terms = 1:8)
wald.test(b = coef(mylogit_train2), Sigma = vcov(mylogit_train2), Terms = 1:9)
wald.test(b = coef(mylogit_train3), Sigma = vcov(mylogit_train3), Terms = 1:10)

#odds ratios
exp(coef(mylogit_train1))
exp(coef(mylogit_train2))
exp(coef(mylogit_train3))

#McFadden's R squared
pscl::pR2(mylogit_train1)["McFadden"]
pscl::pR2(mylogit_train2)["McFadden"]
pscl::pR2(mylogit_train3)["McFadden"]

#importance of each predictor variable
caret::varImp(mylogit_train1)
caret::varImp(mylogit_train2)
caret::varImp(mylogit_train3)

#calculate VIF values for each predictor variables in model to see if multicollinearity is an issue
car::vif(mylogit_train1)
car::vif(mylogit_train2)
car::vif(mylogit_train3)

#WARNING - calculate probability of cardiovascular disease for each individual in train dataset
predicted1 <- predict(mylogit_train1, CV_train, type='response')
predicted1

predicted2 <- predict(mylogit_train2, CV_train, type='response')
predicted2

predicted3 <- predict(mylogit_train3, CV_train, type='response')
predicted3

predicted.classes1 <- ifelse(predicted > 0.5, 'pos', 'neg')
head(predicted.classes1)
predicted.classes2 <- ifelse(predicted > 0.5, 'pos', 'neg')
head(predicted.classes2)
predicted.classes3 <- ifelse(predicted > 0.5, 'pos', 'neg')
head(predicted.classes3)

mean(predicted.classes1 == CV_train$cardio)
mean(predicted.classes2 == CV_train$cardio)
mean(predicted.classes3 == CV_train$cardio)

#logistic regression curve in r
ggplot(CV_train, aes(x=weight, y = cardio)) + geom_point() +
  stat_smooth(method='glm', method.args=list(family="binomial"), se=FALSE)

ggplot(CV_train, aes(x=weight + height, y = cardio)) + geom_point() +
  stat_smooth(method='glm', method.args=list(family="binomial"), se=FALSE)

ggplot(CV_train, aes(x=weight + height + age_years, y = cardio)) + geom_point() +
  stat_smooth(method='glm', method.args=list(family="binomial"), se=FALSE)

ggplot(CV_train, aes(x=weight + height + age_years + smoke_0 + active_0, y = cardio)) + geom_point() +
  stat_smooth(method='glm', method.args=list(family="binomial"), se=FALSE)

ggplot(CV_train, aes(x=height + weight + age_years + gender_1 + cholesterol_1 + gluc_1 + smoke_0 + alco_0 + active_0, y = cardio)) + geom_point() +
  stat_smooth(method='glm', method.args=list(family="binomial"), se=FALSE)




#TEST DATA SET

#logit model
mylogit_test <- glm(cardio ~ age_years + height + weight + ap_lo + gender_1 + cholesterol_1 + gluc_1 + smoke_0 + alco_0 + active_0, data = CV_test, family = 'binomial')
summary(mylogit_test)

#confidence interval estimates for coefficients
confint(mylogit_test)

#confidence interval estimates for coefficients using standard errors
confint.default(mylogit_test)

#Wald's test
wald.test(b = coef(mylogit_test), Sigma = vcov(mylogit_train), Terms = 1:10)

#odds ratios
exp(coef(mylogit_test))

#McFadden's R squared
pscl::pR2(mylogit_test)["McFadden"]

#importance of each predictor variable
caret::varImp(mylogit_test)

#calculate VIF values for each predictor variables in model to see if multicollinearity is an issue
car::vif(mylogit_test)

#logistic regression curve in r
ggplot(CV_test, aes(x=height + weight + age_years + gender_1 + cholesterol_1 + gluc_1 + smoke_0 + alco_0 + active_0, y = cardio)) + geom_point() +
  stat_smooth(method='glm', method.args=list(family="binomial"), se=FALSE)

