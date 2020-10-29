#***********************************************************
# objective 
#***********************************************************

#develop a model workflow for exploring data and making submissions
#that I can use in future kaggle competitions.


#***********************************************************
# setup ----
#***********************************************************

library(tidyverse)
library(tidymodels)
library(randomForest)
library(ranger)

#reading in data
train <- read_csv("data/train.csv") %>% 
  rename_all(tolower) 
  
test <- read_csv("data/test.csv") %>% 
  rename_all(tolower)
preds <- read_csv("data/gender_submission.csv") %>% 
  rename_all(tolower) %>% 
  right_join(test)


#***********************************************************
# data exploration ----
#***********************************************************

#initial approach is to remove seemingly uninformative column and rows (come back later to gauge lost info);
#and look for primary univariate relationships; and confirm those and their relationships.  
#use these

#reducing cols
train_red <- select(train, -name, -ticket, -cabin, -passengerid)

colnames(train)

train %>% count(survived)
train %>% 
  group_by(survived) %>% 
  count(sex)

temp <- lm(survived ~ ., data = train_red)
summary(temp)
map(train_red, unique)
temp2 <- glm(survived ~ ., data = train_red, family = "binomial") %>% summary()

#primary univariate relationships (pclass, sex, age; also sibsp)
count(train_red, survived, pclass)
count(train_red, survived, sex)

#80% males die relative to 25% of females
train_red %>% 
  group_by(survived, sex) %>% 
  summarise(n = n(), mean = mean(age, na.rm = T), class = mean(pclass), sibsp = mean(sibsp), parch = mean(parch))

train_red %>% 
  ggplot(aes(x = age)) + geom_histogram() + facet_grid(~survived + sex) + theme_bw()

#pclass
train_red %>% 
  group_by(survived, pclass) %>% 
  summarise(fn = n(), mean = mean(age, na.rm = T), sibsp = mean(sibsp), parch = mean(parch))

train_red %>% 
  group_by(survived, pclass, sex) %>% 
  summarise(fn = n(), mean = mean(age, na.rm = T), sibsp = mean(sibsp), parch = mean(parch))


#primary interactions (females in first or second class "always" survive; four buckets remain)
train_red %>% 
  ggplot(aes(x = age)) + geom_histogram() + facet_grid(~survived + sex + pclass) + theme_bw()

temp <- glm(survived ~ . + sex*pclass, data = train_red, family = "binomial")
summary(temp)
predict(temp, test) %>% log()

#explore the third class of female
female <- train_red %>% 
  filter(sex == "female", pclass == 3)

female %>% 
  group_by(survived, pclass, sex) %>% 
  summarise(fn = n(), mean = mean(age, na.rm = T), sibsp = mean(sibsp), parch = mean(parch))

female %>% 
  count(survived, sibsp, parch)
female %>% 
  count(survived, sibsp)
female %>% 
  count(survived, parch)

female %>% 
  ggplot(aes(x = age)) + geom_histogram() + facet_grid(~survived + sex + pclass) + theme_bw()

#surviving 3rd class males and young people are two other interesting subgroups

#***********************************************************
# feature engineering ----
#***********************************************************

#impute missingness
train %>% apply(2, function(x) sum(is.na(x))) #age, cabin, and embarked have missing vaulues
test %>% apply(2, function(x) sum(is.na(x)))

#bin age
test <- train_red %>% 
  mutate(age = ifelse(is.na(age), median(age), age),
         age = case_when(
    age <= 16 ~ 1,
    age > 16 & age <= 32 ~ 2,
    age > 32 & age <= 48 ~ 3, 
    age > 48 ~ 4,
    TRUE ~ 5
  ))

#***********************************************************
# model building ----
#***********************************************************

#linear regression
temp <- lm(survived ~ ., data = train_red)

#logistic regression
temp <- glm(survived ~ . + sex*pclass, data = train_red, family = "binomial")

#random forest
rf <- rand_forest(mode = "classification", trees = 200, mtry = 5) %>% 
  set_engine("ranger") %>% 
  fit(as_factor(survived) ~ ., data = na.omit(train_red))

#***********************************************************
# submissions ----
#***********************************************************

#.75666 - baseline is all females survive, all males die. 
baseline <- read_csv("data/gender_submission.csv") 

#female class 1, 2 , and class 3 rule (all this changed was 26 obs; very little change)
set.seed(8484)
pred1 <- preds %>% 
  mutate(survived = case_when(
    sex == "female" & pclass < 3 ~ 1, #females in 3rd class die
    sex == "female" & pclass == 3 & sibsp == 0 ~ 1, #
    sex == "female" & pclass == 3 & sibsp > 0 ~ 99,
    sex == "male" ~ 0,
    TRUE ~ 2),
    survived = ifelse(survived == 99, rbinom(26, 1, .5), survived)) %>%  #got 26 by checking
  select(passengerid, survived) %>% 
  rename(PassengerId = passengerid, Survived = survived)

write_csv(pred1, "data/pred1.csv")

#logistic regression based model (+ predictions for NA)
probs <- temp %>% predict(train_red, type = "response")
predictions <- ifelse(probs > .5, 1, 0)
mean(predictions == train_red$survived, na.rm = T)

probs <- temp %>% predict(test, type = "response")
predictions <- ifelse(probs > .5, 1, 0)

pred2 <- preds %>% 
  mutate(survived = case_when(
    is.na(age) & sex == "female" & pclass < 3 ~ 1, #females in 3rd class die
    is.na(age) & sex == "female" & pclass == 3 & sibsp == 0 ~ 1, #
    is.na(age) & sex == "female" & pclass == 3 & sibsp > 0 ~ 99,
    is.na(age) & sex == "male" ~ 0,
    TRUE ~ predictions),
    survived = ifelse(survived == 99, rbinom(26, 1, .5), survived),
    survived = ifelse(is.na(survived), 0, survived)) %>%  #changing the one obs still missing a prediction
  select(passengerid, survived) %>% 
  rename(PassengerId = passengerid, Survived = survived)

write_csv(pred2, "data/pred2.csv")

#coin flip model
pred3 <- preds %>% 
  mutate(survived = as.numeric(rbernoulli(nrow(preds)))) %>% 
  select(passengerid, survived) %>% 
  rename(PassengerId = passengerid, Survived = survived)

write_csv(pred3, "data/pred3.csv")

#random forest model
pred4 <- preds %>%  # imputing empty classes
  mutate(age = ifelse(is.na(age), mean(preds$age, na.rm = T), age),
         fare = ifelse(is.na(fare), mean(preds$fare, na.rm = T), fare))
pred4 <- predict(rf, pred4, type = "class") %>% 
  bind_cols(pred4 %>% select(passengerid)) %>% 
  rename(PassengerId = passengerid, Survived = .pred_class)

write_csv(pred4, "data/pred4.csv")


#given that there are way more male I will focus now on here. 
Raul Torres; rtorres2120@yahoo.com