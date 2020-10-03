#***********************************************************
# objective 
#***********************************************************

#develop a model workflow for exploring data and making submissions
#that I can use in future kaggle competitions.


#***********************************************************
# setup ----
#***********************************************************

library(tidyverse)

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
  summarise(count(sex))

train %>% 
  pivot_longer(-c(passengerid, name, cabin, ticket, survived), names_to = "var", values_to = "value") %>% head()



temp <- lm(survived ~ ., data = train_red)
summary(temp)
map(train_red, unique)
temp2 <- glm(survived ~ ., data = train_red, family = "binomial") %>% summary()

#primary univariate relationships (pclass, sex, age; also sibsp)
count(train_red, survived, pclass)
count(train_red, survived, sex)
count(train_red, survived, pclass)

#80% males die relative to 25% of females
train_red %>% 
  group_by(survived, sex) %>% 
  summarise(fn = n(), mean = mean(age, na.rm = T), class = mean(pclass), sibsp = mean(sibsp), parch = mean(parch))

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
# model building ----
#***********************************************************





#***********************************************************
# submissions ----
#***********************************************************

#.75666
baseline <- read_csv("data/gender_submission.csv") 

#female class 1, 2 , and class 3 rule (all this changed was 26 obs; very little change)
set.seed(8484)
pred1 <- preds %>% 
  mutate(survived = case_when(
    sex == "female" & pclass < 3 ~ 1,
    sex == "female" & pclass == 3 & sibsp == 0 ~ 1,
    sex == "female" & pclass == 3 & sibsp > 0 ~ 99,
    sex == "male" ~ 0,
    TRUE ~ 2),
    survived = ifelse(survived == 99, rbinom(26, 1, .5), survived)) %>%  #got 26 by checking
  select(passengerid, survived) %>% 
  rename(PassengerId = passengerid, Survived = survived)

write_csv(pred1, "data/pred1.csv")

#given that there are way more male I will focus now on here. 