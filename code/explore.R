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
  
test <- read_csv("data/test.csv")
preds <- read_csv("data/gender_submission.csv")



#***********************************************************
# data exploration ----
#***********************************************************

colnames(train)

train %>% count(survived)
train %>% 
  group_by(survived) %>% 
  summarise(count(sex))

train %>% 
  pivot_longer(-c(passengerid, name, cabin, ticket, survived), names_to = "var", values_to = "value") %>% head()






#***********************************************************
# model building ----
#***********************************************************






