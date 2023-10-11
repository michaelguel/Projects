library(readxl)
library(ggplot2)
library(dplyr)
library(glmnet)
library(lubridate)
library(scatterplot3d)


### LOAD THE DATA IN AND MERGE

session = read.csv('sessions.csv', sep = ',')

transactions = read.csv('transactions.csv', sep = ',')

data= merge(session,transactions,by=c("session_id"))

### USE YMD TO CONVERT DATE TO DATE TYPE

data$session_dt = ymd(data$session_dt)


### ADD TWO FEATURE FOR DATE WHICH TELLS WHAT DAY AND WHAT MONTH

data = data %>% mutate(session_day = weekdays(as.Date(data$session_dt)),session_month = months(as.Date(data$session_dt)))

data$session_day = as.factor(data$session_day)

data$session_month = as.factor(data$session_month)

### EDA

ggplot(data = data, aes(data$session_day)) +
  geom_bar()

ggplot(data = data, aes(data$session_month)) +
  geom_bar()

ggplot(data = data, aes(data$num_impressions)) +
  geom_bar()

ggplot(data = data, aes(data$num_search)) +
  geom_bar()

totconversion = data %>% group_by(conversion) %>% summarise(count = n())

howmanysessions = data %>% group_by(user_id) %>% summarise(count = n())

unique(howmanysessions$count)

scatterplot3d(data$num_impressions, data$num_search, as.numeric(data$conversion), pch = 19, color = "blue")






data = data %>% mutate(diffy = abs(num_impressions - num_search))

max(data$diffy[data$conversion == TRUE])

max(data$diffy[data$conversion == FALSE])

min(data$diffy[data$conversion == TRUE])

min(data$diffy[data$conversion == FALSE])

length(data$diffy[data$conversion == FALSE & data$diffy == 6])

data = data %>% mutate(toohigh = ifelse(data$conversion == FALSE & data$diffy >6, 1,0))


### ADD RANKING FEATURE FOR SEVERAL FEATURES

nexttr = data %>% group_by(user_id) %>% arrange(session_dt) %>% mutate(rank = rank(session_dt,ties.method = 'random')) %>% ungroup %>% group_by(user_id) %>% mutate(imp2sess = ifelse(rank == 1,num_impressions[rank == 1],ifelse(rank == 2, num_impressions[rank == 1] + num_impressions[rank == 2], ifelse(rank == 3,num_impressions[rank == 1] + num_impressions[rank == 2] + num_impressions[rank == 3], ifelse(rank == 4, num_impressions[rank == 1] + num_impressions[rank == 2] + num_impressions[rank == 3] + num_impressions[rank == 4],ifelse(rank == 5,num_impressions[rank == 1] + num_impressions[rank == 2] + num_impressions[rank == 3] + num_impressions[rank == 4] + num_impressions[rank ==5], ifelse(rank == 6, num_impressions[rank == 1] + num_impressions[rank == 2]+ num_impressions[rank == 3] + num_impressions[rank == 4]+ num_impressions[rank == 5] + num_impressions[rank == 6],0))))))) %>% mutate(num2ses = ifelse(rank == 1,num_search[rank == 1],ifelse(rank == 2, num_search[rank == 1] + num_search[rank == 2], ifelse(rank == 3,num_search[rank == 1] + num_search[rank == 2] + num_search[rank == 3], ifelse(rank == 4, num_search[rank == 1] + num_search[rank == 2] + num_search[rank == 3] + num_search[rank == 4],ifelse(rank == 5,num_search[rank == 1] + num_search[rank == 2] + num_search[rank == 3] + num_search[rank == 4] + num_search[rank ==5], ifelse(rank == 6, num_search[rank == 1] + num_search[rank == 2]+ num_search[rank == 3] + num_search[rank == 4]+ num_search[rank == 5] + num_search[rank == 6],0))))))) %>% ungroup


nexttr = nexttr %>% group_by(user_id) %>% mutate(prevrel = ifelse(rank == 1,0,ifelse(rank == 2, avg_relevance[rank == 1], ifelse(rank == 3,avg_relevance[rank == 2], ifelse(rank == 4, avg_relevance[rank == 3],ifelse(rank == 5,avg_relevance[rank == 4], ifelse(rank == 6, avg_relevance[rank == 5],"nah"))))))) %>% ungroup

nexttr = nexttr %>% group_by(user_id) %>% mutate(prevsearch = ifelse(rank == 1,0,ifelse(rank == 2, num_search[rank == 1], ifelse(rank == 3,num_search[rank == 2], ifelse(rank == 4, num_search[rank == 3],ifelse(rank == 5,num_search[rank == 4], ifelse(rank == 6, num_search[rank == 5],"nah"))))))) %>% ungroup

nexttr = nexttr %>% group_by(user_id) %>% mutate(previmp = ifelse(rank == 1,0,ifelse(rank == 2, num_impressions[rank == 1], ifelse(rank == 3,num_impressions[rank == 2], ifelse(rank == 4, num_impressions[rank == 3],ifelse(rank == 5,num_impressions[rank == 4], ifelse(rank == 6, num_impressions[rank == 5],"nah"))))))) %>% ungroup


nexttr = nexttr %>% mutate(b4thisses = (imp2sess - num_impressions))

nexttr = nexttr %>% mutate(searchb4 = (num2ses - num_search))


nexttr$prevrel = as.numeric(nexttr$prevrel)

nexttr$prevsearch = as.numeric(nexttr$prevsearch)

nexttr$previmp = as.numeric(nexttr$previmp)

### DROP FEATURES THAT ARE OF NO VALUE AND NOT BEING USED


use = nexttr[,!(colnames(nexttr)%in% c("session_id","session_dt","user_id","avgrelacross","totsearch","avgsearch","avgsimp","prevrel","sessb4","imppersearch","totses","totimp","b4thisses","searchb4","prevsearch","previmp","session_day"))]


### Split into train, validation and test 

train = use[use$train == TRUE,]

train = train[,!(colnames(train)%in% c("train","score","test"))]

validation = use[use$score == TRUE,]

validation = validation[,!(colnames(validation)%in% c("train","score","test"))]

test = use[use$test == TRUE,]

test = test[,!(colnames(test)%in% c("train","score","test"))]


### CREATE MATRIX FOR FEATURES AND PULL TARGET VARIABLE INTO ITS OWN DATAFRAME

x = model.matrix(conversion ~ ., train)[,-1]

y = as.factor(train$conversion)



### FIT MODEL USING CV AND L1 PENALTY

fit_ridge_cv = cv.glmnet(x, y, alpha = 1,family = "binomial",type.measure = "class")
plot(fit_ridge_cv)

coef(fit_ridge_cv,s = "lambda.min")

coef(fit_ridge_cv,s = "lambda.1se")


### PREDICT AND CHECK FOR ACCURACY TRAIN DATA

xtrain = model.matrix(conversion ~ ., train)[,-1]

ne = data.frame(predict(fit_ridge_cv,xtrain,type = "class", s="lambda.min"))

see = data.frame(train$conversion)

see$pred = ne$lambda.min

table(see$pred,see$train.conversion)

### CALCULATE PREDICTION STATISTICS

cm <- table(see$pred, see$train.conversion)

accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
precision <- cm[4] / sum(cm[4], cm[2])
sensitivity <- cm[4] / sum(cm[4], cm[3])
fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
specificity <- cm[1] / sum(cm[1], cm[2])

accuracy
precision
sensitivity
fscore
specificity

### PREDICT AND CHECK ACCURACY

xval = model.matrix(conversion ~ ., validation)[,-1]

ne = data.frame(predict(fit_ridge_cv,xval,type = "class", s="lambda.min"))

see = data.frame(validation$conversion)

see$pred = ne$lambda.min

table(see$pred,see$validation.conversion)

### CALCULATE PREDICTION STATISTICS

cm <- table(see$pred, see$validation.conversion)

accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
precision <- cm[4] / sum(cm[4], cm[2])
sensitivity <- cm[4] / sum(cm[4], cm[3])
fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
specificity <- cm[1] / sum(cm[1], cm[2])

accuracy
precision
sensitivity
fscore
specificity


### PREDICT AND CHECK ACCURACY ON TEST DATA

xtest = model.matrix(conversion ~ ., test)[,-1]

ne = data.frame(predict(fit_ridge_cv,xtest,type = "class", s="lambda.min"))

see = data.frame(test$conversion)

see$pred = ne$lambda.min

table(see$pred,see$test.conversion)

### CALCULATE PREDICTION STATISTICS

cm <- table(see$pred, see$test.conversion)

accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
precision <- cm[4] / sum(cm[4], cm[2])
sensitivity <- cm[4] / sum(cm[4], cm[3])
fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
specificity <- cm[1] / sum(cm[1], cm[2])

accuracy
precision
sensitivity
fscore
specificity

### COMPARE TO RIDGE REGRESSION
### FIT MODEL USING CV AND L2 PENALTY

use = nexttr[,!(colnames(nexttr)%in% c("session_id","session_dt","user_id"))]


### Split into train, validation and test 

train = use[use$train == TRUE,]

train = train[,!(colnames(train)%in% c("train","score","test"))]

validation = use[use$score == TRUE,]

validation = validation[,!(colnames(validation)%in% c("train","score","test"))]

test = use[use$test == TRUE,]

test = test[,!(colnames(test)%in% c("train","score","test"))]


### CREATE MATRIX FOR FEATURES AND PULL TARGET VARIABLE INTO ITS OWN DATAFRAME

x = model.matrix(conversion ~ ., train)[,-1]

y = as.factor(train$conversion)

fit_ridge_cv = cv.glmnet(x, y, alpha = 0,family = "binomial",type.measure = "class")
plot(fit_ridge_cv)

coef(fit_ridge_cv,s = "lambda.min")

coef(fit_ridge_cv,s = "lambda.1se")


### PREDICT AND CHECK FOR ACCURACY TEST DATA

xtest = model.matrix(conversion ~ ., test)[,-1]

ne = data.frame(predict(fit_ridge_cv,xtest,type = "class", s="lambda.min"))

see = data.frame(test$conversion)

see$pred = ne$lambda.min

table(see$pred,see$test.conversion)

### CALCULATE PREDICTION STATISTICS

cm <- table(see$pred, see$test.conversion)

accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
precision <- cm[4] / sum(cm[4], cm[2])
sensitivity <- cm[4] / sum(cm[4], cm[3])
fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
specificity <- cm[1] / sum(cm[1], cm[2])

accuracy
precision
sensitivity
fscore
specificity

