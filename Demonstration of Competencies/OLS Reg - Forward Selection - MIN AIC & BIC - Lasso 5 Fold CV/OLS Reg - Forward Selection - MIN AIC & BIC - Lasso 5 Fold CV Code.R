library(readxl)
library(leaps)
library(lars)

data = read_xlsx('prostatedate.xlsx')

train = data[data$train == 'T',]

test = data[data$train == 'F',]

train = train[2:10]

test = test[2:10]

############################# Fit an OLS regression model (with intercept). Report its R2, p-values of regressioncoefficients, the set of significant predictors (at the level of 0.05), and the residual sumof squares (RSS) on the training and the testing data. You may use R functions lm()and summary().

x = lm(lpsa~.,data=train)

summary(x)

# R2 = 0.6522

print(deviance(x))

# RSS = 29.42638

############################# TEST DATA

x = lm(lpsa~.,data=test)

summary(x)

# R2 = 0.5748

print(deviance(x))

# RSS = 9.695596

############################ Apply forward selection to select variables use R function regsubsets() in the packageleap. You should get a sequence of eight models M^1, . . . , M^8 in the increasing orderof model size. For each model M^j, j = 1, . . . , 8, report the regression coefficients, RSS2

trainsearch = regsubsets(lpsa~.,data=train,nvmax=8,method="forward")

testsearch = regsubsets(lpsa~.,data=test,nvmax=8,method="forward")



traincoef = summary(trainsearch)

traincoef$rss

traincoef$bic

coeffi = coef(trainsearch,1:8)

coeffi

testcoef = summary(testsearch)

testcoef$rss

testcoef$bic

coeffi = coef(testsearch,1:8)

coeffi

############################## MINIMIZE BIC BEST MODEL

which.min(traincoef$bic)

which.min(testcoef$bic)

# 2 MINIMIZES BIC

coef(trainsearch,2)

coef(testsearch,2)


finalmodelbic = lm(lpsa~lcavol+lweight,data=test)

summary(finalmodelbic)

deviance(finalmodelbic)


############################## In 2, replace BIC by AIC where AIC = nlog(RSStrn/n) + 2|M^j|. Choose the best model by minimizing AIC, and report the set of selected variables.

r = data.frame(testcoef$rss)

i = numeric()

for (x in 1:length(r$testcoef.rss)){
  t = log(((r$testcoef.rss[x])/30))+2*abs(x+1)
  i = c(i,t)
}

min(i)

##### AIC IS LOWEST WITH FIRST ITERATION

coeffi[1]

# lcavol

############################## Use R functions lars() and cv.lars()) to fit Lasso without an intercept for theprostate cancer data (first centralize the data). Specifically, select the tuning parameter(denoted by lambda) by 5-fold cross-validation (CV). Then fit Lasso with the selected lambda and report the estimated regression coefficients and RSStst.

#split matrix of predictors and response

trainpred = data.matrix(train[1:8])

trainrep = data.matrix(train[9])

q = lars(trainpred,trainrep,type = 'lasso')

summary(q)

cv.lars(trainpred,trainrep,K=5,type = 'lasso')

############################## FIND BEST MODEL WITH VALIDATION MATRIX

search= regsubsets(lpsa~.,data=train,nvmax=8,method="forward")
valid.mat=model.matrix(lpsa~.,test)

val.errors=numeric(8)
for(i in 1:8){
  coefi=coef(search,id=i)
  pred=valid.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((test$lpsa-pred)^2)
}
val.errors
best=which.min(val.errors)
best
plot(val.errors,type="b")
points(best,val.errors[best],col="red",cex=2,pch=20)

coef(search,best)
res=summary(search)
res$adjr2[best]

summary(search)
