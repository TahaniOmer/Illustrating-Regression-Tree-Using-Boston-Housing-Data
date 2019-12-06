
# Packages required

library(MASS)
library(ISLR)
library(tree)
library(psych)
?Boston
names(Boston)

#Exploratory data analysis

attach(Boston)
summary(Boston)  #summary statistics
str(Boston)

sum(is.na(Boston))  #Check for missing values

pairs.panels(Boston, gap=0)  #checking correlation between variables

# Fitting Regression Trees

tree.boston=tree(medv~.,Boston)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty=0)
tree.boston

# Split data set into 50:50 train and test data
  
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston1=tree(medv~.,Boston,subset=train)
summary(tree.boston1)

plot(tree.boston1)
text(tree.boston1, pretty=0)

#see how it performs in the test dataset.
yhat=predict(tree.boston1,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
mean((yhat-boston.test)^2)

# Pruning

cv.boston=cv.tree(tree.boston1)
cv.boston

plot(cv.boston$size,cv.boston$dev,type='b',xlab = "Tree-Size",ylab = "Deviance_sqrt(MSE)")

#Now we repeat the commands we had before

prune.boston=prune.tree(tree.boston ,best=5)
plot(prune.boston)
text(prune.boston, pretty =0)

yhat=predict(prune.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)

mean((boston.test-yhat)^2)
#We did not get too much from pruning, except we get a small tree which is easier to interpret

# Fitting Linear-Regression 

set.seed(2)
index <- sample(nrow(Boston), nrow(Boston) * 0.80)
Boston.train <- Boston[index, ]
Boston.test <- Boston[-index, ]
model1 = lm(medv ~ ., data = Boston.train)
model1.sum = summary(model1)
model1.sum

#to refit the model without variables indus and age
model2 = lm(medv ~ . -indus -age, data = Boston.train)
model2.sum = summary(model2)
model2.sum

# Variable Selection
  
#Best subset and cross-validation  techniques
#Best subset selection
library(leaps)

model.subset <- regsubsets(medv ~ ., data = Boston.train, nvmax = 13)
(reg.sum =summary(model.subset))

names(reg.sum)

reg.sum$rsq

plot(reg.sum$adjr2,xlab="Number of Variables ",ylab="adjr2",
     type="l")

which.max(reg.sum$adjr2)

plot(reg.sum$cp ,xlab="Number of Variables ",ylab="Cp",
     type="l")

which.min(reg.sum$cp )

plot(reg.sum$bic ,xlab="Number of Variables ",ylab="BIC",
     type="l")

which.min(reg.sum$bic )

plot(model.subset, scale = "bic")

plot(model.subset, scale = "Cp")

plot(model.subset, scale = "adjr2")

#Cross-validation 

predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

k=10
set.seed(1)
folds=sample (1:k,nrow(Boston),replace=TRUE)
cv.errors =matrix (NA,k,13, dimnames =list(NULL , paste (1:13) ))
for(j in 1:k){
  model.subset=regsubsets (medv~.,data=Boston [folds!=j,],
                           nvmax=13)
  for(i in 1:13){
    pred=predict (model.subset ,Boston [folds ==j,],id=i)
    cv.errors[j,i]= mean( ( Boston$medv[ folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors ,2, mean)
mean.cv.errors

plot(mean.cv.errors, pch=19 ,type="b")

which.min(mean.cv.errors)

reg.best=regsubsets (medv~.,data= Boston , nvmax=13)
coef(reg.best,11)


#Model Assessment
  
#Model Diagnostics for model 2
par(mfrow = c(2,2))
plot(model2)

par(mfrow = c(1,1))


# Model selection 
#R-squared
model1.sum$r.squared

model2.sum$r.squared

#AIC
AIC(model1)

AIC(model2)

#BIC
BIC(model1)

BIC(model2)


#Test error (MSSE)
model1.pred.test <- predict(model1, newdata = Boston.test)
model1.msse = mean((model1.pred.test - Boston.test$medv) ^ 2)
model1.msse


model2.pred.test <- predict(model2, newdata = Boston.test)
model2.mspe <- mean((model2.pred.test - Boston.test$medv) ^ 2)
model2.mspe


# K-Nearest Neighbors Model

library(class)
medv01 = rep(0, length(medv))
medv01[medv > median(medv)] = 1
Boston = data.frame(Boston, medv01)
train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
medv01.test = medv01[test]
train.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
                lstat, crim)[train, ]
test.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
               lstat, crim)[test, ]
train.medv01 = medv01[train]
set.seed(1)
knn.pred1 = knn(train.X, test.X, train.medv01, k = 1)
mean(knn.pred1 != medv01.test)

knn.pred2 = knn(train.X, test.X, train.medv01, k = 2)
mean(knn.pred2 != medv01.test)

knn.pred3 = knn(train.X, test.X, train.medv01, k = 10)
mean(knn.pred3 != medv01.test)


train.X = cbind(zn, chas, nox, rm, dis, rad, tax, ptratio, black, 
                lstat, crim)[train, ]
test.X = cbind(zn, chas, nox, rm, dis, rad, tax, ptratio, black, 
               lstat, crim)[test, ]
knn.pred4 = knn(train.X, test.X, train.medv01, k = 10)
mean(knn.pred4 != medv01.test)

#we got %19.4 test error rate, we choose when K =10 with subset of variables
#And still we do not get too much inprovement compare to tree model  


