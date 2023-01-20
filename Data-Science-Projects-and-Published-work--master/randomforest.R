data=read.csv("swineflu.csv")
str(data)
data$Swineflu=as.factor(data$Swineflu)
table(data$Swineflu)

#data partition
set.seed(123)
ind=sample(2,nrow(data),replace=TRUE,prob = c(0.7,0.3))
train=data[ind==1,]
test=data[ind==2,]


#Random forest
library(randomForest)
set.seed(222)
rf=randomForest(Swineflu~.,data = train,importance=TRUE,proximity=TRUE)
print(rf)
rf$confusion


#Prediction and confusion matrix - train data 
library(caret)
p1=predict(rf,train)
head(p1)
head(train$Swineflu)
confusionMatrix(p1,train$Swineflu)


#Prediction and confusion matrix - test data 
p2=predict(rf,test)
confusionMatrix(p2,test$Swineflu)

#Error rate of random forest
plot(rf)

#Tune mtry
t=tuneRF(train[,-5],train[,5],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 300,
       trace = TRUE,
       improve = 0.05)


#no.of nodes for the trees
hist(treesize(rf),main = "no.of nodes for the trees")

#variable imporatnce
varImpPlot(rf,sort = T,main="variable importance")
importance(rf)
varUsed(rf)

#partial dependence plot
partialPlot(rf,train,Fever,"2")

#extract single tree
getTree(rf,1,labelVar = TRUE)

#multidimensional scaling plot of proximity matrix
MDSplot(rf,train$Swineflu)
########################################
#cross validation in random forest
    
rf.cv = rf.crossValidation(rf, train, p=0.10, n=99)
par(mfrow=c(1,2)) 
plot(rf.cv, type = "cv", main = "CV producers accuracy")
plot(rf.cv, type = "model", main = "Model producers accuracy")

# Plot cross validation verses model oob
par(mfrow=c(1,2)) 
plot(rf.cv, type = "cv", stat = "oob", main = "CV oob error")
plot(rf.cv, type = "model", stat = "oob", main = "Model oob error")	  



