data=read.csv("swineflu.csv")
str(data)
data$Swineflu=as.factor(data$Swineflu)


set.seed(1233)
pd=sample(2,nrow(data),replace = TRUE , prob = c(0.8,0.2))
train=data[pd==1,]
validate=data[pd==2,]

library(party)
tree=ctree(Swineflu~.,data = train, controls=ctree_control(mincriterion=0.99,minsplit=50))
tree
plot(tree)



predict(tree,validate,type="prob")

library(rpart)
tree1=rpart(Swineflu~.,train)
library(rpart.plot)
rpart.plot(tree1,extra=1)


predict(tree1,validate)


#misclassification error with train data
tab=table(predict(tree),train$Swineflu)
print(tab)



1-sum(diag(tab))/sum(tab)


#misclassification error with validate data
testpred=predict(tree,newdata=validate)
tab=table(testpred,validate$Swineflu)
print(tab)


1-sum(diag(tab))/sum(tab)

