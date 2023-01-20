data=read.csv("swineflu.csv",header = TRUE)
str(data)
data$Chill=(data$Chill-min(data$Chill))/(max(data$Chill)-min(data$Chill))
data$Runny2se=(data$Runny2se-min(data$Runny2se))/(max(data$Runny2se)-min(data$Runny2se))
data$Headache=(data$Headache-min(data$Headache))/(max(data$Headache)-min(data$Headache))
data$Fever=(data$Fever-min(data$Fever))/(max(data$Fever)-min(data$Fever))
set.seed(222)
ind=sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
training=data[ind==1,]
testing=data[ind==2,]
library(neuralnet)
set.seed(333)
n=neuralnet(Swineflu~Chill+Runny2se+Headache+Fever,
            data = training,
            hidden=1,
            linear.output = FALSE)
plot(n)

#prediction
output=compute(n,training[,-5])
output
head(output$net.result)
head(training[1,])


#sigmoid fn

in5=4.10201+0+0+2.3435*(0.333333)+2.5941*(1)
in5
out5=1/(1+exp(-in5))
out5

#confusion matrix for training
output1=compute(n,training[,-5])
p1=output1$net.result
pred1=ifelse(p1>0.5,1, 0)
tab=table(pred1,training$Swineflu)
tab
1-sum(diag(tab))/sum(tab)#misclassification error

#summary(output1)
#confusion matrix for testing data
output2=compute(n,testing[,-5])
p2=output2$net.result
pred2=ifelse(p2>0.5,1, 0)
# data[p2<0.5,"predicted"]="no"
# data[p2>0.5,"predicted"]="yes"
tab2=table(pred2,testing$Swineflu)
tab2
1-sum(diag(tab2))/sum(tab2)#misclassification error

#incresing hidden neurons
n1=neuralnet(Swineflu~Chill+Runny2se+Headache+Fever,
            data = training,
            hidden=5,
            linear.output = FALSE)
plot(n1)

#confusion matrix for training
output1=compute(n1,training[,-5])
p1=output1$net.result
pred1=ifelse(p1>0.5,1, 0)
tab=table(pred1,training$Swineflu)
tab
1-sum(diag(tab))/sum(tab)

#confusion matrix for testing data
output2=compute(n1,testing[,-5])
p2=output2$net.result
pred2=ifelse(p2>0.5,1, 0)
tab2=table(pred2,testing$Swineflu)
tab2
1-sum(diag(tab2))/sum(tab2)

#incresing hidden neurons and layers
n2=neuralnet(Swineflu~Chill+Runny2se+Headache+Fever,
             data = training,
             hidden=c(2,2),
             linear.output = FALSE)
plot(n2)

#confusion matrix for training
output1=compute(n2,training[,-5])
p1=output1$net.result
pred1=ifelse(p1>0.5,1, 0)
tab=table(pred1,training$Swineflu)
tab
1-sum(diag(tab))/sum(tab)

#confusion matrix for testing data
output2=compute(n2,testing[,-5])
p2=output2$net.result
pred2=ifelse(p2>0.5,1, 0)
tab2=table(pred2,testing$Swineflu)
tab2
1-sum(diag(tab2))/sum(tab2)

#lifesign and rep
#incresing hidden neurons and layers
n3=neuralnet(Swineflu~Chill+Runny2se+Headache+Fever,
             data = training,
             hidden=c(2,2),
             linear.output = FALSE,
             lifesign = 'full',
             rep=3)
plot(n3)
plot(n3,rep=2)

#confusion matrix for training
output1=compute(n1,training[,-5],rep=2)
p1=output1$net.result
pred1=ifelse(p1>0.5,1, 0)
tab=table(pred1,training$Swineflu)
tab
1-sum(diag(tab))/sum(tab)

#confusion matrix for testing data
output2=compute(n1,testing[,-5],rep=2)
p2=output2$net.result
pred2=ifelse(p2>0.5,1, 0)
tab2=table(pred2,testing$Swineflu)
tab2
1-sum(diag(tab2))/sum(tab2)



predict_testNN = compute(n3, testing[,-5])
predict_testNN = (predict_testNN$net.result * (max(data$Swineflu) - min(data$Swineflu))) + min(data$Swineflu)

plot(data.frame(testing$Swineflu, predict_testNN), col='blue', pch=16, ylab = "predicted rating NN", xlab = "actual")
abline(0,1)
abline(testing$Swineflu,predict_testNN)

###################################################################




RMSE.NN=(sum((testing-predict_testNN)^2)/nrow(testing))^0.5
MSE.lm <- sum((predict_testNN - testing)^2)/nrow(testing)


#cross validation
## Cross validation of neural network model

# install relevant libraries
install.packages("boot")
install.packages("plyr")

# Load libraries
library(boot)
library(plyr)

# Initialize variables
set.seed(50)
k = 5
RMSE.NN = NULL

List = list( )

# Fit neural network model within nested for loop
for(j in 207:299){
  for (i in 1:k) {
    index = sample(1:nrow(data),j )
    
    training = scaled[index,]
    testing = scaled[-index,]
    datatest = data[-index,]
    
    NN = neuralnet(Swineflu ~ ., training, hidden = 3, linear.output= T,data=data)
    predict_testNN = compute(NN,testing[,c(1:5)])
    predict_testNN = (testing$net.result*(max(testing$Swineflu)-min(testing$Swineflu)))+min(testing$Swineflu)
    
    RMSE.NN [i]<- (sum((testing$Swineflu - predict_testNN)^2)/nrow(testing))^0.5
  }
  List[[j]] = RMSE.NN
}

Matrix.RMSE = do.call(cbind, List)

###########################################################################################
predicted=results$prediction * abs(diff(range(consumption))) + min(consumption)
actual=results$actual * abs(diff(range(consumption))) + min(consumption)
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy 





