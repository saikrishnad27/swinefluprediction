library(frbs)
iris=read.csv("swineeflu.csv")
iris

irisShuffled <- iris[sample(nrow(iris)), ]
irisShuffled[, 5] <- unclass(irisShuffled[, 5])
range.data.input <- apply(iris[, -ncol(iris)], 2, range) 
tra.iris <- irisShuffled[1:40, ]
tst.iris <- irisShuffled[41:nrow(irisShuffled), 1:4]
a<- irisShuffled[41:nrow(irisShuffled), ]
real.iris <- matrix(irisShuffled[41:nrow(irisShuffled), 5], ncol = 1)
object.frbcs.w <- frbs.learn(tra.iris, range.data.input,  method.type = "FRBCS.W", control = list(num.labels = 3, type.mf = "TRAPEZOID"))
summary(object.frbcs.w)
plotMF(object.frbcs.w)

pred <- predict(object.frbcs.w, tst.iris)
pred
err <- 100 * sum(pred != real.iris) / length(pred) 
err

