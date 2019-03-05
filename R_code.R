install.packages("rpart.plot")
install.packages("rpart")

data <- read.csv("deadfinalpre.csv",header = TRUE)
data

accu<- function(m){sum(diag(m)/sum(m))}
s.no<-sample(nrow(data), nrow(data) * 0.8)
train.data1 <- data[s.no,]
test.data1 <- data[-s.no,]

#Decision Tree
library(rpart)
library(rpart.plot)

model.tree <- rpart(n~ . , data = train.data1)
rpart.plot(model.tree)

pre.tree <- predict( model.tree, test.data1[,-5], type = "class")
accu(table(test.data1$n, predict=pre.tree))
table(real=test.data1$n, predict=pre.tree)
     