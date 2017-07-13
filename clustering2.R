#install.packages('h2o')
#library(h2o)
#h2o.init()
h2o.shutdown()

Data <- readr::read_csv('dtr.csv')

##########
head(Data)
str(Data)
summary(Data)

plot(Data[,1:20], main = "Sales X SKUs")

d <- Data[,c(1:20)]

##########
wss <- (nrow(d)-1)*sum(apply(d,2,var))

for(i in 1:25){wss[i] <- sum(kmeans(d, centers=i, iter.max = 1000)$withinss)}

plot(1:25, wss, type="b", xlab="No. of Clusters", ylab="wss")

wss


##########
#hd <- as.h2o(d)

Cluster <- kmeans(d, centers = 13, nstart = 100, iter.max = 1000)
#Cluster <- kmeans(d, centers = 20, nstart = 250, iter.max = 1000)
#Cluster <- kmeans(d, centers = 22, nstart = 250, iter.max = 1000)
#Cluster <- kmeans(d, centers = 15, nstart = 100, iter.max = 1000)

agg14 <- aggregate(d, by=list(c14$cluster), FUN = mean)
str(agg14)

Data$Cluster <- Cluster$cluster
#ddf <- as.data.frame(c15)
#ddf <- data.frame(d,c15)


library(nnet)

mod <- multinom(Cluster ~ ., data = Data)
pred <- predict(mod,Data,"probs")
Pred <- round(pred, digits = 4)
