#install.packages('h2o')
#library(h2o)
#h2o.init()
h2o.shutdown()

##########
head(dtr)
str(dtr)
summary(dtr)

plot(dtr[c("s", "g")], main = "Sales X SKUs")

d <- scale(dtr)


##########
wss <- (nrow(d)-1)*sum(apply(d,2,var))

for(i in 1:25){wss[i] <- sum(kmeans(d, centers=i, iter.max = 1000)$withinss)}

plot(1:25, wss, type="b", xlab="No. of Clusters", ylab="wss")

wss


##########
#hd <- as.h2o(d)
c14 <- kmeans(d, centers = 14, nstart = 100, iter.max = 1000)
c14

c20 <- kmeans(d, centers = 20, nstart = 250, iter.max = 1000)
c20

c22 <- kmeans(d, centers = 22, nstart = 250, iter.max = 1000)
c22

c15 <- kmeans(d, centers = 15, nstart = 100, iter.max = 1000)
c15

aggregate(d, by=list(c15$cluster), FUN = mean)
#ddf <- as.data.frame(c15)
#ddf <- data.frame(d,c15)
