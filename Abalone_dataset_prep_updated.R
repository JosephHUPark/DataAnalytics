################################################
#### Evaluating Classification & CLustering ####
################################################

library("caret")
library(GGally)
library(psych)


library(class)
library(caret)
library(cluster)



## read data
abalone <- read.csv("C:/Users/josep/Downloads/abalone/abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_weight', 'viscera_weight', 'shell_weight', 'rings' ) 

## derive age group based in number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]

## convert class labels to strings
abalone.sub$age.group <- as.character(abalone.sub$age.group)

## convert back to factor
abalone.sub$age.group <- as.factor(abalone.sub$age.group)

## split train/test
train.indexes <- sample(4177,0.7*4177)

train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]

## separate x (features) & y (class labels)
X <- train[,1:7] 
Y <- train[,8]

## features subset
# train <- train[,5:8]
# test <- test[,5:8]

## feature boxplots
boxplot(X, main="abalone features")

## class label distributions
plot(Y)


## feature-class plots
featurePlot(x=X, y=Y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

## psych scatterplot matrix
pairs.panels(X,gap = 0,bg = c("pink", "green", "blue")[Y],pch=21)

## GGally 
ggpairs(train, ggplot2::aes(colour = Y))









## EOF ##

i <- sample(1:nrow(abalone.sub), 0.7*nrow(abalone.sub))
train <- abalone.sub[i,]
test  <- abalone.sub[-i,]

# model 1
X1 <- scale(train[,1:7])
T1 <- scale(test[,1:7], attr(X1,"scaled:center"), attr(X1,"scaled:scale"))
p1 <- knn(X1,T1,train$age.group,k=5)
confusionMatrix(p1,test$age.group)

# model 2
X2 <- scale(train[,c(1,2,3)])
T2 <- scale(test[,c(1,2,3)], attr(X2,"scaled:center"), attr(X2,"scaled:scale"))
p2 <- knn(X2,T2,train$age.group,k=5)
confusionMatrix(p2,test$age.group)

# tune
acc <- sapply(1:20,function(k)
  mean(knn(X1,T1,train$age.group,k)==test$age.group))
which.max(acc)

m1 <- table(p1, test$age.group)
m2 <- table(p2, test$age.group)

image(m1, main="Model 1 Confusion")
image(m2, main="Model 2 Confusion")





X <- scale(abalone.sub[,1:7])

# k-means
sil1 <- sapply(2:8,function(k){
  cl <- kmeans(X,k)$cluster
  mean(silhouette(cl,dist(X))[,3])
})
best_k1 <- which.max(sil1)+1
plot(silhouette(kmeans(X,best_k1)$cluster,dist(X)))

# PAM
sil2 <- sapply(2:8,function(k){
  mean(pam(X,k)$silinfo$widths[,3])
})
best_k2 <- which.max(sil2)+1
plot(silhouette(pam(X,best_k2)))

