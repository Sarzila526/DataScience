install.packages("caTools")
install.packages("caret")
install.packages("gmodels")
install.packages("class")
library(caTools)
library(caret)
library(gmodels)
library(class)

rw_dataset <- read.csv("G://portal//Fall 2022-23//Data Science//Final Project//winequality-red.csv", header = TRUE, sep = ",") 
rw_dataset
names(rw_dataset)
summary(rw_dataset)
str(rw_dataset)
colSums(is.na(rw_dataset))
sum(duplicated(rw_dataset))
rw_dataset <- unique(rw_dataset)
str(rw_dataset)
head(rw_dataset)
scale(rw_dataset)

norm <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
norm_rw <- as.data.frame(lapply(rw_dataset[,1:11], norm))
str(norm_rw)

split_rw <- sample(1:nrow(norm_rw), size=nrow(norm_rw)*0.8)
train_rw <- rw_dataset[split_rw,] 
test_rw <- rw_dataset[-split_rw,]  
train_rw
test_rw

knn_1 <- knn(train=train_rw, test=test_rw, cl=train_rw$quality, k=3)
knn_2 <- knn(train=train_rw, test=test_rw, cl=train_rw$quality, k=5)

acc.1 <- 100 * sum(test_rw$quality == knn_1)/NROW(test_rw$quality)
acc.2 <- 100 * sum(test_rw$quality == knn_2)/NROW(test_rw$quality)

cn.m_1 <- table(knn_1 ,test_rw$quality)
cn.m_2 <- table(knn_2 ,test_rw$quality)

confusionMatrix(cn.m_1)
confusionMatrix(cn.m_2)




i=1
k.optm=1
for (i in 1:100){
  knn.mod <- knn(train=train_rw, test=test_rw, cl=train_rw$quality, k=i)
  k.optm[i] <- 100 * sum(test_rw$quality == knn.mod)/NROW(test_rw$quality)
  k=i
  cat(k,'=',k.optm[i],'
') }
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")



