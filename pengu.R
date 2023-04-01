install.packages("caTools")
install.packages("caret")
install.packages("gmodels")
install.packages("class")
library(caTools)
library(caret)
library(gmodels)
library(class)

pengu_data <- read.csv("G://portal//Fall 2022-23//Data Science//Practice//penguins.csv", header = TRUE, sep = ",") 
pengu_data
names(pengu_data)
summary(pengu_data)
str(pengu_data)

sum(duplicated(pengu_data))
colSums(is.na(pengu_data))
rm <- na.omit(pengu_data)
colSums(is.na(rm))
pengu_dataset <- rm
str(pengu_dataset)
scale(pengu_dataset[,4:7])

norm <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
norm_pengu <- as.data.frame(lapply(pengu_dataset[,4:7], norm))
str(norm_pengu)

split_pengu <- sample(1:nrow(norm_pengu), size=nrow(norm_pengu)*0.8)
train_pengu <- pengu_dataset[split_pengu,] 
test_pengu <- pengu_dataset[-split_pengu,] 
train_pengu
test_pengu

train_ta_p1 <- pengu_dataset[split_pengu,2] 
test_ta_p1 <- pengu_dataset[-split_pengu,2] 
train_ta_p2 <- pengu_dataset[split_pengu,3] 
test_ta_p2 <- pengu_dataset[-split_pengu,3] 

knn_p1 <- knn(train=train_pengu, test=test_pengu, cl=train_ta_p1, k=5)
knn_p2 <- knn(train=train_pengu, test=test_pengu, cl=train_ta_p2, k=5)

acr_p1 <- 100 * sum(test_pengu$species == knn_p1)/NROW(test_ta_p1)
cn.m_p1 <- table(knn_p1 ,test_ta_p1)
confusionMatrix(cn.m_p1)



i=1
k.optm=1
for (i in 1:100){
  knn.mod <- knn(train=train_rw, test=test_rw, cl=train_rw$quality, k=i)
  k.optm[i] <- 100 * sum(test_rw$quality == knn.mod)/NROW(test_rw$quality)
  k=i
  cat(k,'=',k.optm[i],'
') }
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")



