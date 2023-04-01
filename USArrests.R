data_1 <- read.csv("G://portal//Fall 2022-23//Data Science//Practice//USArrests.csv", header = TRUE, sep = ",")
data_1
str(data_1)
scale(data_1)
data = scale(data_1[,-1])
k <- fviz_nbclust(data, kmeans, method="wss")
install.packages("ClusterR")
install.packages("factoextra")
library(ClusterR)
library(factoextra)
k
km <- kmeans(data, centers = 4, nstart = 25)
km
df_member <- cbind(data_1, cluster= km$cluster)
aggregate(data_1, by+list(cluster = km$cluster), mean)
df_member
