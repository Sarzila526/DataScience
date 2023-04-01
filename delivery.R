ds_mid <- read.csv("G://portal//Spring 2022-23//Data Science//Dataset_midterm.csv", header = TRUE, sep = ",")
ds_mid
summary(ds_mid)
str(ds_mid)
attributes <- names(ds_mid)
dataType <- c(typeof(ds_mid$id), typeof(ds_mid$Age), typeof(ds_mid$weight.kg.),
              typeof(ds_mid$Delivery_number), typeof(ds_mid$Delivery_time), 
              typeof(ds_mid$Blood), typeof(ds_mid$Heart), typeof(ds_mid$Caesarian))
data.frame(attributes, dataType)

sum(duplicated(ds_mid))
colSums(is.na(ds_mid))
ds_mid$Blood[ds_mid$Blood == ""] <- NA
colSums(is.na(ds_mid))
dataset_mid <- na.omit(ds_mid)
colSums(is.na(dataset_mid))
str(dataset_mid)
summary(dataset_mid)

ds_mid_copy <- dataset_mid
ds_mid_copy$Blood <- factor(ds_mid_copy$Blood, 
                            levels = c("low", "normal", "high"), 
                            labels = c(1, 2, 3)) 
ds_mid_copy$Heart <- factor(ds_mid_copy$Heart, 
                            levels=c(0,1),
                            labels = c("no","yes"))  
ds_mid_copy$Caesarian<-factor(ds_mid_copy$Caesarian,
                              levels=c(0,1),
                              labels = c("no","yes")) 
ds_mid_copy
str(ds_mid_copy)

data.frame(attributes, dataType)
mean_age <- mean(dataset_mid$Age)
mean_weight <- mean(dataset_mid$weight.kg.)
mean_Dno <- mean(dataset_mid$Delivery_number)
mean_Dt <- mean(dataset_mid$Delivery_time)
mean_heart_Disease <- mean(dataset_mid$Heart)
mean_Caesarian <- mean(dataset_mid$Caesarian)
Measure_of_center_mean <- data.frame(mean_age, mean_weight, mean_Dno, mean_Dt,
                                     mean_heart_Disease, mean_Caesarian)
Measure_of_center_mean

median_age <- median(dataset_mid$Age)
median_weight <- median(dataset_mid$weight.kg.)
median_Dno <- median(dataset_mid$Delivery_number)
median_Dt <- median(dataset_mid$Delivery_time)
median_heart_Disease <- median(dataset_mid$Heart)
median_Caesarian <- median(dataset_mid$Caesarian)
Measure_of_center_median <- data.frame(median_age, median_weight, median_Dno, median_Dt, 
                                       median_heart_Disease, median_Caesarian)
Measure_of_center_median

mode_age <- names(sort(-table(dataset_mid$Age))) [1]
mode_weight <- names(sort(-table(dataset_mid$weight.kg.))) [1]
mode_Dt <- names(sort(-table(dataset_mid$Delivery_number))) [1]
mode_Dno <- names(sort(-table(dataset_mid$Delivery_number))) [1]
mode_BP <- names(sort(-table(dataset_mid$Blood))) [1]
mode_Heart_Disease <- names(sort(-table(dataset_mid$Heart))) [1]
mode_caesarian <- names(sort(-table(dataset_mid$Caesarian))) [1]
Measure_of_center_mode <- data.frame(mode_age, mode_weight, mode_Dno, mode_Dt, mode_BP,
                                     mode_Heart_Disease, mode_caesarian)
Measure_of_center_mode


range(dataset_mid$Age, na.rm = TRUE)
range_Age <- max(dataset_mid$Age)-min(dataset_mid$Age)
range_Age
range(dataset_mid$weight.kg., na.rm = TRUE)
range_weight <- max(dataset_mid$weight.kg.)-min(dataset_mid$weight.kg.)
range_weight
range(dataset_mid$Delivery_number, na.rm = TRUE)
range_Dno <- max(dataset_mid$Delivery_number)-min(dataset_mid$Delivery_number)
range_Dno
range(dataset_mid$Delivery_time, na.rm = TRUE)
range_Dt <- max(dataset_mid$Delivery_time)-min(dataset_mid$Delivery_time)
range_Dt
range(dataset_mid$Heart, na.rm = TRUE)
range(dataset_mid$Caesarian, na.rm = TRUE)
Measure_of_spread_range <- data.frame(range_Age, range_weight, range_Dno, range_Dt)
Measure_of_spread_range


s1 <- (dataset_mid$Age)
s2 <- (dataset_mid$weight.kg.)
s3 <- (dataset_mid$Delivery_number)
s4 <- (dataset_mid$Delivery_time)
s5 <- (dataset_mid$Heart)
s6 <- (dataset_mid$Caesarian)
SD_age <- sd(s1)
SD_weight <- sd(s2)
SD_Dno <- sd(s3)
SD_Dt <- sd(s4)
SD_heart <- sd(s5)
SD_Caesarian <- sd(s6)
Measure_of_spread_Standard_Deviation <- data.frame(SD_age, SD_weight, SD_Dno, SD_Dt,
                                                   SD_heart, SD_Caesarian)
Measure_of_spread_Standard_Deviation


colSums(is.na(ds_mid))
mean_recover <- ds_mid
Measure_of_center_mean
mean_recover$Age[is.na(mean_recover$Age)] <- mean_age
mean_recover$weight.kg.[is.na(mean_recover$weight.kg.)] <- mean_weight
mean_recover$Delivery_number[is.na(mean_recover$Delivery_number)] <- mean_Dno
mean_recover$Delivery_time[is.na(mean_recover$Delivery_time)] <- mean_Dt
mean_recover$Heart[is.na(mean_recover$Heart)] <- mean_heart_Disease
mean_recover$Caesarian[is.na(mean_recover$Caesarian)] <- mean_Caesarian
mean_recover
colSums(is.na(mean_recover))
mean_recover$Blood[is.na(mean_recover$Blood)] <- mode_BP
colSums(is.na(mean_recover))


colSums(is.na(ds_mid))
median_recover <- ds_mid
Measure_of_center_median
median_recover$Age[is.na(median_recover$Age)] <- median_age
median_recover$weight.kg.[is.na(median_recover$weight.kg.)] <- median_weight
median_recover$Delivery_number[is.na(median_recover$Delivery_number)] <- median_Dno
median_recover$Delivery_time[is.na(median_recover$Delivery_time)] <- median_Dt
median_recover$Heart[is.na(median_recover$Heart)] <- median_heart_Disease
median_recover$Caesarian[is.na(median_recover$Caesarian)] <- median_Caesarian
median_recover
median_recover$Blood[is.na(median_recover$Blood)] <- mode_BP
colSums(is.na(median_recover))

colSums(is.na(ds_mid))
mode_recover <- ds_mid
Measure_of_center_mode
mode_recover$Age[is.na(mode_recover$Age)] <- mode_age
mode_recover$weight.kg.[is.na(mode_recover$weight.kg.)] <- mode_weight
mode_recover$Delivery_number[is.na(mode_recover$Delivery_number)] <- mode_Dno
mode_recover$Delivery_time[is.na(mode_recover$Delivery_time)] <- mode_Dt
mode_recover$Blood[is.na(mode_recover$Blood)] <- mode_BP
mode_recover$Heart[is.na(mode_recover$Heart)] <- mode_Heart_Disease
mode_recover$Caesarian[is.na(mode_recover$Caesarian)] <- mode_caesarian
mode_recover
colSums(is.na(mode_recover))
