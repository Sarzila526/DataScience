data_1 <- read.csv("G://portal//Fall 2022-23//Data Science//Project//Dataset_1.csv", header = TRUE, sep = ",")
data_1
str(data_1)
summary(data_1)
names(data_1)
typeof(data_1$Borrower)
typeof(data_1$Loan)
typeof(data_1$Interest_rate)
typeof(data_1$Credit_Score)
dataType <- c(typeof(data_1$Borrower),
              typeof(data_1$Loan),
              typeof(data_1$Interest_rate),
              typeof(data_1$Credit_Score))
dataType

mean_Loan <- mean(data_1$Loan)
median_Loan <- median(data_1$Loan)
mode_Loan <- names(sort(-table(data_1$Loan))) [1]
mean_Rate <- mean(data_1$Interest_rate)
median_Rate <- median(data_1$Interest_rate)
mode_InterestRate <- names(sort(-table(data_1$Interest_rate))) [1]
Measure_of_center <- data.frame(mean_Loan, median_Loan, mode_Loan,
                                mean_Rate, median_Rate, mode_InterestRate)
Measure_of_center

range(data_1$Loan, na.rm = TRUE)
range_Loan <- max(data_1$Loan)-min(data_1$Loan)
range_Loan
range(data_1$Interest_rate, na.rm = TRUE)
range_InterestRate <- max(data_1$Interest_rate)-min(data_1$Interest_rate)
range_InterestRate
s1 <- (data_1$Loan)
sd(s1)
s2 <- (data_1$Interest_rate)
sd(s2)

SD_Loan <- sd(s1)
SD_InterestRate <- sd(s2)
Measure_of_spread <- data.frame(range_Loan, range_InterestRate, 
                                  SD_Loan, SD_InterestRate )
Measure_of_spread

mode_CreditScore <- names(sort(-table(data_1$Credit_Score))) [1]
mode_CreditScore

data_2 <- read.csv("G://portal//Fall 2022-23//Data Science//Project//Dataset_2.csv", header = TRUE, sep = ",")
data_2   
data_2$Type[data_2$Type == ""] <- NA
colSums(is.na(data_2))
data_2$Type[data_2$Type == "hhh"] <- NA
data_2 
data_2$Type <- factor(data_2$Type, levels = c("h","m","l"), labels = c(1,2,3))
data_2
remove_mv <- na.omit(data_2)
remove_mv

data_2.1 <- data_2
mean1 <- mean(data_2.1$Rooms, na.rm = TRUE)
mean1
mean2 <- mean(data_2.1$Price, na.rm = TRUE)
mean2
data_2.1$Rooms[is.na(data_2.1$Rooms)] <- mean1
data_2.1$Price[is.na(data_2.1$Price)] <- mean2
data_2.1

data_2.1 <- data_2
median1 <- median(data_2.1$Rooms, na.rm = TRUE)
median1
median2 <- median(data_2.1$Price, na.rm = TRUE)
median2
data_2.1$Rooms[is.na(data_2.1$Rooms)] <- median1
data_2.1$Price[is.na(data_2.1$Price)] <- median2
data_2.1

data_2.1 <- data_2
mode1 <- names(sort(-table(data_2.1$Rooms))) [1]
mode1
mode2 <- names(sort(-table(data_2.1$Price))) [1]
mode2
data_2.1$Rooms[is.na(data_2.1$Rooms)] <- mode1
data_2.1$Price[is.na(data_2.1$Price)] <- mode2
data_2.1
