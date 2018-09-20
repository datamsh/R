# R 첫번째 실습 수업 : Visualization

install.packages("ggplot2")
install.packages("sqldf")
install.packages("reshape2")

library(ggplot2)
library(sqldf)
library(reshape2)

#used for Uploading excel dataset
install.packages("xlsx")
library(xlsx)

#working directory setup
setwd("C:/3학년/_데이터마이닝")

data.housing_xlsx<- read.xlsx("data/BostonHousing.xlsx", sheetName = "Data")
# data.housing <- read.csv(~~)
head(data.housing)
summary(data.housing)

names(data.housing)[14]<-c("CAT.MEDV")
data.housing$CAT.MEDV <- as.factor(data.housing$CAT.MEDV)
data.housing$LSTAT <- as.factor(data.housing$LSTAT)

count <- table(data.housing$CHAS)
barplot(count)

# bar plot using ggplot2
count.df <-as.data.frame(count)

total <-0
for(i in 1:length(count.df[,2])) total <- total+count.df[i,2]

new_col <-data.frame(ratio=c(count.df[,2]/total))
count.df <-cbind(count.df , new_col)
p <- ggplot(data=count.df, aes(x=Var1, y=ratio))
p + geom_bar(stat="identity")+xlab('CHAT')+ylab('% of VAT.MEDV')
# p <- ggplot(data=count.df, aes(x=Var1, y=ratio)) + geom_bar(stat="identity")+xlab('CHAT')+ylab('% of VAT.MEDV')

# scatter plot
plot(data.housing$LSTAT, data.housing$MEDV, xlab="MEDV", ylab:"LSTAT")
ggplot(data=data.housing, aes(x=LSTAT,y=NOX, color=CAT.MEDV))+geom_point()

#Histogram
hist(data.housing$MEDV, xlim=range(data.housing$MEDV), xlab="MEDV")

# Box plot
data.housing$CHAS = as.factor(data.housing$CHAS)
g <- ggplot(data.housing, aes(x=data.housing$CHAS,y=data.housing$MEDV))
g + geom_boxplot()

# HeatMap
cor_mat <-round(cor(data.housing_xlsx),2)
cor_mat[upper.tri(cor_mat)]<-NA

cor_mat_melt <- melt(cor_mat)
g <- ggplot(data=cor_mat_melt, aes(x=Var1, y=Var2, fill=value))+ geom_tile()
g + geom_text(aes(label=value))

#Matrix plot
pairs(data.housing[,1:3])
#using GGally
install.packages("GGally")
library(GGally)
ggpairs(data.housing[,c(1:3,13)])

###########
# Aggregation for Amtrak Passengers
am_data<- read.xlsx("data/AmtrakPassengersMonthly T-Competition.xlsx", sheetName= "Data")

# change the column name
names(am_data)[1] <- "date"
names(am_data)[2] <- "passenger"

#Split data into year, month, day
x <- NULL
x <- gsub("-", " ",am_data$date)
x <- strsplit(x, " ")
x <- t(matrix(unlist(x), 3, 159))

x_frame <- data.frame(x)
names(x_frame)[1] <- "year"
names(x_frame)[2] <- "month"
names(x_frame)[3] <- "day"

# Merge the data
amtrak_data <- cbind(am_data, x_frame)
am_month_traffic <- sqldf("select month, passenger from amtrak_data group by month")
am_year_traffic <- sqldf("select year, passenger from amtrak_data group by year")

# plot
g <- ggplot(data=am_month_traffic, aes(x=am_month_traffic$month, y=am_month_traffic$passenger, group=1))
g+geom_line()

g <- ggplot(data=am_year_traffic, aes(x=am_year_traffic$year, y=am_year_traffic$passenger, group=1)) +geom_line()
g

