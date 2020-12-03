library(readxl)
data <- read_excel("C:/Users/yenita/Downloads/diagnosis.xlsx")
View(data)

# One R
library(OneR)
head(data)
str(data)
data$a2<-as.factor(data$a2)
data$a3<-as.factor(data$a3)
data$a4<-as.factor(data$a4)
data$a5<-as.factor(data$a5)
data$a6<-as.factor(data$a6)
data$d1<-as.factor(data$d1)
data$d2<-as.factor(data$d2)
str(data)
head(data)

set.seed(1234)
sampel<-sample(2,nrow(data),replace = T, prob = c(0.8,0.2))
trainingdat<-data[sampel==1, ]
testingdat<-data[sampel==2, ]
print(paste("Jumlah Train Data: ", nrow(trainingdat), "| Jumlah Test Data: ", nrow(testingdat)))

model.OneR <- OneR(d1~., data = trainingdat, verbose = TRUE)
summary(model.OneR)

model.OneR2 <- OneR(d2~., data = trainingdat, verbose = TRUE)
summary(model.OneR2)

library(party)
library(psych)
library(caret)
prediksi <- predict(model.OneR2, testingdat)
confusionMatrix(table(prediksi,testingdat$d1))

pc <- predict(model.OneR, testingdat, type = "class")
eval_model(pc,testingdat)


# Zero R
install.packages("tidyverse")
library(tidyverse)
library(readxl)
data2 <- read_excel("diagnosis.xlsx")
View(data2)
head(data2)
str(data2)
colSums(is.na(data2))
data2$a2<-as.factor(data2$a2)
data2$a3<-as.factor(data2$a3)
data2$a4<-as.factor(data2$a4)
data2$a5<-as.factor(data2$a5)
data2$a6<-as.factor(data2$a6)
data2$d1<-as.factor(data2$d1)
data2$d2<-as.factor(data2$d2)
str(data2)
head(data2)

table(data2[7])
59/120*100

table(data2[8])
50/120*100
