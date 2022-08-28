a<-read.csv("C:\\Users\\Niharika\\Desktop\\R programs\\data sets\\brainstroke.csv")
str(a)
ab<- a[,-1]
head(ab)
str(ab)
sum(is.na(ab))
sum(duplicated(ab))
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}


ac<- as.data.frame(lapply(ab[,2:6],normalize))
head(ac)
str(ab)
ab$gender <- factor(ab$gender,levels = c("Male", "Female"), labels = c("0", "1"))

summary(ab)
#feature selection
set.seed(111)
#install.packages("Boruta")
library(Boruta)



#data splicing
set.seed(98)
ad<- sample(1:nrow(ac), size=nrow(ac)*0.8, replace=FALSE)
training_data<-ab[ad,]
testing_data <-ab[-ad,]
training_labels<- ab[ad,7]
testing_labels<- ab[-ad,7]
NROW(ad)
sqrt(399)
Boruta=Boruta(stroke~. , data=training_data, doTrace=2)
print(Boruta)
plot(Boruta, las=2)
plotImpHistory(Boruta)
attStats(Boruta)
getNonRejectedFormula(Boruta)
Train_select= subset(training_data,select=c("age","bmi", "heart_disease", "hypertension"))
Test_select= subset(testing_data,select=c("age","bmi", "heart_disease", "hypertension"))
library(class)
# knn model building
sum(is.na(ab))
knn.train <-knn(train=Train_select,test=Test_select,cl=training_labels,k=20)
#calculating the accuracy of our model
accuracy=100*sum(testing_labels==knn.train)/NROW(testing_labels)
accuracy
library(caret)
library(ggplot2)
library(lattice)
confusionMatrix(table(knn.train,testing_labels))
plot(knn.train)
