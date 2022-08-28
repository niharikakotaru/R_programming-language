library(caret)
data(iris)
iris<- iris
head(iris)
# Add column names
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
# Check the result
iris
library(caTools)
sample.split(iris$species,SplitRatio = 0.80)->split_index
subset(iris,split_index==T)->train
subset(iris,split_index==F)->test
knn(species~.,data = train)->knn_model
predict(Species,test)-> knntest_model
cbind(Actual=test$Species,Predicted=result_regress)->Final_Data 
as.data.frame(Final_Data)->Final_Data