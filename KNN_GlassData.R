install.packages('caTools')  #for train and test data split
install.packages('dplyr')    #for Data Manipulation
install.packages('ggplot2')  #for Data Visualization
install.packages('class')    #KNN 
install.packages('caret')    #Confusion Matrix
install.packages('corrplot') #Correlation Plot
library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(ipred)
library(corrplot)
library(class)
library(e1071)
install.packages("caret",
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Depends", "Imports", "Suggests"))

glass <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data",
                  col.names=c("RI","Na","Mg","Al","Si","K","Ca","Ba","Fe","Type"))

standard.features <- scale(glass[,1:9])
#Join the standardized data with the target column
data <- cbind(standard.features,glass[10])
#Check if there are any missing values to impute. 
anyNA(data)
# Looks like the data is free from NA's
head(data)
summary(data)
boxplot(standard.features)


ggplot(glass,aes(x=RI,y=Al))+geom_point()+geom_smooth()+facet_wrap(~Type)

ggplot(glass,aes(x=Na,y=Fe))+geom_point()+geom_smooth()

ggplot(glass,aes(x=Na,y=RI))+geom_point()+geom_smooth()
corrplot(cor(data))

set.seed(101)
#sample <- sample(nrow(data),nrow(data)*0.70)
#train <- data[sample_index,]
#test <- data[-sample_index,]
sample <- sample.split(data$Type,SplitRatio = 0.70)

train <- subset(data,sample==TRUE)
train_glass_labels <- train$Type

test <- subset(data,sample==FALSE)
test_glass_labes <- test$Type

predicted.type <- knn(train[1:9],test[1:9],train$Type,k=1)
#Error in prediction
error <- mean(predicted.type!=test$Type)
#Confusion Matrix
confusionMatrix(table(predicted.type,test$Type))

predicted.type <- NULL
error.rate <- NULL

for (i in 1:10) {
  predicted.type <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(predicted.type!=test$Type)
  
}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))
ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')

predicted.type <- knn(train[1:9],test[1:9],train$Type,k=3)
#Error in prediction
error <- mean(predicted.type!=test$Type)
#Confusion Matrix
confusionMatrix(table(predicted.type,test$Type))


#Using Naive Bayes
glass_classifier <- naiveBayes(train,train_glass_labels,laplace = 0)
glass_predictor <- predict(glass_classifier,test)
confusionMatrix(table(glass_predictor,test_glass_labes,positive = "Yes"))

glass_classifier_lp1 <- naiveBayes(train_glass,train_glass_labels,laplace = 1)
glass_predictor <- predict(glass_classifier_lp1,test_glass)
confusionMatrix(glass_predictor,test_glass_labes,positive = "Yes")