## load library and set seed

library(caret)

library(ggplot2)

library(lattice)

# install.packages("tidyverse")
library(tidyverse)

set.seed(123)



# install.packages("magrittr")
# library(magrittr)

# install.packages("backports")
# library(backports)



## read data: CompleteResponses, SurveyIncomplete from "Files"
responses.df<- read.csv("CompleteResponses.csv" , header = TRUE)
survey.df <- read.csv("SurveyIncomplete.csv" , header = TRUE)


responses.df <- 
  responses.df %>% mutate(elevel=as.factor(elevel),
                          car = as.factor(car),
                          zipcode = as.factor(zipcode),
                          brand= as.factor(brand))

survey.df <- 
  survey.df %>% mutate(elevel=as.factor(elevel),
                       car = as.factor(car),
                       zipcode = as.factor(zipcode),
                       brand= as.factor(brand))

# summary(CompleteResponses)
# summary(SurveyIncomplete)
# typeof(CompleteResponses$brand)


str(responses.df)
str(CompleteResponses)
## skim(CompleteResponses)

str(survey.df)
str(SurveyIncomplete)
# skim(SurveyIncomplete)

## graph 0 acer, 1 sony
hist(CompleteResponses$brand)
# 
hist(responses.df$brand)
responses.df$brand<- as.numeric(responses.df$brand) 
hist(responses.df$brand)


hist(SurveyIncomplete$brand)
# 
hist(survey.df$brand)
survey.df$brand<- as.numeric(survey.df$brand) 
hist(survey.df$brand)


## try  RandomForest and C5.0 to see which is better
## then run predictions on the incomplete data


## Train Test Split
# define an 75%/25% train/test split of the dataset
inTraining<- createDataPartition(responses.df$brand,p=.75, list = FALSE)
training<-responses.df[inTraining,]
testing<- responses.df[-inTraining,]



# fit model
#10 fold cross validation

fitControl <- trainControl(
  method="repeatedcv",
  number = 10,
  repeats = 10)

#install.packages("e1071")
# library(e1071)


## 1. Random Forest

RF_fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

# tuning 
RF_TuneGrid <- expand.grid(mtry = c(1,3,5,7,9))

#not use this one?
RF_Fit1<-train(brand ~ . , data = training ,
               method = 'rf' ,
               trControl = RF_fitControl,
               verbose =FALSE,
               tuneGrid = RF_TuneGrid)

# measure process execution time 
system.time(RF_Fit1<-train(brand ~ . , data = training,
                           method = 'rf' ,
                           trControl = RF_fitControl,
                           verbose =FALSE,
                           tuneGrid = RF_TuneGrid))

# system.time(RF_Fit1<-train(brand ~ . , data = training, method = 'rf', trControl = RF_fitControl,tuneGrid = RF_TuneGrid))

#training results
RF_Fit1

# predict testing 
RF_pred <- predict(RF_Fit1, newdata = testing)

confusionMatrix(data =RF_pred, reference=testing$brand)

# Accuracy and Kappa scores



plot(varImp(RF_Fit1))


## Use on incomplete data - RF predictions

RF_pred_new <-predict(RF_Fit1, newdata = survey.df)

summary(RF_pred_new)


## 2. C5.0 Model
install.packages("C50", dependencies = TRUE)
library(C50)

install.packages("mlbench", dependencies = TRUE )
library(mlbench)

C5_fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

grid<- expand.grid(.window - c(FALSE),.trials =100,.model="tree")

# may skip this?
C5_Fit1<-train(brand ~ . , data = training ,
               method = 'C5.0' ,
               trControl = C5_fitControl,
               verbose =FALSE,
               tuneGrid = Grid)

# may skipy this -same as above
C5_Fit1<-train(brand ~ . , data = training, method = 'C5.0', trControl = C5_fitControl, verbose =FALSE, tuneGrid = Grid)

# might use the one below
System.time(C5_Fit1<-train(brand ~ . , data = training ,
                           method = 'C5.0' ,
                           trControl = C5_fitControl,
                           verbose =FALSE,
                           tuneGrid = Grid))

System.time(C5_Fit1<-train(brand ~ . , data = training, method = 'C5.0', trControl = C5_fitControl, verbose =FALSE, tuneGrid = Grid))

# predict testing
C5_preds<- predict(C5_Fit1, newdata = testing)

confusionMatrix(data = C5_preds , reference = testing$brand)

# Accuracy and Kappa scores


plot(varImp(C5_Fit1))


## Use on incomplete data -c5 predictions
c5_preds_new<- predict(C5_Fit1, newdata = survey.df)

summary(c5_pred_new)



#### the selected classifier is: 

## Use on incomplete data - RF predictions

RF_pred_new <-predict(RF_Fit1, newdata = survey.df)

summary(RF_pred_new)


## Use on incomplete data -c5 predictions
c5_preds_new<- predict(C5_Fit1, newdata = survey.df)

summary(c5_pred_new)



## A chart that displays the customer preference for each brand based on 
## the combination of the actual answers and the predicted answers 
hist(SurveyIncomplete$brand)
