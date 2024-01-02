setwd("C:/周州的数据/PFS预测模型")
library(randomForest)
library(tidymodels)
library('caret')
data2<-read.csv("lasso.csv",header = TRUE)
data2$class<-factor(data2$class,levels=c(0,1),labels=c("NO",'YES'))
data2$classification<-factor(data2$classification,levels=c(1,2,3,4),labels=c('SQCC','ADC','LCC','SCLC'))
data2$gender<-factor(data2$gender,levels=c(0,1),labels=c('male','female'))
rf2<-randomForest(class~CRP+BMI+depression+stress+ECOG+neutrophils+gender+age+anxiety+classification,data=data2,importance=TRUE,proximity=TRUE)
x_train<-data2[,1:10]
y_train<-data2[,11]
set.seed(825)
fitControl=trainControl(method='cv',
                        number=10,
                        classProbs = TRUE,
                        summaryFunction=twoClassSummary,
                        search='random'
                        )
rf1=train(x=x_train,y=y_train,
          method='naive_bayes',
          trControl=fitControl,
          verbose=FALSE,
          metric='ROC',
          tuneLength = 30)
names(getModelInfo())

