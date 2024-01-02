setwd("C:/周州的数据/PFS预测模型")
library(randomForest)
library(tidymodels)
library('caret')
library(pROC)
library(ROCR)
library(Hmisc)
library(dplyr)
library(mclust)
library(bcrm)
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
          method='rf',
          trControl=fitControl,
          verbose=FALSE,
          metric='ROC',
          tuneLength = 30)
data1<-read.csv("Tlasso.csv",header = TRUE)
data1$class<-factor(data1$class,levels=c(0,1),labels=c("NO",'YES'))
data1$classification<-factor(data1$classification,levels=c(1,2,3,4),labels=c('SQCC','ADC','LCC','SCLC'))
data1$gender<-factor(data1$gender,levels=c(0,1),labels=c('male','female'))
x_test<-data1[,1:10]
y_test<-data1[,11]
data.predict=predict(rf1,newdata=x_test,interval='confidence')
confusionMatrix(data.predict,y_test,positive = 'YES')
pre1<-predict(rf1,newdata=x_test,type='prob')
pred1<-prediction(pre1[,2],y_test)
performance(pred1,'auc')@y.values
auc<-roc(y_test,pre1[,2],levels=c('NO','YES'),direction='<')
auc
####################################################################################################################################3
setwd("C:/周州的数据/PFS预测模型")
library(tidymodels)
library('caret')
library(pROC)
library(ROCR)
library(Hmisc)
library(dplyr)
library(mclust)
library(bcrm)
data2<-read.csv("lasso.csv",header = TRUE)
data2$class<-factor(data2$class,levels=c(0,1),labels=c("NO",'YES'))
data2$classification<-factor(data2$classification,levels=c(1,2,3,4),labels=c('SQCC','ADC','LCC','SCLC'))
data2$gender<-factor(data2$gender,levels=c(0,1),labels=c('male','female'))
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
          method='nnet',
          trControl=fitControl,
          verbose=FALSE,
          metric='ROC',
          tuneLength = 30)
names(getModelInfo())
data1<-read.csv("Tlasso.csv",header = TRUE)
data1$class<-factor(data1$class,levels=c(0,1),labels=c("NO",'YES'))
data1$classification<-factor(data1$classification,levels=c(1,2,3,4),labels=c('SQCC','ADC','LCC','SCLC'))
data1$gender<-factor(data1$gender,levels=c(0,1),labels=c('male','female'))
x_test<-data1[,1:10]
y_test<-data1[,11]
data.predict=predict(rf1,newdata=x_test,interval='confidence')
confusionMatrix(data.predict,y_test,positive = 'YES')
ci_sen<-binconf(9,43)
ci_sen
ci_spe<-binconf(99,111)
ci_spe
pre3<-predict(rf1,newdata=x_test,type='prob')
pred3<-prediction(pre3[,2],y_test)
performance(pred3,'auc')@y.values
auc1<-roc(y_test,pre3[,2],levels=c('NO','YES'),direction='<')
auc1
##################################################################################################################
delong_test<-roc.test(auc,auc1,method='delong')
summary(delong_test)
