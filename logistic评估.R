setwd("C:/周州的数据/PFS预测模型")
library('caret')
library(pROC)
library(ROCR)
library(Hmisc)
library(dplyr)
library(mclust)
library(bcrm)
data1<-read.csv("lasso.csv",header = TRUE)
Tdata1<-read.csv("Tlasso.csv",header = TRUE)
data1$class<-factor(data1$class,levels=c(0,1),labels=c("NO",'YES'))
data1$classification<-factor(data1$classification,levels=c(1,2,3,4),labels=c('SQCC','ADC','LCC','SCLC'))
data1$gender<-factor(data1$gender,levels=c(0,1),labels=c('male','female'))
str(data1)
Tdata1$class<-factor(Tdata1$class,levels=c(0,1),labels=c("NO",'YES'))
Tdata1$classification<-factor(Tdata1$classification,levels=c(1,2,3,4),labels=c('SQCC','ADC','LCC','SCLC'))
Tdata1$gender<-factor(Tdata1$gender,levels=c(0,1),labels=c('male','female'))
x_test<-Tdata1[,1:10]
y_test<-Tdata1[,11]
str(data1)
library(ggplot2)
set.seed(512)
fit2<-glm(class~CRP+BMI+depression+stress+classification+ECOG+neutrophils+gender+age+anxiety,data=data1,family=binomial(),x=T,y=T)
prob2<-predict(fit2,newdata=x_test,type='response')
library(ROCR)
pred2<-prediction(prob2,y_test)
performance(pred2,'auc')@y.values
confusionMatrix(data.predict,y_test,positive = 'YES')
ci_sen<-binconf(11,43)
ci_sen
ci_spe<-binconf(111,154)
ci_spe
pre3<-predict(fit2,newdata=x_test,type='link')
pred3<-prediction(pre3,y_test)
performance(pred3,'auc')@y.values
auc1<-roc(y_test,pre3,levels=c('NO','YES'),direction='<')
auc1
auc2<-ci.auc(auc1)
auc2
cutoff=auc1$thresholds[which.max(auc1$sensitivities+auc1$specificities)]
library(Metrics)
brier_score(y_test,prob2)

