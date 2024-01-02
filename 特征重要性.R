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
library(ggplot2)
data2<-read.csv("lasso.csv",header = TRUE)
data2$class<-factor(data2$class,levels=c(0,1),labels=c("NO",'YES'))
data2$Classification<-factor(data2$Classification,levels=c(1,2,3,4),labels=c('SQCC','ADC','LCC','SCLC'))
data2$Gender<-factor(data2$Gender,levels=c(0,1),labels=c('male','female'))
rf2<-randomForest(class~CRP+BMI+Depression+Stress+ECOG+Neutrophils+Gender+Age+Anxiety+Classification,data=data2,importance=TRUE,proximity=TRUE)
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
#########################################################################
data1<-read.csv("Tlasso.csv",header = TRUE)
data1$class<-factor(data1$class,levels=c(0,1),labels=c("NO",'YES'))
data1$Classification<-factor(data1$Classification,levels=c(1,2,3,4),labels=c('SQCC','ADC','LCC','SCLC'))
data1$Gender<-factor(data1$Gender,levels=c(0,1),labels=c('male','female'))
x_test<-data1[,1:10]
y_test<-data1[,11]
library(dplyr)
library(fastshap)
windowsFonts(A=windowsFont('Times New Roman'))
shap<-explain(
  rf1,X=as.data.frame(x_train),
  
  nsim=10,
  pred_wrapper=function(model,newdata){
    predict(model,newdata,type='prob')%>%pull(2)
  }
)
shap_imp<-data.frame(
  Variable=names(x_train),
  Importance=apply(shap,MARGIN = 2,FUN = function(x)sum(abs(x)))
)
tiff(filename = "lasso.tiff",width = 3000,height=1800,res = 300)
plot1<-ggplot(shap_imp,aes(reorder(Variable,Importance),Importance))+
  geom_col(width=0.65,color='black',fill='orange')+
  scale_y_continuous(expand = c(0,0,0,1.5))+
  coord_flip()+
  xlab('')+
  ylab('Mean(|Shapley value|)')+
  theme_classic()+
  geom_text(aes(label=format(Importance,digits=3)),hjust=-0.05,size=4)
plot2<-plot1+theme(axis.text=element_text(size=18,family = 'A',color='black'))
plot2+theme(axis.title = element_text(size=18))
dev.off()

