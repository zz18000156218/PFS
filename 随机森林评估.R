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
#########学习曲线###########################################


#####################################################################################################
library(shiny)
library(tidyverse)
library(caret)
library(randomForest)
library(ROCR)
library(tidyverse)
library(janitor)

ui <- fluidPage(
  titlePanel('高疼痛-疲乏-睡眠障碍症状群的预测模型'),
  sidebarLayout(
    sidebarPanel(
      
      selectInput('gender','性别',choices=c('男性'=0,'女性'=1),selected=0),
      numericInput(inputId = 'age',
                  label='年龄（岁）',
                  min=18,
                  max=100,
                  step=1,
                  value=18),
      numericInput(inputId = 'height',
                   label='身高(m)',
                   min=1.5,
                   max=2,
                   value=1.5),
      numericInput(inputId = 'weight',
                  label='体重(kg)',
                  min=50,
                  max=100,
                  value=50),
      selectInput('classification','肺癌分型',choices=c('鳞癌'=1,'腺癌'=2,'大细胞癌'=3,'小细胞癌'=4),selected=1),
      numericInput(inputId = 'ECOG评分',
                   label='ECOG',
                   min=0,
                   max=4,
                   step=1,
                   value=0),
      numericInput(inputId = 'CRP',
                  label='CRP(mg/L)',
                  min=0,
                  max=100,
                  value=0),
      numericInput(inputId = 'neutrophils',
                  label='中性粒细胞(10^9/L)',
                  min=0,
                  max=20,
                  value=0),
      numericInput(inputId = 'stress',
                  label='感知压力量表评分',
                  min=0,
                  max=56,
                  step=1,
                  value=0),
      numericInput(inputId = 'anxiety',
                  label='HADS（焦虑）',
                  min=0,
                  max=21,
                  step=1,
                  value=0),
      numericInput(inputId = 'depression',
                  label='HADS（抑郁）',
                  min=0,
                  max=21,
                  step=1,
                  value=0),
      actionButton('goButton','预测')
    ),
    mainPanel(
      br(),
      h1('预测结局：'),
      br(),
      h2(strong(textOutput('answer'),style='color:blue')),
      br(),
      br()
    )
  )
)
server<-function(input,output){
  output$answer<-renderPrint({
    data.test<-data.frame(classification=factor(input$classification,levels=c(1,2,3,4),labels=c('SQCC','ADC','LCC','SCLC')),ECOG=input$ECOG,neutrophils=input$neutrophils,CRP=input$CRP,gender=factor(input$gender,levels=c(0,1),labels=c('male','female')),
                          age=input$age,BMI=input$weight/(input$height*input$height),stress=input$stress,anxiety=input$anxiety,depression=input$depression)
    probability1<-predict(rf1,newdata=data.test,type='prob')
    probability<-probability1[,2]
    
    
    
    return(probability)
  })
  

  
}
                     
shinyApp(ui,server)
library(rsconnect)
setAccountInfo(name='high-pain-fatigue-sleep-disturbance',
                          token='23E74E68E7F5FD659E80C372060B1200',
                          secret='mzl1LKGrhbiVUe2FsRtD4p9+lCAdE+1hFX0x4sxF')
deployApp('D:/shinyapp',account = 'high-pain-fatigue-sleep-disturbance')

