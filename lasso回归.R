setwd("C:/周州的数据/PFS预测模型")
data<-read.csv("train.csv",header = TRUE)
library('glmnet')
str(data)
x<-as.matrix(data[,1:32])
y<-as.matrix(data[,33])
set.seed(512)
lasso_model<-glmnet(x,
                    y,
                    family='binomial',
                    alpha=1)
print(lasso_model)
tiff(filename = "lasso.tiff",width = 1800,height=1800,res = 300)
plot(lasso_model,
     xvar='lambda',
     label=F)
dev.off()
#通过调整s,看保留的自变量
coef_lasso<-coef(lasso_model,
                 s=0.02965067)
coef_lasso
#交叉验证选择合适的Lambda
cv_model<-cv.glmnet(x,y,family='binomial',alpha=1,nfolds=10)
tiff(filename = "lasso.tiff",width = 1800,height=1800,res = 300)
plot(cv_model)
dev.off()
#查左边线
lambda_min<-cv_model$lambda.min
lambda_min
#查右边线
lambda_1se<-cv_model$lambda.1se
lambda_1se
