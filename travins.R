rm(list=ls())

library(pROC)
library(caTools)
library(car)
library(corrplot)


df = read.csv("C:/Users/sambi/OneDrive/Documents/Travel Insurance/TravelInsurancePrediction.csv")
df = df[,-1]
View(df)


boxplot(df$Age~df$TravelInsurance)


#use 75% of dataset as training set and 25% as test set
set.seed(48)
index = 1:nrow(df)
rs = sample(index, size = 0.75*nrow(df), replace = F)
train = df[rs,]
test = df[-rs,]


L1 = glm(TravelInsurance~.-TravelInsurance, data = train, family = binomial(link = "logit"))
summary(L1)

L2 = glm(TravelInsurance~.-TravelInsurance-ChronicDiseases-GraduateOrNotYes, data = train, family = binomial(link = "logit"))
summary(L2)




modelled_y_log=fitted.values(L1);modelled_y_log
threshold_log=seq(min(modelled_y_log)+0.001,max(modelled_y_log)-0.001,0.001)
FPR_log=TPR_log=array(0)
cutpoint_log=array(0)
i = 1
for (i in 1:length(threshold_log)) #Here we will get the Confusion Matrix along with all the necessary values
{
  y_hat=ifelse(modelled_y_log>threshold_log[i],1,0)
  m=table(y_hat, train$TravelInsurance)
  
  if(nrow(m) == 1){
    m = rbind(c(0,0), m)
    rownames(m) = c(0,1)
  }
  TPR_log[i]=m[2,2]/(m[2,1]+m[2,2])
  Sensitivity=TPR_log[i]
  FPR_log[i]=m[1,2]/(m[1,1]+m[1,2])
  Specificity=1-FPR_log[i]
  cutpoint_log[i]=Sensitivity*Specificity
}
final_log=data.frame(threshold_log,cutpoint_log);final_log
selected_cutoff_log=final_log[final_log$cutpoint_log==max(cutpoint_log),]
selected_cutoff_log=mean(selected_cutoff_log[,1])

selected_cutoff_log


plot(threshold_log,cutpoint_log,type="l",lwd=3,main="Plot for Logit Model",
     xlab="Threshold",ylab="Specificity*Sensitivity")
abline(h=max(cutpoint_log),v=selected_cutoff_log)
mtext(selected_cutoff_log,side=1,adj=selected_cutoff_log)
mtext(round(max(cutpoint_log),4),side=2,adj=0.99)



y_test=ifelse(modelled_y_log>selected_cutoff_log,1,0)
m1=table(train$TravelInsurance,y_test)
Accuracy_log=(m1[1,1]+m1[2,2])/length(train$Age)
Accuracy_log 



#ROC Curve
par(mfrow=c(1,1))

plot((1-FPR_log), TPR_log, type = 'l')
abline(a=0,b=1)
legend("bottomright",c("Logit Model","Probit Model"),lty=c(1,2),lwd=2,
       col=c(1,2))
#Area Under the Curve
auc(train$TravelInsurance,modelled_y_log)


pred = predict.glm(L1, newdata = test, type = 'response')
pred_class = ifelse(pred > selected_cutoff_log, 1,0)
table