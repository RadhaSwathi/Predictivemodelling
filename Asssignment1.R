#Load data
library(ISLR)
library(ggcorrplot)
View(College)
data(College)
#univariate analysis
summary(College) #no values missing
corrdata<-round(cor(College[-1]),2)
corrplot.mixed(corrdata, title = "Correlation matrix for test data")
ggcorrplot(corrdata, type='full',lab=TRUE)


#Histogram
par(mfrow=c(1,1))
hist(College$Grad.Rate)
#Pairwise correlation
pairs(round(cor(CollegeCont[-1]),2))
summary(CollegeCont)
set.seed(75)
n=nrow(College)
trainIndex<-sample(1:n, size = round(0.8781*n), replace=FALSE)
traindata<-College[trainIndex,]
testdata<-College[-trainIndex,]

summary(traindata)

nullfit<-lm(Grad.Rate~1,data= traindata)
highfit<-lm(Grad.Rate~.,data= traindata)
#Forward model
train.fitFwd<-step(nullfit, scope=list(lower=nullfit, upper=highfit), direction="forward")
summary(train.fitFwd)
anova(train.fitFwd)
##Step:  AIC=3451.61
##Grad.Rate ~ Outstate + Top25perc + perc.alumni + P.Undergrad +Apps + Room.Board + Expend + Private + Personal
#Residual standard error: 12.75 on 666 degrees of freedom
#Multiple R-squared:  0.4658,	Adjusted R-squared:  0.4586 
#F-statistic: 64.52 on 9 and 666 DF,  p-value: < 2.2e-16

model1<-lm(Grad.Rate ~ Outstate + Top25perc + perc.alumni + P.Undergrad +Apps + Room.Board + Expend + Private+ Personal ,data= traindata)
summary(model1)
plot(model1)

#backward model
train.fitback<-step(highfit, scope=list(lower=nullfit, upper=highfit), direction="backward")

##AIC=3451.61
##Grad.Rate ~ Private + Apps + Top25perc + P.Undergrad + Outstate + Room.Board + Personal + perc.alumni + Expend
##Residual standard error: 12.75 on 666 degrees of freedom
##Multiple R-squared:  0.4658,	Adjusted R-squared:  0.4586 
##F-statistic: 64.52 on 9 and 666 DF,  p-value: < 2.2e-16
model2<-lm(Grad.Rate ~ Private + Apps + Top25perc + P.Undergrad + Outstate + Room.Board + Personal + perc.alumni + Expend,data=traindata)
summary(model2)
anova(model2)

#stewise model
trainstep <-step(nullfit, list(lower=nullfit,upper=highfit,direction="both"))
#Grad.Rate ~ Outstate + Top25perc + perc.alumni + P.Undergrad + Apps + Room.Board + Expend + Private + Personal
#AIC=3451.61
model3<-lm(Grad.Rate ~ Outstate + Top25perc + perc.alumni + P.Undergrad + Apps + Room.Board + Expend + Private + Personal,data=traindata)
summary(model3)
#Residual standard error: 12.75 on 666 degrees of freedom
#Multiple R-squared:  0.4658,	Adjusted R-squared:  0.4586 
#F-statistic: 64.52 on 9 and 666 DF,  p-value: < 2.2e-16
library(leaps)
grad=regsubsets(Grad.Rate~., data=traindata, nbest=9)
grad.sum<-summary(grad)
names(grad.sum)
grad.sum$adjr2
#################RATIOS##############################
summary(College) 
CollegeCont<-College[c(-2,-3,-4)]
appenrollrt<-College$Enroll/College$Apps
appnaccprrt<-College$Accept/College$Apps
summary(CollegeCont) 
CollegeCont<-cbind(CollegeCont,appenrollrt,appnaccprrt)
c<-cor(CollegeCont[-1])
ggcorrplot(c, type='full',lab=TRUE)
set.seed(75)
nWI=nrow(CollegeCont)
trainIndexWI<-sample(1:nWI, size = round(0.87*nWI), replace=FALSE)
traindataWI<-College[trainIndexWI,]
testdataWI<-College[-trainIndexWI,]

nullfitWI<-lm(Grad.Rate~1,data= traindataWI)
highfitWI<-lm(Grad.Rate~.,data= traindataWI)


#Forward model
trainWI.fitFwd<-step(nullfitWI, scope=list(lower=nullfitWI, upper=highfitWI), direction="forward")
#Step:  AIC=3451.61
#Grad.Rate ~ Outstate + Top25perc + perc.alumni + P.Undergrad + Apps + Room.Board + Expend + Private + Personal
summary(trainWI.fitFwd)
anova(trainWI.fitFwd)

modelWI1<-lm(formula = Grad.Rate ~ Outstate + Top25perc + perc.alumni + 
               P.Undergrad + Apps + Room.Board + Expend + Private + Personal, 
             data = traindataWI)

summary(modelWI1)
anova(modelWI1)

##Backward
trainWI.fitback<-step(highfitWI, scope=list(lower=nullfitWI, upper=highfitWI), direction="backward")
#Step:  AIC=3451.61
#Grad.Rate ~ Private + Apps + Top25perc + P.Undergrad + Outstate + Room.Board + Personal + perc.alumni + Expend
summary(trainWI.fitback)
anova(trainWI.fitback)

#Stepwise
#Step:  AIC=3451.61
#Grad.Rate ~ Outstate + Top25perc + perc.alumni + P.Undergrad + Apps + Room.Board + Expend + Private + Personal
trainstepWI <-step(nullfitWI, list(lower=nullfitWI,upper=highfitWI,direction="both"))

summary(trainstepWI)
anova(trainstepWI)
library(car)
round(vif(trainstepWI),2)


mm<-lm(1/(Grad.Rate) ~  1/(Top25perc + perc.alumni + P.Undergrad + Apps + Room.Board + Expend + Private + Personal), data=traindataWI)
summary(mm)
anova(mm)
plot(mm)

ff=regsubsets(Grad.Rate~., data=traindataWI, nbest=8)
plot(ff, scale="adjr2")
plot(ff, scale="bic")



#To predict couple's choice using Contraceptive Method Choice data from UIC website
#There are 1473 instances with 10 attributes

#Here the data preparation follows the below steps.

#Attribute Information:


#1. WifeAge - Wife's age (numerical) 
#2. WifeEdu - Wife's education (categorical) 1=low, 2, 3, 4=high 
#3. HusEdu - Husband's education (categorical) 1=low, 2, 3, 4=high 
#4. Childerborn - Number of children ever born (numerical) 
#5. Wifereligion - Wife's religion (binary) 0=Non-Islam, 1=Islam 
#6. Wifeemp - Wife's now working? (binary) 0=Yes, 1=No 
#7. HusOccupation - Husband's occupation (categorical) 1, 2, 3, 4 
#8. Stdlivingindex - Standard-of-living index (categorical) 1=low, 2, 3, 4=high 
#9. MediaExp - Media exposure (binary) 0=Good, 1=Not good 
#10.CMethodUsed - Contraceptive method used (class attribute) 1=No-use, 2=Long-term, 3=Short-term

#Modelling on sample 1 
#In this data the target variable(CMethodUsed) is made binary by combining 1 and 2 categories 

library(readxl)
CoupleSample1 <- read_excel("cmc.csv")
View(CoupleSample1)

summary(CoupleSample1)
str(CoupleSample1)
#pairwise correlation excluding Private column
round(cor(CoupleSample1),2)

#converting variables into factors where it can better serve us as labeled factors.

CoupleSample1$WifeEdu <- factor(CoupleSample1$WifeEdu, labels = c("low", "mid_low", "mid_high", "high"))
CoupleSample1$HusEdu <- factor(CoupleSample1$HusEdu, labels = c("low", "mid_low", "mid_high", "high"))
CoupleSample1$Stdlivingindex <- factor(CoupleSample1$Stdlivingindex, labels = c("low", "mid_low", "mid_high", "high"))


#Converting binary to boolean values

CoupleSample1$Wifereligion <- ifelse(CoupleSample1$Wifereligion == 1, TRUE, FALSE)
CoupleSample1$Wifeemp <- ifelse(CoupleSample1$Wifeemp == 1, FALSE, TRUE)
CoupleSample1$MediaExp <- ifelse(CoupleSample1$MediaExp == 1, FALSE, TRUE)

#Converting other categorical values to factors

CoupleSample1$HusOccupation <- as.factor(CoupleSample1$HusOccupation)



#Model Training on CoupleSample1

n = nrow(CoupleSample1)
set.seed(28)

#Dividing sample 1 into test and training data in the ratio 75:25
coupleIndex = sample(1:n, size = round(0.75*n), replace=FALSE)

#Obtaining training data from sample 1
traincouple1 = CoupleSample1[coupleIndex ,]

#Obtaining Test data from sample 1
testcouple1 = CoupleSample1[-coupleIndex ,]

#To check ratio of records with 0 and 1 in train data
table(traincouple1$CMethodUsed,traincouple1$CMethodUsed>0)

#To check ratio of records with 0 and 1 in test data
table(testcouple1$CMethodUsed,testcouple1$CMethodUsed>0)

#Forward selection regression method 

fiti.couple1 <- glm(CMethodUsed~ 1, data=traincouple1, family=binomial(link=logit))
fita.couple1 <- glm(CMethodUsed~ ., data=traincouple1, family=binomial(link=logit))

fitfwd.couple1 <- step(fiti.couple1, scope=list(lower=fiti.couple1, upper=fita.couple1), 
                       direction="forward")
summary(fitfwd.couple1)

#As Wifereligion is not significant in the  model obtained by forward selection 
#and both way selection below is the model excluding Wifereligion
fitfwd1.couple1<- glm(formula = CMethodUsed ~ WifeAge + Childerborn + HusEdu + 
                        MediaExp , family = binomial(link = logit), 
                      data = traincouple1)

summary(fitfwd1.couple1)

#Backward selection regression model 

fitbck.couple1 = step(fita.couple1)
summary(fitbck.couple1)

#After removing non significant predictors below model has been obtained.
fitbck1.couple1<-glm(formula = CMethodUsed ~ WifeAge + HusEdu + Childerborn
                     , family = binomial(link = logit), 
                     data = traincouple1)

summary(fitbck1.couple1)

#Both way selection model

fitbtw.couple1 = step(fiti.couple1, list(lower=formula(fiti.couple1),upper=formula(fita.couple1)),
                      direction="both", trace=0)
summary(fitbtw.couple1)



library(pROC)
library(ROCR)
library(DescTools)
library(ResourceSelection)


#Model fitfwd1.couple1 
roc(traincouple1$CMethodUsed, fitfwd1.couple1$fitted.values)
plot.roc(traincouple1$CMethodUsed, fitfwd1.couple1$fitted.values)
PseudoR2(fitfwd1.couple1)
hoslem.test(traincouple1$CMethodUsed,fitfwd1.couple1$fitted.values,g=10)
table(traincouple1$CMethodUsed,fitfwd1.couple1$fitted.values>0.5)

#fitfwd1.couple1 Model testing on test data testcouple1.
testpred11 <- predict(fitfwd1.couple1, newdata = testcouple1, type = "response")
pred_cmd11 <- ifelse(testpred11 > 0.5, 1, 0)
pred_cmd11<-as.factor(pred_cmd11)
summary(pred_cmd11)
precouple11 <- factor(pred_cmd11, levels=c(0, 1))
actcouple11 <- testcouple1$CMethodUsed
mean(precouple11 == actcouple11)

#Model fitbck1.couple1 
roc(traincouple1$CMethodUsed, fitbck1.couple1$fitted.values)
PseudoR2(fitbck1.couple1)
hoslem.test(traincouple1$CMethodUsed,fitbck1.couple1$fitted.values,g=10)
table(traincouple1$CMethodUsed,fitbck1.couple1$fitted.values>0.5)

#fitbck1.couple1 Model testing on test data testcouple1.
testpred1 <- predict(fitbck1.couple1, newdata = testcouple1, type = "response")
pred_cmd1 <- ifelse(testpred1 > 0.5, 1, 0)
pred_cmd1<-as.factor(pred_cmd1)
precouple1 <- factor(pred_cmd1, levels=c(0, 1))
actcouple1 <- testcouple1$CMethodUsed
table(precouple1,actcouple1)
mean(precouple1 == actcouple1)

#Modelling on sample 2 
#In this data the target variable(CMethodUsed) is made binary by combining 2 and 3 categories 


CoupleSample2 <- read_excel("H:/Predictive modelling using R/MiniProject/ContraceptiveSample2.xlsx")
View(CoupleSample2)


summary(CoupleSample2)
str(CoupleSample2)
#pairwise correlation excluding Private column
round(cor(CoupleSample2),2)

#converting variables into factors where it can better serve us as labeled factors.

CoupleSample2$WifeEdu <- factor(CoupleSample2$WifeEdu, labels = c("low", "mid_low", "mid_high", "high"))
CoupleSample2$HusEdu <- factor(CoupleSample2$HusEdu, labels = c("low", "mid_low", "mid_high", "high"))
CoupleSample2$Stdlivingindex <- factor(CoupleSample2$Stdlivingindex, labels = c("low", "mid_low", "mid_high", "high"))


#Converting binary to boolean values

CoupleSample2$Wifereligion <- ifelse(CoupleSample2$Wifereligion == 1, TRUE, FALSE)
CoupleSample2$Wifeemp <- ifelse(CoupleSample2$Wifeemp == 1, FALSE, TRUE)
CoupleSample2$MediaExp <- ifelse(CoupleSample2$MediaExp == 1, FALSE, TRUE)

#Converting other categorical values to factors

CoupleSample2$HusOccupation <- as.factor(CoupleSample2$HusOccupation)



#Model Training on CoupleSample2

n = nrow(CoupleSample2)
set.seed(28)

#Dividing sample 1 into test and training data in the ratio 75:25
coupleIndex = sample(1:n, size = round(0.75*n), replace=FALSE)

#Obtaining training data from sample 1
traincouple2 = CoupleSample2[coupleIndex ,]

#Obtaining Test data from sample 1
testcouple2 = CoupleSample2[-coupleIndex ,]

#To check ratio of records with 0 and 1 in train data
table(traincouple2$CMethodUsed,traincouple2$CMethodUsed>0)

#To check ratio of records with 0 and 1 in test data
table(testcouple2$CMethodUsed,testcouple2$CMethodUsed>0)

#Forward selection regression method 

fiti2.couple1 <- glm(CMethodUsed~ 1, data=traincouple2, family=binomial(link=logit))
fita2.couple1 <- glm(CMethodUsed~ ., data=traincouple2, family=binomial(link=logit))

fitfwd.couple2 <- step(fiti2.couple1, scope=list(lower=fiti2.couple1, upper=fita2.couple1), direction="forward")
summary(fitfwd.couple2)

#As Wifereligion is not significant in the  model obtained by forward selection 
#and both way selection below is the model excluding Wifereligion
fitfwd1.couple2<- glm(formula = CMethodUsed ~ WifeAge + Childerborn + 
                        MediaExp + Stdlivingindex + Wifereligion, 
                      family = binomial(link = logit), 
                      data = traincouple2)
summary(fitfwd1.couple2)

#Backward selection regression model 

fitbck.couple2 = step(fita2.couple1)
summary(fitbck.couple2)

fitbck1.couple2<-glm(formula = CMethodUsed ~ WifeAge +  Childerborn + Wifereligion +
                       MediaExp , family = binomial(link = logit), 
                     data = traincouple2)

summary(fitbck1.couple2)

#Both way selection model

fitbtw.couple2 = step(fiti2.couple1, list(lower=formula(fiti2.couple1),upper=formula(fita2.couple1)),
                      direction="both", trace=0)
summary(fitbtw.couple2)

#After removing non significant variables from the model obtained from both way selection
fitbtw1.couple1<-glm(formula = CMethodUsed ~ WifeAge +  Childerborn + Wifereligion +
                       MediaExp , family = binomial(link = logit), 
                     data = traincouple2)
summary(fitbtw1.couple1)

library(pROC)
library(ROCR)
library(DescTools)
library(ResourceSelection)

#Model fitfwd1.couple2 

roc(traincouple2$CMethodUsed, fitfwd1.couple2$fitted.values)
PseudoR2(fitfwd1.couple2)
hoslem.test(traincouple2$CMethodUsed,fitfwd1.couple2$fitted.values,g=10)
table(traincouple2$CMethodUsed,fitfwd1.couple2$fitted.values>0.5)

#fitfwd.couple2 Model testing on test data testcouple2.

testpred2 <- predict(fitfwd1.couple2, newdata = testcouple2, type = "response")
pred_cmd2 <- ifelse(testpred1 > 0.5, 1, 0)
pred_cmd2<-as.factor(pred_cmd1)
precouple2 <- factor(pred_cmd1, levels=c(0, 1))
actcouple2 <- testcouple2$CMethodUsed
mean(precouple2 == actcouple2)

#Model fitbck1.couple2 

roc(traincouple2$CMethodUsed, fitbck1.couple2$fitted.values)
PseudoR2(fitbck1.couple2)
hoslem.test(traincouple2$CMethodUsed,fitbck1.couple2$fitted.values,g=10)
table(traincouple2$CMethodUsed,fitbck1.couple2$fitted.values>0.5)

#fitbck1.couple2 Model testing on test data testcouple2.

testpred12 <- predict(fitbck1.couple2, newdata = testcouple2, type = "response")
pred_cmd12 <- ifelse(testpred12 > 0.5, 1, 0)
pred_cmd12<-as.factor(pred_cmd12)
summary(pred_cmd12)
precouple12 <- factor(pred_cmd12, levels=c(0, 1))
actcouple12 <- testcouple2$CMethodUsed
mean(precouple12 == actcouple12)





