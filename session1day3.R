attach(German_Credit)
plot(German_Credit$CreditAmt,German_Credit$Creditability)
hist(CreditAmt,col='green')
cutpoint<-c(0,500,1000,1500,2000,2500,5000,7500,10000,15000,20000)
Credit_cat<-cut(CreditAmt,cutpoint,right = T)
table(Credit_cat)
Table1<-table(Credit_cat,Creditability)
Table2<-prop.table(Table1,1)
Table3<-cbind(Table2,table(Credit_cat))
round(Table3,2)
odds <- Table3[,2]/(1-Table3[,2])
plot(odds, pch=19, col="blue", main="Odds of Success")

lodds <- log(odds)
plot(lodds, pch=19, col="forest green", main="Log(odds) of Success")

fit1.logistic <- glm(Creditability ~ CreditAmt, data=German_Credit, family=binomial(link=logit))
summary.glm(fit1.logistic)

names(fit1.logistic)


# Probability
with(German_Credit, plot(CreditAmt, fit1.logistic$fitted.values, pch=19, cex=0.4, col="dark blue"))

# Logit
with(German_Credit, plot(CreditAmt, fit1.logistic$linear.predictors, pch=19, cex=0.4, col="blue"))

summary(fit1.logistic$fitted.values)
val<-c(500,600,700)
x<-exp(1.229-0.00012*val)/(1+exp(1.229-0.00012*val))
val1<-c(4000,6000,8000)
x1<-exp(1.229-0.00012*val1)/(1+exp(1.229-0.00012*val1))
val2<-c(15000,20000,25000)
x2<-exp(1.229-0.00012*val2)/(1+exp(1.229-0.00012*val2))

set1<- c(x,x1,x2)
#odds ratio is exp(a+bx)
set2<-c(exp(1.229-0.00012*val),exp(1.229-0.00012*val1),exp(1.229-0.00012*val2))
set2

a<-exp(1.229-0.00012*val)
a
c=3.218772/3.180378
c
exp(0.00012*100)
b<-c(x1[0]/x1[1],x1[1]/x1[2])
b



#credit v/s duration

b <- seq(0, 80, 10)

Duration_cat <- with(German_Credit, cut(DurCredit, b, right=T))
table(Duration_cat)
Table4<-table(Duration_cat, German_Credit$Creditability)
Table5 <- prop.table(Table4,1)
Table6 <- cbind(Table5, table(Duration_cat))
round(Table6, 2)

fit2.logistic <- glm(Creditability ~ DurCredit, data=German_Credit, family=binomial(link=logit))
summary.glm(fit2.logistic)

with(German_Credit, plot(DurCredit, fit2.logistic$fitted.values, pch=19, cex=0.4, col="dark blue"))
with(German_Credit, plot(DurCredit, fit2.logistic$linear.predictors, pch=19, cex=0.4, col="blue"))
exp(3*-0.037)
val<-c(9,12,15)

set2<-c(exp(1.666-0.037538*9),exp(1.667-0.037538*12),exp(1.667-0.037538*15))
set2
3.76/3.39
1/0.89

3.774085/3.375502



fit3.logistic <- glm(Creditability~CreditAmt + DurCredit, data=German_Credit, family=binomial(link=logit))
summary.glm(fit3.logistic)

a=1.670
b1=-0.000023
b2=-0.03412
vals<-c(15000,20000,18000,15000)
valz<-c(12,9,12,18)
odds<-exp(a+b1*vals+b2*valz)
odds
2.03/2.49
exp(b2*(18-12))
exp(b1*3000)
2.33/2.49
exp(-5000*b1+3*b2)
2.49/2.46

#receiver operating cutoff value(AUC-Area under curve)
## FPR vs TPR

library(pROC)
roc(German_Credit$Creditability, fit2.logistic$fitted.values)
plot.roc(German_Credit$Creditability, fit2.logistic$fitted.values)

library(ggplot2)
library(ROCR)

predict2 <- predict(fit2.logistic, type = 'response')
ROCRpred2 <- prediction(as.numeric(predict2),as.numeric(German_Credit$Creditability))
ROCRperf2<- performance(ROCRpred2, 'tpr', 'fpr')
plot(ROCRperf2, colorize=TRUE, text.adj=c(-0.2,1.7))
plot(ROCRperf2, main="Duration of Credit")
perfauc2 <- performance(ROCRpred2, measure = "auc")


fit4.logistic <- glm(Creditability ~ DurCredit + as.factor(SexMS), data=German_Credit,family=binomial(link=logit))
summary.glm(fit4.logistic)
predict4 <- predict(fit4.logistic, type = 'response')
ROCRpred4 <- prediction(as.numeric(predict4),as.numeric(German_Credit$Creditability))
ROCRperf4<- performance(ROCRpred4, 'tpr', 'fpr')
plot(ROCRperf4, colorize=TRUE, text.adj=c(-0.2,1.7))
plot(ROCRperf4, main="Duration, Sex/Marital Status")
perfauc4 <- performance(ROCRpred4, measure = "auc")

roc(German_Credit$Creditability, fit4.logistic$fitted.values)
plot.roc(German_Credit$Creditability, fit4.logistic$fitted.values)


fit6.logistic <- glm(Creditability ~ ., data=German_Credit, family=binomial(link=logit))
summary.glm(fit6.logistic)

detach(German_Credit)


##leverdata
#data cleaning(Eliminate null variable and adjust data type of factors , contnous variable)
summary(ILPD_Short)
summary(as.factor(ILPD$Diagnosis))

summary(ILPD)
summary(ILPD_Patients)

#Data cleaning continued (Eliminated variables with high co relation)
cor(ILPD_Short[,-2])
ILPD_Short$Diagnosis<-as.factor(ILPD_Short$Diagnosis)
#Consider all values
lfit1.logistic <- glm(Diagnosis ~ ., data=ILPD, family=binomial(link=logit))
lfit0.logistic<-glm(Diagnosis ~ 1, data=ILPD, family=binomial(link=logit))
#consider only significant or alomost significant
lfit1.logistic <- glm(Diagnosis ~ Age+TB+SAA1+AAP+AGRatio, data=ILPD_Short, family=binomial(link=logit))
summary.glm(lfit1.logistic)
ILPD$Diagnosis[ILPD$Diagnosis ==2] =0
roc(ILPD_Short$Diagnosis, lfit1.logistic$fitted.values)
plot.roc(ILPD_Short$Diagnosis, lfit1.logistic$fitted.values)
lpredict4 <- predict(lfit1.logistic, type = 'response')
lROCRpred4 <- prediction(as.numeric(lpredict4),as.numeric(ILPD_Short$Diagnosis))
lROCRperf4<- performance(lROCRpred4, 'tpr', 'fpr')
plot(lROCRperf4, colorize=TRUE, text.adj=c(-0.2,1.7))
plot(lROCRperf4, main="Diagnosis,")
lperfauc4 <- performance(lROCRpred4, measure = "auc")

ls<-table(ILPD_Short$Diagnosis,lfit1.logistic$fitted.values>0.5)

154/583

1221.7-1162.9

pchisq(44.6,1)
1-pchisq(15,3)
library(DescTools)
install.packages(DescTools)
#path_to_file='/Users/rsklanu/Downloads/DescTools_0.99.24.tgz'
install.packages(path_to_file, repos = NULL, type="source")
install -l /Users/rsklanu/Downloads/DescTools_0.99.24.tgz

library(ResourceSelection)
PseudoR2(lfit1.logistic)
hoslem.test(ILPD_Short$Diagnosis,lfit1.logistic$fitted.values)

install.packages('ResourceSelection', dependencies=TRUE, repos='http://cran.rstudio.com/')


### Data cleaning
summary(German_Credit)
German_Credit$AcctBalance<-as.factor(German_Credit$AcctBalance)
German_Credit$`Paymnt Status`<-as.factor(German_Credit$`Paymnt Status`)
German_Credit$Purpose<-as.factor(German_Credit$Purpose)
German_Credit$Value<-as.factor(German_Credit$Value)
German_Credit$LengthEmpl<-as.factor(German_Credit$LengthEmpl)
German_Credit$Instalment<-as.factor(German_Credit$Instalment)
German_Credit$SexMS<-as.factor(German_Credit$SexMS)
German_Credit$ConcurrentCredits<-as.factor(German_Credit$ConcurrentCredits)
German_Credit$Telephone<-as.factor(German_Credit$Telephone)
German_Credit$ForeignWorker<-as.factor(German_Credit$ForeignWorker)

low<-glm(Creditability ~ 1, data=German_Credit, family=binomial(link=logit))
summary(low)
#Null deviance: 1221.7  on 999  degrees of freedom Residual deviance: 1221.7  on 999  degrees of freedom AIC: 1223.7

fit.acconly<-glm(Creditability ~ AcctBalance, data=German_Credit, family=binomial(link=logit))
summary(fit.acconly)
#Null deviance: 1221.7  on 999  degrees of freedom Residual deviance: 1090.4  on 996  degrees of freedom AIC: 1098.4
fit.accanddur<-glm(Creditability ~ AcctBalance+DurCredit, data=German_Credit, family=binomial(link=logit))
summary(fit.accanddur)
high.fit1<- step(low, scope=list(lower=low, upper=fit6.logistic), direction="forward")
summary(high.fit1)
#Null deviance: 1221.7  on 999  degrees of freedom Residual deviance: 1051.9  on 995  degrees of freedom AIC: 1061.9


high.fit2<- step(low, scope=list(lower=low, upper=fit6.logistic), direction="both")

fit7.logistic<-glm(formula = Creditability ~ AcctBalance + DurCredit + `Paymnt Status` + 
      Purpose + Value + ForeignWorker + LengthEmpl + MVAA + Apt + 
      Guarantors + Instalment + CreditAmt + Telephone + SexMS + 
      NoCredit, family = binomial(link = logit), data = German_Credit)
roc(ILPD_Short$Diagnosis, fit7.logistic$fitted.values)
roc(Creditability, fit7.logistic$fitted.values)

bckCredit = step(fit6.logistic)
summary(bckCredit)

glm(formula = Creditability ~ AcctBalance + DurCredit + `Paymnt Status` + 
      Purpose + CreditAmt + Value + LengthEmpl + Instalment + SexMS + 
      Guarantors + MVAA + Apt + NoCredit + Telephone + ForeignWorker, 
    family = binomial(link = logit), data = German_Credit)


bothCredit <-step(low, list(lower=formula(low),upper=formula(fit6.logistic)),direction="both", trace=0)
summary(bothCredit)

glm(formula = Creditability ~ AcctBalance + DurCredit + `Paymnt Status` + 
      Purpose + Value + ForeignWorker + LengthEmpl + MVAA + Apt + 
      Guarantors + Instalment + CreditAmt + Telephone + SexMS + 
      NoCredit, family = binomial(link = logit), data = German_Credit)

predictfwd <- predict(high.fit1, type = 'response')
ROCRfwdpred <- prediction(as.numeric(predictfwd),as.numeric(German_Credit$Creditability))
ROCRfwdperf<- performance(ROCRfwdpred, 'tpr', 'fpr')
plot(ROCRfwdperf, colorize=TRUE, text.adj=c(-0.2,1.7))
plot(ROCRfwdperf, main="Forward Selection Model")
perfaucfwd <- performance(ROCRfwdpred, measure = "auc")
perfaucfwd

PseudoR2(fit7.logistic)
##forward
lfit1.logistic <- glm(Diagnosis ~ ., data=ILPD, family=binomial(link=logit))
lfit0.logistic<-glm(Diagnosis ~ 1, data=ILPD, family=binomial(link=logit))
high.fit3<- step(lfit0.logistic, scope=list(lower=lfit0.logistic, upper=lfit1.logistic), direction="forward")
summary(high.fit3)
predictfwd <- predict(high.fit3, type = 'response')
ROCRfwdpred <- prediction(as.numeric(predictfwd),as.numeric(ILPD$Diagnosis))
ROCRfwdperf<- performance(ROCRfwdpred, 'tpr', 'fpr')
plot(ROCRfwdperf, colorize=TRUE, text.adj=c(-0.2,1.7))
plot(ROCRfwdperf, main="Forward Selection Model")
perfaucfwd <- performance(ROCRfwdpred, measure = "auc")
perfaucfwd

roc(ILPD$Diagnosis, high.fit3$fitted.values)
plot.roc(ILPD$Diagnosis, high.fit3$fitted.values)
##Null deviance: 698.37  on 582  degrees of freedom Residual deviance: 582.34  on 578  degrees of freedom AIC: 592.34

#Backward

bckCredit = step(lfit1.logistic)
summary(bckCredit)

roc(ILPD$Diagnosis, bckCredit$fitted.values)
plot.roc(ILPD$Diagnosis, bckCredit$fitted.values)

#stepwise
bothCredit <-step(lfit0.logistic, list(lower=formula(lfit0.logistic),upper=formula(lfit1.logistic)),direction="both", trace=0)
summary(bothCredit)
roc(ILPD$Diagnosis, bothCredit$fitted.values)
plot.roc(ILPD$Diagnosis, bothCredit$fitted.values)

#Mymodel
lfit2.logistic <- glm(formula = Diagnosis ~ Age + DB  + SAA1 + TP + ALB , family = binomial(link = logit), data = ILPD)
summary(lfit2.logistic)
roc(ILPD$Diagnosis, lfit2.logistic$fitted.values)

#Choosing better model
ILPD$Diagnosis<-as.numeric(ILPD$Diagnosis)
PseudoR2(bckCredit)
hoslem.test(ILPD$Diagnosis,bckCredit$fitted.values)
ls<-table(ILPD_Short$Diagnosis,bckCredit$fitted.values>0.5)
ls
#Misclassification ratio
(121+32)/(46+121+32+384)
#mymodel test we select this as all other value are not significantly different and hoslem.test is significantly better.

ILPD$Diagnosis<-as.numeric(ILPD$Diagnosis)
PseudoR2(lfit2.logistic)
hoslem.test(ILPD$Diagnosis,lfit2.logistic$fitted.values)
ls<-table(ILPD$Diagnosis,lfit2.logistic$fitted.values>0.5)
ls
(125+33)/(46+121+32+384)

# forward model
ILPD$Diagnosis<-as.numeric(ILPD$Diagnosis)
PseudoR2(high.fit3)
hoslem.test(ILPD$Diagnosis,high.fit3$fitted.values)
ls<-table(ILPD$Diagnosis,high.fit3$fitted.values>0.5)
ls
(131+27)/(46+121+32+384)


###MODEL TRAINING
summary(PaulBooks1)
data(PaulBooks1)
set.seed(28)
n = nrow(PaulBooks1)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
trainCredit = PaulBooks1[trainIndex ,]
testCredit = PaulBooks1[-trainIndex ,]
ls<-table(trainCredit$Purchase,trainCredit$Purchase>0)
ls1<-table(testCredit$Purchase,testCredit$Purchase>0)
56/700
56/83
27/300
summary(testCredit)
summary(trainCredit)

trainfit0 <- glm(Purchase ~ 1, data=trainCredit, family=binomial(link=logit))
train.fit<- step(trainfit0, scope=list(lower=trainfit0, upper=trainfit), direction="forward")
summary(train.fit)


trainfit <- glm(Purchase ~ Months+NoBought, data=trainCredit, family=binomial(link=logit))
summary(trainfit)
roc(trainCredit$Purchase, trainfit$fitted.values>0)

table(testCredit$Purchase,Credit_pred_num)

testpred <- predict(trainfit, newdata = testCredit, type = "response")
Credit_pred_num <- ifelse(testpred > 0.5, 1, 0)
Credit_pred <- factor(Credit_pred_num, levels=c(0, 1))
summary(as.factor(Credit_pred_num))
Credit_act <- testCredit$Purchase
summary(Credit_act)
mean(Credit_pred == Credit_act)
summary(PaulBooks1)
data(PaulBooks1)
set.seed(28)
n = nrow(PaulBooks1)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
trainCredit = PaulBooks1[trainIndex ,]
testCredit = PaulBooks1[-trainIndex ,]
ls<-table(trainCredit$Purchase,trainCredit$Purchase>0)
ls1<-table(testCredit$Purchase,testCredit$Purchase>0)
56/700
56/83
27/300
summary(testCredit)
summary(trainCredit)

trainfit0 <- glm(Purchase ~ 1, data=trainCredit, family=binomial(link=logit))
train.fit<- step(trainfit0, scope=list(lower=trainfit0, upper=trainfit), direction="forward")
summary(train.fit)


trainfit <- glm(Purchase ~ Months+NoBought, data=trainCredit, family=binomial(link=logit))
summary(trainfit)
roc(trainCredit$Purchase, trainfit$fitted.values>0)

table(testCredit$Purchase,Credit_pred_num)

testpred <- predict(trainfit, newdata = testCredit, type = "response")
Credit_pred_num <- ifelse(testpred > 0.5, 1, 0)
Credit_pred <- factor(Credit_pred_num, levels=c(0, 1))
summary(as.factor(Credit_pred_num))
Credit_act <- testCredit$Purchase
summary(Credit_act)
mean(Credit_pred == Credit_act)

###########

summary(ILPD)
data(ILPD)
set.seed(25)
n = nrow(ILPD)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
trainCredit = ILPD[trainIndex ,]
testCredit = ILPD[-trainIndex ,]
ls<-table(trainCredit$Diagnosis,trainCredit$Diagnosis>0)
ls1<-table(testCredit$Diagnosis,testCredit$Diagnosis>0)
295/(113+295)
121/(121+54)
summary(testCredit)
summary(trainCredit)

trainfit0 <- glm(Diagnosis ~ 1, data=trainCredit, family=binomial(link=logit))
trainfit<- glm(Diagnosis ~ ., data=trainCredit, family=binomial(link=logit))
train.fit<- step(trainfit0, scope=list(lower=trainfit0, upper=trainfit), direction="forward")
summary(train.fit)


trainfit<- glm(formula = Diagnosis ~ SAA1 + DB + Age + AAP, family = binomial(link = logit), data = trainCredit)

summary(trainfit)
roc(trainCredit$Diagnosis, trainfit$fitted.values)

table(testCredit$Diagnosis,Credit_pred_num)

testpred <- predict(trainfit, newdata = testCredit, type = "response")
Credit_pred_num <- ifelse(testpred > 0.5, 1, 0)
Credit_pred <- factor(Credit_pred_num, levels=c(0, 1))
summary(as.factor(Credit_pred_num))
Credit_act <- testCredit$Diagnosis
summary(Credit_act)
mean(Credit_pred == Credit_act)

(18+108)/(18+36+13+108)

###
summary(ILPD)
data(ILPD)
set.seed(25)
n = nrow(ILPD)
trainIndex = sample(1:n, size = round(0.75*n), replace=FALSE)
trainCredit = ILPD[trainIndex ,]
testCredit = ILPD[-trainIndex ,]
ls<-table(trainCredit$Diagnosis,trainCredit$Diagnosis>0)
ls1<-table(testCredit$Diagnosis,testCredit$Diagnosis>0)
324/(123+314)
102/(102+44)
summary(testCredit)
summary(trainCredit)

trainfit0 <- glm(Diagnosis ~ 1, data=trainCredit, family=binomial(link=logit))
trainfit<- glm(Diagnosis ~ ., data=trainCredit, family=binomial(link=logit))
train.fit<- step(trainfit0, scope=list(lower=trainfit0, upper=trainfit), direction="forward")
summary(train.fit)


trainfit<- glm(formula = Diagnosis ~ SAA1 + DB + Age + AAP, family = binomial(link = logit), data = trainCredit)

summary(trainfit)
roc(trainCredit$Diagnosis, trainfit$fitted.values)



testpred <- predict(trainfit, newdata = testCredit, type = "response")
Credit_pred_num <- ifelse(testpred > 0.5, 1, 0)
Credit_pred <- factor(Credit_pred_num, levels=c(0, 1))
summary(as.factor(Credit_pred_num))
Credit_act <- testCredit$Diagnosis
summary(Credit_act)
mean(Credit_pred == Credit_act)
table(testCredit$Diagnosis,Credit_pred_num)

(29+11)/(15+29+11+91)

summary(ILPD)

set.seed(25)
n = nrow(ILPD)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
trainCredit = ILPD[trainIndex ,]
testCredit = ILPD[-trainIndex ,]
ls<-table(trainCredit$Diagnosis,trainCredit$Diagnosis>0)
ls1<-table(testCredit$Diagnosis,testCredit$Diagnosis>0)
336/(130+336)
80/(37+80)
summary(testCredit)
summary(trainCredit)

trainfit0 <- glm(Diagnosis ~ 1, data=trainCredit, family=binomial(link=logit))
trainfit<- glm(Diagnosis ~ ., data=trainCredit, family=binomial(link=logit))
train.fit<- step(trainfit0, scope=list(lower=trainfit0, upper=trainfit), direction="forward")
summary(train.fit)


trainfit<- glm(formula = Diagnosis ~ SAA1 + DB + Age + AAP, family = binomial(link = logit), data = trainCredit)

summary(trainfit)
roc(trainCredit$Diagnosis, trainfit$fitted.values)



testpred <- predict(trainfit, newdata = testCredit, type = "response")
Credit_pred_num <- ifelse(testpred > 0.5, 1, 0)
Credit_pred <- factor(Credit_pred_num, levels=c(0, 1))
summary(as.factor(Credit_pred_num))
Credit_act <- testCredit$Diagnosis
summary(Credit_act)
mean(Credit_pred == Credit_act)
table(testCredit$Diagnosis,Credit_pred_num)
(27+4)/(10+27+4+76)
