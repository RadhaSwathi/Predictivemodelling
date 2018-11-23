summary(Healthcare)

with(Healthcare, plot(PerCapGDP, AvgCancerSpend, pch=19, col="darkblue"))
with(Healthcare, plot(PerCapGDP, AvgHeartSpend, pch=19, col="darkblue"))
with(Healthcare, plot(PerCapGDP, AvgOrganSpend, pch=19, col="darkblue"))
with(Healthcare, boxplot(PerCapGDP))
with(Healthcare, boxplot(AvgCancerSpend)) 
d.fit<-lm(formula = AvgCancerSpend ~ PerCapGDP,data =Healthcare )
summary(d.fit)
d.fit1<-lm(formula = AvgHeartSpend ~ PerCapGDP,data =Healthcare )
summary(d.fit1)
d.fit2<-lm(formula = AvgOrganSpend ~ PerCapGDP,data =Healthcare )
summary(d.fit2)

d.fit<-lm(formula = AvgCancerSpend ~ PerCapGDP+Spendtype,data =Healthcare )
summary(d.fit)

d.fit<-lm(formula = AvgCancerSpend ~ PerCapGDP,data =PublicHealth )
summary(d.fit)

d.fit<-lm(formula = AvgCancerSpend ~ PerCapGDP,data =InsureHealthcare )
summary(d.fit)


d.fit<-lm(formula = AvgCancerSpend ~ PerCapGDP+Spendtype+Region,data =Healthcare )
summary(d.fit)
-14540-61320

Ac.fit<-lm(formula = AvgCancerSpend ~ PerCapGDP+Spendtype+Region,data =Healthcare )
summary(Ac.fit)
anova(Ac.fit)
plot(Ac.fit)
mean(Ac.fit$residuals)

Ah.fit<-lm(formula = AvgHeartSpend ~ PerCapGDP+Spendtype+Region,data =Healthcare )
summary(Ah.fit)
anova(Ah.fit)
plot(Ah.fit)
Ao.fit<-lm(formula = AvgOrganSpend ~ PerCapGDP+Spendtype+Region,data =Healthcare )
summary(Ao.fit)
anova(Ao.fit)
plot(Ao.fit)


d.fit<-lm(formula = AvgHeartSpend ~ Spendtype+Region,data =Healthcare )
summary(d.fit)
d.fit<-lm(formula = AvgOrganSpend ~ Spendtype+Region,data =Healthcare )
summary(d.fit)


library("leaps")
library("car")
Ac.fit1<-lm(formula = AvgCancerSpend ~ Spendtype+Region+PerCapGDP,data =Healthcare )
summary(Ac.fit1)
anova(Ac.fit1)
plot(Ac.fit)


summary(Healthcaretrans)
Ac1.fit<-lm(formula = logAvg ~ PerCapGDP+Spendtype+Region,data =Healthcaretrans )
summary(Ac1.fit)
anova(Ac1.fit)
plot(Ac1.fit)

Ac1.fit<-lm(formula = invAvg ~ PerCapGDP+Spendtype+Region,data =Healthcaretrans )
summary(Ac1.fit)
anova(Ac1.fit)
plot(Ac1.fit)

Ac1.fit<-lm(formula = sqrtAvg ~ PerCapGDP+Spendtype+Region,data =Healthcaretrans )
summary(Ac1.fit)
anova(Ac1.fit)
plot(Ac1.fit)



### Model seclection with concrete data

null=lm(CompStrength~1, data=Concrete_Data)
Concrete.fit1=lm(CompStrength~., data=Concrete_Data)
Concrete.fit4 <- step(null, scope=list(lower=null, upper=Concrete.fit1), direction="forward")
Concrete.fit5 <- step(null, scope=list(lower=null, upper=Concrete.fit1), direction="both")


Concrete.fit6=regsubsets(CompStrength~., data=Concrete_Data, nbest=3)
summary(Concrete.fit6)
plot(Concrete.fit6, scale="adjr2")
plot(Concrete.fit6, scale="bic")

concrete.fit7<-lm(CompStrength ~ Cement + Superplasticize + Age + BlastFurnaceSlag + 
                    Water + FlyAsh, data=Concrete_Data) 
summary(concrete.fit7)
mean(concrete.fit7$residuals)
round(vif(concrete.fit7),2)
d.fit5<-lm(CompStrength ~ Cement+FlyAsh+Water+Superplasticize+CoarseAggregate+FineAggregate+Age , data=Concrete_Data) 
summary(d.fit5)
mean(d.fit5$residuals)
round(vif(d.fit5),2)