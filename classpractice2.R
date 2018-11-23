summary(Highway)
high.fit1<-lm(Rate ~ ., data=Highway) 
summary(high.fit1)

Highway$Hwy=as.factor(Highway$Hwy)
b <- relevel(Highway$Hwy, ref = "two")

null=lm(Rate~1, data=Highway)
high.fit1=lm(Rate~., data=Highway)
high.fit2 <- step(null, scope=list(lower=null, upper=high.fit1), direction="forward")
high.fit2 <-lm(formula = Rate ~ Acpt + Len + Slim  , data = Highway)

summary(high.fit2)
plot(high.fit2)

high.fit5<-lm(Rate~Acpt + Len + Slim + Sigs, data=Highway)
summary(high.fit5)
anova(high.fit5)
plot(high.fit5)
high.fit3 <- step(null, scope=list(lower=null, upper=high.fit1), direction="both")
round(vif(high.fit3),2)
summary(high.fit3)
anova(high.fit3)

hight.fit4=regsubsets(Rate~., data=Highway, nbest=3)
summary(hight.fit4)
plot(hight.fit4, scale="adjr2")
plot(hight.fit4, scale="bic")


high.fit5<-lm(Rate~Acpt + Len + Slim + Sigs+relevel(Highway$Hwy, ref = "three"), data=Highway)
summary(high.fit5)

plot(high.fit5)
