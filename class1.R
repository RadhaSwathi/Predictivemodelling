#Read data
rm(list =  ls())
setwd('/Users/rsklanu/Predictivemodelling')
library(readxl)
AutoDSE <- read_excel("AutoDSE.xlsx")
View(AutoDSE)
summary(AutoDSE)
with(AutoDSE, plot(horsepower, mpg, pch=19, col="darkblue"))
with(AutoDSE, plot(displacement, mpg, pch=19, col="darkred", main="Plot of MPG versus Displacement for Cars"))
with(AutoDSE, plot(acceleration, mpg, pch=19, col="darkblue", main="Plot of MPG versus Time to Accelerate for Cars"))

#To get corerelation between pair of columns
with(AutoDSE, cor(displacement, mpg))
with(AutoDSE, cor(acceleration, mpg))
#to get correlation metrics for data frame
round(cor(AutoDSE),2)
#to fit a regression line
d.fit1 <- lm(mpg ~ displacement, data=AutoDSE) 
names(d.fit1)
typeof(d.fit1)
d.fit1$coefficients 
28.51-(0.042*360)
28.51-(0.042*232)
((28.51-(0.042*232))-(28.51-(0.042*360)))


d.fit1$residuals
d.fit1$fitted.values
d.fit1$fitted.values-AutoDSE$mpg
ave(d.fit1$residuals)

# Regress mpg on the predictors in the data set
summary(d.fit1)
anova(d.fit1)

newd <- data.frame(displacement=c(300, 500))
predict.lm(d.fit1, newdata=newd)


##regression

d.fit2 <- lm(mpg ~ horsepower, data=AutoDSE) 
summary(d.fit2)
anova(d.fit2)
plot(d.fit2)
d.fit3 <- lm(mpg ~ weight, data=AutoDSE) 
summary(d.fit3)
anova(d.fit3)
plot(d.fit3)
d.fit4 <- lm(mpg ~ acceleration, data=AutoDSE) 
summary(d.fit4)
anova(d.fit4)
plot(d.fit4)
d.fit2
d.fit3
d.fit4
newd <- data.frame(horsepower=c(300, 500))
29*var(AutoDSE$mpg)
241.476+86.733
-8.829 *-8.829 
##Multiple regression
d.fit5 <- lm(mpg ~ displacement+horsepower+weight+acceleration+cylinders, data=AutoDSE) 
d.fit5 <- lm(mpg ~., data=AutoDSE) 
summary(d.fit5)
anova(d.fit5)

d.fit6<-lm(mpg~horsepower+weight, data=AutoDSE)
summary(d.fit6)
newd1<-data.frame(horsepower=c(90,150),weight=c(3200,4260))
predict.lm(d.fit6,newdata = newd1)


library(car)
round(vif(d.fit5),2)

## eliminate max VIF and refit (VIF to check multi colinearity)
d.fit6 <- lm(mpg ~ displacement+weight+acceleration+cylinders, data=AutoDSE)
round(vif(d.fit6),2)
d.fit7 <- lm(mpg ~ displacement+weight+acceleration, data=AutoDSE)
round(vif(d.fit7),2)
summary(d.fit7)



d.fit8 <- lm(mpg ~ weight+acceleration, data=AutoDSE)
round(vif(d.fit8),2)
summary(d.fit8)
anova(d.fit8)
plot(d.fit8)
mean(d.fit8$residuals)

d.fit9<-lm(mpginv~ weight+acceleration, data=AutoDSE)
summary(d.fit9)
anova(d.fit9)
plot(d.fit9)
