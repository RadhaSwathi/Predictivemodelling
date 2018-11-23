# Import Healthcare Data #

## Scatterplots ##
with(Healthcare, plot(PerCapGDP, AvgCancerSpend, pch=19, cex=0.5, col="darkblue", main="GDP versus Cancer Spend"))
with(Healthcare, plot(PerCapGDP, AvgHeartSpend, pch=19, col="darkred", cex=0.5, main="GDP versus Heart Spend"))
with(Healthcare, plot(PerCapGDP, AvgOrganSpend, pch=19, cex=0.5, col="forestgreen", main="GDP versus Organ Spend"))

## Pairwise Correlation ##
with(Healthcare, cor(PerCapGDP, AvgCancerSpend))
with(Healthcare, cor(PerCapGDP, AvgHeartSpend))
with(Healthcare, cor(PerCapGDP, AvgOrganSpend))

## Regress Cancer Spend on GDP  ##
Cancer.fit1 <- lm(AvgCancerSpend ~ PerCapGDP, data=Healthcare) 

## Adds regression line to scatterplot ##
with(Healthcare, plot(PerCapGDP, AvgCancerSpend, pch=19, cex=0.5, col="darkblue", main="GDP versus Cancer Spend"))
abline(lm(AvgCancerSpend~PerCapGDP, data=Healthcare), col="darkred")

## Regression Result ##
summary(Cancer.fit1)
anova(Cancer.fit1)

## Regress Heart Spend on GDP  ##
Heart.fit1 <- lm(AvgHeartSpend ~ PerCapGDP, data=Healthcare) 
with(Healthcare, plot(PerCapGDP, AvgHeartSpend, pch=19, col="darkred", cex=0.5, main="GDP versus Heart Spend"))
abline(lm(AvgHeartSpend~PerCapGDP, data=Healthcare), col="forestgreen")
summary(Heart.fit1)
anova(Heart.fit1)

## Regress Organ Spend on GDP  ##
Organ.fit1 <- lm(AvgOrganSpend ~ PerCapGDP, data=Healthcare) 
with(Healthcare, plot(PerCapGDP, AvgOrganSpend, pch=19, cex=0.5, col="forestgreen", main="GDP versus Organ Spend"))
abline(lm(AvgOrganSpend~PerCapGDP, data=Healthcare), col="darkblue")
summary(Organ.fit1)
anova(Organ.fit1)
######################################

## Healthcare Public ##

with(Healthcare_Public, plot(PerCapGDP, AvgCancerSpend, pch=19, cex=0.5, col="darkblue", main="GDP versus Cancer Spend: Public"))
with(Healthcare_Public, plot(PerCapGDP, AvgHeartSpend, pch=19, col="darkred", cex=0.5, main="GDP versus Heart Spend: Public"))
with(Healthcare_Public, plot(PerCapGDP, AvgOrganSpend, pch=19, cex=0.5, col="forestgreen", main="GDP versus Organ Spend: Public"))

## Pairwise Correlation ##
with(Healthcare_Public, cor(PerCapGDP, AvgCancerSpend))
with(Healthcare_Public, cor(PerCapGDP, AvgHeartSpend))
with(Healthcare_Public, cor(PerCapGDP, AvgOrganSpend))

## Healthcare Insurance ##

with(Healthcare_Insurance, plot(PerCapGDP, AvgCancerSpend, pch=19, cex=0.5, col="darkblue", main="GDP versus Cancer Spend: Insurance"))
with(Healthcare_Insurance, plot(PerCapGDP, AvgHeartSpend, pch=19, col="darkred", cex=0.5, main="GDP versus Heart Spend: Insurance"))
with(Healthcare_Insurance, plot(PerCapGDP, AvgOrganSpend, pch=19, cex=0.5, col="forestgreen", main="GDP versus Organ Spend: Insurance"))

## Pairwise Correlation ##
with(Healthcare_Insurance, cor(PerCapGDP, AvgCancerSpend))
with(Healthcare_Insurance, cor(PerCapGDP, AvgHeartSpend))
with(Healthcare_Insurance, cor(PerCapGDP, AvgOrganSpend))

## Regression with Dummy Variables ##
## Regress Cancer Spend on GDP and Type  ##
Cancer.fit2 <- lm(AvgCancerSpend ~ PerCapGDP + Spendtype, data=Healthcare2) 
summary(Cancer.fit2)
anova(Cancer.fit2)

## Regress Cancer Spend on GDP, Type and Region  ##
Cancer.fit3 <- lm(AvgCancerSpend ~ PerCapGDP + Spendtype + Region, data=Healthcare2) 
summary(Cancer.fit3)
anova(Cancer.fit3)

## Regress Cancer Spend on GDP, Type and Region  ##
Cancer.fit4 <- lm(AvgCancerSpend ~ PerCapGDP + Spendtype + Region + Spendtype*Region, data=Healthcare2) 
summary(Cancer.fit4)
anova(Cancer.fit4)

## Regress Heart Spend on GDP, Type and Region  ##
Heart.fit3 <- lm(AvgHeartSpend ~ PerCapGDP + Spendtype + Region, data=Healthcare2) 
summary(Heart.fit3)
anova(Heart.fit3)

Heart.fit4 <- lm(AvgHeartSpend ~ PerCapGDP + Spendtype + Region + Spendtype*Region, data=Healthcare2) 
summary(Heart.fit4)
anova(Heart.fit4)

## Regress Organ Spend on GDP, Type and Region  ##
Organ.fit3 <- lm(AvgOrganSpend ~ PerCapGDP + Spendtype + Region, data=Healthcare2) 
summary(Organ.fit3)
anova(Organ.fit3)

Organ.fit4 <- lm(AvgOrganSpend ~ PerCapGDP + Spendtype + Region + Spendtype*Region, data=Healthcare2) 
summary(Organ.fit4)
anova(Organ.fit4)

#### Model Selection with Concrete Data ####
null=lm(CompStrength~1, data=Concrete_Data)
Concrete.fit1=lm(CompStrength~., data=Concrete_Data)
Concrete.fit4 <- step(null, scope=list(lower=null, upper=Concrete.fit1), direction="forward")
Concrete.fit5 <- step(null, scope=list(lower=null, upper=Concrete.fit1), direction="both")

Concrete.fit6=regsubsets(CompStrength~., data=Concrete_Data, nbest=3)
plot(Concrete.fit6, scale="adjr2")
plot(Concrete.fit6, scale="bic")
