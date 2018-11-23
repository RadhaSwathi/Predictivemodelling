summary(winequality_red)
wine.fit1<-lm(quality ~ ., data=winequality_red) 
summary(wine.fit1)
round(vif(wine.fit1),2)

wine.fit1<-lm(quality ~ volatileacidity+citricacid+residualsugar+chlorides+freesulfurdioxide+totalsulfurdioxide+density+pH+sulphates+alcohol, data=winequality_red) 
summary(wine.fit1)

round(cor(winequality_red),2)
round(vif(wine.fit1),2)



null=lm(quality~1, data=winequality_red)
win.fit1=lm(quality~., data=winequality_red)
win.fit2 <- step(null, scope=list(lower=null, upper=win.fit1), direction="forward")
wine.fit1<-lm(quality ~ alcohol + volatileacidity + sulphates + totalsulfurdioxide + chlorides + pH + freesulfurdioxide, data=winequality_red) 
wine.fit1$coefficients<-round(wine.fit1$coefficients,0)
wine.fit1$residuals