rm(list =  ls())
 library(readxl)
 Concrete_Data <- read_excel("Concrete_Data.xlsx")
 View(Concrete_Data)
 
 #Summary
 summary(Concrete_Data)
 #Data visualisation
 plot(Concrete_Data)
 #correlation
 round(cor(Concrete_Data),2)
 #Linear regression
 d.fit1 <- lm(CompStrength ~ Cement, data=Concrete_Data) 
 summary( d.fit1)
 anova(d.fit1)
 #multi regression
 d.fit<-lm(CompStrength ~ . , data=Concrete_Data) 
 summary(d.fit)
 annova(d.fit)
 round(vif(d.fit),2)
 #working model
 d.fit1<-lm(CompStrength ~ BlastFurnaceSlag+FlyAsh+Water+Superplasticize+CoarseAggregate+FineAggregate+Age , data=Concrete_Data) 
 summary(d.fit1)
 plot(d.fit1)
 round(vif(d.fit1),2)
 d.fit2<-lm(CompStrength ~ BlastFurnaceSlag+FlyAsh+Superplasticize+CoarseAggregate+FineAggregate+Age , data=Concrete_Data) 
 summary(d.fit2)
 round(vif(d.fit2),2)
 d.fit3<-lm(CompStrength ~ FlyAsh+Superplasticize+CoarseAggregate+FineAggregate+Age , data=Concrete_Data) 
 summary(d.fit3)
 round(vif(d.fit3),2)
 d.fit4<-lm(CompStrength ~ FlyAsh+CoarseAggregate+FineAggregate+Age , data=Concrete_Data) 
 round(vif(d.fit4),2)
 summary( d.fit4)
 plot( d.fit)
 
 
 d.fit5<-lm(CompStrength ~ Cement+FlyAsh+Water+Superplasticize+CoarseAggregate+FineAggregate+Age , data=Concrete_Data) 
 summary(d.fit5)
 round(vif(d.fit5),2)
 plot(d.fit5)
 
 ind1 <- getData("GADM", country = "india", level = 1)
 