library(readxl)
library(randomForest)
library(ggplot2)
ESR <- read_excel("Rshiny/Dashboard/ESR.xlsx")
View(ESR)
set.seed(100)
n=nrow(ESR)
summary(ESR)
ESR$PCC[is.na(ESR$PCC)] <- 0
ESR$State<-as.factor(ESR$State)
ESR$Project<-as.factor(ESR$Project)
ESR$`SesimicZone`<-as.factor(ESR$`SesimicZone`)
ESR$`NatureofStrata`<-as.factor(ESR$`NatureofStrata`)
trainIndex<-sample(1:n, size = round(0.8*n), replace=FALSE)
traindata<-ESR[trainIndex,]
testdata<-ESR[-trainIndex,]
testPCC<-testdata$PCC
testRCC<-testdata$RCC
testfw<-testdata$Formwork
teststeel<-testdata$`Rft`
testrate<-traindata$`RatePerLit`
MPCC <- randomForest(PCC ~ ., data=traindata[c(1,3,4,5,7,8,9)], ntree=5)
MRCC <- randomForest(RCC ~ ., data=traindata[c(1,3,4,5,7,8,10)], ntree=5)
Mfw <-randomForest(Formwork ~ ., data=traindata[c(1,3,4,5,7,8,11)], ntree=5)
MSteel <-randomForest(Rft ~ ., data=traindata[c(1,3,4,5,7,8,12)], ntree=5)
Mrate <-randomForest(RatePerLit ~ ., data=traindata[c(1,3,4,5,7,8,13)], ntree=5)
predPCC<-predict(MPCC, testdata[c(1,3,4,5,7,8,9)])
predRCC<-predict(MRCC, testdata[c(1,3,4,5,7,8,10)])
predfw<-predict(Mfw, testdata[c(1,3,4,5,7,8,11)])
predSteel<-predict(MSteel, testdata[c(1,3,4,5,7,8,12)])
predrate<-predict(Mrate, testdata[c(1,3,4,5,7,8,12)])

line(testfw~predfw)
plot(testfw~predfw)

ggplot(data=ESR, aes(x=Capacity, y=PCC, group=1)) +
  geom_line()+
  geom_point()


  
  ggplot(ESR, aes(Capacity)) + 
  geom_line(aes(y = PCC, colour = "PCC")) + 
  geom_line(aes(y = RCC, colour = "RCC"))+
    geom_line(aes(y = Formwork, colour = "Formwork"))+
    geom_line(aes(y = Rft, colour = "Rft"))+
    geom_line(aes(y = RatePerLit, colour = "RatePerLit"))+
    geom_point(predPCC)
  
  
  pred_grid <- data.frame( State='Madhya Pradesh',SesimicZone='Zone -2', WindSpeed=39,   SBC=20, Capacity = 180  , StagingHeight=13 )
  pred_grid$State<-as.factor(pred_grid$State)
  pred_grid$SesimicZone<-as.factor(pred_grid$SesimicZone)
  levels(pred_grid$State) <- levels(testdata$State)
  levels(pred_grid$SesimicZone) <- levels(testdata$SesimicZone)
  predRCC<-predict(MRCC, pred_grid)
  summary(pred_grid)
  

summary(traindata)
traindata[c(1,3,4,5,7,8,9)]
ESR_NoSBC<-ESR[-5]
ESR_NoSBC$PCC[is.na(ESR_NoSBC$PCC)] <- 0
ESR_NoSBC$State<-as.factor(ESR_NoSBC$State)
ESR_NoSBC$Project<-as.factor(ESR_NoSBC$Project)
ESR_NoSBC$`SesimicZone`<-as.factor(ESR_NoSBC$`SesimicZone`)
ESR_NoSBC$`NatureofStrata`<-as.factor(ESR_NoSBC$`NatureofStrata`)
trainIndex_Nosbc<-sample(1:n, size = round(0.8*n), replace=FALSE)
traindata_Nosbc<-ESR_NoSBC[trainIndex,]
testdata_Nosbc<-ESR_NoSBC[-trainIndex,]
testPCC_NoSBC<-traindata$PCC
testRCC_NoSBC<-traindata$RCC
testfw_NoSBC<-traindata$Formwork
teststeel_NoSBC<-traindata$`Rft`
testrate_NoSBC<-traindata$`RatePerLit`
MPCC_NoSBC <- randomForest(PCC ~ ., data=traindata_Nosbc[c(1,3,4,6,7,8)], ntree=5)
MRCC_NoSBC <- randomForest(RCC ~ ., data=traindata_Nosbc[c(1,3,4,6,7,9)], ntree=5)
Mfw_NoSBC <-randomForest(Formwork ~ ., data=traindata_Nosbc[c(1,3,4,6,7,10)], ntree=5)
MSteel_NoSBC <-randomForest(Rft ~ ., data=traindata_Nosbc[c(1,3,4,6,7,11)], ntree=5)
Mrate_NoSBC <-randomForest(RatePerLit ~ ., data=traindata_Nosbc[c(1,3,4,6,7,12)], ntree=5)
predPCC_NoSBC<-predict(MPCC_NoSBC, testdata_Nosbc[c(1,3,4,6,7,8)])
predRCC_NoSBC<-predict(MRCC_NoSBC, testdata_Nosbc[c(1,3,4,6,7,9)])
predfw_NoSBC<-predict(Mfw_NoSBC, testdata_Nosbc[c(1,3,4,6,7,10)])
predSteel_NoSBC<-predict(MSteel_NoSBC, testdata_Nosbc[c(1,3,4,6,7,11)])
predrate_NoSBC<-predict(Mrate_NoSBC, testdata_Nosbc[c(1,3,4,6,7,12)])
traindata[c(1,3,4,5,7,8,9)]

# Change line t

