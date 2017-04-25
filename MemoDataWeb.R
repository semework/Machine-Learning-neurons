library(dplyr)
library(readr)
library(plyr) 
library(data.table)
library(ggplot2)
library(nortest)
library(class)
library(stats)
library(MASS)
library(ISLR)
require(graphics)
library(class)
library(boot)
require(MASS)
require(ggplot2)
require(scales)
require(gridExtra)


## #####    ###  #######    ###  #######    ###  #######    ###  #######  
#                                                                        #
#   This code imports Neural data,                                     # 
#   analyzes, such as by regression, LDA, PCA, LOOCV, GLM, LM,       #
#   K-Nearest and other     machine learning tools,                 # 
#   and plots relevant conclusions                                # 
#   Mulugeta Semework March 19, 2017                                 #
#                                                                       #
## #####    ###  #######    ###  #######    ###  #######    ###  #######  


# setpath ------------------------------
muluPath = "/Users/mulugetasemework/Documents/R/MemoryData"
setwd(muluPath)


# list .csv files  ----------------------------------------------------------------------
files = list.files(pattern="*.csv")
files
DT = read.csv(file="MetaTable.csv",header=T)
names(DT)
summary(DT)

# regress mean data ===========
reg = lm(pMemoBase_ranksum~basePSTH_PREMEAN2, data=DT,subset=(basePSTH_PREMEAN2<40))
summary(reg)
glm.probs = predict(reg,type="response")

glm.pred = ifelse(glm.probs<0.05,"Up","Down")
attach(DT)
table(glm.pred,pMemoBase_ranksum)
mean(glm.pred <= pMemoBase_ranksum)


glreg<-glm(pMemoBase_ranksum~basePSTH_PREMEAN2, data=DT,subset=(basePSTH_PREMEAN2<40))
glm.probs = predict(glreg,type="response")

glm.pred = ifelse(glm.probs<0.05,"Up","Down")
attach(DT)
table(glm.pred,pMemoBase_ranksum)
mean(glm.pred<=pMemoBase_ranksum)



confint(glreg)
fit2=lm(memPSTH_memMEAN2~basePSTH_PREMEAN2+pVisVis_MAX_ranksum,data=DT)
fit3=glm(memPSTH_memMEAN2~basePSTH_PREMEAN2+pVisVis_MAX_ranksum,data=DT)

glm.probs = predict(fit3,type="response")

glm.pred = ifelse(glm.probs<0.05,"Up","Down")
attach(DT)
table(glm.pred,memPSTH_memMEAN2)
mean(glm.pred==memPSTH_memMEAN2)

fit4=lm(pMemoBase_MED_ranksum2~base_PREMEDIAN2+pVISBase_MEDVIS_ranksumPOST,data=DT)

fit5 = lm(pMemoBase_ranksum2~base_PREMEDIAN2*pVisVis_MAX_ranksum*
            basePSTH_PREMEAN2*basePSTH_PREMeanMEAN2,data=DT)
fit6 = lm(pMemoBase_MED_ranksum2~base_PREMEDIAN2*pVISBase_MEDVIS_ranksumPOST,data=DT)
fit5y=glm(pMemoBase_MED_ranksum2~pVisVis_MAX_ranksum,data=DT);summary(fit5y)

# logistic regression ================================================================
trainPVis = pVisVis_MAX_ranksum < 0.05
trainPMemMED2 = pMemoBase_MED_ranksum2 < 2
fit5 = lm(pMemoBase_MED_ranksum2~pVisVis_MAX_ranksum,data=DT,subset= pMemoBase_MED_ranksum2 < 2)
glm.probs = predict(fit5,type="response")
glm.probs

glm.pred = ifelse(glm.probs < 0.05,"Up","Down" )
glm.pred 

attach(DT)
table(glm.pred, pMemoBase_MED_ranksum2)
mean(glm.pred < pMemoBase_MED_ranksum2)

#make training and test set
trainPVis = pVisVis_MAX_ranksum < 0.05
largePvals = pVisVis_MAX_ranksum[!train]
trainPMemMED = pMemoBase_MED_ranksum2 < 0.05
trainPMemMED2 = pMemoBase_MED_ranksum2 < 2
newdataPMem = pMemoBase_MED_ranksum2[trainPMemMED2]
newdataPVis = pVisVis_MAX_ranksum[trainPMemMED2]


plot(newdataPVis,newdataPMem)
fit15 = lm(newdataPMem~newdataPVis)

graphics.off()

par(mfrow = c(2, 2)) 
plot(fit15)

glm.fit = glm(newdataPMem[train]~newdataPVis[train])

glm.probs = predict(glm.fit,newdata=newdataPMem[!train],type="response")
glm.pred = ifelse(glm.probs  < 0.05,"Up","Down")
predGoodM = newdataPMem[!train]<0.05
Response.05 = newdataPMem[predGoodM]
table(glm.probs[predGoodM], Response.05)
mean(glm.probs[predGoodM] <= Response.05)

par(mfrow = c(2, 2)) 
plot(glm.fit)
# LDA ================================================================
#==========using vis max ----------------------
lda.fit = lda(memMEMResponse1~basePreEventMAXRAW+basePSTHFROMEAN_mean_PRE+
                baseGauss_PREMeanMEAN2+memPSTHGauss_PREMeanMEAN2,data=DT,subset=pVisVis_MAX_ranksum<0.05)

graphics.off()

par(mfrow = c(2, 2)) 
plot(lda.fit)

DT.p05 = subset(DT,pVisVis_MAX_ranksum > 0.05)
lda.pred= predict(lda.fit,DT.p05)
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,DT.p05$memMEMResponse1)
mean(lda.pred$class==DT.p05$memMEMResponse1)
 
#==========using vis max
lda.fit = lda(memMEMResponse1~basePreEventMAXRAW+basePSTHFROMEAN_mean_PRE+
                baseGauss_PREMeanMEAN2+memPSTHGauss_PREMeanMEAN2,data=DT,subset=pVisBase_ranksum < 0.05)

graphics.off()

par(mfrow = c(2, 2)) 
plot(lda.fit)

DT.p05 = subset(DT,pVisBase_ranksum > 0.05)
lda.pred= predict(lda.fit,DT.p05)
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,DT.p05$memMEMResponse1)
mean(lda.pred$class==DT.p05$memMEMResponse1)
 

#==========using vis max   
lda.fit = lda(memMEMResponse1~pVisBase_ranksum,data=DT,subset=pVisBase_ranksum < 0.05)

graphics.off()

par(mfrow = c(2, 2)) 

DT.p05 = subset(DT,pVisBase_ranksum > 0.05)
lda.pred = predict(lda.fit,DT.p05)
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,DT.p05$memMEMResponse1)
mean(lda.pred$class==DT.p05$memMEMResponse1)
 

plot(lda.fit)
 
ldaCV.fit = lda(memMEMResponse1~pVisBase_ranksum,data=DT,subset=pVisBase_ranksum < 0.05)

graphics.off()

par(mfrow = c(2, 2)) 

DT.p05 = subset(DT,pVisBase_ranksum > 0.05)
ldaCV.pred = predict(ldaCV.fit,DT.p05)
class(ldaCV.pred)
data.frame(ldaCV.pred)[1:5,]
table(ldaCV.pred$class,DT.p05$memMEMResponse1)
mean(ldaCV.pred$class==DT.p05$memMEMResponse1)
plot(ldaCV.fit)


#==========using vis pVISBase_MEDVIS_ranksumPOST 
lda.fit = lda(memMEMResponse1~pVISBase_MEDVIS_ranksumPOST,data=DT,subset = pVISBase_MEDVIS_ranksumPOST < 0.05)

graphics.off()

par(mfrow = c(2, 2)) 
plot(lda.fit)

DT.p05 = subset(DT,pVISBase_MEDVIS_ranksumPOST > 0.05)
lda.pred = predict(lda.fit,DT.p05)
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,DT.p05$memMEMResponse1)
mean(lda.pred$class==DT.p05$memMEMResponse1)
plot(lda.fit)
# k-nearest================================================================
 
#training set
attach(DT)
trainSet = base_memMeanMEDIAN2 
train = pVisBase_ranksum2 < 0.05 
goodMem = pMemoBase_MED_ranksum2 < 0.05
knn.pred = knn(data.frame(trainSet[train]),data.frame(trainSet[!train]),goodMem[train],k=1)#train,test,response,nearest
# knn.pred = knn(trainSet[train,],trainSet[!train,],goodMem[train],k=1)
table(knn.pred,goodMem[!train])
mean(knn.pred==goodMem[!train])#accuracey
realVsPred = data.frame(pMemoBase_MED_ranksum2[!train],as.integer(goodMem[!train]),as.integer(knn.pred== "TRUE"))
matplot (1: length(as.integer(goodMem[!train])), cbind (as.integer(goodMem[!train]), as.integer(knn.pred== "TRUE")), pch = 19,
         main = bquote(paste( "Visual response-based prediction accuracy: ", .(mean(knn.pred==goodMem[!train])))))

# k-nearest==base base med ransksum======
attach(DT)
trainSet = cbind(basePreEventMAXRAW,base_PREMEDIAN2)
train = pBaseBase_MED_ranksum2 < 0.05 
goodMem = pMemoBase_MED_ranksum2 < 0.05
knn.pred = knn (trainSet[train,],trainSet[!train,],goodMem[train],k=1)#train,test,response,nearest
table(knn.pred,goodMem[!train])
mean(knn.pred==goodMem[!train])#accuracey
realVsPred = data.frame(pMemoBase_MED_ranksum2[!train],as.integer(goodMem[!train]),as.integer(knn.pred== "TRUE"))
matplot (1: length(as.integer(goodMem[!train])), cbind (as.integer(goodMem[!train]), as.integer(knn.pred== "TRUE")), pch = 19,
         main = bquote(paste( "Baseline median-based prediction accuracy: ", .(mean(knn.pred==goodMem[!train])))))

# k-nearest==pBaseBase_ranksum32======
attach(DT)
trainSet = basePreEventMAXRAW 
train = pBaseBase_ranksum32 < 0.05#seems like the best. 0.5443038 for k=1,
goodMem = pMemoBase_MED_ranksum2 < 0.05
knn.pred = knn (trainSet[train,],trainSet[!train,],goodMem[train],k=2)#train,test,response,nearest
table(knn.pred,goodMem[!train])

mean(knn.pred==goodMem[!train])#accuracey
realVsPred = data.frame(pMemoBase_MED_ranksum2[!train],as.integer(goodMem[!train]),as.integer(knn.pred== "TRUE"))
matplot (1: length(as.integer(goodMem[!train])), cbind (as.integer(goodMem[!train]), as.integer(knn.pred== "TRUE")), pch = 19,
         main = bquote(paste( "Baseline max-based Prediction accuracy: ", .(mean(knn.pred==goodMem[!train])))))


# k-nearest==pBaseBase_MED_ranksum32======
attach(DT)
trainSet = cbind(basePreEventMAXRAW,base_PREMEDIAN2)
train = pBaseBase_MED_ranksum32 < 0.05#1:round(length(basePreEventMAXRAW)/2)
goodMem = pMemoBase_MED_ranksum2 < 0.05
knn.pred = knn (trainSet[train,],trainSet[!train,],goodMem[train],k=1)
table(knn.pred,goodMem[!train])
mean(knn.pred==goodMem[!train])#accuracey
realVsPred = data.frame(pMemoBase_MED_ranksum2[!train],as.integer(goodMem[!train]),as.integer(knn.pred== "TRUE"))
matplot (1: length(as.integer(goodMem[!train])), cbind (as.integer(goodMem[!train]), as.integer(knn.pred== "TRUE")), pch = 19,
         main = bquote(paste( "Baseline short-median-based Prediction accuracy: ", .(mean(knn.pred==goodMem[!train])))))
 

#==================Cross-validation for Generalized Linear Models==============

glm.fit=glm(pMemoBase_ranksum2~base_PREMEDIAN2,data=DT,subset=pMemoBase_ranksum2<1)

plot(pMemoBase_ranksum2~base_PREMEDIAN2,data=DT,subset=pMemoBase_ranksum2<1)

cv.glm(DT,glm.fit)$delta
#=================leave one out cross-validation error

graphics.off()
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}
loocv(glm.fit)
cv.error = rep(0.5)
degree=1:5

for (d in degree){
  glm.fit = glm(pMemoBase_ranksum2~poly(base_PREMEDIAN2,d),data=DT,subset=pMemoBase_ranksum2<1)
cv.error[d]=loocv(glm.fit)
  }
plot(degree,cv.error,type="b",main="LOO (black) vs 10 fold CV (red), \nbaseline PreMedian predicting memory")

#=================10 fold cross-validation error
cv.error10 = rep(0.5)
for (d in degree){
  glm.fit = glm(pMemoBase_ranksum2~poly(base_PREMEDIAN2,d),data=DT,subset=pMemoBase_ranksum2<1)
  cv.error10[d]= cv.glm(DT,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")


##========= BOOTSTRAP ==========assessing uncertainty in estimates========

#-----------bootstrap     (pMemoBase_ranksum2,base_PREMEDIAN2 -----------
 
alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(pMemoBase_ranksum2,base_PREMEDIAN2)
 
alpha.fn=function(data,index){
  with(data[index,],alpha(pMemoBase_ranksum2,base_PREMEDIAN2)) 
}
alpha.fn(DT,1:100)
#bootstrap uses random sampling, to get reproducible results, set seed
set.seed(1)

alpha.fn(DT,sample(1:100,100,replace=TRUE))#bootstrap done once
boot.out = boot(DT,alpha.fn,R=1000)#do 1000 boostraps
boot.out
plot(boot.out)


#-----------bootstrap     alpha(pMemoBase_ranksum2,pVISBase_MEDVIS_ranksumPOST) ------------------------------
 
graphics.off()
alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(pMemoBase_ranksum2,pVISBase_MEDVIS_ranksumPOST)
 
alpha.fn=function(data,index){
  with(data[index,],alpha(pMemoBase_ranksum2,,pVISBase_MEDVIS_ranksumPOST))#with takes right observations for this bootstrap sample
}
alpha.fn(DT,1:100)
 
set.seed(1)

alpha.fn(DT,sample(1:100,100,replace=TRUE)) 
boot.out = boot(DT,alpha.fn,R=1000) 
boot.out
plot(boot.out) 


 
#=================== OTHER FITS ==================================

fit7 = lm(pMemoBase_ranksum2~base_PREMEDIAN2*pVisVis_MAX_ranksum*
           basePSTH_PREMEAN2*basePSTH_PREMeanMEAN2,data=DT)
summary(fit7)

fit8 = lm(pMemoBase_ranksum2~basePreEventMAXRAW*basePSTHFROMEAN_mean_PRE*baseGauss_PREMeanMEAN2*memPSTHGauss_PREMeanMEAN2,data=DT)
summary(fit8)


fit9 = lm(pMemoBase_ranksum2~baseGauss_PREMEAN2*baseGauss_PREMEDIAN2*base_PREGEOMeanMEAN2*
            baseMEM_PRE_MEDI_RAW,data=DT)
summary(fit9)

##############################
fit10 = lm(pMemoBase_MED_ranksum2~base_PREMEDIAN2*pVisVis_MAX_ranksum*
            basePSTH_PREMEAN2*basePSTH_PREMeanMEAN2,data=DT)
summary(fit10)

fit11 = lm(pMemoBase_MED_ranksum2~basePreEventMAXRAW*basePSTHFROMEAN_mean_PRE*baseGauss_PREMeanMEAN2*memPSTHGauss_PREMeanMEAN2,data=DT)
summary(fit11)


fit12 = lm(pMemoBase_MED_ranksum2~baseGauss_PREMEAN2*baseGauss_PREMEDIAN2*base_PREGEOMeanMEAN2*
            baseMEM_PRE_MEDI_RAW,data=DT)
summary(fit12)


dput(summary(fit7),file="myfits.csv",control="all")
res=dget("summary_lm.txt")
class(res)

#1 "PreSacc Baseline Firing rate influence memory response?" --------------------------
graphics.off()

par(mfrow = c(2, 2)) 

plot( DT$pMemoBase_ranksum~DT$basePSTH_PREMEAN2,data=DT,subset=(basePSTH_PREMEAN2<40),xlab="pre-saccadic baseline (sp/s)",
     ylab="memory block vs block 1 p-value",main="Does Firing rate influence memory response?",
     col.axis=258,bg=632,col="red",pch=16,col.lab="magenta",col.sub=654,col.main=268)
abline(reg,lwd=2,col="blue")
abline(glreg,lwd=2,col="green")

#2 "How is age distributed?"
hist(DT$basePSTH_PREMEAN2,breaks = 100,xlab="MEAN pre-saccadic baseline (sp/s)",ylab="Frequency",
     main="How is baseline rate distributed?",col="red") 


fit2=lm(memPSTH_memMEAN2~basePSTH_PREMEAN2,data=DT)
fit3=glm(memPSTH_memMEAN2~basePSTH_PREMEAN2,data=DT)

##
baseline_pre_Mean <- DT$basePSTH_PREMEAN2
memory_p_value <- DT$pMemoBase_ranksum
predict(lm(memory_p_value  ~ baseline_pre_Mean))
predictor_mean <- data.frame(baseline_pre_Mean = seq(0, 100,by=2))
predict(lm(memory_p_value  ~ baseline_pre_Mean), predictor_mean, se.fit = TRUE)
pred.w.plim <- predict(lm(memory_p_value  ~ baseline_pre_Mean), predictor_mean, interval = "prediction")
pred.w.clim <- predict(lm(memory_p_value  ~ baseline_pre_Mean), predictor_mean, interval = "confidence")
matplot(predictor_mean$baseline_pre_Mean, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted memory p_value")

## glm
baseline_pre_Mean <- DT$basePSTH_PREMEAN2
memory_p_value <- DT$pMemoBase_ranksum
predict(glm(memory_p_value  ~ baseline_pre_Mean))
predictor_mean <- data.frame(baseline_pre_Mean = seq(0, 100,by=2))
predict(glm(memory_p_value  ~ baseline_pre_Mean), predictor_mean, se.fit = TRUE)
pred.w.plim <- predict(glm(memory_p_value  ~ baseline_pre_Mean), predictor_mean, interval = "prediction")
pred.w.clim <- predict(glm(memory_p_value  ~ baseline_pre_Mean), predictor_mean, interval = "confidence")
matplot(predictor_mean$baseline_pre_Mean, cbind(pred.w.clim, pred.w.plim),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted memory p_value")


## Predictions

predict(fit2)
x=DT$basePSTH_PREMEAN2 
new <-  data.frame(DT$basePSTH_PREMEAN2= seq(1,60,by=2))
new

predict(fit2, new, se.fit = TRUE)
pred.w.plim <- predict(fit2, new, interval = "prediction")
pred.w.clim <- predict(fit2, new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")


plot(memPSTH_memMEAN2~basePSTH_PREMEAN2,data=DT,xlab="pre-saccadic baseline (sp/s)",
     ylab="memory block firing rate (sp/s)",main="Does presacc baseline Firing rate influence memory response?",
     col.axis=258,bg=632,col="red",pch=16,col.lab="magenta",col.sub=654,col.main=268,xaxt='n')


abline(fit2,lwd=2,col="blue")
abline(fit3,lwd=2,col="green")

#4
plot( DT$pMemoBase_ranksum~DT$base_PREMEDIAN2 ,data=DT,subset=(basePSTH_PREMEAN2<400),xlab="MEDIAN pre-saccadic baseline (sp/s)",
      ylab="memory block vs block 1 p-value",main="Does MEDIAN_Firing rate influence memory response?",
      col.axis=258,bg=632,col="red",pch=16,col.lab="magenta",col.sub=654,col.main=268)
abline(reg,lwd=2,col="blue")
abline(glreg,lwd=2,col="green")


#
attach(DT)
##### -----------------------using baseline pre -MEDIAN and MAX ##------------------#
dd = data.frame(base_PREMEDIAN2,basePreEventMAXRAW)


pca <- prcomp(dd,
              center = TRUE,
              scale. = TRUE) 

prop.pca = pca$sdev^2/sum(pca$sdev^2)



x=1:length(pMemoBase_MED_ranksum2)
memType = rep("NA")
for (d in x) {
  if (pMemoBase_MED_ranksum2[d] < 0.05)
  { memType[d] = "Good Memory"
  }
  else if ((pMemoBase_MED_ranksum2[d] > 0.05) && (pMemoBase_MED_ranksum2[d] < 2))
    memType[d] = "Bad Memory"
  else
  {memType[d] = "NA"}
}

r <- lda(memType  ~ .,dd) 

prop.lda = r$svd^2/sum(r$svd^2)

plda <- predict(object = r,
                newdata =dd)

dataset = data.frame(pMemoBase_MED_ranksum2,
                     pca = pca$x, lda = plda$x)

p1 <- ggplot(dataset) + geom_point(aes(r$scaling[1], r$scaling[2],colour = memType, shape = memType), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))

p2 <- ggplot(dataset) + geom_point(aes(pca.PC1, pca.PC2,colour = memType, shape = memType), size = 2.5) + 
  labs(x = paste("PC1 (", percent(prop.pca[1]), ")","      using baseline pre -MEDIAN and MAX", sep=""),
       y = paste("PC2 (", percent(prop.pca[2]), ")", sep=""))

grid.arrange(p1, p2)

