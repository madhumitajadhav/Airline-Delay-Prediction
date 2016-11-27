whole2015<-read.csv("C:/Users/madhumitaj/Documents/ATL_2015.csv")
whole2015$WD[whole2015$WEATHER_DELAY==0] <- 0
whole2015$WD[whole2015$WEATHER_DELAY!=0] <- 1

whole2015<-whole2015[,-c(1:32)]

#-------------------------------  GBM  -----------------------------------

library(gbm)
library(SDMTools)

set.seed(2000)
index_2015 <- 1:nrow(whole2015)
testindex_2015 <- sample(index_2015, trunc(length(index_2015)/3))
whole2015$WD<-as.factor(whole2015$WD)
testset_2015 <-whole2015[testindex_2015,]
trainset_2015 <-whole2015[-testindex_2015,]
write.csv(testset_2015,file="testset_2015.csv")
write.csv(trainset_2015,file="trainset_2015.csv")


gbm_07<-trainset_2015
delay<-gbm_07$WD

train=gbm_07[,-23]

str(train)

end_trn<-nrow(train)
ntrees=50
#.....ignore GBM .......
model=gbm.fit(x=train[1:end_trn,],y=delay,
              distribution = "multinomial",
              n.trees=ntrees,
              shrinkage = 0.01,
              interaction.depth = 10,
              n.minobsinnode = 5,
              nTrain=round(end_trn*0.8),
              verbose = TRUE)
x<-summary(model)
x

#-------------------------  IV.MULT ------------------------------------------------

library(devtools)
library(woe)

row.names(trainset_2015) <- 1:nrow(trainset_2015) 
IV<-iv.mult(trainset_2015,y="WD",TRUE)
var<-IV[which(IV$InformationValue>0.01),]
var1<-var[which(var$InformationValue<1.0),]
final_var<-var1$Variable
final_var
x_train<-trainset_2015[final_var]
iv.plot.summary(IV)

#--------------------------   SVM   -----------------------------------------------

library("e1071")
#trainset_2015<-x_train
# obj <- tune.svm(trainset_2015$WD~., data = trainset_2015, 
#                 cost = 2^(2:8), 
#                 kernel = "linear") 

svm.model <- svm(trainset_2015$WD ~ ., data = trainset_2015, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset_2015[,-23])

library(rpart)
rpart.model <- rpart(trainset_2015$WD ~ ., data = trainset_2015)
rpart.pred <- predict(rpart.model, testset_2015[,-23], type = "class")

table(pred = svm.pred, true = testset_2015[,23])
# true
# pred      0     1
# 0     15890  1768
# 1       147   162
table(pred = rpart.pred, true = testset_2015[,23])
# true
# pred       0     1
# 0      16037  1930
# 1          0     0

SVM_pred<-as.data.frame(svm.pred)

length(pred_prob)
pred_prob
pred_prob<-as.data.frame(pred_prob)
pred_prob$Probability[pred_prob$pred_prob>0.1071429]<-"Yes"
pred_prob$Probability[pred_prob$pred_prob<=0.1071429]<-"No"

anova(svm.model,rpart.model,fit)

#------------------------ Logistic Regression -------------------------------------

library(devtools)
library(woe)

fit <- glm(trainset_2015$WD~.,data=trainset_2015,family=binomial())
summary(fit) # display results

pred_prob<-predict (fit, newdata=testset_2015, type="response")


library (ROCR)
pred <- prediction(pred_prob, testset_2015$WD)
performance(pred, 'auc')
roc <- performance (pred,"tpr","tnr")
plot (roc)
perf <-as.data.frame(cbind(roc@alpha.values[[1]], roc@x.values[[1]], roc@y.values[[1]]))
colnames(perf) <-c("Probability","TNR","TPR")
perf <-perf[-1,]

library(reshape)
perf2<- melt(perf, measure.vars = c("TNR", "TPR"))

library(ggplot2)
g<-ggplot(perf2, aes(Probability, value, colour = variable)) + geom_line()+ theme_bw()

g+geom_hline(yintercept = 0.8)

plot(perf2$Probability, perf2$value)
abline(h=0.8)

f2 <- approxfun(perf2$value, perf2$Probability)
v0 <- 0.8
f2(v0)

library(SDMTools)
confusion.matrix (testset_2015$WD, pred_prob, threshold =0.170952)

# obs
# pred       0    1
# 0      12949  402
# 1       3088 1528

length(pred_prob)
pred_prob
pred_prob<-as.data.frame(pred_prob)
pred_prob$Probability[pred_prob$pred_prob>0.1071429]<-1
pred_prob$Probability[pred_prob$pred_prob<=0.1071429]<-0

SVM_LogisticProb<-cbind(SVM_pred,pred_prob$Probability)
colnames(SVM_LogisticProb)[1] <- "SVM"
colnames(SVM_LogisticProb)[2] <- "Logistic_Regression"


SVM_LogisticProb$SVM[SVM_LogisticProb$SVM=="0"] <- 0
SVM_LogisticProb$SVM[SVM_LogisticProb$SVM=="1"] <- 1
SVM_LogisticProb$SVM<-as.numeric(SVM_LogisticProb$SVM)-1

Addition<-as.numeric(SVM_LogisticProb$SVM)+as.numeric(SVM_LogisticProb$Logistic_Regression)
cbind(SVM_LogisticProb,Addition)

table(pred = Addition$Addition, true = testset_2015[,23])

# true
# pred       0     1
# 0      12319   310
# 1       3718  1620

SVM_LogisticProb<-cbind(SVM_LogisticProb,Addition$Addition)

#------------------------------- Random Forest -----------------------

library(randomForest)
fit <- randomForest(trainset_2015$WD ~.,   data=kyphosis)
print(fit) # view results 
importance(fit) 

trainset_2015<-trainset_2015[,-20]
datasets.rf <- randomForest::randomForest(trainset_2015[,-1], trainset_2015[,1], prox=TRUE)
datasets.rf
datasets.p <- randomForest::classCenter(trainset_2015[,-1], trainset_2015[,5], datasets.rf$prox)
datasets.p

x<-datasets.rf$confusion
x
z<-x[,-3]
library(psych)
tr(z)/nrow(trainset_2015)
#92.55%

#----------------------- KNN Algorithm -------------------------------

library(class)

prc_test_pred <- knn(train = trainset_2015, test = testset_2015,cl = prc_train_labels, k=10)

#------------------------ One R    ------------------------

library(mlbench)
library("FSelector")

weights <- oneR(trainset_2015$WD~., trainset_2015)
weights

----------------------------------------------------------------------
  
library(party) 
ind = sample(2,nrow(trainset_2015),replace=TRUE,prob=c(0.8,0.2))
myFormula <- trainset_2015$WD ~ .
flight_ctree <- ctree(myFormula,data = trainset_2015[ind==1,])
Ctree_pred<-predict(flight_ctree)
table(predict(flight_ctree))
plot(flight_ctree)
table(pred = Ctree_pred, true = trainset_2015[,23])
