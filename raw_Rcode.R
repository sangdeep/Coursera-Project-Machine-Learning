
setwd("G:/Coursera/Course 8 - Machine Learning/Courser Project")

Activity<-read.csv("pml-training.csv",header = T)


names(Activity)

str(Activity)





!is.na(colSums(Activity[,grep("^gyros",names(Activity))]))

!is.na(colSums(Activity[,grep("^accel",names(Activity))]))

!is.na(colSums(Activity[,grep("^magnet",names(Activity))]))

!is.na(colSums(Activity[,grep("^roll",names(Activity))]))

!is.na(colSums(Activity[,grep("^pitch",names(Activity))]))

!is.na(colSums(Activity[,grep("^yaw",names(Activity))]))


selcols<-grep("^((gyros)|(accel)|(magnet)|(roll)|(pitch)|(yaw))",names(Activity))
Activity_F<-Activity[,selcols]
Activity_F$classe<-Activity$classe

table(Activity_F$classe)

library(caret)
train<-createDataPartition(Activity_F$classe,p=0.7,list = F)

Activity_train<-Activity_F[train,]
Activity_test<-Activity_F[-train,]

library(tree)
tree.ac<-tree(classe~.,data = Activity_train)
summary(tree.ac)
tree.pred<-predict(tree.ac,newdata = Activity_test,type = "class")
table(tree.pred,Activity_test$classe)

cv.ac<-cv.tree(tree.ac,FUN = prune.misclass)
cv.ac$size
cv.ac$dev
plot(cv.ac$size,cv.ac$dev,type = "b")
plot(cv.ac$k,cv.ac$dev,type = "b")
prune.ac<-prune.misclass(tree.ac,best = 17)
plot(prune.ac)
text(prune.ac,pretty = 0)

prune.pred<-predict(prune.ac,newdata = Activity_test,type = "class")

((table(prune.pred,Activity_test$classe)[1,1]+
    table(prune.pred,Activity_test$classe)[2,2]+
    table(prune.pred,Activity_test$classe)[3,3]+
    table(prune.pred,Activity_test$classe)[4,4]+
    table(prune.pred,Activity_test$classe)[5,5])/nrow(Activity_test))*100

library(randomForest)

forest.ac<-randomForest(classe~.,data = Activity_train,importance = T)

forest.ac1<-randomForest(classe~.,data = Activity_train,importance = T)


plot(forest.ac$)

?randomForest

forest.pred<-predict(forest.ac,newdata = Activity_test,type = "class",rfc)

summary(forest.ac)

100 - (((table(forest.pred,Activity_test$classe)[1,1]+
    table(forest.pred,Activity_test$classe)[2,2]+
    table(forest.pred,Activity_test$classe)[3,3]+
    table(forest.pred,Activity_test$classe)[4,4]+
    table(forest.pred,Activity_test$classe)[5,5])/nrow(Activity_test))*100)

importance(forest.ac)

forest.ac$oob.times

library(gbm)


plot(forest.ac)

varImpPlot(forest.ac)


final_test<-read.csv("pml-testing.csv",header = T)

selcol1s<-grep("^((gyros)|(accel)|(magnet)|(roll)|(pitch)|(yaw))",names(final_test))


activity.cv<-rfcv(Activity_train[,-49],Activity_train[,49])

final_test1<-final_test[,selcol1s]


plot(activity.cv$n.var,activity.cv$error.cv,xlab = "No. Of Variable",
     ylab = "Misclassification Rate",main = "Cross Validation Output")

final.pred<-predict(forest.ac,newdata = final_test1,type = "class")
final.pred

rm(list = ls())

Activity_train[,rownames(top6)[1:6]]
