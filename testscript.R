rm(list=setdiff(ls(), c("train", "test")))

fitControl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 5)

nzvAll <- nzv(train)
train2 <- train[,-nzvAll]
test2 <- test[,-nzvAll]

naCols <- colSums(is.na(train2))
quantile(naCols) # a quick look at how many NAs per column there are overall
train3 <- train2[,-which(naCols > 0)]
test3 <- test2[,-which(naCols > 0)]

vecList <- unlist(lapply(split(train3, train3$user_name), nzv))
nzvByUser <- sort(unique(vecList))
nzvByUser <- nzvByUser[2:length(nzvByUser)]

train4 <- train3[,-nzvByUser]
test4 <- test3[,-nzvByUser]
train5 <- train4[,c(2,7:53)]
test5 <- test4[,c(2,7:53)]

tmpTrain <- createDataPartition(train5$classe, p = 0.75, list = FALSE)

trainNN <- train5[,2:48]

#train a non-PCA model without username cols
tmp <- Sys.time()
modFitNN <- train(classe~., data = trainNN[tmpTrain,], method = "rf", trControl = fitControl)
tdifNN <- Sys.time() - tmp
cmNN <- confusionMatrix(trainNN$classe[-tmpTrain], predict(modFitNN,trainNN[-tmpTrain,]))
predNN <- predict(modFitNN,test5[,2:48])


#train a non-PCA model with username cols
dmyNames <- dummyVars("~ user_name", data = train5)
trsf <- data.frame(predict(dmyNames,train5))
train6 <- cbind(trsf, train5[,2:48])
dmyNames2 <- dummyVars("~ user_name", data = test5)
trsf2 <- data.frame(predict(dmyNames2,test5))
test6 <- cbind(trsf2, test5[,2:48])

tmp <- Sys.time()
modFitWN <- train(classe~., data = train6[tmpTrain,], method = "rf", trControl = fitControl)
tdifWN <- Sys.time() - tmp
cmWN <- confusionMatrix(train6$classe[-tmpTrain], predict(modFitWN,train6[-tmpTrain,]))
predWN <- predict(modFitWN,test6)

#train a PCA model without username cols
ppNN <- preProcess(train5[tmpTrain,2:47],method="pca")
trainPNN <- predict(ppNN,train5[tmpTrain,2:47])
trtestPNN <- predict(ppNN,train5[-tmpTrain,2:47])

predTestPNN <- predict(ppNN,test5[,2:47])
tmp <- Sys.time()
modPNN <- train(train5$classe[tmpTrain]~., method = "rf", data = trainPNN, trControl = fitControl)
tdifPNN <- Sys.time() - tmp
cmPNN <- confusionMatrix(train5$classe[-tmpTrain],predict(modPNN,trtestPNN))
predPNN <- predict(modPNN,predTestPNN)

#train a PCA model with username cols
ppWN <- preProcess(train6[tmpTrain,-53],method="pca")
trainPWN <- predict(ppWN,train6[tmpTrain,-53])
trtestPWN <- predict(ppWN,train6[-tmpTrain,-53])

predTestPWN <- predict(ppWN,test6[,-53])
tmp <- Sys.time()
modPWN <- train(train6$classe[tmpTrain] ~.,method = "rf", data = trainPWN, trControl = fitControl)
tdifPWN <- Sys.time() - tmp
cmPWN <- confusionMatrix(train6$classe[-tmpTrain],predict(modPWN,trtestPWN))
predPWN <- predict(modPWN, predTestPWN)
