tSmall <- createDataPartition(train5$classe, p = 0.75, list = FALSE)


pp <- preProcess(train5[tSmall,-48],method="pca")
trainPC <- predict(pp,train5[tSmall,-48])

modPPgbm <- train(train5$classe[tSmall] ~.,method = "gbm", data = trainPC, verbose = FALSE)
modPPrf <- train(train5$classe[tSmall] ~.,method = "rf", data = trainPC)

testPC5 <- predict(pp, train5[-tSmall,-48])

#confusionMatrix(train5$classe[-tSmall],predict(modPP,testPC5))

pp2 <- preProcess(train6[tSmall,-49],method="pca")
trainPC2 <- predict(pp2,train6[tSmall,-49])

modPPgbm2 <- train(train6$classe[tSmall] ~.,method = "gbm", data = trainPC2, verbose = FALSE)
modPPrf2 <- train(train6$classe[tSmall] ~.,method = "rf", data = trainPC2)

testPC6 <- predict(pp2, train6[-tSmall,-49])


confusionMatrix(train5$classe[-tSmall],predict(modPPgbm,testPC5))
confusionMatrix(train5$classe[-tSmall],predict(modPPrf,testPC5))
confusionMatrix(train6$classe[-tSmall],predict(modPPgbm2,testPC6))
confusionMatrix(train6$classe[-tSmall],predict(modPPrf2,testPC6))

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("predictions/problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
