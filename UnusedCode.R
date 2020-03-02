# to extract each no-multicol of each combination

indices <- vector()
temp <- multicolRes[multicolRes$res == "No",]
lastEnergy <- str_match(string = temp[nrow(temp),]$vs, pattern = "^[a-z]{5}y(.{1,2}) \\&")
lastEnergy <- lastEnergy[1,2]
rownames(temp) <- NULL
for (i in 1:lastEnergy) {
     idx <- grep(pattern = paste0("energy", i, " "), x = temp$vs)[1]
     indices <- c(indices, idx)
}
indices <- indices[!is.na(indices)]
temp[indices,]

# comparing both models by using ROC/AUC

## get probability values of KNN predictions
knn_prob <- knn(train = X_train.scaled, test = X_test.scaled, cl = y_train, k = round(K), prob = T)
predict_knnProb <- attr(knn_prob, which = "prob")

## ROC curve
options(scipen = 999)
df_ <- data.frame("pred_LogRes" = predict_log,
                  "pred_KNN" = predict_knnProb,
                  "trueclass" = as.numeric(y_test == "R"))
LogResROC <- prediction(df_$pred_LogRes, df_$trueclass)
perfLogRes <- performance(LogResROC, "tpr", "fpr")
KNN.ROC <- prediction(df_$pred_KNN, df_$trueclass)
perfKNN <- performance(KNN.ROC, "tpr", "fpr")
rocDF.LogRes <- data.frame(threshold = perfLogRes@alpha.values[[1]],
                           tpr = perfLogRes@y.values[[1]],
                           fpr = perfLogRes@x.values[[1]])
rocDF.KNN <- data.frame(threshold = perfKNN@alpha.values[[1]],
                        tpr = perfKNN@y.values[[1]],
                        fpr = perfKNN@x.values[[1]])
ggplot(rocDF.LogRes, aes(x = fpr, y = tpr)) +
     geom_line(color = "blue") +
     geom_point(color = "black") +
     # geom_line(data = rocDF.KNN, aes(x = fpr, y = tpr), color = "red") +
     # geom_point(data = rocDF.KNN, aes(x = fpr, y = tpr), color = "black") +
     geom_abline(color = "grey", intercept = 0, slope = 1, linetype="dashed") +
     labs(main = "Receiver operating characteristic curve",
          x = "False positive rate",
          y = "True positive rate")

# show AUC value
auc <- ROCR::performance(prediction.obj = LogResROC, "auc")
auc@y.values[[1]]

## Tuning threshold
options(scipen = 999)
df_ <- data.frame("prediction" = predict_log2, 
                  "trueclass" = as.numeric(y_test == "R"))
df__roc <- prediction(df_$prediction, df_$trueclass)
perf <- performance(df__roc, "tpr", "fpr")
rocDF <- data.frame(threshold = perf@alpha.values[[1]],
                    tpr = perf@y.values[[1]],
                    fpr = perf@x.values[[1]])
ggplot(rocDF, aes(x = fpr, y = tpr)) +
     geom_line(color = "blue") +
     geom_point(color = "black") +
     # geom_roc(n.cuts=20,labels=FALSE) +
     # style_roc(theme = theme_grey) +
     geom_abline(color = "grey", intercept = 0, slope = 1, linetype="dashed") +
     labs(main = "Receiver operating characteristic curve",
          x = "False positive rate",
          y = "True positive rate")

## find best threshold
rocDF$threshold[1] <- 1
rocDF$spec <- 1 - rocDF$fpr # find specificity value by 1 - false positive rate
rocDF[which.max(rocDF$tpr + rocDF$spec),] # get the best threshold by finding maximum true positive rate and maximum specificity
optThreshold <- rocDF[which.max(rocDF$tpr + rocDF$spec),1]