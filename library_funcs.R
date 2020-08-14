diskcleanup <- function(nm) {
  rm(list=nm, envir = .GlobalEnv); invisible( gc() )
}

dummydataframe <- function() {
  return (data.frame(matrix(ncol=0,nrow=0)))
}

CountWords<-function(x){
  str_count(x, '\\w+')
}

UniqueWords<-function(x){ #remove short len words (<=3)
  unique(unlist(strsplit(x, " ")))
}

RemoveShortWords<-function(x){ #remove short len words (<=3)
  y = unlist(strsplit(x, " ")) #lower the case
  paste(y[nchar(y)>=gminwordlen], collapse=" ")
}

RemoveStopWords<-function(x){ 
  x = unlist(rm_stopwords(x, tm::stopwords("english")))
  paste(x, collapse=" ")
}

model.predict.accu<-function(model, df){
  prediction <- predict(model, df)
  xtab <- table(df$Y, prediction)
  A=as.matrix(xtab)
  acc = sum(A[row(A)==col(A)])/nrow(df)
  return (acc)
}

GBMModel.predict.accu<-function(model, df, ntrees){
  
  predict.class <- predict(model,  newdata=df, n.trees = ntrees,
                           type = "response")
  pred_class <- apply(predict.class, 1, which.max)
  xtab <- table(df$Y, pred_class)
  A=as.matrix(xtab)
  acc = sum(A[row(A)==col(A)])/nrow(df)
  return (acc)
}

#fill NA with mean
FillNAWithMean<-function(data){
  for(i in 1:ncol(data)) {
    data[ , i][is.na(data[ , i])] <- mean(data[ , i], na.rm = TRUE)
  }
  return (data)  
}

#calculate the metric with the help of the caret package
model.predict.onevsall.metrics <- function(model, df, predictions) {
  
  cm <- vector("list", length(levels(df$Y)))
  for (i in seq_along(cm)) {
    positive.class <- levels(df$Y)[i]
    # in the i-th iteration, use the i-th class as the positive class
    cm[[i]] <- confusionMatrix(predictions, df$Y, 
                               positive = positive.class)
  }
  
  c <- cm[[1]]$byClass # a single matrix is sufficient
  re <- sum(c[, "Recall"]) / nrow(c)
  pr <- sum(c[, "Precision"]) / nrow(c)
  acc <- sum(c[, "Balanced Accuracy"]) / nrow(c)
  f1 <- 2 * ((re * pr) / (re + pr))
  print(paste0("Macro F1 is: ", round(f1, 2)))
  
  acc2 = length(which(predictions == df$Y)) / length(df$Y)
  print(paste0("Macro Accuracy is: ", round(acc, 2), ", ", round(acc2, 2)))
  
  metrics <- c("Balanced Accuracy", "Precision", "Recall", "F1")
  print("Classwise metrics is - ")
  print(cm[[1]]$byClass[, metrics])
}
