combino <- readRDS("data/data.Rda")
library(caret)
library(pROC)
library(Metrics)
library(e1071)
library(ranger)
require(compiler)
multiClassSummary <- cmpfun(function (data, lev = NULL, model = NULL)
{
  #Load Libraries
  require(Metrics)
  require(caret)
  #Check data
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
    stop("levels of observed and predicted data do not match")
  #Calculate custom one-vs-all stats for each class
  prob_stats <- lapply(levels(data[, "pred"]), function(class)
  {
    #Grab one-vs-all data for the class
    pred <- ifelse(data[, "pred"] == class, 1, 0)
    obs <- ifelse(data[, "obs"] == class, 1, 0)
    prob <- data[,class]
    #Calculate one-vs-all AUC and logLoss and return
    cap_prob <- pmin(pmax(prob, .000001), .999999)
    prob_stats <- c(auc(obs, prob), logLoss(obs, cap_prob))
    names(prob_stats) <- c("ROC", "logLoss")
    return(prob_stats)
  })
  prob_stats <- do.call(rbind, prob_stats)
  rownames(prob_stats) <- paste( "Class:" , levels(data[, "pred"]))
  
  #Calculate confusion matrix-based statistics
  CM <- confusionMatrix(data[, "pred"], data[, "obs"])
  #Aggregate and average class-wise stats
  #Todo: add weights
  class_stats <- cbind(CM$byClass, prob_stats)
  class_stats <- colMeans(class_stats)
  #Aggregate overall stats
  overall_stats <- c(CM$overall)
  
  #Combine overall with class-wise stats and remove some stats we don't want
  stats <- c(overall_stats, class_stats)
  stats <- stats[! names(stats) %in% c("AccuracyNull","Prevalence", "Detection Prevalence")]
  
  #Clean names and return
  
  names(stats) <- gsub('[[:blank:]] +', '_' , names(stats))
  return(stats)
})

library(ROCR)
set.seed(12345)
inTrainingSet <- createDataPartition(combino$V11, p=.7, list=FALSE)
labelTrain <- combino[ inTrainingSet,]

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     summaryFunction = multiClassSummary,
                     classProbs = TRUE)


set.seed(1024)

rfFit <- train(V11 ~ ., data = labelTrain,
               ## training model: svm >>>
               method = "ranger",
               metric = "ROC",
               tuneLength = 10,
               trControl = ctrl)

peakfunc <- function (x, y){
  return(x[which(diff(sign(diff(y)))==-2)])
}
library(Hmisc)

interval <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5)
interval
transform1 <- function(bb){
  answers8 <- peakfunc(density(bb$DNA_Index)$x, density(bb$DNA_Index)$y)
  answers8
  
  interval <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5)
  interval
  
  matrix2 <- rep(0, 10)
  z1 <- interval[1:10]
  z2 <- interval[2:11]
  for (i in answers8){
    for (j in 1:10){
      if (i > z1[j] & i < z2[j]){
        matrix2[j] <- matrix2[j] + 1
      }
    }
  }
  matrix2 <- as.data.frame(t(matrix2))
  matrix2$V11 <- "c"
  matrix2$V11 <- as.factor(matrix2$V11)
  matrix2 <- rbind(matrix2, matrix2)
  return(matrix2)
}

plotmatrix2 <- function(bb){
  answers8 <- peakfunc(density(bb$DNA_Index)$x, density(bb$DNA_Index)$y)
  answers8
  
  interval <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5)
  interval
  
  matrix2 <- rep(0, 10)
  z1 <- interval[1:10]
  z2 <- interval[2:11]
  for (i in answers8){
    for (j in 1:10){
      if (i > z1[j] & i < z2[j]){
        matrix2[j] <- matrix2[j] + 1
      }
    }
  }
  matrix2 <- as.data.frame(t(matrix2))
  matrix2$V11 <- "c"
  matrix2$V11 <- as.factor(matrix2$V11)
  matrix2 <- rbind(matrix2, matrix2)
  jk <- as.numeric(as.matrix(matrix2[1,1:10]))
  names(jk) = c("0.5-1.5", "1.5-2.5", "2.5-3.5", "3.5-4.5", "4.5-5.5", 
                "5.5-6.5", "6.5-7.5", "7.5-8.5", "8.5-9.5", "9.5-10.5")
  barplot(jk, xlab = "intervals", ylab = "peaks")
  text(labels = round(jk, digits = 2), x = seq(0.7, 12, by = 1.2),
       y = rep(1.5, 10))
}

fabaf <- function(bb){
  answers8 <- peakfunc(density(bb$DNA_Index)$x, density(bb$DNA_Index)$y)
  answers8
  
  interval <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5)
  interval
  
  matrix2 <- rep(0, 10)
  z1 <- interval[1:10]
  z2 <- interval[2:11]
  for (i in answers8){
    for (j in 1:10){
      if (i > z1[j] & i < z2[j]){
        matrix2[j] <- matrix2[j] + 1
      }
    }
  }
  matrix2 <- as.data.frame(t(matrix2))
  matrix2$V11 <- "c"
  matrix2$V11 <- as.factor(matrix2$V11)
  matrix2 <- rbind(matrix2, matrix2)
  rfProbs <- predict(rfFit, matrix2, type = "prob")
  return(rfProbs[1,1]*100)
  
}

load("C:/Users/Yicheng/Desktop/chenresearch/exfoliative cell data 20161230/MotionDetection/rforest/data/suprema.Rda")
rfProbs1 <- predict(rfFit, suprema, type = "prob")
rfProbs1

ploty1 <- function(bb){
  
  answers8 <- peakfunc(density(bb$DNA_Index)$x, density(bb$DNA_Index)$y)
  answers8
  
  interval <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5)
  interval
  
  matrix2 <- rep(0, 10)
  z1 <- interval[1:10]
  z2 <- interval[2:11]
  for (i in answers8){
    for (j in 1:10){
      if (i > z1[j] & i < z2[j]){
        matrix2[j] <- matrix2[j] + 1
      }
    }
  }
  matrix2 <- as.data.frame(t(matrix2))
  matrix2$V11 <- "c"
  matrix2$V11 <- as.factor(matrix2$V11)
  matrix2 <- rbind(matrix2, matrix2)
  rfProbs <- predict(rfFit, matrix2, type = "prob")
  
  par(mfrow=c(1,2), pin = c(2,2))
  labels <- c(rep("OSCC", 93), rep("Normal", 102), rep("OLK", 82))
  dts <- c(rfProbs1$c)
  dt2plot <- as.data.frame (list (lab = labels, dt = dts))
  boxplot(dt ~ lab, data = dt2plot, ylab = "OCRI", outline = FALSE,ylim = c(0,1))
  stripchart(dt ~ lab, vertical = TRUE, data = dt2plot
             , add = TRUE, pch = 20, col = "green")
  if(rfProbs[1,1] > 0.5){
    plot(rfProbs[1,1], ylim = c(0,1), xaxt='n', xlab = "Patient", ylab = "OCRI", col = "red", pch = 20)
  }
  else{
    plot(rfProbs[1,1], ylim = c(0,1), xaxt='n', xlab = "Patient", ylab = "OCRI", col = "blue", pch = 20)
  }
}

ploty2 <- function(bb){
  
  answers8 <- peakfunc(density(bb$DNA_Index)$x, density(bb$DNA_Index)$y)
  answers8
  
  interval <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5)
  interval
  
  matrix2 <- rep(0, 10)
  z1 <- interval[1:10]
  z2 <- interval[2:11]
  for (i in answers8){
    for (j in 1:10){
      if (i > z1[j] & i < z2[j]){
        matrix2[j] <- matrix2[j] + 1
      }
    }
  }
  matrix2 <- as.data.frame(t(matrix2))
  matrix2$V11 <- "c"
  matrix2$V11 <- as.factor(matrix2$V11)
  matrix2 <- rbind(matrix2, matrix2)
  rfProbs <- predict(rfFit, matrix2, type = "prob")
  
  
  if(rfProbs[1,1] > 0.5){
    plot(rfProbs[1,1], ylim = c(0,1), xaxt='n', xlab = "Patient", ylab = "OCRI", type='n')
    text(1,rfProbs[1,1],label=rfProbs[1,1],col='red')
  }
  else{
    plot(rfProbs[1,1], ylim = c(0,1), xaxt='n', xlab = "Patient", ylab = "OCRI", type='n')
    text(1,rfProbs[1,1],label=rfProbs[1,1],col='blue')
  }
}