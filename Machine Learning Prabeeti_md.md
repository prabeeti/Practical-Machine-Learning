1. Background
=============

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now
possible to collect a large amount of data about personal activity
relatively inexpensively. These type of devices are part of the
quantified self movement – a group of enthusiasts who take measurements
about themselves regularly to improve their health, to find patterns in
their behavior, or because they are tech geeks. One thing that people
regularly do is quantify how much of a particular activity they do, but
they rarely quantify how well they do it.

2. Objective
============

In this project, my objective is to use data from accelerometers on the
belt, forearm, arm, and dumbell of 6 participants and predict the manner
in which these particiapnts have prerformed these exercises. They were
asked to perform barbell lifts correctly and incorrectly in 5 different
ways. \#Data Source & References:-
\#<a href="http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har" class="uri">http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har</a>
\#(refer-Weight Lifting Exercise Data) \#Training data -
<a href="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv" class="uri">https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv</a>
\#Test
Set-<a href="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv" class="uri">https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv</a>

\# 3. Data Loading and Exploratory Analysis

    # Following library are used
    library(knitr)
    library(caret)
    library(rpart)
    library(rpart.plot)
    library(rattle)
    library(randomForest)
    library(corrplot)
    library(janitor)
    set.seed(123)
    # Data Extraction
    Training_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    Testing_url  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    Training_data<-"pml-traininig.csv"
    Testing_data<-"pml-testing.csv"

    if(!file.exists(Training_data))
    {
        download.file(Training_url,destfile = Training_data,method="curl")
    }
    training <- read.csv(Training_data, header = TRUE, sep = ",", na.strings=c("NA","#DIV/0!",""))
    dim(training)

    ## [1] 19622   160

    if(!file.exists(Testing_data))
    {
        download.file(Testing_url,destfile = Testing_data,method="curl")
    }
    testing  <- read.csv(Testing_data, header = TRUE, sep = ",", na.strings=c("NA","#DIV/0!",""))
    dim(testing)

    ## [1]  20 160

    # create a partition using caret with the training dataset on 60,40 ratio
    inTraining  <- createDataPartition(training$classe, p=0.6, list=FALSE)

    Training_dataset <- training[inTraining, ]

    Test_dataset  <- training[-inTraining, ]
    # Exploring the data in training and test data
    dim(Training_dataset)

    ## [1] 11776   160

    dim(Test_dataset)

    ## [1] 7846  160

    # Training and testing dataset both have 160 variables, with records count divided as 60:40 ratio
    str(Training_dataset)

    ## 'data.frame':    11776 obs. of  160 variables:
    ##  $ X                       : int  1 2 5 7 12 13 17 18 19 20 ...
    ##  $ user_name               : Factor w/ 6 levels "adelmo","carlitos",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ raw_timestamp_part_1    : int  1323084231 1323084231 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 ...
    ##  $ raw_timestamp_part_2    : int  788290 808298 196328 368296 528316 560359 692324 732306 740353 788335 ...
    ##  $ cvtd_timestamp          : Factor w/ 20 levels "02/12/2011 13:32",..: 9 9 9 9 9 9 9 9 9 9 ...
    ##  $ new_window              : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ num_window              : int  11 11 12 12 12 12 12 12 12 12 ...
    ##  $ roll_belt               : num  1.41 1.41 1.48 1.42 1.43 1.42 1.51 1.55 1.57 1.59 ...
    ##  $ pitch_belt              : num  8.07 8.07 8.07 8.09 8.18 8.2 8.12 8.08 8.06 8.07 ...
    ##  $ yaw_belt                : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
    ##  $ total_accel_belt        : int  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ kurtosis_roll_belt      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ kurtosis_picth_belt     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ kurtosis_yaw_belt       : logi  NA NA NA NA NA NA ...
    ##  $ skewness_roll_belt      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ skewness_roll_belt.1    : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ skewness_yaw_belt       : logi  NA NA NA NA NA NA ...
    ##  $ max_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ max_picth_belt          : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ max_yaw_belt            : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ min_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ min_pitch_belt          : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ min_yaw_belt            : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ amplitude_roll_belt     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ amplitude_pitch_belt    : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ amplitude_yaw_belt      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ var_total_accel_belt    : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ avg_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ stddev_roll_belt        : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ var_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ avg_pitch_belt          : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ stddev_pitch_belt       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ var_pitch_belt          : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ avg_yaw_belt            : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ stddev_yaw_belt         : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ var_yaw_belt            : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ gyros_belt_x            : num  0 0.02 0.02 0.02 0.02 0.02 0 0 0 0.02 ...
    ##  $ gyros_belt_y            : num  0 0 0.02 0 0 0 0 0.02 0 0 ...
    ##  $ gyros_belt_z            : num  -0.02 -0.02 -0.02 -0.02 -0.02 0 -0.02 0 -0.02 -0.02 ...
    ##  $ accel_belt_x            : int  -21 -22 -21 -22 -22 -22 -21 -21 -20 -22 ...
    ##  $ accel_belt_y            : int  4 4 2 3 2 4 4 5 5 5 ...
    ##  $ accel_belt_z            : int  22 22 24 21 23 21 22 21 21 22 ...
    ##  $ magnet_belt_x           : int  -3 -7 -6 -4 -2 -3 -6 1 -3 -1 ...
    ##  $ magnet_belt_y           : int  599 608 600 599 602 606 598 600 603 604 ...
    ##  $ magnet_belt_z           : int  -313 -311 -302 -311 -319 -309 -317 -316 -313 -314 ...
    ##  $ roll_arm                : num  -128 -128 -128 -128 -128 -128 -129 -129 -129 -129 ...
    ##  $ pitch_arm               : num  22.5 22.5 22.1 21.9 21.5 21.4 21.3 21.2 21.2 21.1 ...
    ##  $ yaw_arm                 : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
    ##  $ total_accel_arm         : int  34 34 34 34 34 34 34 34 34 34 ...
    ##  $ var_accel_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ avg_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ stddev_roll_arm         : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ var_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ avg_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ stddev_pitch_arm        : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ var_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ avg_yaw_arm             : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ stddev_yaw_arm          : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ var_yaw_arm             : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ gyros_arm_x             : num  0 0.02 0 0 0.02 0.02 0.02 0.02 0.02 0.02 ...
    ##  $ gyros_arm_y             : num  0 -0.02 -0.03 -0.03 -0.03 -0.02 0 -0.02 -0.02 -0.02 ...
    ##  $ gyros_arm_z             : num  -0.02 -0.02 0 0 0 -0.02 -0.02 -0.03 -0.02 -0.02 ...
    ##  $ accel_arm_x             : int  -288 -290 -289 -289 -288 -287 -289 -288 -289 -289 ...
    ##  $ accel_arm_y             : int  109 110 111 111 111 111 110 108 109 109 ...
    ##  $ accel_arm_z             : int  -123 -125 -123 -125 -123 -124 -122 -124 -122 -125 ...
    ##  $ magnet_arm_x            : int  -368 -369 -374 -373 -363 -372 -371 -373 -369 -373 ...
    ##  $ magnet_arm_y            : int  337 337 337 336 343 338 337 336 340 335 ...
    ##  $ magnet_arm_z            : int  516 513 506 509 520 509 512 510 509 514 ...
    ##  $ kurtosis_roll_arm       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ kurtosis_picth_arm      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ kurtosis_yaw_arm        : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ skewness_roll_arm       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ skewness_pitch_arm      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ skewness_yaw_arm        : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ max_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ max_picth_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ max_yaw_arm             : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ min_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ min_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ min_yaw_arm             : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ amplitude_roll_arm      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ amplitude_pitch_arm     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ amplitude_yaw_arm       : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ roll_dumbbell           : num  13.1 13.1 13.4 13.1 13.1 ...
    ##  $ pitch_dumbbell          : num  -70.5 -70.6 -70.4 -70.2 -70.5 ...
    ##  $ yaw_dumbbell            : num  -84.9 -84.7 -84.9 -85.1 -84.9 ...
    ##  $ kurtosis_roll_dumbbell  : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ kurtosis_picth_dumbbell : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ kurtosis_yaw_dumbbell   : logi  NA NA NA NA NA NA ...
    ##  $ skewness_roll_dumbbell  : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ skewness_pitch_dumbbell : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ skewness_yaw_dumbbell   : logi  NA NA NA NA NA NA ...
    ##  $ max_roll_dumbbell       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ max_picth_dumbbell      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ max_yaw_dumbbell        : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ min_roll_dumbbell       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ min_pitch_dumbbell      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ min_yaw_dumbbell        : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ amplitude_roll_dumbbell : num  NA NA NA NA NA NA NA NA NA NA ...
    ##   [list output truncated]

    # Data cleansing - cleaning the data for NA, The Near Zero variance (NZV) and ID variables 
    Training_nearZeroVar <- nearZeroVar(Training_dataset)
    Training_dataset <- Training_dataset[, -Training_nearZeroVar]
    Test_nearZeroVar <- nearZeroVar(Test_dataset)
    Test_dataset  <- Test_dataset[, -Test_nearZeroVar]
    label <- apply(Training_dataset, 2, function(x) mean(is.na(x))) > 0.95
    Training_dataset <- Training_dataset[, -which(label, label == FALSE)]
    label_test <- apply(Test_dataset, 2, function(x) mean(is.na(x))) > 0.95
    Test_dataset <- Test_dataset[, -which(label_test, label_test == FALSE)]


    Training_dataset <- Training_dataset[ , -(1:5)]
    Test_dataset <- Test_dataset[ , -(1:5)]

    dim(Training_dataset)

    ## [1] 11776    54

    dim(Test_dataset)

    ## [1] 7846   54

    # Training and testing dataset  have 54 and 54  variables respectively after cleansing.

4.Predictive Modelling Processes
================================

4.1 Decision Tree Model
=======================

    # DEcision tree build
    set.seed(123)
    model_decisionTree <- rpart(classe ~ ., Training_dataset, method="class")
    fancyRpartPlot(model_decisionTree)

    ## Warning: labs do not fit even at cex 0.15, there may be some overplotting

![](Machine%20Learning%20Prabeeti_md_files/figure-markdown_strict/unnamed-chunk-15-1.png)

    # Prediction with decision tree model
    predict_DT<- predict(model_decisionTree, Test_dataset, type = "class")
    confusionMatrix_DT <-confusionMatrix(predict_DT, Test_dataset$classe)
    confusionMatrix_DT

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1955  122    3   25    8
    ##          B  124 1147   59   43   80
    ##          C    0   76 1100   39    4
    ##          D  134  130  177 1052  109
    ##          E   19   43   29  127 1241
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.8278          
    ##                  95% CI : (0.8193, 0.8361)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.7828          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.8759   0.7556   0.8041   0.8180   0.8606
    ## Specificity            0.9719   0.9516   0.9816   0.9162   0.9660
    ## Pos Pred Value         0.9252   0.7894   0.9024   0.6567   0.8506
    ## Neg Pred Value         0.9517   0.9420   0.9596   0.9625   0.9685
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2492   0.1462   0.1402   0.1341   0.1582
    ## Detection Prevalence   0.2693   0.1852   0.1554   0.2042   0.1860
    ## Balanced Accuracy      0.9239   0.8536   0.8929   0.8671   0.9133

    # plot confusion matrix results
    plot(confusionMatrix_DT$table, col = confusionMatrix_DT$byClass, 
         main = paste("Decision Tree - Accuracy =",
                      round(confusionMatrix_DT$overall['Accuracy'], 4)))

![](Machine%20Learning%20Prabeeti_md_files/figure-markdown_strict/unnamed-chunk-15-2.png)

4.2 Randon Forest Model
=======================

    # Random Forest build
    set.seed(123)
    train_ctrl <-  trainControl(method = "cv", classProbs=TRUE,savePredictions=TRUE,allowParallel=TRUE, number = 10)
    model_randomForest <- randomForest(classe ~ ., data = Training_dataset, method = "rf", importance = T, trControl = train_ctrl,na.action = na.exclude)


    # Prediction with random forest model
    predict_RF<- predict(model_randomForest, Test_dataset, type = "class")
    confusionMatrix_RF <-confusionMatrix(predict_RF, Test_dataset$classe)
    confusionMatrix_RF

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2231    2    0    0    0
    ##          B    0 1515   11    0    0
    ##          C    0    1 1357   11    0
    ##          D    0    0    0 1274    2
    ##          E    1    0    0    1 1440
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9963          
    ##                  95% CI : (0.9947, 0.9975)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9953          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9996   0.9980   0.9920   0.9907   0.9986
    ## Specificity            0.9996   0.9983   0.9981   0.9997   0.9997
    ## Pos Pred Value         0.9991   0.9928   0.9912   0.9984   0.9986
    ## Neg Pred Value         0.9998   0.9995   0.9983   0.9982   0.9997
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2843   0.1931   0.1730   0.1624   0.1835
    ## Detection Prevalence   0.2846   0.1945   0.1745   0.1626   0.1838
    ## Balanced Accuracy      0.9996   0.9981   0.9951   0.9952   0.9992

    # plot confusion matrix results for random forest
    plot(model_randomForest)

![](Machine%20Learning%20Prabeeti_md_files/figure-markdown_strict/unnamed-chunk-16-1.png)

4.3 Generalised Boosting Model
==============================

    # Generalised Boosting Model  build
    set.seed(123)
    gbm_control <- trainControl(method = "repeatedcv", number = 5, repeats =1,verboseIter = FALSE)
    model_Boosting <- train(classe ~ ., method = "gbm",data= Training_dataset,
                        verbose = FALSE,
                        trControl = gbm_control)

    model_Boosting

    ## Stochastic Gradient Boosting 
    ## 
    ## 11776 samples
    ##    53 predictor
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold, repeated 1 times) 
    ## Summary of sample sizes: 9420, 9421, 9420, 9421, 9422 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  Accuracy   Kappa    
    ##   1                   50      0.7572160  0.6919090
    ##   1                  100      0.8325396  0.7880080
    ##   1                  150      0.8694789  0.8348284
    ##   2                   50      0.8828099  0.8516050
    ##   2                  100      0.9369047  0.9201733
    ##   2                  150      0.9617859  0.9516436
    ##   3                   50      0.9290921  0.9102227
    ##   3                  100      0.9700235  0.9620708
    ##   3                  150      0.9864979  0.9829213
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## Tuning parameter 'n.minobsinnode' was held constant
    ##  at a value of 10
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10.

    plot(model_Boosting)

![](Machine%20Learning%20Prabeeti_md_files/figure-markdown_strict/unnamed-chunk-17-1.png)

    # Prediction with Generalised Boosting Model model
    predict_GBM<- predict(model_Boosting, Test_dataset)
    confusionMatrix_GBM <-confusionMatrix(predict_GBM, Test_dataset$classe)
    confusionMatrix_GBM

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2227   16    0    1    1
    ##          B    5 1481   14    5    4
    ##          C    0   16 1349   14    8
    ##          D    0    5    5 1262   10
    ##          E    0    0    0    4 1419
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9862          
    ##                  95% CI : (0.9834, 0.9887)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9826          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9978   0.9756   0.9861   0.9813   0.9840
    ## Specificity            0.9968   0.9956   0.9941   0.9970   0.9994
    ## Pos Pred Value         0.9920   0.9814   0.9726   0.9844   0.9972
    ## Neg Pred Value         0.9991   0.9942   0.9971   0.9963   0.9964
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2838   0.1888   0.1719   0.1608   0.1809
    ## Detection Prevalence   0.2861   0.1923   0.1768   0.1634   0.1814
    ## Balanced Accuracy      0.9973   0.9856   0.9901   0.9891   0.9917

Conclusion
==========

\#Accuracy obtained from 3 models are as below:- \#1. Decision Tree
Model -0.8278 \#2. Random Forest Model - 0.9963 \#3. Boosting Model -
0.9862

Out of these, accuracy is best for random forest model , so Random
Forest model is applied to predict the 20 quiz results

    predict_RF<- predict(model_randomForest, testing)
    predict_RF

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E

I have used assumption in processing data is to limit dataset for
attributes that are non-zero in the Training and tEsting dataset. I
haved faced lots of error about missing attributes in Testing data but
available in Training dataset but this got tackeled by using this
assumption.
