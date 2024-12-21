#1. Loading data 
  setwd("/Users/kitsanasudsaneh/Desktop/fall2024/MAX503/final Project")
  wine.raw <- read.csv("vivino.csv")

#2.Inspect the dataset 

#---Dimensions of the dataset
  n_rows <- nrow (wine.raw) #Number of observations (rows)
  n_columns <- ncol (wine.raw) #Number of variables (columns)

  cat("The dataset contains: \n")
  cat(n_rows,"Oberservation (rows)\n")
  cat(n_columns,"Variables (column)\n")

#---Sumarize the dataset 
  summary.wine <- summary(wine.raw)
  cat("Summary of the dataset: \n")
  summary.wine

#---Structure of the dataset 
  cat("Structure of the dataset: \n")
  str(wine.raw)

#---Correlation Plot 
  wine.df <- wine.raw[,-12] 
  #Exclude 'quality': group the data under the quality column

#compute correlation matrix:
  library(corrplot)
  corr_matrix <-cor(wine.df, use = "complete.obs")
  corr_matrix
#plot the correltion:
  corrplot(corr_matrix, method = "circle", type = "upper", 
      tl.cex = 0.8, tl.col = "black", addCoef.col = "black", 
      main = "\n\n\nPhysicochemical Properties of Bordeaux wine Correlation Plot")

#3.Cleaning and Preparing the data

#---Create a New Column with two groups(bad and good)
  wine.raw$quality.label <-ifelse(wine.raw$quality > 6, "Good","Bad")
  print(head(wine.raw$quality.label))
  table(wine.raw$quality.label)
  head(wine.raw) 
  
#---Bar Plot: Distribution of "Good" and "Bad" wines
  barplot(table(wine.raw$quality.label),
          main = "Distribution of Good vs. Bad Wines",
          col = c("cornflowerblue", "darkolivegreen"), names.arg = c("Bad", "Good"))
  

#---Splitting Data into Training and Testing Sets
  set.seed(04625)
  train.prop <- 0.70 #70% training, 30% testing
  train.cases <- sample(nrow(wine.raw), nrow(wine.raw) * train.prop)
  train.cases
  
  wine.df.train <- wine.raw[train.cases,]
  wine.df.test <- wine.raw[-train.cases,]

#---Checking the detail of sampling 
  cat("Training data dimensions:", dim(wine.df.train), "\n")
  print(table(wine.df.train$quality.label)) 
  #>> The training data set contains 956 wines labeled as "Bad" and 
  #>> 163 wines labeled as "Good": model will learn patterns from these sampling
  cat("Testing data dimensions :", dim(wine.df.test), "\n")
  print(table(wine.df.test$quality.label))
  #>> The testing data set contains 426 wines labeled as "Bad" and 
  #>> 54 wines labeled as "Good": not seen these sampling 

###---------------------------------------------------------------------###
##----------------------Naive Bayes------------------------------------##
  # Load the necessary libraries
  library(e1071)    # For Naive Bayes
  library(corrplot)   # For correlation plot
  library(cluster)    # For cluster plot
  library(mclust)     # For Adjusted Rand Index

  #----------- 4. Training the Naive Bayes Model ---------#
  
  # Load the necessary library
  library(e1071)
  
  # Ensure the target variable is a factor
  wine.df.train$quality.label <- as.factor(wine.df.train$quality.label)
  wine.df.test$quality.label <- as.factor(wine.df.test$quality.label)
  
  # Train the Naive Bayes model using the training data
  wine.nb <- naiveBayes(quality.label ~ . - quality, data = wine.df.train)
  
  
  #----------- 5. Making Predictions ---------#
  
  # Make predictions using the trained Naive Bayes model
  wine.nb.pred <- predict(wine.nb, wine.df.test)
  
  # View the first few predicted values
  head(wine.nb.pred)
  
  # Confusion Matrix
  cat("Confusion Matrix:\n")
  conf_matrix <- table(wine.nb.pred, wine.df.test$quality.label)
  print(conf_matrix)
  
  # Calculate Accuracy
  accuracy <- mean(wine.nb.pred == wine.df.test$quality.label)
  cat("Accuracy:", accuracy, "\n")
  
  
  #----------- 6. Adjusted Rand Index (ARI) ---------#
  
  # Load library for ARI calculation
  library(mclust)
  
  # Calculate Adjusted Rand Index (ARI) to measure the agreement between predicted and actual labels
  ari_nb <- adjustedRandIndex(wine.nb.pred, wine.df.test$quality.label)
  cat("Adjusted Rand Index (ARI):", ari_nb, "\n")
  
  
  #----------- 7. Summary Function ---------#
  
  # Create a function to summarize the features based on predicted quality label
  wine.summ <- function(data, groups) {
    aggregate(data, list(groups), function(x) mean(as.numeric(x)))  # Calculate mean of features based on predicted groups
  }
  
  # Summary for predicted values
  cat("Summary of Predicted Quality (Test Data):\n")
  summary_pred <- wine.summ(wine.df.test, wine.nb.pred)
  
  # Summary for actual values
  cat("Summary of Actual Quality (Test Data):\n")
  summary_actual <- wine.summ(wine.df.test, wine.df.test$quality.label)
  
  # Print the summaries
  print(summary_pred)
  print(summary_actual)

  #----------- 8. Visualizing Predictions (Cluster Plot) ---------#
  
  # Load library for cluster plot visualization
  library(cluster)
  
  # Visualize the predicted classes using a cluster plot
  clusplot(wine.df.test[, -12], wine.nb.pred, color = TRUE, shade = TRUE, labels = 4, lines = 0, 
           main = "Cluster Plot of Naive Bayes Predictions")

  
  
###---------------------------------------------------------------------###
##-----------------------------Random Forest--------------------------##

#---Ensure the target variable is a factor
  wine.df.train$quality.label <-as.factor(wine.df.train$quality.label)
  wine.df.test$quality.label <- as.factor(wine.df.test$quality.label)
  
#10.Building a Random Forest Model: 
  library(randomForest)
  set.seed(123)
  wine.rf <- randomForest(quality.label ~.-quality, #all features except quality   
                          data = wine.df.train, ntree = 3000)
  wine.rf

#11.Making Predictions for the test data: 
  wine.rf.predict <- predict(wine.rf,wine.df.test)
  wine.rf.predict

#12.Visualizing Predictions: 
  library(cluster)
  clusplot(wine.df.test[-13], wine.rf.predict, color = TRUE, shade = TRUE,
           labels = 2, lines = 0, 
           main ="Cluster Plot of Random Forest Predictions") 

#13.Handing Imbalanced Classes
  
#---Building a Random Forest Model: with Balanced Sampling 
  #the model is now forced to focus equally on both classes. 
  
  library(randomForest)
  set.seed(123)
  wine.rf <- randomForest(quality.label ~.-quality, #all features except quality   
                          data = wine.df.train, ntree = 3000, 
                          sampsize = c(163,163)) #Match the smaller class size
  wine.rf
#---Making Predictions for the test data: 
  wine.rf.predict <- predict(wine.rf,wine.df.test)
  wine.rf.predict
  
#---Visualizing Predictions: 
  library(cluster)
  clusplot(wine.df.test[-13], wine.rf.predict, color = TRUE, shade = TRUE,
           labels = 2, lines = 0, 
           main ="Cluster Plot of Random Forest Predictions
                  with balance classes") 
  

#14.Evaluating the model:How well did we predict the test data?

#---Accuracy Method: which is the proportion pf correct predictions. 
  mean(wine.df.test$quality.label == wine.rf.predict)

#---Adjust Rand Index (ARI) 
  library(mclust)
  adjustedRandIndex(wine.df.test$quality.label, wine.rf.predict)

#---Confusion Matrix 
  table(wine.rf.predict, wine.df.test$quality.label)

#---Summary Functions: 
  wine.summ <- function(data, groups) 
  {
    aggregate(data, list(groups), function(x) mean(as.numeric(x)))
  }
  wine.summ(wine.df.test, wine.rf.predict) #proposed quality
  wine.summ(wine.df.test, wine.df.test$quality.label) #actual quality

#15.Variable importance: Random Forest can tell us which variable (features) are 
  #the most important for making predictions

wine.rf <- randomForest(quality.label ~.-quality, #all features except quality   
                                   data = wine.df.train, ntree = 3000,
                        importance = TRUE)
wine.rf
importance(wine.rf)

#---Variable importance plot
varImpPlot(wine.rf, main = "Feature Importance for Wine Quality Prediction")

#---Heatmap for variable important 
library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(wine.rf)[,1:2]), key = FALSE, col = brewer.pal(9, "Blues"),
          dend = "none", trace = "none", margins = c(10,10),
          main = "\n\n\nHeatmap of Feature Importance \n for Wine Quality Prediction",
          cexRow = 1.5)
  
