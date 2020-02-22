####INSTALL PACKAGES####

if(require("pacman")=="FALSE"){
  install.packages('pacman')
  library('pacman')
  pacman::p_load(here, stringr, readxl, plyr, caret, dplyr, doParallel, 
                 lubridate, crayon, corrplot, ggplot2, e1071, reshape2, tidyverse)
} else {
  library('pacman')
  pacman::p_load(here, stringr, readxl, plyr, caret, dplyr, doParallel, 
                 lubridate, crayon, corrplot, ggplot2, e1071, reshape2, tidyverse)
}

#### SPLIT THE DATA ####

Splitting_Data <- function(Data, Dependent, Pourcentage, Seed) {
set.seed(Seed)
inTraining <- createDataPartition(Dependent, p = Pourcentage, list = FALSE)
Training <- Data[inTraining,]
Testing <- Data[-inTraining,]
list(Training = Training, Testing = Testing)
}

list <- Splitting_Data(Data_4, Data_4$Volume, 0.80, 123)

#### CHECK BEST MODELS ####


Models <- c("lm", "rf", "knn")

Features <- c("Volume ~ x4StarReviews",
              "Volume ~ ServiceReviews",
              "Volume ~ x4StarReviews + ServiceReviews",
              "Volume ~ x4StarReviews + ServiceReviews + x2StarReviews")

compare.model <- c()
The_Main_List <- list()

for(i in Models) {
  The_Models_List <- list()
  for (y in Features) { 
    print(paste(i, "", "with", "", y))
    The_Models_List[[y]] <- train(formula(y), data = list$Training, method = i)
    print(The_Models_List[[y]]$results)
    pred <- predict(The_Models_List[[y]], newdata = list$Testing)
    pred.metric <- postResample(list$Testing$Volume, pred)
    compare.model <- cbind(compare.model , pred.metric)
  }
  The_Main_List[[i]] <- The_Models_List
}


?train

# Name the colomnes

v <- c()

for (i in Models) {
  for (y in Features)
    v <- cbind(v, print(paste(i, y)))
}

colnames(compare.model) <- v
Rank_Models <- sort(compare.model[2,], decreasing=TRUE)
Rank_Models

# Plot the results

CM1 <- as.data.frame(t(compare.model))
CM2 <- tibble::rownames_to_column(CM1)
CM3 <- arrange(CM2, Rsquared)
CM4 <- melt(CM3, id = c("rowname"))

ggplot(CM4, aes(rowname, value, order = value)) +
  geom_col() + 
  facet_grid(variable ~ ., scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#### Apply the Model ####

Run_The_Model <- function(Data, Formula, Method) {
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) 
  rfGrid <- expand.grid(degree=2, scale=0.100, C=1)
  Model <- train(Formula, data = Data, method = Method, trControl = fitControl, tuneGrid=rfGrid)
  Model
  }

Model <- Run_The_Model(list$Training, Volume ~ x4StarReviews + ServiceReviews, "svmPoly")

# Check the testing metrics

pred <- as.integer(predict(Model, newdata = list$Testing))
list$Testing$AE <- as.integer(abs(pred - list$Testing$Volume))
list$Testing$RE <- as.integer(abs(pred - list$Testing$Volume) / list$Testing$Volume * 100)


# Plot de testing metrics

ggplot(list$Testing, aes(x4StarReviews + x2StarReviews + ServiceReviews, Volume)) +
  geom_point() +
  geom_smooth(col = "Black") +
  geom_point(aes(x4StarReviews + x2StarReviews + ServiceReviews, pred), col = 'red') +
  geom_smooth(data = list$Testing, aes(x4StarReviews + x2StarReviews + ServiceReviews, pred), col = "red")


#### Save model ####

#save(Model, file="../Models/SVM_Poly.rda")

#### CHECK RESULTS ####

# Subset

Smartphone <- subset(Data_3, ProductType.Smartphone == 1)
Laptop <- subset(Data_3, ProductType.Laptop == 1)
PC <- subset(Data_3, ProductType.PC == 1)
NetBook <- subset(Data_3, ProductType.Netbook == 1)
All <- subset(Data_3, ProductType.Smartphone == 1 | ProductType.Laptop == 1 | ProductType.PC == 1 | ProductType.Netbook == 1)


x <- c()
y <- c()
for (i in (1:78)) {
  y = Data_2$x4StarReviews[i] + Data_2$x2StarReviews[i]
  if (y > 60)
    x = cbind(x, i)
}

# Plot

ggplot(All, aes(x4StarReviews + ServiceReviews + x2StarReviews, Volume)) + 
    geom_point() +
    geom_smooth(data = Smartphone, col = "red") +
    geom_smooth(data = Laptop, col = "yellow") +
    geom_smooth(data = PC, col = "Blue") +
    geom_smooth(data = NetBook, col = "Orange")

