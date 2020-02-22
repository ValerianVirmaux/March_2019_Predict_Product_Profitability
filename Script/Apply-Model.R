#### LOAD PACKAGES ####

if(require("pacman")=="FALSE"){
  install.packages('pacman')
  library('pacman')
  pacman::p_load(here, stringr, readxl, plyr, caret, dplyr, doParallel, 
                 lubridate, crayon, corrplot, ggplot2, e1071,rstudioapi)
} else {
  library('pacman')
  pacman::p_load(here, stringr, readxl, plyr, caret, dplyr, doParallel, 
                 lubridate, crayon, corrplot, ggplot2, e1071, rstudioapi)
}

#### IMPORT DATA ####

current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
list.files("Datasets/")
NewData <- read.csv("../Datasets/newproductattributes2017.csv")

#### RUN THE MODEL ####

Apply <- function(Model, Data) {
  NewDataD <- Dummyfied_function(Data)
  NewDataD$ServiceReviews <- NewDataD$PositiveServiceReview + NewDataD$NegativeServiceReview
  Result <- predict(Model, NewDataD)
  FinalData <- cbind(NewDataD, Result)
  FinalData$Volume <- as.integer(FinalData$Result)
  FinalData$Result <- NULL
  FinalData
}


FinalData <- Apply(Model, Smartphone)


Smartphone <- subset(FinalData, ProductType.Smartphone == 1)
Laptop <- subset(FinalData, ProductType.Laptop == 1)
PC <- subset(FinalData, ProductType.PC == 1)
NetBook <- subset(FinalData, ProductType.Netbook == 1)

ggplot(Smartphone, aes(ProductNum, Volume)) + 
  geom_point() +
  geom_smooth(data = Smartphone, aes(ProductNum, ServiceReviews), col = "red") +
  geom_smooth(data = Smartphone, aes(ProductNum, x4StarReviews), col = "blue")


# Plot results

ggplot(FinalData, aes(ServiceReviews + x4StarReviews + x2StarReviews, Result)) +
  geom_point() + 
  geom_smooth()
  labs(x = "Service Reviews", y = "Volume", 
     title = "4 Star Reviews and Volume correlation in original data set",
     subtitle = "...",
     caption = "Source : Valerian V.")