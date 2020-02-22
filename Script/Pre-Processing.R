#### INSTALL PACKAGES ####

if(require("pacman")=="FALSE"){
  install.packages('pacman')
  library('pacman')
  pacman::p_load(here, stringr, readxl, plyr, caret, dplyr, doParallel, 
                 lubridate, crayon, corrplot, ggplot2, rstudioapi,
                 partykit, ggpubr)
} else {
  library('pacman')
  pacman::p_load(here, stringr, readxl, plyr, caret, dplyr, doParallel, 
                 lubridate, crayon, corrplot, ggplot2, rstudioapi,
                 partykit, ggpubr)
}

#### IMPORT DATA ####
 
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
# setwd("..")
# rm(current_path)
# #list.files("Datasets/")
Data <- read.csv("../data/existingproductattributes2017.csv")

Dummyfy <- dummyVars(" ~ .", data = Data)
dataset_1 <- data.frame(predict(Dummyfy, newdata = Data))

dataset_1$ProductType.Laptop <- as.factor(dataset_1$ProductType.Laptop)
dataset_1$Volume

t.test(Volume ~ ProductType.Laptop, data = dataset_1)

ggplot(dataset_1, aes(ProductType.Laptop,Volume )) +
  geom_boxplot()

dataset_1 %>% group_by(ProductType.Laptop) %>% mean()



####DUMMIFYING####

Dummyfied_function <- function(dataset) {
    Dummyfy <- dummyVars(" ~ .", data = dataset)
    dataset_1 <- data.frame(predict(Dummyfy, newdata = dataset))
    write.csv(dataset_1, file="../data/Dummyfied.csv")
    Data_D <- read.csv(file="../data/Dummyfied.csv")
    Data_D
    }

Data_1 <- Dummyfied_function(Data)

####PREPROCESSING####

# REMOVE NA FEATURES

Data$BestSellersRank <- NULL

# REMOVE OUTLIERS FROM DEPENDENT VARIABLE

RemoveOutlier <- function(DataSet, DataFeature) {
  Outliers <- boxplot(DataFeature)$out 
  Outliers_Numbers <- which(DataFeature %in% Outliers)
  DataSet <- DataSet[-c(Outliers_Numbers),]
}

Data_2 <- RemoveOutlier(Data, Data$Volume)


# CREATE A NEW FEATURE

Data_2 <- Data

Data_2$ServiceReviews <- Data_2$PositiveServiceReview + Data_2$NegativeServiceReview

#CHECK FEATURE IMPORTANCE WITH CORRELATION MATRIX

C_Matrix <- function(Dataset) {
  correlation_matrix <- cor(Dataset)
  corrplot(correlation_matrix)
  correlation_matrix
  }

C_Matrix(Data_2[,-1])


cor(Data_2)
# Check ANOVA

anova_1 <- aov(formula = Data$Volume ~ Data$ProductType)
plot(TukeyHSD(anova_1))

# Remove colinearity feature

Linear_Correlation <- c("x5StarReviews", "x3StarReviews", 
                        "x1StarReviews", "PositiveServiceReview", 
                        "NegativeServiceReview")

Data_3 <- Data_2[,!(colnames(Data_2) %in% Linear_Correlation)]

#CHECK FEATURE IMPORTANCES WITH varImp

set.seed(123)
rf <- train(Volume ~ ., data = Data_3, method = "knn", importance = TRUE)
varImp(rf) 

#CHECK FEATURE IMPORTANCES WITH 

Tree <- ctree(Volume ~., data = Data_3)
plot(Tree)

# REMOVE IRRELEVENT FEATURES

Rm_Feat <- c("ShippingWeight", "ProductHeight", "Price", 
             "Recommendproduct", "ProductNum", "ProfitMargin", 
             "ProductDepth", "ProductWidth")

Data_4 <- Data_3[,!(colnames(Data_3) %in% Rm_Feat)]



ggplot(Data_2, aes(Volume, x4StarReviews)) +
  geom_point() +
  geom_hline(yintercept= mean(Data_2$x4StarReviews), color = "red") +
  geom_smooth(method='lm', formula= y~x, se=F, color = "blue") +
  geom_vline(xintercept = mean(Data_2$Volume), color = "green") +
  geom_smooth(color = "black", se = F) +
  theme_classic()

ggplot(Data_2, aes(Volume)) +
  geom_density()  
  geom_vline(xintercept = mean(Data_2$Volume), color = "blue") +
  geom_vline(xintercept = median(Data_2$Volume), color = "red") +

  
  
ggplot(Data, aes(Volume, Price)) +
    geom_point()
    
  
plot(Data$Volume, Data$Price)  


?cor
str(Data)
  ?geom_abline
var(Data_2$Volume)       
?var()
sd(Data$Volume)
median(Data$Volume)
boxplot(Data_2$Volume)
