library("splitstackshape")
library("Amelia")
library("caret")
library("mice")
library("randomForest")
library("kernlab")
library("rpart")

#Reading the Data into R

data_1<-read.csv("C:/Users/Sushmita/Desktop/Semester 2/Interview Data Sets/Enova/training_data.csv", header = TRUE)
test_data_1 <- read.csv ("C:/Users/Sushmita/Desktop/Semester 2/Interview Data Sets/Enova/sushmita_score.csv",header=TRUE )

#Exploratory Data Analysis
#Dimension of the Dataset
dim(data_1)
#Summary of the Dataset
summary(data_1)
#Missing values vizualisation
missmap(data_1, main= "Missing vs Observed Values")
#box plot
boxplot(data_1$age)
boxplot(data_1$weight)
boxplot(data_1$height)

#Removing serial number and diagnosis_date 
data_1 <- data_1[,-c(1,2)]
test_data_1 <-test_data_1[,-c(1,2)]

#split<-as.array(strsplit(as.character(data_1$symptoms), ","))
#Converting Data Types
typeof(data_1$symptoms)

data_1$symptoms <- as.character(data_1$symptoms)
data_1$t_score <- as.character(data_1$t_score)
data_1$n_score <- as.character(data_1$n_score)
data_1$m_score  <- as.character(data_1$m_score)
data_1$stage <- as.character(data_1$stage)
data_1$side <- as.character(data_1$side)
symp <- as.data.frame(data_1$symptoms)
names(symp) <- "symptoms"

test_data_1$symptoms <- as.character(test_data_1$symptoms)
test_data_1$t_score <- as.character(test_data_1$t_score)
test_data_1$n_score <- as.character(test_data_1$n_score)
test_data_1$m_score  <- as.character(test_data_1$m_score)
test_data_1$stage <- as.character(test_data_1$stage)
test_data_1$side <- as.character(test_data_1$side)
symp <- as.data.frame(test_data_1$symptoms)
names(symp) <- "symptoms"
#Spliting the symptoms in the comma separated row 
symp_split <- cSplit(symp, splitCols = 'symptoms', sep=",", direction = "long", stripWhite = TRUE, makeEqual = NULL, type.convert=FALSE)

#identifying unique symptoms and converting them into columns 

symptom_list <- as.vector(t(as.matrix(unique(symp_split))))
#
t_score_list <- as.vector(t(as.matrix(unique(data_1$t_score))))
n_score_list <- as.vector(t(as.matrix(unique(data_1$n_score))))
m_score_list <- as.vector(t(as.matrix(unique(data_1$m_score))))
stage_list <- as.vector(t(as.matrix(unique(data_1$stage))))
side_list <- as.vector(t(as.matrix(unique(data_1$side))))

t_score_list <- as.vector(t(as.matrix(unique(test_data_1$t_score))))
n_score_list <- as.vector(t(as.matrix(unique(test_data_1$n_score))))
m_score_list <- as.vector(t(as.matrix(unique(test_data_1$m_score))))
stage_list <- as.vector(t(as.matrix(unique(test_data_1$stage))))
side_list <- as.vector(t(as.matrix(unique(test_data_1$side))))



data_2 <- as.data.frame(matrix(data = 0, nrow = nrow(data_1), ncol = length(symptom_list)))
data_5 <- as.data.frame(matrix(data = 0, nrow = nrow(data_1), ncol = length(t_score_list)))
data_6 <- as.data.frame(matrix(data = 0, nrow = nrow(data_1), ncol = length(n_score_list)))
data_7 <- as.data.frame(matrix(data = 0, nrow = nrow(data_1), ncol = length(m_score_list)))
data_8 <- as.data.frame(matrix(data = 0, nrow = nrow(data_1), ncol = length(stage_list)))
data_9 <- as.data.frame(matrix(data = 0, nrow = nrow(data_1), ncol = length(side_list)))


test_data_2 <- as.data.frame(matrix(data = 0, nrow = nrow(test_data_1), ncol = length(symptom_list)))
test_data_5 <- as.data.frame(matrix(data = 0, nrow = nrow(test_data_1), ncol = length(t_score_list)))
test_data_6 <- as.data.frame(matrix(data = 0, nrow = nrow(test_data_1), ncol = length(n_score_list)))
test_data_7 <- as.data.frame(matrix(data = 0, nrow = nrow(test_data_1), ncol = length(m_score_list)))
test_data_8 <- as.data.frame(matrix(data = 0, nrow = nrow(test_data_1), ncol = length(stage_list)))
test_data_9 <- as.data.frame(matrix(data = 0, nrow = nrow(test_data_1), ncol = length(side_list)))

colnames(data_2) <- symptom_list
colnames(data_5) <- t_score_list
colnames(data_6) <- n_score_list
colnames(data_7) <- m_score_list
colnames(data_8) <- stage_list
colnames(data_9) <- side_list

colnames(test_data_2) <- symptom_list
colnames(test_data_5) <- t_score_list
colnames(test_data_6) <- n_score_list
colnames(test_data_7) <- m_score_list
colnames(test_data_8) <- stage_list
colnames(test_data_9) <- side_list


write.csv(data_5,"data_5.csv")
#columns<-colnames(data_5)


for(i in 1:nrow(data_8))
{
  data_8[i,data_1[i,"stage"]]=1
}


for(i in 1:nrow(data_9))
{
  data_9[i,data_1[i,"side"]]=1
}


for(i in 1:nrow(data_7))
{
  data_7[i,data_1[i,"m_score"]]=1
}

for(i in 1:nrow(data_6))
{
  data_6[i,data_1[i,"n_score"]]=1
}

for(i in 1:nrow(data_5))
{
  #data_5[1,columns[1]]=1
  data_5[i,data_1[i,"t_score"]]=1
  #t_score_list[[i]]<-lapply(data_1$t_score,function(t) ifelse(t==i,1,0))
  
}

#iteration for test data


for(i in 1:nrow(test_data_8))
{
  test_data_8[i,test_data_1[i,"stage"]]=1
}


for(i in 1:nrow(test_data_9))
{
  test_data_9[i,test_data_1[i,"side"]]=1
}


for(i in 1:nrow(test_data_7))
{
  test_data_7[i,test_data_1[i,"m_score"]]=1
}

for(i in 1:nrow(test_data_6))
{
  test_data_6[i,test_data_1[i,"n_score"]]=1
}

for(i in 1:nrow(test_data_5))
{
  test_data_5[i,test_data_1[i,"t_score"]]=1
  
}



write.csv(data_6,"data_6.csv")
write.csv(data_1,"data_1.csv")

#Binding all the datasets created for the training and test dataset

data_3 <- cbind(data_1,data_2,data_5,data_6,data_7,data_8,data_9)
test_data_3 <- cbind(test_data_1,test_data_2,test_data_5,test_data_6,test_data_7,test_data_8,test_data_9)

write.csv(data_3,"data_3.csv")
write.csv(test_data_3,"test_data_3.csv")

#Converting variable symptoms from categorical to numerical for training and test data

for(i in 1:nrow(data_3))
{
  splits<-unlist(strsplit(data_3[i,"symptoms"],","))
  data_3[i,splits]<-1
}

data_4 <- data_3[,which(colnames(data_3) %in% ("symptoms") == FALSE)]

for(i in 1:nrow(test_data_3))
{
  splits<-unlist(strsplit(test_data_3[i,"symptoms"],","))
  test_data_3[i,splits]<-1
}

test_data_4 <- test_data_3[,which(colnames(test_data_3) %in% ("symptoms") == FALSE)]

#Imputing the missing values in the training dataset using PMM
missing_data <- mice(data_4,m = 2,maxit = 2,method = 'pmm',seed = 500)
imputed_data <- complete(missing_data, 1)
write.csv(imputed_data,"imputed_data.csv")

#Removing the redudant columns in the training dataset

imputed_data_final <- imputed_data[c(-2,-3,-4,-5,-14)]

#imputing for missing data in the test data
test_missing_data <- mice(test_data_4,m = 2,maxit = 2,method = 'pmm',seed = 500)
test_imputed_data <- complete(test_missing_data, 1)
write.csv(test_imputed_data,"test_imputed_data.csv")
write.csv(imputed_data_final,"imputed_data_final.csv")

#Removing the redundant columns in the test dataset
test_imputed_data_final <- test_imputed_data[c(-2,-3,-4,-5,-14)]

#Feature Selection
#Performing feature selection using logistic regression
#Data Validation
correlationMatrix <- cor(imputed_data_final[,1:66])
print(correlationMatrix)
#Finding variables that are highly correlated
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.3)
print(highlyCorrelated)
#Removing the highly correlated variables from the training dataset
#Feature Selection
feature_sel <- imputed_data_final[-highlyCorrelated]
write.csv(feature_sel,"feature_sel.csv")

#Building a Model
#Creating a logistic regression model with survival_7_year as the class variable

model <-glm(survival_7_years~., family= binomial(logit), data=feature_sel)
summary(model)
#predicting the values of the class variable in the test dataset
test_final <- test[,(colnames(test) %in% colnames(test_imputed_data_final))]
fitted.results <- predict(model,newdata=test_final,type='response')
#Converting the predicted output into a binary value
test_imputed_data_final$survival_7_years <- ifelse(fitted.results > 0.5,1,0)
dec_test <- read.csv("C:/Users/Sushmita/Desktop/Semester 2/Interview Data Sets/Enova/test_decision_tree.csv", header =TRUE)

#2. Creating a classification model using decision tree
require(tree)
decision_tree <- rpart(survival_7_years ~., data= feature_sel,method="class") 
head(decision_tree)
dec_test$survival_7_years<- predict(decision_tree, newdata=subset(test_imputed_data_final,select=c(-highlyCorrelated)),type='class')
write.csv(dec_test,"test_final_tree.csv")

#Decision Tree Visualization 
library(rpart.plot)
library(rattle)	
prp(decision_tree)
fancyRpartPlot(decision_tree)

#Splitting the training dataset by 75:25 ratio
split_training_data <- floor(0.75 * nrow(imputed_data_final))
set.seed(123)
train_ind <- sample(seq_len(nrow(imputed_data_final)), size = split_training_data)
#Training dataset
train <- imputed_data_final[train_ind, ]
#Test dataset
test <- imputed_data_final[-train_ind, ]

#Using logistic regression model on training dataset

#Data Validation
correlationMatrix_train <- cor(train[,1:66])
print(correlationMatrix_train)
highlyCorrelated_train <- findCorrelation(correlationMatrix_train, cutoff=0.3)
print(highlyCorrelated_train)
#Feature Selection
feature_sel_train <- train[-highlyCorrelated_train]
write.csv(feature_sel_train,"feature_sel_train.csv")
#Building the model
model_test <-glm(survival_7_years~., family= binomial(logit), data=feature_sel_train)
summary(model_test)
test_final_valid <- test[,(colnames(test) %in% colnames(feature_sel_train))]
fitted.results.train <- predict(model_test,newdata=test_final_valid,type='response')
test$survival_7_years_pred <- ifelse(fitted.results.train > 0.5,1,0)
#Evaluating the model
accuracy <- sum(test$survival_7_years == test$survival_7_years_pred)/nrow(test)  
accuracy
write.csv(test,"test_split_logistic.csv")

#Evaluating the model using classfication by decision Trees on training dataset
decision_tree_train <- rpart(survival_7_years ~., data= feature_sel_train,method="class") 
test$survival_7_years_pred_tree<- predict(decision_tree_train,newdata=test,type='class')
accuracy_tree <- sum(test$survival_7_years == test$survival_7_years_pred_tree)/nrow(test)
accuracy_tree
