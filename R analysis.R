
#-------------- Study:  Insurance Persistency Analysis
#------------- Author:  Neha Savant
#------ Last Modified:  2020-02-09

#--------- Objective :  To identify which attributes after payment persistency


#  Clean environment
rm(list=ls())

# Load Packages
library(dplyr)
library(RRF)

# Set Working Directory, Read CSV
setwd("F:/My Profile/IN")
insurance = read.csv('insurance.data.csv')

# Check data
head(insurance)


# Check structure of the data
str(insurance)
names(insurance)

# Eliminate columns
insurance = insurance[, -c(1,2,16)]
#Policy_No, Owner_Client_Id, RM_V1

insurance$Policy_Issue_Date = NULL
insurance$Policy_Inforce_Date = NULL
insurance$Agent_Ingenium_Code = NULL
insurance$Ageny_Manager_ID  = NULL

# Convert to Factor
for(i in 3:ncol(insurance)){
  insurance[,i] = as.factor(as.character(insurance[,i]))
}

# Check structure of the data
str(insurance)
head(insurance)
nrow(insurance)

insurance$Payment.Status = ifelse(insurance$Payment.Status == 'Lapsed', 0, 1)
insurance$Payment.Status = as.factor(insurance$Payment.Status)


set.seed(3)
rows = sample(1: nrow(insurance), 0.7*nrow(insurance))
train = insurance[rows, ]
test = insurance[-rows, ]

summary(train$Payment.Status)
summary(test$Payment.Status)
prop.table(table(train$Payment.Status))
prop.table(table(test$Payment.Status))

# As Random Forest cannot take too many levels
train$Branch_Code = NULL
test$Branch_Code = NULL
str(train)
nrow(train)

# Fit the RF Model
fit <- randomForest(Payment.Status ~ .,   data=train,  importance=TRUE)

# view results
print(fit) 

# Check important Variables
fit$importance

# Prediction on Test Data
test$pred <- predict(fit, newdata = test)
confusionMatrix(test$Payment.Status, test$pred)

#Plot

ggplot(insurance, aes(x=Payment.Status, y=Orginal_AP)) +
  geom_boxplot(fill='#2E89BD', color="black")+
  labs(title="Although the separation shows over-lapping, 
       ,a premium above 18K has 60% more chances of being paid.", 
       x="Payment Status", y="Annual Premium Range")+
  ylim(c(-10000, 90000))

