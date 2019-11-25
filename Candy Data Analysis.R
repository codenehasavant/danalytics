


#  Clean environment
rm(list=ls())


# Load Packages
packages = c("dplyr","ggplot2","polycor","reshape2", "lattice", "boot", "MASS",
             "cvTools",  "DAAG", "randomForest", "glmnet", "caret", 
             "rpart", 'FactoMineR', "factoextra", "Boruta", "RRF", "ggpubr", "DALEX", 
             "rpart.plot", "party")

ipak = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)}

ipak(packages)


# Set Working Dir
setwd("F:/My Profile/DE/Zooplus")

# Load Data
candy.data = read.csv('candy-data.csv')

# Remove the first column - competitorname (candy names)
candy = candy.data[,-1]

# Check data, first hand analysis
head(candy)


# Check structure of the data
str(candy)

# convert columns 1 to 9 to factors
for(i in 1:9){
  candy[,i] = as.factor(as.character(candy[,i]))
}

str(candy)

# Round decimals
candy$sugarpercent = round(candy$sugarpercent,2)
candy$pricepercent = round(candy$pricepercent,2)


#*************************************************************
#*********************** I . PLOTS ***************************

# 1.A. Plot Win Percentage


ggplot(data=candy, aes(candy$winpercent)) + 
  geom_histogram (breaks=seq(0, 100, by=5),
                  col="black", 
                  aes(fill=..count..)) +
  scale_fill_gradient("Count", low="light blue", high="dark blue")+ 
  labs(title="Histogram for Winning Percentage", x="Win Percent", y="Frequency")+
  ylim(c(0,15))+
  geom_vline(aes(xintercept=mean(winpercent)),
             color="yellow", linetype="dashed", size=0.8)


# 1.B Plot Sugar Percentage
ggplot(data=candy, aes(candy$sugarpercent)) + 
  geom_histogram (breaks=seq(0, 1, by = 0.095),
                  col="black", 
                  aes(fill=..count..)) +
  scale_fill_gradient("Count", low="light blue", high="dark blue")+ 
  labs(title="Histogram for Sugar Percentage", x="SUgar Percent", y="Frequency")+
  xlim(c(0,1))

# 1.c Plot Price Percentage
ggplot(data=candy, aes(candy$pricepercent)) + 
  geom_histogram (breaks=seq(0, 1, by = 0.095),
                  col="black", 
                  aes(fill=..count..)) +
  scale_fill_gradient("Count", low="light blue", high="dark blue")+ 
  labs(title="Histogram for Price Percentage", x="Price Percent", y="Frequency")+
  xlim(c(0,1))


#1.d Check Density
ggplot(candy) + geom_density(aes(x = winpercent, fill = chocolate), alpha = 0.2)+
  labs(title="Ranking Density distribution of Chocolate", x="Ranking", y="Density")

#-----------------------------------------------------------------------------
# 2. Plot Chocolate wrt Win Percentage

ggplot(candy, aes(x=chocolate, y=winpercent)) +
  geom_boxplot(fill='#2E89BD', color="black")+
  labs(title="Distribution of Win Percent against Presence of Chocolate", 
       x="Presence of Chocolate", y="Win Percent")+
  ylim(c(0,100))


# 3. Plot Fruity wrt Win Percentage
ggplot(candy, aes(x=fruity, y=winpercent)) +
  geom_boxplot(fill='#2E89BD', color="black")+
  labs(title="Distribution of Win Percent against Presence of Fruit", 
       x="Presence of Fruit", y="Win Percent")+
  ylim(c(0,100))

a= ggplot(candy, aes(x=fruity, y=winpercent)) +
  geom_boxplot(fill='#2E89BD', color="black")+
  labs(title="Distribution of Win Percent against Presence of Fruit", 
       x="Presence of Fruit", y="Win Percent")+
  ylim(c(0,100))
b= ggplot(candy, aes(x=caramel, y=winpercent)) +
  geom_boxplot(fill='#2E89BD', color="black")+
  labs(title="Distribution of Win Percent against Presence of Caramel", 
       x="Presence of Caramel", y="Win Percent")+
  ylim(c(0,100))
c= ggplot(candy, aes(x=peanutyalmondy, y=winpercent)) +
  geom_boxplot(fill='#2E89BD', color="black")+
  labs(title="Distribution of Win Percent against Presence of Nuts", 
       x="Presence of Nuts", y="Win Percent")+
  ylim(c(0,100))
d= ggplot(candy, aes(x=nougat, y=winpercent)) +
  geom_boxplot(fill='#2E89BD', color="black")+
  labs(title="Distribution of Win Percent against Presence of Nougat", 
       x="Presence of Nougat", y="Win Percent")+
  ylim(c(0,100))
e= ggplot(candy, aes(x=crispedricewafer, y=winpercent)) +
  geom_boxplot(fill='#2E89BD', color="black")+
  labs(title="Distribution of Win Percent against Presence of Cookie", 
       x="Presence of Cookie", y="Win Percent")+
  ylim(c(0,100))
f= ggplot(candy, aes(x=hard, y=winpercent)) +
  geom_boxplot(fill='#2E89BD', color="black")+
  labs(title="Distribution of Win Percent against Hardness", 
       x="Hardness", y="Win Percent")+
  ylim(c(0,100))
g= ggplot(candy, aes(x=bar, y=winpercent)) +
  geom_boxplot(fill='#2E89BD', color="black")+
  labs(title="Distribution of Win Percent for Bar and Non Bar-Shaped Candies ", 
       x="Bar Shaped Candies\n 0=No, 1=Yes", y="Win Percent")+
  ylim(c(0,100))
h= ggplot(candy, aes(x=pluribus, y=winpercent)) +
  geom_boxplot(fill='#2E89BD', color="black")+
  labs(title="Distribution of Win Percent for Single and Pluribus Candies", 
       x="Pluribus Candies\n 0=No, 1=Yes", y="Win Percent")+
  ylim(c(0,100))

ggarrange(a,b,c,d, ncol = 2, nrow = 2)
ggarrange(e,f,g,h, ncol = 2, nrow = 2)

#4. Plot continuous variable sugar content and price wrt win percentage

ggplot(candy, aes(x=sugarpercent, y=winpercent)) +
  geom_point(colour = "#2E89BD", size= 2.5) +
  geom_smooth(method='auto', se=TRUE, linetype = 'longdash',level = 0.95, colour= "black")+
  labs(title="Spread of Choice as a function of Sugar Content ", 
       x="Sugar Content", y="Win Percent")+
  ylim(c(0,100)) + xlim(c(0,1))



ggplot(candy, aes(x=pricepercent, y=winpercent)) +
  geom_point(colour = "#2E89BD", size= 2.5) +
  geom_smooth(method='auto', se=TRUE, level = 0.95, colour= "black",
              linetype = 'longdash', span= 3)+
  labs(title="Spread of Choice as a function of Price ", 
       x="Price", y="Win Percent")+
  ylim(c(0,100)) + xlim(c(0,1))
# Has a quadratic term?


## 5. Positional Graphs

## a. Position of Candies as per Sugar % and Price %
ggplot( data = candy.data, 
        aes(x = sugarpercent, y = pricepercent,label = competitorname)) +
  geom_point(color ="#2E89BD") + 
  geom_text(check_overlap = T,
            hjust = "right", 
            size = 2.5,
            color ="black") +
  labs(title = "Position of Candies as per Sugar Content and Price",
       y = "Sugar %", 
       x = "Price %") 
## b. 

ggplot( data = candy.data, 
        aes(x = pricepercent, y = winpercent,label = competitorname)) +
  geom_point(color ="#2E89BD") + 
  geom_text(check_overlap = T,
            hjust = "left",
            size = 3.2,
            color ="black") +
  labs(title = "Position of Candies as per Price and Ranking",
       y = "Ranking", 
       x = "Price") +
  geom_hline(yintercept = 50, linetype = "dashed", colour='brown') +
  geom_vline(xintercept = .50, linetype = "dashed", colour='brown')

## c. 

ggplot( data = candy.data, 
        aes(x = sugarpercent, y = winpercent,label = competitorname)) +
  geom_point(color ="#2E89BD") + 
  geom_text(check_overlap = T,
            hjust = "left", 
            size = 3.2,
            color ="black") +
  labs(title = "Position of Candies as per Sugar and Ranking",
       y = "Ranking", 
       x = "Sugar") +
  geom_hline(yintercept = 50, linetype = "dashed", colour='brown') +
  geom_vline(xintercept = .50, linetype = "dashed", colour='brown')


## 6. Ranking Plot
candy.data = candy.data %>% arrange(desc(winpercent))
candy.50 = head(candy.data[,c(1,13)], 50)


ggplot(candy.50, aes(x=reorder(competitorname, winpercent), y=winpercent)) + 
geom_segment(aes(xend=reorder(competitorname, winpercent), yend=0), color ="#2E89BD",
             linejoin = 'bevel', size = 1) + 
geom_point(color ="#2E89BD") +
labs(title = "Top 50 Candies based on Rankings",
     y = "Win Percent", 
     x = "Candies") +
coord_flip() 



#**************************************************************
#**************** II. CORRELATION******* **********************

# Correlation
cor = hetcor(candy)

cormat = round(cor$correlations, 2)
head(cormat)

# Get lower triangle of the correlation matrix
get_lower_tri=function(cormat){
  cormat[upper.tri(cormat)] = NA
  return(cormat)}

# Get upper triangle of the correlation matrix
get_upper_tri = function(cormat){
  cormat[lower.tri(cormat)]= NA
  return(cormat)}

upper_tri = get_upper_tri(cormat)

melted_cormat = melt(cormat)
melted_cormat = melt(upper_tri, na.rm = TRUE)

# Plot Heat Map of Correlation
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "dark blue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(label = value), size=3, color = "black") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  labs(title = 'Heat Map of Correlation Matrix', x="", y="")+
  coord_fixed()



#***************************************************************
#**************** III . REGRESSION MODELS **********************


# Model 1: Multivariate Regression Model 
lm.model = lm(winpercent~. , data = candy)
summary(lm.model)

# Test 1. Histogram of residuals should be normal
hist(lm.model$residuals, 
     xlim = c(-30, 30),
     ylim = c(0,  20),
     xlab = 'Residuals',
     ylab= 'Frequency',
     main = 'Histogram of Residuals from Model 1 \n(Normally Distributed)', 
     col = '#2E89BD')
# The histogram of residuals = (approximately) normally distributed

# Test 2. Sum of Residuals must equal zero 
sum(lm.model$residuals) # = -2.153833e-14 ~ 0
#  sum of residuals = 0

#***************************************************************
# Model 2 : Stepwise Regressed Version of Model 1, 
#           penalises insignificant attributes to quote Adusted R squared

lm.model.2 = stats::step(lm.model)
summary(lm.model.2)

summary(lm.model.2)


#***************************************************************
# Model 3, 4 :  Feature Engineering

candy$sugarbyprice = candy$sugarpercent/candy$pricepercent
candy$winbyprice = candy$winpercent/candy$pricepercent

lm.model.3 = lm(winpercent~. , data = candy)
summary(lm.model.3) 

lm.model.4 = stats::step(lm.model.3)
summary(lm.model.4)

# Check validity of LR model
# Test 1. Histogram of residuals should be normal
hist(lm.model.4$residuals, 
     xlim = c(-30, 30),
     ylim = c(0,  20),
     xlab = 'Residuals',
     ylab= 'Frequency',
     main = 'Histogram of Residuals from Model 4 \n(Normally Distributed)', 
     col = '#2E89BD')
# The histogram of residuals = (approximately) normally distributed

# Test 2. Sum of Residuals must equal zero 
sum(lm.model.4$residuals) # = -6.49e-15 ~ 0
#  sum of residuals = 0


#***************************************************************
# Model 5, 6: Add a quadratic pricepercent term

candy$pricepercent.sq = (candy$pricepercent)^2

lm.model.5 = lm(winpercent~., data = candy)
summary(lm.model.5)

lm.model.6 = stats::step(lm.model.5)
summary(lm.model.6)

# Check validity of LR model

# Test 1. Histogram of residuals should be normal
hist(lm.model.6$residuals, 
     xlim = c(-30, 30),
     ylim = c(0,  20),
     xlab = 'Residuals',
     ylab= 'Frequency',
     main = 'Histogram of Residuals from Linear Regression Model \n(Normally Distributed)', 
     col = '#2E89BD')
# The histogram of residuals = (approximately) normally distributed

# Test 2. Sum of Residuals must equal zero 
sum(lm.model.6$residuals) # = 5.01e-15 ~ 0
#  sum of residuals = 0

#***************************************************************
# Model  7: Random Forest

set.seed(3)

rf.model.7=randomForest(winpercent ~ ., data = candy, importance=TRUE)
rf.model.7
#% Var explained: 54.9

mean(rf.model.7$mse) # MSE = 99.6
mean(rf.model.7$rsq) # R square = 53.5%

importance(rf.model.7, type=1)
importance(rf.model.7, type=2)

#Dotchart of variable importance as measured by a Random Forest
varImpPlot(rf.model.7, sort = TRUE, 
           main = 'Variable Importance from Random Forest Regression Model',
           color = "#2E89BD", lcolor = "#2E89BD",pch = 16,cex = 0.9)


rfImp = varImp(rf.model.7, scale=F)
rfImp 


#***************************************************************
# Model  8, 9: Random Forest with selected variables

# Considering a combination of top 4 (optimum) features for the next tree as per Inc MSE
set.seed(3)
rf.model.8=randomForest(winpercent ~  winbyprice + chocolate + pricepercent + pricepercent.sq
                        , data = candy, importance=TRUE)
rf.model.8
#% Var explained: 63.43% 


# Considering top 6 (optimum) features for the next tree as per Inc Node Impurity
set.seed(3)
rf.model.9=randomForest(winpercent ~ 
                          winbyprice + chocolate + pricepercent + pricepercent.sq + sugarbyprice
                        +peanutyalmondy 
                        , data = candy, importance=TRUE)
rf.model.9
#% Var explained: 60.56% 



#***************************************************************
# Model  10:  Penalized regression methods - LASSO

# Predictor variables
x = model.matrix(winpercent~., candy)[,-1]
# Outcome variable
y = candy$winpercent


# Choosing the best lambda using cross-validation
set.seed(3) 
cv = cv.glmnet(x, y, alpha = 1)

# Display the best lambda value
cv$lambda.min
#0.394

# Fit the lasso model on data
lasso.model.10 = glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
lasso.model.10

# % Deviance Explained = 53.87%

# Display regression coefficients
coef(lasso.model.10)

#***************************************************************
# Model  11:  Penalized regression methods - RIDGE

# Find the best lambda using cross-validation

set.seed(3) 
cv = cv.glmnet(x, y, alpha = 0)

# Display the best lambda value
cv$lambda.min 
# 2.59

# Fit the final model on the data
rigde.model.11 = glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
rigde.model.11
# % Deviance Explained = 53%

# Display regression coefficients
coef(rigde.model.11)

#***************************************************************
# Model  12:  Penalized regression methods - ELASTIC NET

# Build the model 
set.seed(3)
elasticnet.model.12 = train(
  winpercent ~., data = candy, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Best tuning parameter
elasticnet.model.12$bestTune

#     alpha lambda
# 68   0.7   1.51
# The best alpha and lambda values are those values 
# that minimize the cross-validation error 

coef(elasticnet.model.12$finalModel, elasticnet.model.12$bestTune$lambda)

#***************************************************************
# Model  13:  PRINCIPAL cOMPONENT ANALYSIS

# Since 9 vars are categorical, we have to convert them into numeric using one hot encoding
candy.ohe = candy[1:9]
str(candy.ohe)

for(i in 1 : 9){
  candy.ohe[, i]= as.numeric(as.character(candy.ohe[,i]))
}
candy.ohe = cbind(candy.ohe, candy[10:15])

res.pca = PCA(candy.ohe[, -12], graph = FALSE)

#The proportion of variation retained by the principal components 
#(PCs) can be extracted as follow :

eigenvalues = res.pca$eig

# The amount of variation retained by each PC is called eigenvalues. 
# The first PC corresponds to the direction with the 
# maximum amount of variation in the data set.
# The importance of PCs can be visualized using a scree plot :


fviz_screeplot(res.pca, choice = "eigenvalue",
               xlab = "Principal Component",
               ylab = "Eigen Values",
               main = 'Scree Plot for PCA')

fviz_screeplot(res.pca, choice = "variance",
               xlab = "Principal Component",
               ylab = "Cumulative Proportion of Variance Explained",
               main = 'Cumulative Scree Plot for PCA')

#The correlation between a variable and a PC is called loading. 
#The variables can be plotted as points in the component space 
#using their loadings as coordinates.

(res.pca$var$coord)

fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="cos2", title = "Squared Loadings for Variables") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal()


#***************************************************************
# Model 14: BORUTA

# Perform Boruta search
set.seed(3)
boruta_model.14 = Boruta(winpercent ~ ., data=candy, doTrace=0)  
names(boruta_model.14)

boruta_model.14

# Get significant variables including tentatives
boruta_signif = getSelectedAttributes(boruta_model.14, withTentative = TRUE)
print(boruta_signif)

roughFixMod = TentativeRoughFix(boruta_model.14)
#There are no Tentative attributes! Returning original object.

boruta_signif = getSelectedAttributes(roughFixMod)
print(boruta_signif)

#Boruta has no 'Tentative' variables. 
#Let's find out the importance scores of these variables.

# Variable Importance Scores
imps = attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
imps2[order(-imps2$meanImp), ]  # descending sort

# Plot variable importance
plot(boruta_model.14, cex.axis=.7, las=2, xlab="", main="Variable Importance from Boruta")  

# The columns in green are 'confirmed' and the ones in red are not. 
# There are couple of blue bars representing ShadowMax and ShadowMin. 
# They are not actual features, 
# but are used by the boruta algorithm to decide if a variable is important or not.

#***************************************************************
# Model 15: Feature selection using rpart model

set.seed(3)
rpart.model.15 = train(winpercent ~ ., data=candy, method="rpart")
rpartImp = varImp(rpart.model.15)
rpartImp

tree = rpart(winpercent~., data=candy, cp=.002)
rpart.plot(tree)
#***************************************************************
# Model 16: Recursive Feature Elimination (RFE)

set.seed(3)
subsets = c(1:6)

ctrl = rfeControl(functions = rfFuncs,
                  method = "repeatedcv",
                  repeats = 5,
                  verbose = FALSE)

lmProfile.16 = rfe(x=candy[, -12], y=candy$winpercent,
                   sizes = subsets,
                   rfeControl = ctrl)

lmProfile.16

lmProfile.16$optVariables

#***************************************************************
# Model 17: Genetic Algorithm

# Define control function
ga_ctrl = gafsControl(functions = rfGA,  # another option is `caretGA`.
                      method = "cv",
                      repeats = 2)

# Genetic Algorithm feature selection
set.seed(3)
ga_obj.17 = gafs(x=candy[, -12], 
                 y=candy[, 12], 
                 iters = 2,   # normally much higher (100+)
                 gafsControl = ga_ctrl)

ga_obj.17

# Optimal variables
ga_obj.17$optVariables

#***************************************************************
# Model 18: Simulated Annealing

# Define control function
sa_ctrl = safsControl(functions = rfSA,
                      method = "repeatedcv",
                      repeats = 3,
                      improve = 5) # n iterations without improvement before a reset

# Genetic Algorithm feature selection
set.seed(3)
sa_obj.18 = safs(x=candy[, -12], 
                 y=candy[, 12], 
                 safsControl = sa_ctrl)

sa_obj.18

# Optimal variables
sa_obj.18$optVariables



#***************************************************************
# Model 18: DALEX Package


# Train random forest model
set.seed(3)
rf_model.19 = randomForest(candy$winpercent ~ ., data=candy[,-12], ntree=100)
rf_model.19
#   % Var explained: 554.07%


# Variable importance with DALEX
explained_rf = explain(rf_model.19, data=candy[,-12], y=candy$winpercent)
explained_rf

# Get the variable importances
varimps = variable_dropout(explained_rf, type='raw')

v = varimps[,1:2]
v= v[-c(1,16), ]

class(varimps)
plot(v$variable, v$dropout_loss)

#how important a variable is based on a dropout loss, that is how much loss is incurred by removing a variable from the model.


ggplot(v, aes(x=reorder(v$variable, v$dropout_loss), y=v$dropout_loss)) + 
  geom_segment(aes(xend= reorder(variable, dropout_loss), yend=0), color ="#2E89BD",
               size = 1) + 
  geom_point(color ="#2E89BD") +
  labs(title = "Variable importance with DALEX",
       y = "Dropout Loss: The loss incurred by removing the corresponding characteristic", 
       x = "Characteristics") +
  coord_flip() 


#****************************************************************************************************************************