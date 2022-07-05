# ST3189 Coursework  
# Author: ST3189_190399375

##Question 1------------------------------------------------

#Set working directory and read csv
setwd("C:/Users/Jonathan Koh/Desktop/ST3189 ML Coursework")
ewcs <- read.csv("EWCS_2016.csv")

#Checks dimension and summary of file
dim(ewcs)
summary(ewcs)
#We can see that there for all variables there is a min of -999 which shows that the data set contains invalid rows for analysis

#Set libraries
library(Hmisc)
library(factoextra)
library(dendextend)

#Check unique values for each columns
describe(ewcs)
#We can see that the only invalid number in the data set is only -999 for all variables

#Set invalid rows to NA and remove NA rows
is.na(ewcs) <- ewcs == "-999"
ewcs_cleaned <- na.omit(ewcs)
summary(ewcs_cleaned)

#Check if there is any multicollinearity 
library("corrplot")
ewcs_cor <- cor(ewcs_cleaned)
corrplot(ewcs_cor, method = 'number')
#We can see that there is a no correlation with gender and their survey responses
#no correlation with age and the responses
#Responses on their overall well being are positively correlated

#Check mean and variance of data set after cleaning
apply(ewcs_cleaned, 2, mean)
apply(ewcs_cleaned, 2, var)
#variance for Q2b is rather high compared to the other variables which indicates the need for scaling

#Scaling and PCA ---------------------------------------------------------------
ewcs_pca <- prcomp(ewcs_cleaned, scale.=TRUE)
summary(ewcs_pca)
#First 2 principal components captures only 52%
#First 4 principal components captures 70% variance, first 5 captures 77%.

#plot of PC 1 and 2
plot(ewcs_pca$x[,1], ewcs_pca$x[,2])

#Check weightage of PCA components
ewcs_pca$rotation
biplot(ewcs_pca, scale=0)
#We can see that PC1 explains Q87a to Q87e well and PC2 explains Q90a to Q90f well

#Diagnostic to determine how many principal components to use
ewcs_pca_var=ewcs_pca$sdev^2
ewcs_pca_var
ewcs_pca_var_exp =ewcs_pca_var/sum(ewcs_pca_var)
ewcs_pca_var_exp
par(mfrow=c(1,2))
plot(ewcs_pca_var_exp, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(ewcs_pca_var_exp), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
par(mfrow=c(1,1))

#Standardize data
ewcs_scaled <- scale(ewcs_cleaned)

#Use elbow method to determine optimal number of clusters
ecws_elbow <- fviz_nbclust(ewcs_scaled, kmeans, method = "wss") + labs(subtitle = "Elbow method")

# Silhouette method
ecws_sil <- fviz_nbclust(ewcs_scaled, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")
#We see that the silhouette score is maximized at k = 2. 
library(gridExtra)
grid.arrange(ecws_elbow, ecws_sil, nrow = 2)

library(cluster)
#K-Means Clustering ------------------------------------------------------------
#K = 2
set.seed(2019)
ewcs_k2 <- kmeans(ewcs_scaled, centers = 2, nstart =25)
ewcs_k2
summary(ewcs_k2)
plot_k2 <- fviz_cluster(ewcs_k2, geom = "point", data = ewcs_scaled) + ggtitle("k = 2")
print(aggregate(ewcs_cleaned, by=list(ewcs_k2$cluster), FUN=mean))

#K = 3
set.seed(2019)
ewcs_k3 <- kmeans(ewcs_scaled, centers = 3, nstart =25)
ewcs_k3
summary(ewcs_k3)
plot_k3 <- fviz_cluster(ewcs_k3, geom = "point", data = ewcs_scaled) + ggtitle("k = 3")
print(aggregate(ewcs_cleaned, by=list(ewcs_k3$cluster), FUN=mean))
##From the 4 clusters, there is no evident pattern from group 2 and 4.
##In Group 1, the mean age is the highest and it has generally higher scores in each scale compared to the rest of the groups. 
##In group 3, the mean age is the lowest and it has generally lower scores in each scale compared to the rest.

grid.arrange(plot_k2, plot_k3, nrow = 2)

# Hierarchical Clustering ----------------------------------------------------
distance_ewcs <- dist(ewcs_scaled, method = 'euclidean')
set.seed(2019)
#K = 2
#Average Method
hc_average = hclust(distance_ewcs, method ="average")
plot(hc_average , main ="Average Linkage for K=2", xlab="", sub ="", cex =.9)
cut_avg <- (cutree(hc_average, k=2))
table(cut_avg)
plot(hc_average)
rect.hclust(hc_average , k = 2)
plot_hc_k2_avg <- fviz_cluster(geom = "point", list(data = ewcs_scaled, cluster = cut_avg)) + ggtitle("Average Linkage for K=2")
print(aggregate(ewcs_cleaned, by=list(cut_avg), FUN=mean))

#Complete Method
hc_complete = hclust(distance_ewcs, method ="complete")
plot(hc_complete , main ="Complete Linkage for K=2", xlab="", sub ="", cex =.9)
cut_com <- (cutree(hc_complete, k=2))
table(cut_com)
plot(hc_complete)
rect.hclust(hc_complete , k = 2)

#Using colours to differentiate the clusters
con_dend_obj <- as.dendrogram(hc_complete)
com_col_dend <- color_branches(con_dend_obj, k=2)
plot(com_col_dend)
plot_hc_k2_com <- fviz_cluster(geom = "point", list(data = ewcs_scaled, cluster = cut_com)) + ggtitle("Complete Linkage for K=2")
print(aggregate(ewcs_cleaned, by=list(cut_com), FUN=mean))

#K = 3
#Average Method
hc_average_2 = hclust(distance_ewcs, method ="average")
plot(hc_average_2 , main ="Average Linkage", xlab="", sub ="", cex =.9)
cut_avg_2 <- (cutree(hc_average_2, k=3))
table(cut_avg_2)
#Only 1 data in cluster 3, skip average method

#Complete Method
hc_complete_2 = hclust(distance_ewcs, method ="complete")
plot(hc_complete_2 , main ="Complete Linkage for K=3", xlab="", sub ="", cex =.9)
cut_com_2 <- (cutree(hc_complete_2, k=3))
table(cut_com_2)
plot(hc_complete_2)
rect.hclust(hc_complete_2 , k = 3)
fviz_cluster(geom = "point", list(data = ewcs_scaled, cluster = cut_com_2)) + ggtitle("Complete Linkage for K=3")
print(aggregate(ewcs_cleaned, by=list(cut_com_2), FUN=mean))

#Using colours to differentiate the clusters
con_dend_obj_2 <- as.dendrogram(hc_complete_2)
com_col_dend_2 <- color_branches(con_dend_obj_2, k=3)
plot(com_col_dend_2)
plot_hc_k3_com <- fviz_cluster(geom = "point", list(data = ewcs_scaled, cluster = cut_com_2)) + ggtitle("Complete Linkage for K=3")
print(aggregate(ewcs_cleaned, by=list(cut_com_2), FUN=mean))

#Compare plots
library(gridExtra)
grid.arrange(plot_k2, plot_k3, plot_hc_k2_avg, plot_hc_k2_com, plot_hc_k3_com, nrow = 3)


##Question 2------------------------------------------------

library(rpart)
library(rpart.plot)
library(caTools)
#Import both csv files
student_mat <- read.csv("student-mat.csv",sep=";", stringsAsFactors = T)
student_por <- read.csv("student-por.csv",sep=";", stringsAsFactors = T)

#Check for missing values and duplicates
sum(is.na(student_mat)) 
sum(is.na(student_por)) 
sum(duplicated(student_mat))
sum(duplicated(student_por))

#Correlation
library(GGally)
ggcorr(student_mat, name = "Correlation", label = T, label_round = 2,
       low = "#3B9AB2",
       mid = "#EEEEEE",
       high = "#F21A00")
#G1 and G2 highly correlated
library(GGally)
ggcorr(student_por, name = "Correlation", label = T, label_round = 2,
       low = "#3B9AB2",
       mid = "#EEEEEE",
       high = "#F21A00")

#remove G1 and G2 variables 
s_mat <- subset(student_mat, select = -c(G1,G2))
s_por <- subset(student_por, select = -c(G1,G2))

#Factor all categorical variables
#student-mat
s_mat$Medu <- factor(s_mat$Medu)
s_mat$Fedu <- factor(s_mat$Fedu)
s_mat$traveltime <- factor(s_mat$traveltime)
s_mat$studytime <- factor(s_mat$studytime)
s_mat$failures <- factor(s_mat$failures)
s_mat$famrel <- factor(s_mat$famrel)
s_mat$freetime<- factor(s_mat$freetime)
s_mat$goout <- factor(s_mat$goout)
s_mat$Dalc <- factor(s_mat$Dalc)
s_mat$Walc <- factor(s_mat$Walc)
s_mat$health <- factor(s_mat$health)

#student-por
s_por$Medu <- factor(s_por$Medu)
s_por$Fedu <- factor(s_por$Fedu)
s_por$traveltime <- factor(s_por$traveltime)
s_por$studytime <- factor(s_por$studytime)
s_por$failures <- factor(s_por$failures)
s_por$famrel <- factor(s_por$famrel)
s_por$freetime <- factor(s_por$freetime)
s_por$goout <- factor(s_por$goout)
s_por$Dalc <- factor(s_por$Dalc)
s_por$Walc <- factor(s_por$Walc)
s_por$health <- factor(s_por$health) 

#One-hot encoding
library(caret)
s_mat_cleaned <- dummyVars(" ~ .", data = s_mat, fullRank = T)
s_mat_new <- data.frame(predict(s_mat_cleaned, newdata = s_mat))
s_por_cleaned <- dummyVars(" ~ .", data = s_por, fullRank = T)
s_por_new <- data.frame(predict(s_por_cleaned, newdata = s_por))

#Codes to check if scaling affects RMSE of Lasso and Regression
#s_mat_new$age <- scale(s_mat_new$age)
#s_mat_new$absences <- scale(s_mat_new$absences)
##Checked and there was no difference in RMSE

### Q2A Mathematics ------------------------------------------------------------

#Train-test split for student-mat 
set.seed(2019)
s_mat_train <- sample.split(Y=s_mat_new$G3, SplitRatio = 0.7)
s_mat_trainset <- subset(s_mat_new, s_mat_train==T)
s_mat_testset <- subset(s_mat_new, s_mat_train==F)

#Check distributions of categorical variables in Trainset vs Testset
summary(subset(s_mat_trainset, select = -c(age, absences, G3)))  # All categories represented
summary(subset(s_mat_testset, select = -c(age, absences, G3)))  # All categories represented

##Linear Regression
set.seed(2019)
m_linear <- lm(G3 ~ ., data = s_mat_trainset)
summary(m_linear)
m_predict <- predict(m_linear, newdata = s_mat_testset)
plot(m_predict)
par(mfrow = c(2,2))
plot(m_linear)
par(mfrow = c(1,1))
m_rmse_test_lreg <- round(sqrt(mean((s_mat_testset$G3 - predict(m_linear, newdata = s_mat_testset))^2)),2)
m_lin_increasing <- sort(m_linear$coefficients)
print(m_lin_increasing)
m_lin_decreasing <- sort(m_linear$coefficients, decreasing = T)
print(m_lin_decreasing)

#Backward Elimination
set.seed(2019)
m_back <- lm(G3 ~ ., data = s_mat_trainset)
m_back_2 <- step(m_back)
summary(m_back_2)
m_predict_back <- predict(m_back_2, newdata = s_mat_testset)
par(mfrow = c(2,2))
plot(m_predict_back)
par(mfrow = c(1,1))
m_rmse_test_back <- round(sqrt(mean((s_mat_testset$G3 - predict(m_back_2, newdata = s_mat_testset))^2)),2)
m_back_inc <- sort(m_back_2$coefficients)
print(m_back_inc)
m_back_dec <- sort(m_back_2$coefficients, decreasing = T)
print(m_back_dec)

#Forward Selection
set.seed(2019)
m_for <- lm(G3 ~ 1, data = s_mat_trainset) #1 means only intercept term, no x variable
summary(m_for)
m_biggest <- formula(lm(G3 ~ ., data = s_mat_trainset))
m_for_2 <- step(m_for, direction = "forward", scope = m_biggest)
summary(m_for_2)
m_predict_for <- predict(m_for_2, newdata = s_mat_trainset)
par(mfrow = c(2,2))
plot(m_predict_for)
par(mfrow = c(1,1))
m_rmse_test_for <- round(sqrt(mean((s_mat_testset$G3 - predict(m_for_2, newdata = s_mat_testset))^2)),2)
m_for_inc <- sort(m_for_2$coefficients)
print(m_for_inc)
m_for_dec <- sort(m_for_2$coefficients, decreasing = T)
print(m_for_dec)

# CART optimal using 1SE rule
set.seed(2019)
m_cart <- rpart(G3 ~ ., method = "anova", control = rpart.control(minsplit = 5, cp = 0), data = s_mat_trainset)
printcp(m_cart, digits = 3)
plotcp(m_cart)
m_cart
# Compute min CVerror + 1SE in maximal CART
m_CVerror_cap <- m_cart$cptable[which.min(m_cart$cptable[,"xerror"]), "xerror"] + m_cart$cptable[which.min(m_cart$cptable[,"xerror"]), "xstd"]
m_CVerror_cap

# Find the optimal CP region whose CV error is just below CVerror_cap in maximal tree m_cart.
i <- 1; j<- 4
while (m_cart$cptable[i,j] > m_CVerror_cap) {
  i <- i + 1
}
m_cart$cptable
# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
m_cp_opt = ifelse(i > 1, sqrt(m_cart$cptable[i,1] * m_cart$cptable[i-1,1]), 1)
# Prune to get 1SE optimal CART via cp.opt
m_cart_1SE <- prune(m_cart, cp = m_cp_opt)
m_cart_1SE$variable.importance
m_rmse_test_cart <- round(sqrt(mean((s_mat_testset$G3 - predict(m_cart_1SE, newdata = s_mat_testset))^2)),2)
rpart.plot(m_cart_1SE, nn=T)

library(randomForest)
set.seed(2019)
##Random Forest with default ntree & mtry
m_RF <- randomForest(G3 ~ . , data = s_mat_trainset)
m_RF
 #OOB MSE is 17.17
sqrt(m_RF$mse[m_RF$ntree]) #Out-of-bag RMSE
plot(m_RF) #confirms that tree stabilizes before 500 tree
importance(m_RF)
varImpPlot(m_RF)
varUsed(m_RF)

#Train-test
m_rmse_test_rf <- round(sqrt(mean((s_mat_testset$G3 - predict(m_RF, newdata = s_mat_testset))^2)),2)
s_mat_testset$G3_predicted <- round(predict(object = m_RF,newdata = s_mat_testset),2)
s_mat_predictions <- subset(s_mat_testset, select = c(G3,G3_predicted))
s_mat_predictions

#Set to matrix
s_mat_matrix <- as.matrix(s_mat_new)
x <- s_mat_matrix[, 1:69]
y <- s_mat_matrix[,70]

# 70-30 train-test split 
set.seed(2019)
s_mat_train <- sample.split(Y=s_mat_new$G3, SplitRatio = 0.7)
s_mat_trainset <- subset(s_mat_matrix, s_mat_train==T)
s_mat_testset <- subset(s_mat_matrix, s_mat_train==F)

# Ridge Regression 
library(glmnet)
set.seed(2019)
grid <- 10^seq(10,-2,length=100)
ridge_mod <- glmnet(x = s_mat_trainset[, 1:69], y = s_mat_trainset[,70], alpha = 0, lambda = grid)
cv_out_ridge <- cv.glmnet(x = s_mat_trainset[, 1:69], y = s_mat_trainset[,70], alpha = 0)
plot(cv_out_ridge)
r_bestlam <- cv_out_ridge$lambda.min
r_bestlam #6.41
ridge_pred <- predict(ridge_mod, s = r_bestlam, newx = s_mat_testset[, 1:69])
m_rmse_test_ridge <- round(sqrt(mean((ridge_pred-s_mat_testset[, 70])^2)),2)
#Ridge Equation
out_ridge <- glmnet(x,y,alpha=0,lambda=grid)
ridge_coef <- predict(out_ridge,type="coefficients",s=r_bestlam)
ridge_coef

# Lasso Regression 
set.seed(2019)
lasso_mod <- glmnet(x = s_mat_trainset[, 1:69], y = s_mat_trainset[,70], alpha = 1, lambda = grid)
cv_out_lasso <- cv.glmnet(x = s_mat_trainset[, 1:69], y = s_mat_trainset[,70], alpha = 1)
plot(cv_out_lasso)
l_bestlam <- cv_out_lasso$lambda.min
l_bestlam #0.24
lasso_pred <- predict(lasso_mod, s = l_bestlam, newx = s_mat_testset[, 1:69])
m_rmse_test_lasso <- round(sqrt(mean((lasso_pred-s_mat_testset[, 70])^2)),2)
#Lasso Equation
out <- glmnet(x,y,alpha=1,lambda=grid)
lasso.coef <- predict(out,type="coefficients",s=l_bestlam)
lasso.coef

#Combine RMSE
Math_Model <- c("Linear Regression", "Backward Elimination", "Forward Selection", "CART", "Random Forest", 'Ridge Regression', "Lasso Regression")
Math_Testset_RMSE <- c(m_rmse_test_lreg, m_rmse_test_back, m_rmse_test_for, m_rmse_test_cart, m_rmse_test_rf, m_rmse_test_ridge, m_rmse_test_lasso)
s_mat_results <- data.frame(Math_Model, Math_Testset_RMSE)
print(s_mat_results)

###Q2B Portuguese language------------------------------------------------------

#Train-test split for student-mat 
set.seed(2019)
s_por_train <- sample.split(Y=s_por_new$G3, SplitRatio = 0.7)
s_por_trainset <- subset(s_por_new, s_por_train==T)
s_por_testset <- subset(s_por_new, s_por_train==F)

#Check distributions of categorical variables in Trainset vs Testset
summary(subset(s_por_trainset, select = -c(age, absences, G3)))  # All categories represented
summary(subset(s_por_testset, select = -c(age, absences, G3)))  # All categories represented

#Linear Regression
set.seed(2019)
p_linear <- lm(G3 ~ ., data = s_por_trainset)
summary(p_linear)
p_predict <- predict(p_linear, newdata = s_por_testset)
plot(p_predict)
par(mfrow = c(2,2))
plot(p_linear)
par(mfrow = c(1,1))
p_rmse_test_lreg <- round(sqrt(mean((s_por_testset$G3 - predict(p_linear, newdata = s_por_testset))^2)),2)
p_lin_increasing <- sort(p_linear$coefficients)
print(p_lin_increasing)
p_lin_decreasing <- sort(p_linear$coefficients, decreasing = T)
print(p_lin_decreasing)

#Backward Elimination
set.seed(2019)
p_back <- lm(G3 ~ ., data = s_por_trainset)
p_back_2 <- step(p_back)
summary(p_back_2)
p_predict_back <- predict(p_back_2, newdata = s_por_testset)
par(mfrow = c(2,2))
plot(p_predict_back)
par(mfrow = c(1,1))
p_rmse_test_back <- round(sqrt(mean((s_por_testset$G3 - predict(p_back_2, newdata = s_por_testset))^2)),2)
p_back_inc <- sort(p_back_2$coefficients)
print(p_back_inc)
p_back_dec <- sort(p_back_2$coefficients, decreasing = T)
print(p_back_dec)

#Forward Selection
set.seed(2019)
p_for <- lm(G3 ~ 1, data = s_por_trainset) #1 means only intercept term, no x variable
summary(p_for)
p_biggest <- formula(lm(G3 ~ ., data = s_por_trainset))
p_for_2 <- step(p_for, direction = "forward", scope = p_biggest)
summary(p_for_2)
p_predict_for <- predict(p_for_2, newdata = s_por_trainset)
par(mfrow = c(2,2))
plot(p_predict_for)
par(mfrow = c(1,1))
p_rmse_test_for <- round(sqrt(mean((s_por_testset$G3 - predict(p_for_2, newdata = s_por_testset))^2)),2)
p_for_inc <- sort(p_for_2$coefficients)
print(p_for_inc)
p_for_dec <- sort(p_for_2$coefficients, decreasing = T)
print(p_for_inc)

##CART optimal using 1SE rule
set.seed(2019)
p_cart <- rpart(G3 ~ ., method = "anova", control = rpart.control(minsplit = 5, cp = 0), data = s_por_trainset)
printcp(p_cart, digits = 3)
plotcp(p_cart)
p_cart
# Compute min CVerror + 1SE in maximal CART
p_CVerror_cap <- p_cart$cptable[which.min(p_cart$cptable[,"xerror"]), "xerror"] + p_cart$cptable[which.min(p_cart$cptable[,"xerror"]), "xstd"]
p_CVerror_cap
# Find the optimal CP region whose CV error is just below CVerror_cap in maximal tree m_cart.
i <- 1; j<- 4
while (p_cart$cptable[i,j] > p_CVerror_cap) {
  i <- i + 1
}

p_cart$cptable
# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
p_cp_opt = ifelse(i > 1, sqrt(p_cart$cptable[i,1] * p_cart$cptable[i-1,1]), 1)
# Prune to get 1SE optimal CART via cp.opt
p_cart_1SE <- prune(p_cart, cp = p_cp_opt)
p_cart_1SE$variable.importance
rpart.plot(p_cart_1SE, nn=T)
p_rmse_test_cart <- round(sqrt(mean((s_por_testset$G3 - predict(p_cart_1SE, newdata = s_por_testset))^2)),2)

##Random Forest with default ntree & mtry
set.seed(2019)
p_RF <- randomForest(G3 ~ . , data = s_por_trainset)
p_RF #OOB MSE is 7.68
sqrt(p_RF$mse[p_RF$ntree])#Out-of-bag RMSE 2.77
plot(p_RF) #confirms that tree stabilizes before 500 tree
importance(p_RF)
varImpPlot(p_RF)
varUsed(p_RF)

#Train-test rmse
p_rmse_test_rf <- round(sqrt(mean((s_por_testset$G3 - predict(p_RF, newdata = s_por_testset))^2)),2)

#Predictions
s_por_testset$G3_predicted <- round(predict(object = p_RF,newdata = s_por_testset),2)
s_por_predictions <- subset(s_por_testset, select = c(G3,G3_predicted))
s_por_predictions

#Set to matrix
s_por_matrix <- as.matrix(s_por_new)
x <- s_por_matrix[, 1:69]
y <- s_por_matrix[,70]

# 70-30 train-test split 
set.seed(2019)
s_por_train <- sample.split(Y=s_por_new$G3, SplitRatio = 0.7)
s_por_trainset <- subset(s_por_matrix, s_por_train==T)
s_por_testset <- subset(s_por_matrix, s_por_train==F)

# Ridge Regression 
set.seed(2019)
grid <- 10^seq(10,-2,length=100)
ridge_mod_2 <- glmnet(x = s_por_trainset[, 1:69], y = s_por_trainset[,70], alpha = 0, lambda = grid)
cv_out_ridge_2 <- cv.glmnet(x = s_por_trainset[, 1:69], y = s_por_trainset[,70], alpha = 0)
plot(cv_out_ridge_2)
r_bestlam_2 <- cv_out_ridge_2$lambda.min
r_bestlam_2 #1.78
ridge_pred_2 <- predict(ridge_mod_2, s = r_bestlam_2, newx = s_por_testset[, 1:69])
p_rmse_test_ridge <- round(sqrt(mean((ridge_pred_2-s_por_testset[, 70])^2)),2)
#Ridge Equation
out_ridge_2 <- glmnet(x,y,alpha=0,lambda=grid)
ridge_coef_2 <- predict(out_ridge_2,type="coefficients",s=r_bestlam_2)
ridge_coef_2

# Lasso Regression 
set.seed(2019)
lasso_mod_2 <- glmnet(x = s_por_trainset[, 1:69], y = s_por_trainset[,70], alpha = 1, lambda = grid)
cv_out_lasso_2 <- cv.glmnet(x = s_por_trainset[, 1:69], y = s_por_trainset[,70], alpha = 1)
plot(cv_out_lasso_2)
l_bestlam_2 <- cv_out_lasso_2$lambda.min
l_bestlam_2 #0.097
lasso_pred_2 <- predict(lasso_mod_2, s = l_bestlam_2, newx = s_por_testset[, 1:69])
p_rmse_test_lasso <- round(sqrt(mean((lasso_pred_2-s_por_testset[, 70])^2)),2)
#Lasso Equation
out_2 <- glmnet(x,y,alpha=1,lambda=grid)
lasso_coef_2 <- predict(out_2,type="coefficients",s=l_bestlam_2)
lasso_coef_2

#Combine RMSE
Por_Model <- c("Linear Regression", "Backward Elimination", "Forward Selection", "CART", "Random Forest", 'Ridge Regression', "Lasso Regression")
Por_Testset_RMSE <- c(p_rmse_test_lreg, p_rmse_test_back, p_rmse_test_for, p_rmse_test_cart, p_rmse_test_rf, p_rmse_test_ridge, p_rmse_test_lasso)
s_por_results <- data.frame(Por_Model, Por_Testset_RMSE)
print(s_por_results)


###Question 3 --------------------------------------------------------------
bank <- read.csv("bank.csv",sep=";", stringsAsFactors = T)
str(bank)
summary(bank)

#Change negative values to positive
bank2 <- bank
bank$pdays <- abs(bank$pdays)
bank2$pdays <- abs(bank2$pdays)
summary(bank2)

#Check for missing values and duplicates
sum(is.na(bank2)) 
sum(duplicated(bank2))
#no missing or duplicated rows

#glmnet requires matrix 
bank2$job <- as.numeric(bank2$job)
bank2$marital <- as.numeric(bank2$marital)
bank2$education <- as.numeric(bank2$education)
bank2$default <- ifelse(bank2$default == "yes", 1,0)
bank2$housing <- ifelse(bank2$housing == "yes", 1,0)
bank2$loan <- ifelse(bank2$loan == "yes", 1,0)
bank2$contact <- as.numeric(bank2$contact)
bank2$month <- as.numeric(bank2$month)
bank2$poutcome <- as.numeric(bank2$poutcome)
bank2$y <- ifelse(bank2$y == "yes", 1,0)

library(corrplot)
#plotting correlation
bank_cor = cor(bank2)
corrplot(bank_cor, method = 'number')
## We can see that poutcome and pdays are highly negatively correlated. Remove either one
#Remove pdays due to multicollinearity
bank2 <- subset(bank2, select = -c(pdays))
bank2 <- subset(bank2, select = -c(duration))

#Scaling 
bank2$age <- scale(bank2$age)
bank2$balance <- scale(bank2$balance)
bank2$day <- scale(bank2$day)
bank2$previous <- scale(bank2$previous)
bank2$campaign <- scale(bank2$campaign)

library(MASS)
library(caTools)
library(class)
#Train-test split 
set.seed(2019)
b_train <- sample.split(Y = bank2$y, SplitRatio = 0.7)
b_trainset <- subset(bank2, b_train ==T)
b_testset <- subset(bank2, b_train == F)

#Logistic Regression
set.seed(2019)
library(glmnet)     
b_log <- glm(y ~ ., family = binomial, data = b_trainset)
summary(b_log)

#Prediction and misclassification of the model
prediction_log <- predict.glm(b_log, newdata = b_testset, type = "response")
prediction_log[prediction_log > 0.5] <- 1
prediction_log[prediction_log <= 0.5] <- 0
table(prediction_log, b_testset$y)
library(gmodels)
library(ROSE)
CrossTable(b_testset$y, prediction_log, prop.chisq = FALSE)
table()
## 99.7% of people not subscribing the term deposit in the data set is predicted correctly.
## 6.0% of people subscribing term deposit is predicted correctly.
#Error, accuracy and AUC
me_log <- 1 - length(prediction_log[prediction_log == b_testset$y]) / length(prediction_log)
print(me_log)#Misclassification error of 11.72%
accuracy_log <- mean(prediction_log == b_testset$y)
print(accuracy_log) #88.27
log_test_pred <- predict(b_log, b_testset)
auc_log <- roc.curve(b_testset$y, log_test_pred)
print(auc_log) #AUC 0.673

#KNN Model
#K = 3
set.seed(2019)
library(class)
knn_pred_3 = knn(b_trainset, b_testset, b_trainset$y, k = 3)
table(b_testset$y, knn_pred_3)
CrossTable(b_testset$y, knn_pred_3, prop.chisq = FALSE)
## 99.3% of people not subscribing the term deposit in the data set is predicted correctly.
## 26.9% of people subscribing term deposit is predicted correctly.
#Error, accuracy and AUC
me_knn3 <- 1 - length(b_testset$y[b_testset$y == knn_pred_3]) / length(b_testset$y)
print(me_knn3) #Misclassification error of 9.07%
accuracy_knn3 <- mean(knn_pred_3 == b_testset$y)
print(accuracy_knn3) #90.9%
auc_knn3 <- roc.curve(b_testset$y, knn_pred_3)
print(auc_knn3) #AUC 0.631

#K = 5
set.seed(2019)
knn_pred_5 = knn(b_trainset, b_testset, b_trainset$y, k = 5)
table(b_testset$y, knn_pred_5)
CrossTable(b_testset$y, knn_pred_5, prop.chisq = FALSE)
## 99.8% of people not subscribing the term deposit in the data set is predicted correctly.
## 15.4% of people subscribing term deposit is predicted correctly.
#Error, accuracy and AUC
me_knn5 <- 1 - length(b_testset$y[b_testset$y == knn_pred_5]) / length(b_testset$y)
print(me_knn5)#Misclassification error of 9.95%
accuracy_knn5 <- mean(knn_pred_5 == b_testset$y)
print(accuracy_knn5) #90.04%
auc_knn5 <- roc.curve(b_testset$y, knn_pred_5)
print(auc_knn5) #AUC 0.576

b_testset$y <- as.factor(b_testset$y)
b_trainset$y <- as.factor(b_trainset$y)

#Random Forest
library(randomForest)
set.seed(2019)
##Random Forest with 500 ntree & 3 mtry
b_RF <- randomForest(y ~ . , data = b_trainset, ntree =500, mtry=3, importance = TRUE, proximity = TRUE)
print(b_RF) #OOB Error 11.09%
attributes(b_RF)
#Prediction % Confusion Matrix for Train data
library(caret)
p1 <- predict(b_RF, newdata = b_trainset)
confusionMatrix(b_trainset$y, p1)
#98.86%

#Prediction % Confusion Matrix for Test data
p2 <- predict(b_RF, newdata = b_testset)
confusionMatrix(b_testset$y, p2) #89.16%

#Error, accuracy and AUC
me_rf <- 1 - length(b_testset$y[b_testset$y == p2]) / length(b_testset$y)
print(me_rf)#Misclassification Rate 10.84
accuracy_rf <- mean(p2 == b_testset$y)
print(accuracy_rf) #Accuracy of 89.23%
auc_rf <- roc.curve(b_testset$y, p2)
print(auc_rf) #0.560

#Error rate
plot(b_RF)
t <- tuneRF(b_trainset[,-15], b_trainset[,15], stepFactor = 0.6, plot = TRUE, ntreetry =300, trace = TRUE, improve = 0.05) 

#Importance
varImpPlot(b_RF)
importance(b_RF)
varUsed(b_RF)

#CART
library(rpart)
library(rpart.plot)
set.seed(2019)
b_cart <- rpart(y ~ ., method = "class", control = rpart.control(minsplit = 15, cp = 0), data = b_trainset)
rpart.plot(b_cart, nn=T)
print(b_cart)
printcp(b_cart, digits = 3)
prediction_cart <- predict(b_cart, b_testset, type = "class")
#Cross Table
table(b_testset$y, prediction_cart)
CrossTable(b_testset$y, prediction_cart, prop.chisq = FALSE)
## 97.0% of people not subscribing the term deposit in the data set is predicted correctly.
## 19.2% of people subscribing term deposit is predicted correctly.

#Error, accuracy and AUC
me_cart <- 1 - length(b_testset$y[b_testset$y == prediction_cart]) / length(b_testset$y)
print(me_cart)#Misclassification Rate 11.94
accuracy_cart <- mean(prediction_cart == b_testset$y)
print(accuracy_cart) #88.05%
auc_cart <- roc.curve(b_testset$y, prediction_cart)
legend("topleft", c("CART"), col=1:2, lty=1:2, lwd=2)
print(auc_cart) #0.581

#Combine Accuracy
Bank_Methods <- c("Logistic Regression", "K-Nearest Neighbour(3)", "K-Nearest Neighbour(5)", "Random Forest", "CART")
Misclassification_Errors <- c(me_log, me_knn3, me_knn5, me_rf, me_cart)
Methods_Accuracy <- c(accuracy_log, accuracy_knn3, accuracy_knn5, accuracy_rf, accuracy_cart)
bank_results <- (data.frame(Bank_Methods, Misclassification_Errors, Methods_Accuracy))
print(bank_results)

#AUC scores
print(auc_log)
print(auc_knn3)
print(auc_knn5)
print(auc_rf)
print(auc_cart)
#-------------------------------------END---------------------------------------





