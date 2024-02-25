if (!require(corrplot)) {
  install.packages("corrplot")
  library(corrplot)
}

#   Read the loans data set saved in comma-separated value format
loans.full <- read.csv("C:/Users/lloyd/Documents/GitHub/works/R/models/code/loans-default.csv")


#########################################
## Start of clustering  ##
#########################################

#   Display the structure of the object read in, and summarize its fields
str(loans.full)
summary(loans.full)

#   Compute and display the fraction of defaulters in the data set
default_fraction <- mean(loans.full$default)
cat("Fraction of defaulters:", default_fraction, "\n")

#########################################
## Descriptive statistics and graphics ##
#########################################

#   Single variable summary of continuous variables
boxplot(loans.full$fico, horizontal = TRUE, xlab = "FICO")

boxplot(loans.full$installment, horizontal = TRUE, xlab = "installment")
boxplot(loans.full$dti, horizontal = TRUE, xlab = "log.annual.inc")
boxplot(loans.full$log.annual.inc, horizontal = TRUE, xlab = "log.annual.inc")

#   Cross-tabulate how the target variable and other categorical variables are related
xtabs(~credit.policy + default, data = loans.full)
xtabs(~purpose + default, data = loans.full)

#   Visualize the variation of default with additional categorical variables
 barplot(table(loans.full$default, loans.full$log.annual.inc),
               main = "Default Distribution by log.annual.inc",
               xlab = "log.annual.inc",
              col = c("green", "red"))

barplot(table(loans.full$default, loans.full$revol.bal),
               main = "Default Distribution by revol.bal",
               xlab = "revol.bal",
              col = c("green", "red"))

#  Simple histograms of the default variable versus categorical variables
barplot(table(loans.full$default),
        main = "Paid versus not paid loans",
        xlab = "Status",
        border = "black",
        col = c("green", "red"))

#   Adapt the above command to visually represent the variation of defaulters with other categorical variables

#   Box-and-whisker plots to see dispersion of continuous vars across default and not default
boxplot(loans.full$int.rate ~ loans.full$default, main = "Interest Rates across Default")

#   Adapt the above command to visually represent the variation of defaulters with other continuous variables
boxplot(loans.full$installment ~ loans.full$default, main = "Installment Amount across Default")
boxplot(loans.full$days.with.cr.line ~ loans.full$default, main = "days.with.cr.line Amount across Default")
boxplot(loans.full$delinq.2yrs ~ loans.full$default, main = "delinq.2yrs Amount across Default")

#   Compute correlation matrix among numerical attributes
correlations <- cor(loans.full[, c(1, 3:14)])

#   Display the correlation matrix
print("Correlation Matrix:")
print(correlations)

#   Generate a heat map of correlated predictors
corrplot(correlations, order = "hclust")

##########################################
## Adding derived variables             ##
##########################################

#   The total payment on a 36-month loan if no default
loans.full$tot.payment <- 36 * loans.full$installment

#   We use the compound interest formula for 3 years at the given interest rate to calculate the principal
loans.full$principal <- loans.full$tot.payment / exp(3 * loans.full$int.rate)
loans.full$interest <- loans.full$tot.payment - loans.full$principal

#   Save result with additional columns as a csv file
write.csv(loans.full, row.names = FALSE, file = "C:/Users/lloyd/Documents/GitHub/works/R/models/code/loans-kpi.csv")


#################################################################################################################################
## Metrics Logistics Regression
#################################################################################################################################

if (!require(corrplot)) {
  install.packages("corrplot")
  library(corrplot)
}
if (!require(gmodels)) {
  install.packages("gmodels")
  library(gmodels)
}
if (!require(gplots)) {
  install.packages("gplots")
  library(gplots)
}
if (!require(lattice)) {
  install.packages("lattice")
  library(lattice)
}
if (!require(psych)) {
  install.packages("psych")
  library(psych)
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

loans.full <- read.csv("C:/Users/lloyd/Documents/GitHub/works/R/models/code/loans-kpi.csv")

#   Display the structure of the object read in, and summarize its fields
str(loans.full)
summary(loans.full)

#   Compute correlations
correlations <- cor(loans.full[, c(1, 3:17)])

#   Generate a heat map of correlated predictors
corrplot(correlations, order = "hclust")

#   K-Means clustering

#   Define the variable list for clustering
varlist <- c(
  "credit.policy",
  "log.annual.inc",
  "dti",
  "fico",
  "inq.last.6mths",
  "delinq.2yrs",
  "pub.rec"
)

#   Standardize the data
xloan <- scale(loans.full[varlist])

#   Perform k-means clustering with k=5
grpA <- kmeans(xloan, centers = 5)

#   Examine how the clusters split the loans that obey the credit policy versus those that do not
xtabs(~loans.full$credit.policy + grpA$cluster)

# E. xamine how the clusters split other categories (e.g., default behavior)
xtabs(~loans.full$default + grpA$cluster)

#   Scree plot to determine the number of clusters
#   Compute multiple cluster solutions
set.seed(124895792) 
grpA2 <- kmeans(xloan, centers = 2, nstart = 30)  
grpA3 <- kmeans(xloan, centers = 3, nstart = 30)
grpA4 <- kmeans(xloan, centers = 4, nstart = 30)
grpA5 <- kmeans(xloan, centers = 5, nstart = 30)
grpA6 <- kmeans(xloan, centers = 6, nstart = 30)
grpA7 <- kmeans(xloan, centers = 7, nstart = 30)
grpA8 <- kmeans(xloan, centers = 8, nstart = 30)
grpA9 <- kmeans(xloan, centers = 9, nstart = 30)
grpA10 <- kmeans(xloan, centers = 10, nstart = 30)
grpA15 <- kmeans(xloan, centers = 15, nstart = 30)
grpA20 <- kmeans(xloan, centers = 20, nstart = 30)
grpA30 <- kmeans(xloan, centers = 30, nstart = 30)

#   Compute between and within SS
kclust <- c(2:10, 15, 20, 30)
bss <- c(
  grpA2$betweenss, grpA3$betweenss, grpA4$betweenss, grpA5$betweenss, grpA6$betweenss,
  grpA7$betweenss, grpA8$betweenss, grpA9$betweenss, grpA10$betweenss,
  grpA15$betweenss, grpA20$betweenss, grpA30$betweenss
)
wss <- c(
  grpA2$tot.withinss, grpA3$tot.withinss, grpA4$tot.withinss, grpA5$tot.withinss, grpA6$tot.withinss,
  grpA7$tot.withinss, grpA8$tot.withinss, grpA9$tot.withinss, grpA10$tot.withinss,
  grpA15$tot.withinss, grpA20$tot.withinss, grpA30$tot.withinss
)

#   Plot the results and look for the "Hockey-Stick" effect
par(mfrow = c(1, 1))
plot(kclust, wss, type = "l", main = "Within SS for k-means")   
points(kclust, wss)

plot(kclust, bss, type = "l", main = "Between SS for k-means")  
points(kclust, bss)
plot(kclust, bss / (wss + bss), type = "l", main = "R-Squared for k-means")  
points(kclust, bss / (wss + bss))

#   Choose the number of clusters
k <- 7

# Compute k-means clustering with chosen k
set.seed(1248765792)
grpB <- kmeans(xloan, centers = k)

#   Set cluster names
knames <- as.character(1:k)

#   Plot the solutions against credit.policy and inquiries in the last 6 months
par(mfrow = c(1, 1), mar = c(5, 4, 4, 1) + 0.1)
plot(jitter(xloan[, "credit.policy"]), jitter(xloan[, "inq.last.6mths"]), xlab = "cr.pol", ylab = "inq6m", col = grpB$cluster)
points(grpB$centers[, c("credit.policy", "inq.last.6mths")], col = 1:k, pch = 8, cex = 2)
legend("topright", pch = 8, bty = "n", col = 1:k, knames)

#   Plot another variable combination (e.g., fico and dti)
plot(xloan[, "fico"], (xloan[, "dti"]), xlab = "fico", ylab = "dti", col = grpB$cluster)
points(grpB$centers[, c("fico", "dti")], col = 1:k, pch = 8, cex = 2)
legend("topleft", pch = 8, bty = "n", col = 1:k, knames)

#   Compare the cluster solutions with loan purpose
(result = xtabs(~loans.full$purpose + grpB$cluster))

#   Cross-tabulation with a BalloonPlot
CrossTable(loans.full$purpose, grpB$cluster)

#   More visual representation with a BalloonPlot
if (!require(gplots)) {
  install.packages("gplots")
  library(gplots)
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
par(mfrow = c(1, 1))
balloonplot(result)

#   Summarize the centroids
round(grpB$centers, 2)

#   Create a parallel plot to visualize the centroid values
if (!require(lattice)) {
  install.packages("lattice")
  library(lattice)
}
parallelplot(
  grpB$centers,
  varnames = varlist,
  auto.key = list(text = knames, space = "top", columns = 1, lines = T),
  scales = list(cex = 0.5)
)

#   A parallel plot with a subset of variables
shortvarlist <- c("fico", "dti", "log.annual.inc")
round(grpB$centers[, shortvarlist], 2)
parallelplot(grpB$centers[, shortvarlist], varnames = shortvarlist, auto.key = list(text = knames, space = "top", columns = 3, lines = T))

pairs(sapply(loans.full[shortvarlist], jitter, amount = 0.2), col = grpB$cluster)

write.csv(grpB$cluster, file = "C:/Users/lloyd/Documents/GitHub/works/R/models/code/Loans_ResultsBcluster.csv")  # cluster assignments
write.csv(grpB$centers, file = "C:/Users/lloyd/Documents/GitHub/works/R/models/code/Loans_ResultsBcenters.csv")  # cluster centroids

pcs <- prcomp(loans.full[, varlist], scale = TRUE)

#   Display summary and the first few principal components
summary(pcs)
print(pcs)

#   Plot the principal components
plot(pcs, main = "")
mtext(side = 1, "Loan Data Principal Components", line = 1, font = 2)

#   Display the standard deviation of each component
pcs$sdev

#   Calculate the variance explained by each component
pr.var <- pcs$sdev^2
pr.var

#   Calculate the percentage of variance explained by each component
pve <- pr.var / sum(pr.var)
pve

#   Plot the variance explained and cumulative variance
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = 'b')
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal Component", ylim = c(0, 1), col = "brown3")

#   For a bottom-up interpretation of the PCs, examine how each PC combines the variables
t(round(pcs$rotation[, 1:2], 2))

#   Create a heatmap of the loadings
corrplot(pcs$rotation, main = "Correlation of each feature on PC's")

# Smaller subset of data for biplot
loans.sample <- loans.full[sample(nrow(loans.full), size = 500), ]
pcs.sample <- prcomp(loans.sample[, varlist], scale = TRUE)
summary(pcs.sample)
print(pcs.sample)
biplot(pcs.sample, scale = 0)

xloans.mean <- colMeans(loans.full[varlist])
xloans.sd <- apply(loans.full[varlist], 2, sd)

#   Translate the centers back to the original scale
grpBcenter.orig <- sweep(grpB$centers, MARGIN = 2, xloans.sd, '*')  
grpBcenter.orig <- sweep(grpBcenter.orig, MARGIN = 2, xloans.mean, '+') 
#   Print the Centers on the original scale
print(t(grpBcenter.orig))
#   Summarize the centroids with a parallel plot
parallelplot(
  grpBcenter.orig,
  auto.key = list(text = as.character(1:3), common.scale = TRUE, space = "top", columns = 3, lines = T)
)

#   Copy the labels from your clustering scheme
mycluster <- grpB$cluster

#   Compute a table of descriptive statistics of standardized data for each cluster
describeBy(xloan, group = mycluster)

#   Compute a boxplot for a selected variable to understand the distribution within each cluster
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1)) 
boxplot(loans.full$fico ~ mycluster, data = loans.full, notch = TRUE, xlab = "Cluster", ylab = "fico") 

#   A for loop goes across every variable
par(mfrow = c(1, 1), mar = c(3, 4, 1, 1)) 
for (i in varlist) {
  boxplot(loans.full[, i] ~ mycluster, data = loans.full, notch = TRUE, xlab = "Cluster", ylab = i) 
}
par(mfrow = c(1, 1)) 

#   Violin Plots
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))  # Setup margins for one panel
ggplot(loans.full, aes(factor(mycluster), dti)) + geom_violin() + geom_jitter(height = 0, width = 0.1)
ggplot(loans.full, aes(factor(mycluster), fico)) + geom_violin() + geom_jitter(height = 0, width = 0.1)

#   Hierarchical clustering
par(mfrow = c(1, 1))
(grphQ = hclust(dist(xloan[subset = sample(nrow(xloan), 100)]), method = "complete"))
plot(grphQ, cex = 0.7)

(grphP = hclust(dist(as.matrix(t(xloan), method = "complete"))))
plot(grphP, cex = 0.7)


#################################################################################################################################
## Validation Tree models
#################################################################################################################################

loans.full <- read.csv("loans-kpi.csv")

#   Display the structure of the object read in, and summarize its fields
str(loans.full)
summary(loans.full)

mean(loans.full$default)

##   Descriptive graphics
#   Barplots and correlation heatmap
barplot(table(loans.full$default),
        main="Paid versus not paid loans",
        xlab="Status",
        border="black", 
        col=c("green","red"))

barplot(table(loans.full$default, loans.full$credit.policy),
        main="Default Distribution by Credit Policy",
        xlab="Cust meets underwriting criteria",
        col=c("green","red"))

(correlations <- cor(loans.full[,c(1,3:17)]))

if (!require(corrplot)) {install.packages("corrplot"); library(corrplot)}
#   Generate a heat map of correlated predictors
corrplot(correlations, order="hclust")

####################################################
## Prepare training and testing sets for modeling ##
####################################################

Nfull <- nrow(loans.full)
randvalue <- runif(Nfull)
trainsample <- randvalue < .7
testsample <- (randvalue >= .7)

train <- loans.full[trainsample,]
test <- loans.full[testsample,]

train$default <- as.factor(train$default)
test$default <- as.factor(test$default)

#########################
## Logistic Regression ##
#########################

#   Basic Logistic regression model using all columns
lrfit = glm(default ~ ., data=train, family="binomial")
summary(lrfit)
#   index of Significant predictors
id = which(summary(lrfit)$coeff[,4] < 0.05)
#   Significant predictors
coeff.sig = summary(lrfit)$coeff[,1][id]
names(coeff.sig)

lrfit2 = glm(default ~ . - tot.payment, data=train, family="binomial")

####################################################
# Stepwise Regression

null = glm(default~1,data=train,family="binomial")

full = glm(default~.,data=train,family="binomial") 

swlrmdl = step(null, scope=formula(full),steps=15,dir="forward")  # can increase beyond 15 steps, just takes more time
summary(swlrmdl)

################################
#   You can use the stepwise model (or your own model with carefully selected variables)
lrfit = swlrmdl

######################################
#   Estimate the AUC (Area Under Curve)
#   Predict the probability of not fully paying back using lrmodel and test set
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
lr.default.prob = predict(lrfit, newdata=test, type="response")
pred.lr = prediction(lr.default.prob, test$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 
plot(perf.lr, col=rainbow(10))
#   Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr,"auc")
(auc.lr = as.numeric(auc.tmp@y.values))

####################################################
#   Visualization of results against variables
#   Plot the response of the model as a function of each of the other variables while holding
#   all other variables at their median values
if (!require(plotmo)) {install.packages("plotmo"); library(plotmo)}  #


#################################################################################################################################
## Linear and Integer Optimization
#################################################################################################################################

loans.full = read.csv("loans-kpi.csv")

##   Descriptive graphics
#   Barplot of paid versus not paid loans
barplot(table(loans.full$default),
        main="Paid versus not paid loans",
        xlab="Status",
        border="black", 
        col=c("green","red"))

#   Barplot of default distribution by credit policy
barplot(table(loans.full$default, loans.full$credit.policy),
        main="Default Distribution by Credit Policy",
        xlab="Cust meets underwriting criteria",
        col=c("green","red"))

#   Build a preliminary tree on the whole data set
if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}

#   Fit a full tree on the entire dataset
(fulltree <- rpart(default ~ ., data=loans.full))
#   Display summary and plot the tree
summary(fulltree)
prp(fulltree)

set.seed(77777)

Nfull <- nrow(loans.full)

#   Make two boolean indicator vectors called pos and neg of defaulters and non-defaulters in the data set
pos <- (loans.full$default == 1)
neg <- (loans.full$default == 0)
#   Store their numbers
(np <- sum(pos))
(nn <- sum(neg))

#   Save the indices of all the positive examples
idxpos = which(pos)  
#   Save the indices of all the negative examples
allneg = which(neg)
#   Pick out as many negative examples as np, the number of positive examples
idxneg = allneg[1:np]  

#   Concatenate an equal number of pos and neg examples to create a balanced dataset index
l = c(idxpos, idxneg)
#   Check the length of l - it must be twice the size of the defaulters or 2 X 1533
length(l)

#   Take the dataset to only have the subset we collected in the list l
loans = loans.full[l,]
#   Reset the number of observations in the dataset
N = nrow(loans)

##   Repeat the histograms of the defaulters and non-defaulters after downsampling
barplot(table(loans$default),
        main="Paid versus not paid loans",
        xlab="Status",
        border="black", 
        col=c("green","red"))
#... and the distribution across categorical variables such as credit.policy
barplot(table(loans$default, loans$credit.policy),
        main="Default Distribution by Credit Policy",
        xlab="Cust meets underwriting criteria",
        col=c("green","red"))

#   Prepare training and testing sets for modeling
randvalue <- runif(N)
trainsample <- randvalue < 0.7
testsample <- (randvalue >= 0.7)

#   Use the subsets to differentiate the training and testing data sets
train <- loans[trainsample,]
test <- loans[testsample,]

train$default <- as.factor(train$default)
test$default <- as.factor(test$default)



####################################
## Predictive Modeling with Trees ##
####################################
#   Build a default full tree model with all the variables, view summary, and plot the tree
ftree <- rpart(default ~ ., data=train)
par(mfrow=c(1,1))        
plot(ftree); text(ftree)  
prp(ftree)                
prp(ftree, extra=101)     

if (!require(plotmo)) {install.packages("plotmo"); library(plotmo)}  # show model response
plotmo(ftree)   

#######################################################
##             Predicting with the tree model        ##
#######################################################
#   Thus the values along the second column of this result are the probabilities of default
default.prediction.full <- predict(ftree, test, type="prob")
#   The next line creates a boolean (True/False) array of those with default probability above threshold
pred.ftree50 <- (default.prediction.full[,2] > 0.5)
#   The table command puts the first argument along the rows and the second along columns
table(test$default, pred.ftree50)

#######################################################
##        Calculate ROC and AUC 
#######################################################
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
fit.pr.tree <- predict(ftree, test, type="prob")[,2]
#   Recall that a prediction of type vector returns a matrix which has the probabilities in column 2
fit.pred.tree <- prediction(fit.pr.tree, test$default)

fit.perf.tree <- performance(fit.pred.tree, "tpr", "fpr")
plot(fit.perf.tree, lwd=2, col="blue",
     main="ROC:  Classification Trees on Loan Default Dataset")
abline(a=0, b=1)
#   Commands for calculating AUROC using ROCR
auc.tmp <- performance(fit.pred.tree, "auc")
(auc.tree <- as.numeric(auc.tmp@y.values))

summary(ftree2)
prp(ftree2, extra=101)

ftree3 = rpart(default ~ ., data=train, control=rpart.control(cp=0.008))
summary(ftree3)
prp(ftree3)

#   But remember the default tree is chosen to avoid overfitting!
######################################################################################

#   Set the model to be the final model you want to test
rtree <- ftree2

#########################
## Logistic Regression ##
#########################

####################################################
#   Using Stepwise Regression as a stub instead of your model
null = glm(default ~ 1, data=loans, family="binomial")
#   second estimate a complete model (with all variables that you are interested in)
full = glm(default ~ .- int.rate - credit.policy, data=loans, family="binomial") 
#   finally estimate the step-wise regression starting with the null model
swlrmdl = step(null, scope=formula(full), steps=15, dir="forward") 
summary(swlrmdl)

#   Use the stepwise model for counterfactuals
lrfit = swlrmdl
summary(lrfit)
plotmo(lrfit)             
id = which(summary(lrfit)$coeff[,4] < 0.05)
#   Significant predictors
coeff.sig = summary(lrfit)$coeff[,1][id]
names(coeff.sig)

#   Estimate the AUC (Area Under Curve)
#   Predict the probability of not fully paying back using lrmodel and test set
lr.default.prob = predict(lrfit, newdata=test, type="response")
pred.lr = prediction(lr.default.prob, test$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 
plot(perf.lr, col=rainbow(10))
#   Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr, "auc")
(auc.lr = as.numeric(auc.tmp@y.values))

###############################################################################
### compare models using ROC plot
###############################################################################

#   plot both ROC curves together
plot(perf.lr, col="red"); abline(a=0, b=1)
plot(fit.perf.tree, add=TRUE, col="blue")
legend("bottomright", c("LogRegr", "Tree"), pch=15, col=c("red", "blue"), bty="n")
###############################################################################################

##   Counterfactual on the WHOLE data set using tree
########################################################
full.loans = read.csv("loans-kpi.csv")
tree.default.prob = predict(rtree, newdata=full.loans, type="prob")
#   Create the confusion matrix of the full set for tree model
(Conf.tree.tree = table(full.loans$default, tree.default.prob[,2] > 0.5))

#   False negative rate of the tree model on the full set
(Conf.tree.tree[2,1]/(Conf.tree.tree[1,1]+Conf.tree.tree[2,1]))
#   Baseline rate of default
(sum(full.loans$default)/nrow(full.loans))
#   Output the subset of loans from the full set filtered by the rpart model for further ROI calc
write.csv(full.loans[tree.default.prob[,2] < 0.5, ], "loans_imputed_tree_05.csv")

########################################################
## Counterfactual on the whole data set using LR
########################################################

full.lr.default.prob = predict(lrfit, newdata=full.loans, type="response")
#   Create the confusion matrix of the full set
Conf.lr.full = table(full.loans$default, full.lr.default.prob > 0.5)

Conf.lr.full
#   False negative rate of the lr model on the full set
(Conf.lr.full[2,1]/(Conf.lr.full[1,1]+Conf.lr.full[2,1]))
#   Baseline rate of default
(sum(full.loans$default)/nrow(full.loans))
#   Output the subset of loans from the full set filtered by the lr model for further ROI calc
write.csv(full.loans[full.lr.default.prob < 0.5, ], "loans_imputed_lr_05.csv")
#   You can again output different subsets filtered by different cutoffs modifying the above

write.csv(cbind(full.loans, tree.default.prob[,2], full.lr.default.prob), "loans_imputed_full_2models.csv")
###################################################################################################

##   Random Forest Model
if (!require(randomForest)) {install.packages("randomForest"); library(randomForest)} 
rf.loans <- randomForest(default ~ log.annual.inc + dti + fico + days.with.cr.line + revol.bal + revol.util + inq.last.6mths + delinq.2yrs + pub.rec + principal, data=train)
rf.pred <- predict(rf.loans, test, type="class")
( rf.results <- table(rf.pred, test$default) )
importance(rf.loans)
varImpPlot(rf.loans)

bag.loans <- randomForest(default ~ log.annual.inc + dti + fico + days.with.cr.line + revol.bal + revol.util + inq.last.6mths + delinq.2yrs + pub.rec + principal, data=train, mtry=10, importance=TRUE)
bag.pred <- predict(bag.loans, test, type="class")
( bag.results <- table(bag.pred, test$default) )
importance(bag.loans)
varImpPlot(bag.loans)

#   Plot all ROC curves together
fit.pr.tree <- predict(rtree, test, type="prob")[,2]
fit.pred.tree <- prediction(fit.pr.tree, test$default)
fit.perf.tree <- performance(fit.pred.tree, "tpr", "fpr")

rf.default.prob = predict(rf.loans, newdata=test, type="prob")
pred.rf = prediction(rf.default.prob[,2], test$default)
perf.rf = performance(pred.rf, measure = "tpr", x.measure = "fpr") 

plot(perf.lr, col="red"); abline(a=0, b=1)
plot(fit.perf.tree, add=TRUE, col="blue")
plot(perf.rf, add=TRUE, col="green")
legend("bottomright", c("LogRegr", "Tree", "Random Forest"), pch=15, col=c("red", "blue", "green"), bty="n")


#   Read the loans data set saved in comma separated value format
loans.full = read.csv("loans-kpi.csv")
##   Descriptive graphics

barplot(table(loans.full$default),
        main="Paid versus not paid loans",
        xlab="Status",
        border="black", 
        col=c("green","red"))

barplot(table(loans.full$default, loans.full$credit.policy),
        main="Default Distribution by Credit Policy",
        xlab="Cust meets underwriting criteria",
        col=c("green","red"))

######################################################
### Build a preliminary tree on the whole data set ###
######################################################
if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}

(fulltree <- rpart(default ~ ., data=loans.full))
#   Note that the parantheses around the command output the resulting object
summary(fulltree)
prp(fulltree)

####################################
### Down-sample the non-defaulters
####################################
set.seed(77777)

Nfull <- nrow(loans.full)

#   Make two boolean indicator vectors called pos and neg of defaulters and non-defaulters in the data set
pos <- (loans.full$default == 1)
neg <- (loans.full$default == 0)
#   Store their numbers; Remember the brackets will print them out
(np <- sum(pos))
(nn <- sum(neg))

#   Save the indices of all the positive examples
idxpos=which(pos)  
#   Save the indices of all the negative examples
allneg=which(neg)
#   Pick out as many negative examples as np, number of positive examples
idxneg=allneg[1:np]  

#   Concatenate equal number of pos and neg examples to create balanced dataset index
l = c(idxpos,idxneg)
#   Check the length of l - it must be twice the size of the defaulters or 2 X 1533
length(l)

#   Take the data set to only have the subset we collected in the list l
loans = loans.full[l,]
#   Reset the number of observations in the dataset
N = nrow(loans)

##   Repeat the histograms of the defaulters and non-defaulters after downsampling
barplot(table(loans$default),
        main="Paid versus not paid loans",
        xlab="Status",
        border="black", 
        col=c("green","red"))
#... and the distribution across categorical variables such as credit.policy
barplot(table(loans$default, loans$credit.policy),
        main="Default Distribution by Credit Policy",
        xlab="Cust meets underwriting criteria",
        col=c("green","red"))


####################################################
## Prepare training and testing sets for modeling ##
####################################################
# Prepare new values in 'randvalue' using a random uniform number generator
randvalue <- runif(N)
trainsample <- randvalue < .7
testsample <- (randvalue >= .7)
#   Use the subsets to differentiate the training and testing data sets
train <- loans[trainsample,]
test <- loans[testsample,]

train$default <- as.factor(train$default)
test$default <- as.factor(test$default)

####################################
## Predictive Modeling with Trees ##
####################################
#   Build default full tree model with all the variables, view summary and plot the tree
ftree <- rpart(default ~ ., data=train)
par(mfrow=c(1,1))         
plot(ftree); text(ftree)  
prp(ftree)                
prp(ftree,extra=101)    

if (!require(plotmo)) {install.packages("plotmo"); library(plotmo)}  
plotmo(ftree)   


#######################################################
##             Predicting with the tree model        ##
#######################################################

default.prediction.full <- predict(ftree,test,type="prob")
pred.ftree50 <- (default.prediction.full[,2] > 0.5)
table(test$default,pred.ftree50)

#######################################################
##        Calculate ROC and AUC 
#######################################################
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
fit.pr.tree <- predict(ftree,test,type="prob")[,2]
#   Recall that a prediction of type vector returns a matrix which has the probabilities in column 2
fit.pred.tree <- prediction(fit.pr.tree,test$default)

fit.perf.tree <- performance(fit.pred.tree,"tpr","fpr")
plot(fit.perf.tree,lwd=2,col="blue",
     main="ROC:  Classification Trees on Loan Default Dataset")
abline(a=0,b=1)
#   Commands for calculating AUROC using ROCR
auc.tmp <- performance(fit.pred.tree,"auc")
(auc.tree <- as.numeric(auc.tmp@y.values))


(ftree2 <- rpart(default ~ .-int.rate -credit.policy, data=train))
summary(ftree2)
prp(ftree2,extra=101)

rtree <- ftree2

#########################
## Logistic Regression ##
#########################

####################################################
#   Using Stepwise Regression as a stub instead of your model
null = glm(default~1,data=loans,family="binomial")
#   Second estimate a complete model (with all variables that you are interested in)
full = glm(default~.- int.rate -credit.policy,data=loans,family="binomial") 
#   Finally estimate the step wise regression starting with the null model
swlrmdl = step(null, scope=formula(full),steps=15,dir="forward")  
summary(swlrmdl)

#   Use the stepwise model for counterfactuals
lrfit = swlrmdl
summary(lrfit)
plotmo(lrfit)             
#   Index of Significant predictors
id = which(summary(lrfit)$coeff[,4] < 0.05)
#   Significant predictors
coeff.sig = summary(lrfit)$coeff[,1][id]
names(coeff.sig)

#   Estimate the AUC (Area Under Curve)
#   Predict the probability of not fully paying back using lrmodel and test set
lr.default.prob = predict(lrfit, newdata=test, type="response")
pred.lr = prediction(lr.default.prob, test$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 
plot(perf.lr, col=rainbow(10))
#   Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr,"auc")
(auc.lr = as.numeric(auc.tmp@y.values))

###############################################################################
### Compare models using ROC plot
###############################################################################

#   Plot both ROC curves together
plot(perf.lr,col="red"); abline(a=0,b=1)
plot(fit.perf.tree,add=TRUE,col="blue")
legend("bottomright",c("LogRegr","Tree"),pch=15,col=c("red","blue"),bty="n")
###############################################################################################

#######################################################
## Counterfactual Analysis on the WHOLE data set
########################################################
full.loans = read.csv("loans-kpi.csv")
tree.default.prob = predict(rtree, newdata=full.loans, type="prob")
#   Create the confusion matrix of the full set for tree model
(Conf.tree.tree = table(full.loans$default, tree.default.prob[,2] > 0.5))

#   False negative rate of the tree model on the full set
(Conf.tree.tree[2,1]/(Conf.tree.tree[1,1]+Conf.tree.tree[2,1]))
#   Baseline rate of default
(sum(full.loans$default)/nrow(full.loans))
#   Output the subset of loans from the full set filtered by the rpart model for further ROI calc
write.csv(full.loans[tree.default.prob[,2]<0.5,],"loans_imputed_tree_05.csv")

full.lr.default.prob = predict(lrfit, newdata=full.loans, type="response")
#   Create the confusion matrix of the full set
Conf.lr.full = table(full.loans$default, full.lr.default.prob > 0.5)

Conf.lr.full
#   False negative rate of the lr model on the full set
(Conf.lr.full[2,1]/(Conf.lr.full[1,1]+Conf.lr.full[2,1]))
#   Baseline rate of default
(sum(full.loans$default)/nrow(full.loans))
#   Output the subset of loans from the full set filtered by the lr model for further ROI calc
write.csv(full.loans[full.lr.default.prob<0.5,],"loans_imputed_lr_05.csv")

write.csv(cbind(full.loans,tree.default.prob[,2],full.lr.default.prob),"loans_imputed_full_2models.csv")
###################################################################################################


#   Display the structure of the object read in, and summarize its fields
str(loans.full)
summary(loans.full)
#   The fraction of defaulters in the data set can be computed by taking the mean
mean(loans.full$default)

####################################
### Prepare the dataset for analysis
####################################
#   Data Preprocessing

set.seed(9872398)

loans <- loans.full
#   Number of observations in the sampled dataset
N = nrow(loans)

randvalue = runif(N)
trainsample = randvalue < .7
testsample = (randvalue >= .7)

####################################################
## Prepare training and testing sets for modeling ##
####################################################

train = loans[trainsample,]
test = loans[testsample,]

train$default = as.factor(train$default)
test$default = as.factor(test$default)

####################################################
#   Using Stepwise Regression as a stub instead of your model

null = glm(default ~ 1, data=train, family="binomial")
full = glm(default ~ . -credit.policy -int.rate, data=train, family="binomial")  
fwd = step(null, scope=formula(full),steps=15,dir="forward")
#   Summary of Trained Model Parameters
lrfit = fwd

summary(lrfit)
#   Index of Significant predictors
id = which(summary(lrfit)$coeff[,4] < 0.05)
#   Significant predictors
coeff.sig = summary(lrfit)$coeff[,1][id]
names(coeff.sig)

#   Predict the probability of not fully paying back using lrmodel and test set
lr.default.prob = predict(lrfit, newdata=test, type="response")
#   Add the predicted.risk variable to the test set
test$lr.default.prob = lr.default.prob
#   Create the confusion matrix of the test set
Conf.lr = table(test$default, lr.default.prob > 0.15)
Conf.lr
#   Accuracy of the logistic regression model
(Conf.lr[1,1]+Conf.lr[2,2])/sum(Conf.lr)
#   False negative rate of the lr model
(Conf.lr[2,1]/(Conf.lr[1,1]+Conf.lr[2,1]))

#   Estimate the AUC (Area Under Curve)
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
lr.default.prob = predict(lrfit, newdata=test, type="response")
pred.lr = prediction(lr.default.prob, test$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 
plot(perf.lr, col=rainbow(10))
#.  Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr,"auc")
(auc.lr = as.numeric(auc.tmp@y.values))

###############################################################################
###  Random Forest
###############################################################################

## Random Forest Model
if (!require(randomForest)) {install.packages("randomForest"); library(randomForest)} 
rf.loans <- randomForest(default ~ log.annual.inc + dti + fico + days.with.cr.line + revol.bal + revol.util + inq.last.6mths + delinq.2yrs + pub.rec + principal, data=train)

importance(rf.loans)
varImpPlot(rf.loans)

#   Predict the probability of not fully paying back using rfmodel and test set
rf.default.prob = predict(rf.loans, newdata=test, type="prob")
#   Add the predicted.risk variable to the test set
test$rf.default.prob = rf.default.prob[,2]
#   Create the confusion matrix of the test set
Conf.rf = table(test$default, rf.default.prob[,2] > 0.15)
Conf.rf
#   Accuracy of the random forest model
(Conf.rf[1,1]+Conf.rf[2,2])/sum(Conf.rf)
#   False negative rate of the rf model
(Conf.rf[2,1]/(Conf.rf[1,1]+Conf.rf[2,1]))

#   Estimate the AUC (Area Under Curve)
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
pred.rf = prediction(rf.default.prob[,2], test$default)
perf.rf = performance(pred.rf, measure = "tpr", x.measure = "fpr") 
plot(perf.rf, col=rainbow(10))
#.  Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.rf,"auc")
(auc.rf = as.numeric(auc.tmp@y.values))


#   plot both ROC curves together
plot(perf.lr,col="red"); abline(a=0,b=1)
plot(perf.rf,add=TRUE,col="green")
legend("bottomright",c("LogRegr","Random Forest"),pch=15,col=c("red","green"),bty="n")

################ Output predictions from lr models for further analysis and optimization ######
write.csv(test,"loans_test.csv")



