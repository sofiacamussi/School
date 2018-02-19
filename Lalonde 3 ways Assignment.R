library(Matching)
data(lalonde)

### SFPD CURRENT DECISION
degree0 <- subset(lalonde, nodegr == 0)
degree1 <- subset(lalonde, nodegr == 1)


#boosting
install.packages(("gbm"))
library(gbm)
boost<- gbm(re78~re74+re75+age+educ+black+married+hisp+nodegr+u74+u75+treat, data = lalonde, n.trees = 5000, interaction.depth = 4)
plot(boost)
summary(boost)

### 1. SIMPLE REGRESSION MODEL WITH MOST IMPORTANT VARIABLES
lm.1 <- lm(re78 ~ re75 + age + re74 + nodegr + treat, data = degree0) ## WITH DEGREE
lm.2 <- lm(re78 ~ re75 + age + re74 + nodegr + treat, data = degree1) ##NO DEGREE
summary(lm.1) ## WITH DEGREE
summary(lm.2) # NO DEGREE


# What do your results suggest about the effect and statistical significance of the treatment on re78?

#Estimate confidence interva

ls 
#(using the standard error results, or via simulation like we did in class last unit.) 

confint(lm.1, level=0.95) ##DEGREE
confint(lm.2, level=0.95) ##NODEGREE

#Does your regression allow for different effects for different individuals 
#or does it require constant treatment effects for all? Explain.


#(2) 

### SECOND REGRESSION MODEL WITH INTERACTING VARIABLE.
lm.3 <- lm(re78 ~ treat + re74 + educ + treat + I(nodegr*treat), data = lalonde)
summary(lm.3)

confint(lm.3, level=0.95) #Getting 95% confidence interval using inbuilt function 
lm.3.sim<- sim(lm.3) 
coef(lm.3.sim)   #simulated coefficients all equally likely under this model
meanoft <- apply(coef(lm.3.sim), 2, mean)  #Standard deviation of simulated coefficients
sderoft <- apply(coef(lm.3.sim), 2, sd)
upp <- meanoft + 2*sderoft
low <- meanoft - 2*sderoft
upp
low

install.packages("sjPlot")
library(sjPlot)
sjp.lm(lm.3, show.loess.ci = T, show.values = T, show.summary = T)
sjp.int(lm.3, show.values = T, show.ci = T)

#(3)
#Create a new variable (u78), defined in a manner consistent with re74/u74 and re75/u75. 
lalonde$u78[lalonde$re78==0]<-1 #if re78 = 0, makes u78 = 1 ##NO EARNINGS IN 1978
lalonde$u78[lalonde$re78>0]<-0  #if re78 = 1, makes u78 = 0 ##YES EARNINGS IN 1978

#Run two logistic regressions where the dependent variable is u78 — one for each subgroup, as in question (1) above.
#Estimate the treatment effect for each subgroup and produce 95% confidence intervals.

glm.degree0 <- glm(u78 ~ re75 + age + re74 + nodegr + treat, data = lalonde)
glm.degree1 <- glm(u78 ~ re75 + age + re74 + nodegr + treat, data = lalonde)
summary(glm.degree0)
summary(glm.degree1)
confint(glm.degree0, level=0.95)
confint(glm.degree1, level=0.95)

#(4)
#Lastly, use random forests to enhance your analysis somehow — exactly how is up to you. Whatever you end up doing, 
#copy/paste a variable importance plot into your assignment, and explain your results and their relevance.


#creating randomforests
library(randomForest)
library(devtools)
library(tree)
set.seed(1414)
Forest <- randomForest(re78~., data=lalonde,
                       mtry=8, ntree = 5000, importance =TRUE)
plot(Forest)
cat("There are", length(which(lalonde$u78 == 1)),
    "unemployed observations in the total in 1978.")

## How does the OOB test error compare to the error in the test set?

## check the importance of the variables
varImpPlot(Forest)
importance(Forest)
lalonde.treat <- subset(lalonde, treat==1)
mean1 <- mean(lalonde.treat$re78)
Forest <- randomForest(re78~u78+re74+re75+age+educ+treat, data=lalonde, importance=TRUE, ntree=1000) 
varImpPlot(Forest)
lalonde.treat$treat <- 0 
lalonde.treat.predicted <- predict(fit.treat.rf, lalonde.treat) 
mean2 <- mean(lalonde.treat.predicted) 
ATE.treated <- mean1 - mean2 
print(ATE.treated)



#HINT: when you enter your formula in a regression or random forest function, specify the data set in the function (don’t identify the variable names using “$”)

