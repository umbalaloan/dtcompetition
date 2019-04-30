# Apply Tunning Process
# Apply Tunning Process
# Step 1: Load Transform Data
training <- read.csv("training_transformed.csv", sep = ",", header = TRUE)
test <- read.csv("test_transformed.csv", sep = ",", header = TRUE)
# Process training data
training$GAS <- as.factor(training$GAS)
training$MARKET_DOMAIN_DESCR <- as.factor(training$MARKET_DOMAIN_DESCR)
training$TIMESTAMP...Month <- as.factor(training$TIMESTAMP...Month)

# Pre-process testing data
test$GAS <- as.factor(test$GAS)
test$MARKET_DOMAIN_DESCR <- as.factor(test$MARKET_DOMAIN_DESCR)
test$TIMESTAMP...Month <- as.factor(test$TIMESTAMP...Month)

#Loading package
library(mlr)
library(dplyr)
library(bst)

# Get training data
training_data <- training %>% select(GAS, MARKET_DOMAIN_DESCR, MOD_VOLUME_CONSUMPTION, Sum.of.Sales_CR, ZIPcode, TIMESTAMP...Month, TIMESTAMP...Year)
testing_data <- test %>% select(GAS, MARKET_DOMAIN_DESCR, MOD_VOLUME_CONSUMPTION, Sum.of.Sales_CR, ZIPcode, TIMESTAMP...Month, TIMESTAMP...Year)

# Tunning model
# Create Task
task <- makeRegrTask(data = training_data, target = 'MOD_VOLUME_CONSUMPTION', id = 'customer')
# Configure learners with probability type. Using Generalized Linear Regression
glm_learner <- makeLearner('regr.glm', predict.type = 'se')
# Configure tune control search and a 5-CV stratified sampling
ctrl  <- makeTuneControlGrid()
rdesc <- makeResampleDesc("CV", iters = 5L, stratify = FALSE)

# Get parameter
getParamSet(glm_learner)
#Set Tunning Parameter:
ps_glm <- makeParamSet(
  makeDiscreteParam('gaussian.link', values=c("identity","log","inverse")),
  makeDiscreteParam('poisson.link', values = c("log", "identity", "sqrt")),
  makeDiscreteParam('Gamma.link', values = c("inverse", "identity", "log"))
)

# Configure tune wrapper with tune-tuning settings
glm_tunedLearner <- makeTuneWrapper(glm_learner, rdesc,mse, par.set= ps_glm, ctrl)
glm_tuneWrapper  <- train(glm_tunedLearner, task)
