rm(list = ls())
diabetes <- read.csv('diabetes.csv', header = T)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(diabetes), replace=TRUE, prob=c(5/6,1/6))
train  <- diabetes[sample, ]
test   <- diabetes[!sample, ]
train_s <- train[c('Pregnancies', 'Glucose', 'BloodPressure', 'Age', 'Outcome')]
test_s <- test[c('Pregnancies', 'Glucose', 'BloodPressure', 'Age', 'Outcome')]


# Logistic Regression
glm_fit <- glm(Outcome ~ ., data = train, family = binomial)
glm_probs <- predict(glm_fit, test, type = "response")
glm_pred <- rep(0, 126)
glm_pred[glm_probs > .5] = 1
accuracy <- mean(glm_pred == test$Outcome)
accuracy

glm_fit <- glm(Outcome ~ ., data = train_s, family = binomial)
glm_probs <- predict(glm_fit, test_s, type = "response")
glm_pred <- rep(0, 126)
glm_pred[glm_probs > .5] = 1
accuracy <- mean(glm_pred == test_s$Outcome)
accuracy

# LDA
library(MASS)
lda.fit <- lda(Outcome ~ ., data = train)
lda.pred <- predict(lda.fit, test)
lda.class <- lda.pred$class
accuracy <- mean(lda.class == test$Outcome)
accuracy

lda.fit <- lda(Outcome ~ ., data = train_s)
lda.pred <- predict(lda.fit, test_s)
lda.class <- lda.pred$class
accuracy <- mean(lda.class == test_s$Outcome)
accuracy

# Naive Bayes
library(naivebayes)
train$Outcome <- as.factor(train$Outcome)
test$Outcome <- as.factor(test$Outcome)
fit <- naive_bayes(Outcome ~ ., data = train)
naive.pred <- predict(fit, test)
accuracy <- mean(naive.pred == test$Outcome)
accuracy

train_s$Outcome <- as.factor(train_s$Outcome)
test_s$Outcome <- as.factor(test_s$Outcome)
fit <- naive_bayes(Outcome ~ ., data = train_s)
naive.pred <- predict(fit, test_s)
accuracy <- mean(naive.pred == test_s$Outcome)
accuracy

# KNN
library(class)
kn <- knn(train, test, cl=train$Outcome, k=5)
accuracy <- mean(kn == test$Outcome)
accuracy

kn <- knn(train_s, test_s, cl=train_s$Outcome, k=5)
accuracy <- mean(kn == test_s$Outcome)
accuracy
