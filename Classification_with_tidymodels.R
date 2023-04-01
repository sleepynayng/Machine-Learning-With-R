library(tidymodels)
library(tidyverse)
library(parsnip)
library(MASS)
library(ISLR)
library(discrim)

# Train/Test
data <- initial_split(data=Smarket, prop=0.7)
train <- data %>% training()
test <- data %>% testing()

### Logistic Regression
model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

model.fit <- model %>% 
  fit(Direction~.-Year-Today, data=train)

print(model.fit)
print(tidy(model.fit))

class_pred <- predict(model.fit, test, type = "class")
prob_pred <- predict(model.fit, test, type = "prob")
res <- test %>% 
  dplyr::select("Direction") %>% 
  bind_cols(class_pred, prob_pred)

# Confustion Matrix
conf_mat(res, truth = "Direction", estimate = ".pred_class")
sens(res, truth = "Direction", estimate = ".pred_class")
spec(res, truth = "Direction", estimate = ".pred_class")
accuracy(res, truth = "Direction", estimate = ".pred_class")
recall(res, truth = "Direction", estimate = ".pred_class")
precision(res, truth = "Direction", estimate = ".pred_class")
res_metrics <- metric_set(accuracy, sens, spec, recall, precision, f_meas)
res_metrics(res, truth = "Direction", estimate = ".pred_class")

# ROC-Curve
res %>% roc_curve(truth = "Direction", ".pred_Down") %>% 
  autoplot()
roc_auc(res, truth = "Direction", ".pred_Down")


### LDA
lda_model <- discrim_linear() %>% 
  set_engine("MASS") %>% 
  set_mode("classification")

lda.fit <- lda_model %>% 
  fit(Direction~.-Year-Today, data=train)

print(lda.fit)

class_pred <- predict(lda.fit, test, type="class")
prob_pred <- predict(lda.fit, test, type="prob")
res <- test %>% 
  dplyr::select(Direction) %>% 
  bind_cols(class_pred, prob_pred)

# Confustion Matrix
conf_mat(res, truth = "Direction", estimate = ".pred_class")
sens(res, truth = "Direction", estimate = ".pred_class")
spec(res, truth = "Direction", estimate = ".pred_class")
accuracy(res, truth = "Direction", estimate = ".pred_class")
recall(res, truth = "Direction", estimate = ".pred_class")
precision(res, truth = "Direction", estimate = ".pred_class")
res_metrics <- metric_set(accuracy, sens, spec, recall, precision, f_meas)
res_metrics(res, truth = "Direction", estimate = ".pred_class")

# ROC-Curve
res %>% roc_curve(truth = "Direction", ".pred_Up") %>% 
  autoplot()
roc_auc(res, truth = "Direction", ".pred_Up")


### QDA
qda_model <- discrim_quad() %>% 
  set_engine("MASS") %>% 
  set_mode("classification")

qda.fit <- qda_model %>% 
  fit(Direction~.-Year-Today, data=train)

print(qda.fit)

class_pred <- predict(qda.fit, test, type="class")
prob_pred <- predict(qda.fit, test, type="prob")
res <- test %>% 
  dplyr::select(Direction) %>% 
  bind_cols(class_pred, prob_pred)

# Confustion Matrix
conf_mat(res, truth = "Direction", estimate = ".pred_class")
sens(res, truth = "Direction", estimate = ".pred_class")
spec(res, truth = "Direction", estimate = ".pred_class")
accuracy(res, truth = "Direction", estimate = ".pred_class")
recall(res, truth = "Direction", estimate = ".pred_class")
precision(res, truth = "Direction", estimate = ".pred_class")
res_metrics <- metric_set(accuracy, sens, spec, recall, precision, f_meas)
res_metrics(res, truth = "Direction", estimate = ".pred_class")

# ROC-Curve
res %>% roc_curve(truth = "Direction", ".pred_Up") %>% 
  autoplot()
roc_auc(res, truth = "Direction", ".pred_Up")
