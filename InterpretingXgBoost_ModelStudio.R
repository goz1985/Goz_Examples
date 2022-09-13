# This file is for examples gotten from twitter to test various packages...
library(modelStudio)
library(DALEX)
library(tidyverse)
library(tidymodels)

data <- mpg %>%
  select(hwy,manufacturer:drv,fl,class)
data

# Making predictive model
fitboost <- boost_tree(learn_rate = 0.3) %>%
  set_mode("regression")%>%
  set_engine("xgboost")%>%
  fit(hwy~., data = data)

fitboost

# Interpreting the model
explainer <- DALEX::explain(
  model = fitboost,
  data = data,
  y = data$hwy,
  label = "XGBOOST"
)

# Running Model studio
modelStudio::modelStudio(explainer)