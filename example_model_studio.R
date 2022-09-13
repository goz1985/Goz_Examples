# Trying this on my dataset for Kariki_Farm.
# Want to leanr how to use pipes, model XGBoost then use model studio

library(RoughSets)
library(RoughSetKnowledgeReduction)
library(googlesheets4)
library(arules)
library(dplyr)
library(RColorBrewer)
library(arulesViz)

k_farm <- read_sheet("https://docs.google.com/spreadsheets/d/1y29ch-sv9UXSZUX9mRxqx6NleN6-XO3ifXDqkDzeOiE/edit#gid=0") %>%
  (n<-nrow(k_farm))%>%
  c(as.character(k_farm$Date[1]), as.character(k_farm$Date[n])) %>%
  head(k_farm$Rain) %>%
  k_farm$Rain <- factor(k_farm$Rain) %>%
  k_farm$Date <- as.Date(k_farm$Date, '%m/%d/%Y') %>%
  str(k_farm) %>%
  view(k_farm) %>%
  (cols_withNa <- apply(k_farm, 2, function(x) sum(is.na(x)))) %>%
  kariki_farm2 <- k_farm[complete.cases(k_farm),]
kariki_farm2
kariki_farm2$Date<-NULL
kariki_farm2$Windspeed_low <- NULL
kariki_farm2$Rain<-NULL
str(kariki_farm2)
kariki_farm2$Rain <- as.factor(kariki_farm2$Rain)

## Xgboost
karikiboost <- boost_tree(learn_rate = 0.3) %>%
  set_mode("regression")%>%
  set_engine("xgboost")%>%
  fit(Precipitation_amount~., data = kariki_farm2) 

karikiboost
## For model interpretability]

explainer_kariki <- DALEX::explain(
  model = karikiboost,
  data = kariki_farm2,
  y = kariki_farm2$Precipitation_amount,
  label = "XGBOOST"
)

# Running Model studio
modelStudio::modelStudio(explainer_kariki)
### Was able to use the model studio and xgboost to create a model using XGBoost, then made interpretable via 
