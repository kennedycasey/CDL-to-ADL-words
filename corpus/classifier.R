library(tidyverse)
library(caret)
library(xgboost)
library(tidymodels)
library(pROC)
library(xgboostExplainer)

speaker.colors <- c("child" = "#235789", 
                    "caregiver" = "#0288d1")

caregiver <- read_csv("corpus/data/combined-other.csv") %>%
  filter(speaker_role %in% c("Mother", "Father")) %>%
  mutate(variant = ifelse(form == "CDS", 0, 1), 
         lex.rarity = rarity, 
         n.words = num_tokens, 
         n.verbs = verbs, 
         lex.complexity = complexity_ratings) %>%
  select(variant, lex.rarity, lex.complexity, n.words, n.verbs, age) %>%
  na.omit()

child <- read_csv("corpus/data/combined-child.csv") %>%
  mutate(variant = ifelse(form == "CDS", 0, 1), 
         lex.rarity = rarity, 
         n.words = num_tokens, 
         n.verbs = verbs, 
         lex.complexity = complexity_ratings) %>%
  select(variant, lex.rarity, lex.complexity, n.words, n.verbs, age) %>%
  na.omit()

for (i in c("child", "caregiver")) {
  dat <- data.frame(eval(as.symbol(i)))
  
  split <- initial_split(dat, strata = variant, prop = 0.9)
  train <- training(split)
  test <- testing(split)
  cv <- createFolds(train[,"variant"], k = 5)
  
  set.seed(123)
  variant <- which(names(train) == "variant") # class column number
  xgb.train.data <- xgb.DMatrix(data.matrix(train[,-variant]), label = train[,variant], missing = NA)
  param <- list(objective = "binary:logistic", base_score = 0.5)
  xgboost.cv = xgb.cv(param = param, data = xgb.train.data, folds = cv, 
                      nrounds = 1000, early_stopping_rounds = 100, metrics = "auc")
  best_iteration = xgboost.cv$best_iteration
  xgb.model <- xgboost(param = param, data = xgb.train.data, nrounds = best_iteration)
  xgb.test.data = xgb.DMatrix(data.matrix(test[,-variant]), missing = NA)
  xgb.preds = predict(xgb.model, xgb.test.data)
  
  # ROC on the test set
  xgb.roc_obj <- roc(test[,variant], xgb.preds)
  assign(paste0("xgb.roc.", i), xgb.roc_obj)
  
  col_names = attr(xgb.train.data, ".Dimnames")[[2]]
  xgb.imp <- xgb.importance(col_names, xgb.model)
  assign(paste0("xgb.imp.", i), xgb.imp)
}

png(file = "corpus/figs/roc.png", 
    width = 4, height = 4, units = "in", 
    res = 600)
plot(10, 10, xlim = c(1, 0), ylim = c(0, 1), 
     xlab = "Specificity", ylab = "Sensitivity")
abline(a = c(1,0), b = -1, col = "gray") 
plot(xgb.roc.child,
     col = speaker.colors["child"], 
     add = TRUE) 
plot(xgb.roc.caregiver,
     col = speaker.colors["caregiver"], 
     add = TRUE) 
text(x = 0.2, y = 0.45, 
     paste0("AUC = ", round(auc(xgb.roc.child), 2)),
     col = speaker.colors["child"]) 
text(x = 0.2, y = 0.35, 
     paste0("AUC = ", round(auc(xgb.roc.caregiver), 2)), 
     col = speaker.colors["caregiver"])
dev.off()

mutate(xgb.imp.child, speaker = "child") %>%
  bind_rows(mutate(xgb.imp.caregiver, speaker = "caregiver")) %>%
  mutate(Feature = factor(Feature, levels = c("n.verbs", "n.words", "age",
                                              "lex.complexity", "lex.rarity"), 
                          labels = c("Syntactic\ncomplexity", "Utterance\nlength", "Age", 
                                     "Lexical\ncomplexity", "Lexical\nrarity"))) %>%
  ggplot(aes(x = Gain, y = Feature, fill = speaker)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = speaker.colors) + 
  labs(x = "Feature Importance") + 
  theme_test(base_size = 15) +
  theme(legend.position = c(0.85, 0.12),
        legend.key.size = unit(5, 'mm'),
        legend.title = element_blank(),
        axis.title.y = element_blank())
ggsave("corpus/figs/feature-importance.png", width = 6, height = 5, dpi = 600)
# 
# explainer = buildExplainer(xgb.model, xgb.train.data, type = "binary",
#                            base_score = 0.5, trees_idx = NULL)
# pred.breakdown = explainPredictions(xgb.model, explainer, xgb.test.data)
# showWaterfall(xgb.model, explainer, xgb.test.data, data.matrix(test[,-variant]),
#               300, type = "binary")

xgb.plot.tree(model = xgb.model, trees = 12, plot_width = 1000,
              plot_height = 500)