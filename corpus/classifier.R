library(tidyverse)
library(caret)
library(xgboost)
library(tidymodels)
library(pROC)
library(xgboostExplainer)

# set colors for plots
speaker.colors <- c("child" = "#235789", 
                    "caregiver" = "#0288d1")

# read in CHILDES linguistic input info
# see https:github.com/kennedycasey/RegisterShift for scripts used to generate csvs
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

# for both speaker types, run xgboost model with 4 linguistic features + age
for (i in c("child", "caregiver")) {
  data <- data.frame(eval(as.symbol(i)))
  
  # 90/10 split for training and test sets
  split <- initial_split(data, strata = variant, prop = 0.9)
  train <- training(split)
  test <- testing(split)
  
  # create cross-validation folds
  cv <- createFolds(train[,"variant"], k = 5)
  
  set.seed(123)
  variant <- which(names(train) == "variant")
  xgb.train.data <- xgb.DMatrix(data.matrix(train[,-variant]), label = train[,variant], missing = NA)
  param <- list(objective = "binary:logistic", base_score = 0.5)
  xgboost.cv = xgb.cv(param = param, data = xgb.train.data, folds = cv, 
                      nrounds = 1000, early_stopping_rounds = 100, metrics = "auc")
  best_iteration = xgboost.cv$best_iteration
  xgb.model <- xgboost(param = param, data = xgb.train.data, nrounds = best_iteration)
  xgb.test.data = xgb.DMatrix(data.matrix(test[,-variant]), missing = NA)
  xgb.preds = predict(xgb.model, xgb.test.data)
  
  # store roc from the test set
  xgb.roc_obj <- roc(test[,variant], xgb.preds)
  assign(paste0("xgb.roc.", i), xgb.roc_obj)
  
  # store feature importance scores
  col_names = attr(xgb.train.data, ".Dimnames")[[2]]
  xgb.imp <- xgb.importance(col_names, xgb.model)
  assign(paste0("xgb.imp.", i), xgb.imp)
}

# create combined roc plot for children and their caregivers
data.frame(sensitivity = xgb.roc.caregiver$sensitivities, 
           specificity = xgb.roc.caregiver$specificities, 
           speaker = "caregiver") %>%
  bind_rows(data.frame(sensitivity = xgb.roc.child$sensitivities, 
                       specificity = xgb.roc.child$specificities, 
                       speaker = "child")) %>%
  ggplot(aes(x = specificity, y = sensitivity, color = speaker)) + 
  scale_x_reverse() +
  geom_point(size = 1.25) + 
  geom_abline(intercept = 1, linetype = "dotted") + 
  annotate(geom = "label", x = 0.2, y = 0.45, 
           label = paste0("AUC: ", round(auc(xgb.roc.child), 2)), 
           color = speaker.colors["child"], 
           size = 8) +
  annotate(geom = "label", x = 0.2, y = 0.35, 
           label = paste0("AUC: ", round(auc(xgb.roc.caregiver), 2)), 
           color = speaker.colors["caregiver"], 
           size = 8) +
  scale_color_manual(values = speaker.colors) +
  labs(x = "Specificity", y = "Sensitivity", slope = 1) + 
  theme_test(base_size = 25) +
  theme(legend.position = "none")
ggsave("corpus/figs/roc.png", width = 6, height = 6, dpi = 600)

# create combined feature importance  plot for children and their caregivers
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
  theme_test(base_size = 27) +
  theme(legend.position = c(0.85, 0.12),
        legend.key.size = unit(5, 'mm'),
        legend.title = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 35))
ggsave("corpus/figs/feature-importance.png", width = 10, height = 8, dpi = 600)

# explain xgboost model
explainer = buildExplainer(xgb.model, xgb.train.data, type = "binary",
                           base_score = 0.5, trees_idx = NULL)
pred.breakdown = explainPredictions(xgb.model, explainer, xgb.test.data)
showWaterfall(xgb.model, explainer, xgb.test.data, data.matrix(test[,-variant]),
              300, type = "binary")

# plot example tree - set "trees" to the tree number of interest
xgb.plot.tree(model = xgb.model, trees = 1, plot_width = 1000,
              plot_height = 500)