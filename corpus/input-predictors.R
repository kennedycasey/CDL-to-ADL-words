library(tidyverse)
library(lme4)
library(broom.mixed)
library(ggeffects)
library(ggpubr)
library(grid)

# list linguistic input features
input_predictors <- c("lex.rarity", 
                      "lex.complexity",
                      "n.verbs", 
                      "n.words")

# set colors for plots
speaker.colors <- c("child" = "#235789", 
                    "caregiver" = "#0288d1")

# read in CHILDES linguistic input info
# see https:github.com/kennedycasey/RegisterShift for scripts used to generate csvs
input <- read_csv("corpus/data/combined-other.csv") %>%
  filter(speaker_role %in% c("Mother", "Father")) %>%
  bind_rows(read_csv("corpus/data/combined-child.csv")) %>%
  rename(lex.rarity = rarity, 
         n.words = num_tokens, 
         n.verbs = verbs, 
         lex.complexity = complexity_ratings) %>%
  mutate(across(input_predictors, ~ scale(.))) %>%
  pivot_longer(input_predictors, names_to = "predictor", values_to = "value") %>%
  mutate(speaker_type = str_replace(speaker_type, "other", "caregiver"),
         variant = ifelse(form == "CDS", "CDL", "ADL"),
         type = paste0(speaker_type, " (", variant, ")"))
  
summary.input <- input %>%
  group_by(type, speaker_type, predictor) %>%
  summarize(mean = mean(value, na.rm = TRUE), 
            se = sd(value, na.rm = TRUE)/sqrt(n())) %>%
  mutate(predictor = factor(predictor, levels = c("lex.rarity", "lex.complexity", 
                                                  "n.words", "n.verbs"), 
                          labels = c("Lexical\nrarity", "Lexical\ncomplexity",
                                     "Utterance\nlength", "Syntactic\ncomplexity")), 
         speaker = factor(speaker_type, levels = c("child", "caregiver")), 
         variant = ifelse(str_detect(type, "CDL"), "CDL", "ADL"), 
         variant = factor(variant, levels = c("CDL", "ADL")))

# plot scaled feature values
ggplot(summary.input, aes(x = speaker, y = mean, fill = speaker, 
                          color = speaker, pattern = variant)) + 
  facet_wrap(.~predictor, ncol = 2) +
  geom_bar_pattern(stat = "identity", position = "dodge", 
                   pattern_color = "white", 
                   pattern_fill = "white") +
  geom_errorbar(aes(x = speaker, ymin = mean - 1.96*se, ymax = mean + 1.96*se, group = variant), 
                color = "black", width = 0.15, size = 0.5, position = position_dodge(width = 0.9)) +
  scale_pattern_manual(values = c(ADL = "none", CDL = "stripe")) +
  scale_color_manual(values = speaker.colors) +
  scale_fill_manual(values = speaker.colors) +
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  guides(color = "none", fill = "none") +
  labs(y = "Mean Feature Value (scaled)") +
  theme_test(base_size = 25) + 
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 20))
ggsave("corpus/figs/predictors.png", width = 10, height = 8, dpi = 600)

# for each speaker type, model individual effects of linguistic features on variant production
for (i in c("child", "caregiver")) {
  model.input <- input %>%
    filter(speaker_type == i)
  
  for (j in input_predictors) {
  model <- glmer(form_numeric ~ eval(as.symbol(j)) * scale(age) + 
                   (1|pair) + 
                   (1|speaker_id), 
                 data = filter(model.input, !is.na(eval(as.symbol(j)))), 
                 family = binomial, 
                 control = glmerControl(optimizer = "bobyqa"))
  summary(model)
  
  # store model outputs in individual csvs
  tidy(model) %>%
    filter(effect == "fixed") %>%
    mutate(term = str_remove_all(term, "scale|eval|as.symbol|[()]"), 
           term = str_replace_all(term, "j", j)) %>%
    write_csv(paste0("corpus/model-outputs/input-predictors/", 
                     str_replace(j, "_", "-"), "-", i, ".csv"))
 }
}

# read in model outputs for both speaker types
path <- "corpus/model-outputs/input-predictors/"
filenames <- list.files(path, "*.csv")
files <- lapply(paste0(path, filenames[!str_detect(filenames, "-child")]), read_csv)
input.models <- bind_rows(files, .id = "source") %>%
  rename(Predictor = term, 
         Estimate = estimate, 
         SE = std.error) %>%
  group_by(source) %>%
  mutate(order = row_number(), 
         type = case_when(
           order == 1 ~ "intercept", 
           order == 2 ~ "main_effect", 
           order == 3 ~ "age", 
           order == 4 ~ "interaction"), 
         Sig = ifelse(p.value < .05, "sig", "not_sig"), 
         Predictor = factor(str_remove(Predictor, ":age"), 
                            levels = c("n.verbs", "n.words","lex.complexity", "lex.rarity"), 
                            labels = c("Syntactic\ncomplexity", "Utterance\nlength",
                                       "Lexical\ncomplexity", "Lexical\nrarity")),
         speaker = "caregiver")

files.child <- lapply(paste0(path, filenames[!str_detect(filenames, "-caregiver")]), read_csv)
input.models.child <- bind_rows(files.child, .id = "source") %>%
  rename(Predictor = term, 
         Estimate = estimate, 
         SE = std.error) %>%
  group_by(source) %>%
  mutate(order = row_number(), 
         type = case_when(
           order == 1 ~ "intercept", 
           order == 2 ~ "main_effect", 
           order == 3 ~ "age", 
           order == 4 ~ "interaction"), 
         Sig = ifelse(p.value < .05, "sig", "not_sig"), 
         Predictor = factor(str_remove(Predictor, ":age"), 
                            levels = c("n.verbs", "n.words","lex.complexity", "lex.rarity"), 
                            labels = c("Syntactic\ncomplexity", "Utterance\nlength",
                                       "Lexical\ncomplexity", "Lexical\nrarity")),
         speaker = "child")

input.models.combined <- bind_rows(input.models.child, input.models) %>%
  filter(!is.na(Predictor))

# plot main effects of each linguistic feature and interactions with age
main.effects <- ggplot(filter(input.models.combined, type == "main_effect"), 
                       aes(x = Estimate, y = Predictor, color = speaker)) +
  geom_vline(xintercept = 0, size = 0.75, linetype = "dotted", 
             color = "gray", alpha = 0.5) +
  geom_pointrange(aes(x = Estimate, xmin = Estimate - SE, xmax = Estimate + SE, color = speaker), 
                width = 0, size = 1, position = position_dodge(width = 0.25)) + 
  scale_color_manual(values = speaker.colors) +
  scale_x_continuous(limits = c(-0.6, 0.6), breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)) +
  labs(title = "Main Effect", x = "Coefficient Estimate", y = "Linguistic Feature") +
  theme_test(base_size = 25) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        axis.title = element_blank(), 
        axis.text.y = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "transparent", color = NA),  
        plot.background = element_rect(fill = "transparent", color = NA))

interactions <- ggplot(filter(input.models.combined, type == "interaction"), 
                       aes(x = Estimate, y = Predictor, color = speaker)) +
  geom_vline(xintercept = 0, size = 0.75, linetype = "dotted", 
             color = "gray", alpha = 0.5) +
  geom_pointrange(aes(x = Estimate, xmin = Estimate - SE, xmax = Estimate + SE, color = speaker), 
                  width = 0, size = 1, position = position_dodge(width = 0.25)) + 
  scale_color_manual(values = speaker.colors) +
  scale_x_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, by = 0.1)) +
  labs(title = "Interaction with Age", x = "Coefficient Estimate", y = "Linguistic Feature") +
  theme_test(base_size = 25) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.background = element_rect(fill = "transparent", color = NA),  
        plot.background = element_rect(fill = "transparent", color = NA))

model.fig <- ggarrange(main.effects, interactions, widths = c(4.3, 3.2))

annotate_figure(model.fig, bottom = textGrob("Coefficient Estimate", gp = gpar(fontsize = 35, face = "bold")),
                left = textGrob("Linguistic Feature", rot = 90, vjust = 1, gp = gpar(fontsize = 35, face = "bold")))
ggsave("corpus/figs/predictor-model.png", width = 10, height = 8, dpi = 600)