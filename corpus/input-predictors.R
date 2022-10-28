library(tidyverse)
library(lme4)
library(broom.mixed)
library(ggeffects)

input_predictors <- c("lex.rarity", 
                      "lex.complexity",
                      "n.verbs", 
                      "n.words")

speaker.colors <- c("child (ADL)" = "#235789", 
                    "caregiver (ADL)" = "#0288d1", 
                    "child (CDL)" = "#C1292E", 
                    "caregiver (CDL)" = "#F8766D")

speaker.colors2 <- c("child" = "#235789", 
                    "caregiver" = "#0288d1")

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
  mutate(predictor = factor(predictor, levels = c("lex.complexity", "lex.rarity", 
                                                  "n.words", "n.verbs"), 
                          labels = c("Lexical\ncomplexity", "Lexical\nrarity", 
                                     "Utterance\nlength", "Syntactic\ncomplexity")), 
         type = factor(type, levels = c("child (CDL)", "child (ADL)",
                                                 "caregiver (CDL)", "caregiver (ADL)")), 
         speaker_type = factor(speaker_type, levels = c("child", "caregiver")))

ggplot(summary.input, aes(x = speaker_type, y = mean, fill = type, color = type)) + 
  facet_wrap(.~predictor, ncol = 2) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(x = speaker_type, ymin = mean - 1.96*se, ymax = mean + 1.96*se, group = type), 
                color = "black", width = 0.15, size = 0.25, position = position_dodge(width = 0.9)) +
  scale_color_manual(values = speaker.colors) +
  scale_fill_manual(values = speaker.colors) +
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  labs(y = "Mean Feature Value (scaled)") +
  theme_test(base_size = 15) + 
  theme(legend.position = "bottom",
        legend.key.size = unit(4, "mm"),
        axis.title.x = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 10))
ggsave("corpus/figs/predictors.png", width = 6, height = 5, dpi = 600)