library(childesr)
library(tidyverse)
library(data.table)
library(ggpubr)
library(grid)
library(lme4)
library(ggeffects)
library(broom.mixed)

childes_utterances = data.table(get_utterances(collection = "Eng-NA"))
ldp_utterances = data.table(read_csv("corpus/ldp_data_prepped.csv"))
metadata <- read_csv("item-metadata.csv")

# set colors for plots
speaker.colors <- c("child (corpus)" = "#235789", 
                    "caregiver (corpus)" = "#0288d1", 
                    "child (survey)" = "#C1292E", 
                    "caregiver (survey)" = "#F8766D")

fig.input.list <- list()

for (measure in c("corpus", "survey")) {
  for (i in c("child", "caregiver")) {
    model.type = paste0(i, " (", measure, ")")
    
    if (measure == "corpus") {
    childes.utts <- childes_utterances %>%
      filter(speaker_role %in% c("Target_Child", "Mother", "Father")) %>%
      mutate(speaker = ifelse(speaker_role == "Target_Child",
                              "child", "caregiver"),
             gloss = paste0(' ', tolower(gloss), ' '), 
             age = round(target_child_age, digits = 0)) %>%
      filter(age < 84 & speaker == i) %>%
      select(gloss, age)
    
    ldp.utts <- ldp_utterances %>%
      mutate(speaker = ifelse(speaker == "target_child",
                              "child", "caregiver")) %>%
      filter(speaker == i) %>%
      select(gloss, age)
    
    utts <- bind_rows(childes.utts, ldp.utts)
    
    items <- metadata %>%
      filter(cogsci == "y") %>%
      pull(survey_word) %>%
      unique()
    
    pairs <- metadata %>%
      filter(cogsci == "y") %>%
      pull(pair) %>%
      unique()
    
    for (j in items) {
      
      search <- metadata %>%
        filter(survey_word == j) %>%
        pull(search_dictionary) %>%
        unique()
      
      utts[str_detect(gloss, search),
           paste0(j) := str_count(gloss, search)]
      
    }
    
    model.data.list <- list() 
    for (k in pairs) {
      CDL <- paste(gsub("_.*", "", k))
      ADL <- paste(gsub(".*_", "", k))
      
      model.data <- utts %>%
        filter(!is.na(eval(as.symbol(CDL))) | !is.na(eval(as.symbol(ADL)))) %>%
        select(age, CDL, ADL) %>%
        group_by(age) %>%
        summarize(CDL = sum(eval(as.symbol(CDL)), na.rm = TRUE),
                  ADL = sum(eval(as.symbol(ADL)), na.rm = TRUE)) %>%
        pivot_longer(c(CDL, ADL), names_to = "variant", values_to = "count") %>%
        mutate(word = case_when(
          variant == "CDL" ~ paste(gsub("_.*", "", k)), 
          variant == "ADL" ~ paste(gsub(".*_", "", k)))) %>%
        distinct() %>%
        mutate(variant_numeric = case_when(
          variant == "CDL" ~ 0, 
          variant == "ADL" ~ 1), 
          pair = paste0(k)) 
      
      model.data.list[[k]] <- model.data
    }
    
    model.data.merged <- do.call(rbind, model.data.list)
    rownames(model.data.merged) <- 1:nrow(model.data.merged)
    
    model.data <- model.data.merged[rep(seq(nrow(model.data.merged)), model.data.merged$count), 1:6]
    
    model.input <- model.data %>%
      mutate(variant = variant_numeric, 
             speaker = model.type)
    
    fig.input <- model.data %>%
      mutate(variant = variant_numeric, 
             speaker = model.type) %>%
      select(age, pair, speaker, variant)
    
    fig.input.list[[model.type]] <- fig.input

    }
    else {
      data <- read_csv("survey/anon-data/variant-probabilities.csv") %>%
        mutate(id = as.character(id))
      metadata <- read_csv("item-metadata.csv")
      
      # transform probabilities for binomial regression (0s and 1s for variant types)
      model.data <- data %>%
        mutate(ADL = value, CDL = 100 - value)
      
      model.data.CDL <- model.data[rep(seq(nrow(model.data)), model.data$CDL), ]
      model.data.CDL <- model.data.CDL %>%
        select(id, age, pair, speaker) %>%
        mutate(variant = 0)
      
      model.data.ADL <- model.data[rep(seq(nrow(model.data)), model.data$ADL), ]
      model.data.ADL <- model.data.ADL %>%
        select(id, age, pair, speaker) %>%
        mutate(variant = 1)
      
      model.data <- bind_rows(model.data.ADL, model.data.CDL) %>%
        # calculate age in months
        mutate(age = age/30.437, 
               speaker = case_when(
                 speaker == "child" ~ "child (survey)", 
                 speaker == "caregiver" ~ "caregiver (survey)")) %>%
        left_join(select(metadata, pair, cogsci), by = "pair")
      
      fig.input <- model.data %>%
        filter(cogsci == "y" & speaker == model.type) %>%
        select(age, pair, speaker, variant)
      
      fig.input.list[[model.type]] <- fig.input
    }
  }
}
  
fig.input.combined <- do.call(rbind, fig.input.list)

fig.input.combined %>%
  mutate(pair = str_replace(pair, "_", "/"), 
         pair = factor(pair, levels = c("birdie/bird", "blankie/blanket", 
                                        "froggy/frog", "piggy/pig",
                                        "duckie/duck", "horsey/horse", "kitty/cat",
                                        "nightnight/goodnight", "dolly/doll", "doggy/dog", 
                                        "mommy/mom", "tummy/stomach", "potty/bathroom", 
                                        "daddy/dad", "bunny/rabbit"))) %>%
  ggplot(aes(x = age/12, y = variant, color = speaker, fill = speaker)) +
  facet_wrap(.~pair, ncol = 5) +
  geom_hline(yintercept = 0.5, linetype = "dotted", size = 1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              size = 1.5, se = FALSE) +
  scale_color_manual(values = speaker.colors) +
  scale_fill_manual(values = speaker.colors) +
  scale_x_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Age (years)", y = "Probability of producing ADL variant",
       color = "Speaker", fill = "Speaker") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_test(base_size = 35) +
  theme(axis.title = element_text(face = "bold"), 
        legend.title = element_blank(), 
        legend.position = "bottom", 
        strip.text = element_text(size = 22), 
        axis.text = element_text(size = 25))
ggsave(paste0("combined/figs/bypair-shift-timing.png"), 
       height = 12, width = 18, dpi = 600, limitsize = FALSE)