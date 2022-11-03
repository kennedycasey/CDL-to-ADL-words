library(childesr)
library(tidyverse)
library(data.table)
library(ggpubr)
library(grid)
library(lme4)
library(ggeffects)
library(broom.mixed)

# load corpus data
childes_utterances = data.table(get_utterances(collection = "Eng-NA"))
metadata <- read_csv("item-metadata.csv")

# run with LDP data? (set ldp.access to TRUE if you have "ldp-input.csv")
ldp.access <- TRUE

if (ldp.access) {
  ldp_utterances = data.table(read_csv("corpus/data/ldp-input.csv"))
}

# set colors for plots
speaker.colors <- c("child" = "#235789", 
                    "caregiver" = "#0288d1")

# create list of items and pairs
items <- metadata %>%
  filter(cogsci == "y") %>%
  pull(survey_word) %>%
  unique()

pairs <- metadata %>%
  filter(cogsci == "y") %>%
  pull(pair) %>%
  unique()

# create empty lists to be populated with 4 types of information
model.outputs.list <- list()
shift.trends.list <- list()
switch.points.list <- list()
fig.input.list <- list()

# for each speaker type, find all utterances with 1+ target items
for (i in c("child", "caregiver")) {
      childes.utts <- childes_utterances %>%
        filter(speaker_role %in% c("Target_Child", "Mother", "Father")) %>%
        mutate(speaker = ifelse(speaker_role == "Target_Child",
                                "child", "caregiver"),
               gloss = paste0(' ', tolower(gloss), ' '), 
               age = round(target_child_age, digits = 0)) %>%
        filter(age < 84 & speaker == i) %>%
        select(gloss, age)
      
      if (ldp.access) {
        ldp.utts <- ldp_utterances %>%
          mutate(speaker = ifelse(speaker == "target_child",
                                  "child", "caregiver")) %>%
          filter(speaker == i) %>%
          select(gloss, age)
        
        utts <- bind_rows(childes.utts, ldp.utts)
      } else {
        utts <- childes.utts
      }
      
      # for each item, store count of tokens detected in each utterance
      for (j in items) {
        search <- metadata %>%
          filter(survey_word == j) %>%
          pull(search_dictionary) %>%
          unique()

        utts[str_detect(gloss, search),
                        paste0(j) := str_count(gloss, search)]
      }

      # for each pair, store counts for utterances with CDL vs. ADL variants
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
               speaker = paste0(i))
      
      fig.input <- model.data %>%
        mutate(variant = variant_numeric, 
               speaker = paste0(i))
      
      fig.input.list[[i]] <- fig.input
      
          model <- glmer(variant ~ scale(age) + (1 + scale(age) | pair),
                         data = model.input,
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))
          
          # store model output summary
          model.summary <- tidy(model) %>%
            mutate(term = str_remove_all(term, "scale")) %>%
            filter(effect == "fixed" & term == "(age)")
          
          # store model-predicted shift trajectory
          trend <- ggpredict(model, c("age [all]"), type = "random") %>%
            mutate(model = i)
          
          # compute the age at which ADL variants are produced >50% of the time 
          xintercept <- trend %>%
            filter(predicted >= 0.5) %>%
            slice_head() %>%
            pull(x)
          
          model.outputs.list[[i]] <- model.summary
          shift.trends.list[[i]] <- trend
          switch.points.list[[i]] <- xintercept
          
          # plot overall shift trajectory (by speaker type)
          ggplot() + 
            geom_smooth(data = model.input,
                        aes(x = age, y = variant, group = pair),
                        method = "glm", method.args = list(family = "binomial"),
                        color = "#F5F5F5", se = FALSE) +
            geom_ribbon(data = trend,
                        aes(x = x, ymin = conf.low, ymax = conf.high),
                        fill = speaker.colors[i], alpha = 0.25) +
            geom_line(data = trend,
                      aes(x = x, y = predicted), color = speaker.colors[i], size = 2) +
            scale_x_continuous(limits = c(0, 84), breaks = seq(0, 84, by = 12)) +
            labs(x = "Age (months)", y = "Probability of producing ADL variant") +
            geom_hline(yintercept = 0.5, linetype = "dotted", size = 1) +
            theme_test(base_size = 15) +
            theme(axis.title = element_text(face = "bold")) +
            coord_cartesian(ylim = c(0, 1))
          ggsave(paste0("corpus/figs/", i, ".png"), dpi = 300, height = 5, width = 6)
}

# plot item-pair shift trajectories for child and caregiver speakers
fig.input.combined <- do.call(rbind, fig.input.list)
          ggplot(fig.input.combined, 
                 aes(x = age, y = variant, color = speaker, fill = speaker)) +
            facet_wrap(.~pair, ncol = 5) +
            geom_hline(yintercept = 0.5, linetype = "dotted", size = 1) +
            geom_smooth(method = "glm", method.args = list(family = "binomial")) +
            scale_color_manual(values = speaker.colors) +
            scale_fill_manual(values = speaker.colors) +
            scale_x_continuous(limits = c(12, 84), breaks = seq(12, 84, by = 12)) +
            coord_cartesian(ylim = c(0, 1)) +
            labs(x = "Age (months)", y = "Probability of producing ADL variant",
                 color = "Speaker", fill = "Speaker") +
            theme_test(base_size = 15) +
            theme(axis.title = element_text(face = "bold"), legend.title = element_text(face = "bold"))
          ggsave(paste0("corpus/figs/bypair-shift-timing.png"), height = 10, width = 15, dpi = 300)

model.outputs <- do.call(rbind, model.outputs.list) %>%
  rownames_to_column(var = "model") %>%
  write_csv("corpus/model-outputs/glms.csv")

shift.trends <- do.call(rbind, shift.trends.list) %>%
  data.frame() %>%
  write_csv("corpus/model-outputs/trend-lines.csv")

switch.points <- do.call(rbind, switch.points.list) %>%
  data.frame() %>%
  rownames_to_column(var = "model") %>%
  rename(., age = `.`) %>%
  write_csv("corpus/model-outputs/switch-points.csv")