library(here)
library(tidyverse)
library(lme4)
library(ggeffects)
library(broom.mixed)

# read in data
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
  mutate(age = age/30.437) %>%
  left_join(select(metadata, pair, cogsci), by = "pair")

# set colors for plots
speaker.colors <- c("child" = "#23773B", 
                    "caregiver" = "#23C889")

model.outputs.list <- list()
shift.trends.list <- list()
switch.points.list <- list()
for (i in c("child", "caregiver")) {
  for (j in c("full", "cogsci")) {
    
    if (j == "cogsci") {
      model.input <- model.data %>%
        filter(speaker == i & cogsci == "y")
      
      fig.input <- model.data %>%
        filter(cogsci == "y")
      
      fig.height <- 10
    }
    else {
      model.input <- model.data %>%
        filter(speaker == i)
      
      fig.input <- model.data
      
      fig.height <- 20
    }
    
    # pre-registered model fails to converge
    # model even with simplified random effects structure 
    # (by-child or by-pair intercepts only) also fails to converge
    # model <- glmer(variant ~ scale(age) + (1 + scale(age) | pair) + (1 + scale(age) | id),
    #                data = model.input,
    #                family = binomial,
    #                control = glmerControl(optimizer = "bobyqa"))
    
    model.type <- paste0(i, "-", j)
    
    model <- glm(variant ~ scale(age), 
                 data = model.input, 
                 family = binomial)
    
    # store model output summary
    model.summary <- tidy(model) %>%
      filter(term == "scale(age)")
    
    # store model-predicted shift trajectory
    trend <- ggpredict(model, c("age [all]")) %>%
      mutate(model = model.type)
    
    # compute the age at which ADL variants are produced >50% of the time 
    xintercept <- trend %>%
      filter(predicted >= 0.5) %>%
      slice_head() %>%
      pull(x)
    
    model.outputs.list[[model.type]] <- model.summary
    shift.trends.list[[model.type]] <- trend
    switch.points.list[[model.type]] <- xintercept
    
    ggplot() + 
      geom_smooth(data = model.input,
                  aes(x = age/12, y = variant, group = pair),
                  method = "glm", method.args = list(family = "binomial"),
                  color = "#F5F5F5", se = FALSE) +
      geom_ribbon(data = trend,
                  aes(x = x/12, ymin = conf.low, ymax = conf.high),
                  fill = speaker.colors[i], alpha = 0.25) +
      geom_line(data = trend,
                aes(x = x/12, y = predicted), color = speaker.colors[i], size = 2) +
      scale_x_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
      labs(x = "Age (years)", y = "Probability of producing ADL variant") +
      geom_hline(yintercept = 0.5, linetype = "dotted", size = 1) +
      theme_test(base_size = 15) +
      theme(axis.title = element_text(face = "bold")) +
      coord_cartesian(ylim = c(0, 1))
      ggsave(paste0("survey/figs/", i, "-", j, ".png"), dpi = 300, height = 5, width = 6)
      
      fig.input %>%
      mutate(pair = str_replace(pair, "_", "/")) %>%
      ggplot(aes(x = age/12, y = variant, color = speaker, fill = speaker)) +
        facet_wrap(.~pair, ncol = 5) +
        geom_hline(yintercept = 0.5, linetype = "dotted", size = 1) +
        geom_smooth(method = "glm", method.args = list(family = "binomial"), size = 3) +
        scale_color_manual(values = speaker.colors) +
        scale_fill_manual(values = speaker.colors) +
        scale_x_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(x = "Age (years)", y = "Probability of producing ADL variant",
             color = "Speaker", fill = "Speaker") +
        theme_test(base_size = 45) +
        theme(axis.title = element_text(face = "bold"), 
              legend.title = element_text(face = "bold"), 
              legend.position = "bottom")
      ggsave(paste0("survey/figs/bypair-shift-timing.png"), height = fig.height, width = 15, dpi = 300)
  }
}
model.outputs <- do.call(rbind, model.outputs.list) %>%
  rownames_to_column(var = "model") %>%
  write_csv("survey/model-outputs/glms.csv")

shift.trends <- do.call(rbind, shift.trends.list) %>%
  data.frame() %>%
  write_csv("survey/model-outputs/trend-lines.csv")

switch.points <- do.call(rbind, switch.points.list) %>%
  data.frame() %>%
  rownames_to_column(var = "model") %>%
  rename(., age = `.`) %>%
  write_csv("survey/model-outputs/switch-points.csv")

# correlations ------------------------------------------------------------
overall.cor <- data %>%
  left_join(select(metadata, pair, cogsci)) %>%
  filter(cogsci == "y") %>%
  select(id, age, pair, speaker, value) %>%
  distinct() %>%
  pivot_wider(names_from = speaker, values_from = value) %>%
  na.omit() %>%
  group_by(id, age) %>%
  mutate(count = n()) %>%
  filter(count >= 2)
  
overall.cor.stats <- tidy(cor.test(overall.cor$caregiver, 
                                   overall.cor$child, 
                                   method = "spearman")) %>%
  mutate(estimate = ifelse(p.value < 0.001, 
                           paste0(round(estimate, digits = 2), "***"),
                           estimate))

ggplot(overall.cor, aes(x = caregiver, y = child)) + 
  geom_jitter(alpha = 0.25) + 
  geom_smooth(method = "lm", 
              color = speaker.colors[1], 
              fill = speaker.colors[1]) + 
  annotate(geom = "label", x = 10, y = 70, 
           label = paste0("ρ = ", overall.cor.stats$estimate), 
           color = speaker.colors[1]) +
  labs(x = "Caregiver ADL", y = "Child CDL") +
  theme_test(base_size = 25) 

cor.over.age <- data %>%
  left_join(select(metadata, pair, cogsci)) %>%
  filter(cogsci == "y") %>%
  select(id, age, pair, speaker, value) %>%
  distinct() %>%
  pivot_wider(names_from = speaker, values_from = value) %>%
  na.omit() %>%
  group_by(id, age) %>%
  mutate(count = n()) %>%
  filter(count >= 2) %>%
  summarize(r = tidy(cor.test(caregiver, child, method = "spearman"))$estimate) %>%
  mutate(age = age/30.437/12)

summary(lm(r ~ age, cor.over.age))

ggplot(cor.over.age, aes(x = age, y = r)) + 
  geom_point(color = speaker.colors[1],
             fill = speaker.colors[1]) + 
  geom_smooth(method = "glm", 
              color = speaker.colors[1],
              fill = speaker.colors[1]) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  scale_x_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Age (years)", y = "Child/Caregiver ADL Correlation") + 
  theme_test(base_size = 20)