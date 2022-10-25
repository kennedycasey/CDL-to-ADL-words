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
speaker.colors <- c("child" = "#235789", 
                    "caregiver" = "#0288d1")

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
    
    model <- glm(variant ~ age, 
                 data = model.input, 
                 family = binomial)
    
    # store model output summary
    model.summary <- tidy(model) %>%
      filter(term == "age")
    
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
      ggsave(paste0("survey/figs/", i, "-", j, ".png"), dpi = 300, height = 5, width = 6)
      
      ggplot(fig.input, 
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
      ggsave(paste0("survey/figs/bypair-shift-timing-", j, ".png"), height = fig.height, width = 15, dpi = 300)
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