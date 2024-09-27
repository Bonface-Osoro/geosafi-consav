library(ggpubr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(sf)
library(readr)
library(RColorBrewer)
library(dplyr)
library("cowplot")

suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

############################################
##SSA per user Regions ##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_decile_emissions.csv'))
data <- data %>%
  mutate(phase_per_user_kg_trimmed = ifelse(per_user_ghg_kg > 
         quantile(per_user_ghg_kg, 0.9), quantile(per_user_ghg_kg, 0.9),
         ifelse(per_user_ghg_kg < quantile(per_user_ghg_kg, 0.1), 
         quantile(per_user_ghg_kg, 0.1), per_user_ghg_kg)))

df <- data %>%
  group_by(decile, cell_generation) %>%
  summarize(mean_phase_per_user = mean(phase_per_user_kg_trimmed),
            sd_phase_per_user = sd(phase_per_user_kg_trimmed))

ggplot(df, aes(x = factor(decile), y = mean_phase_per_user, fill = factor(cell_generation))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_phase_per_user - sd_phase_per_user, 
                    ymax = mean_phase_per_user + sd_phase_per_user),
                position = position_dodge(0.9), width = 0.2) + 
  scale_fill_brewer(palette = "Spectral", direction = -1) + 
  labs(colour = NULL, title = " ", subtitle = "(a) Emissions per user", x = NULL, 
    y = bquote("Emissions (kg CO"["2"] ~ " eq./user)"),
    fill = 'Cellular Generation') + scale_y_continuous(labels = function(y)
    format(y, scientific = FALSE), expand = c(0, 0)) + 
  theme(legend.position = 'bottom',
    axis.text.x = element_text(size = 5),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = 'bold'),
    plot.subtitle = element_text(size = 10, face = 'bold'),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    axis.title.x = element_text(size = 8)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Cellular Generation')) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 299))


################# FOR SUPPLEMENTARY SECTION ################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_decile_emissions.csv'))
scaling_factor <- 0.5

df = data %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(per_user_ghg_kg),
            sd = sd(per_user_ghg_kg),
            scaled_sd = sd(per_user_ghg_kg) * scaling_factor,  # Scaling the SD
            mean_ghgs = round(mean(per_user_ghg_kg)),
            scaled_sd_ghgs = round(scaled_sd))

ggplot(df, aes(x = as.factor(decile), y = mean_ghgs, fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +  # Bar plot
  geom_errorbar(aes(ymin = mean_ghgs - scaled_sd_ghgs, ymax = mean_ghgs + scaled_sd_ghgs),
                width = 0.2, position = position_dodge(0.7)) +  # Error bars
  labs(x = "Decile", y = "Mean GHGs", title = "Mean GHGs by Decile and Cell Generation")
















