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
###########################
##SSA per user Emissions ##
###########################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_decile_emissions.csv'))
data$decile = factor(data$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
  'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
  'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
  'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
  'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
  'Decile 10 \n(<9 km²)'))

data <- data %>%
  mutate(phase_per_user_kg_trimmed = ifelse(per_user_ghg_kg > 
         quantile(per_user_ghg_kg, 0.95), quantile(per_user_ghg_kg, 0.95),
         ifelse(per_user_ghg_kg < quantile(per_user_ghg_kg, 0.05), 
         quantile(per_user_ghg_kg, 0.05), per_user_ghg_kg)))

df <- data %>%
  group_by(decile, cell_generation) %>%
  summarize(mean_phase_per_user = mean(phase_per_user_kg_trimmed),
            sd_phase_per_user = sd(phase_per_user_kg_trimmed))

label_totals <- df %>%
  group_by(decile, cell_generation) %>%
  summarize(mean_value = sum(mean_phase_per_user))

per_user_ghgs <- ggplot(df, aes(x = factor(decile), y = mean_phase_per_user, 
                                fill = factor(cell_generation))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_phase_per_user - sd_phase_per_user, 
                    ymax = mean_phase_per_user + sd_phase_per_user),
                position = position_dodge(0.9), width = 0.2, size = 0.2) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
     digits = 2, format = "fg", flag = "#")), color = 'black', size = 3, position = 
     position_dodge(0.9), vjust = -0.5, hjust = 0.5) +
  scale_fill_viridis_d(direction = 1) + 
  labs(colour = NULL, title = "Mobile broadband Greenhouse Gas (GHG) emissions", 
       subtitle = "(A) Per user GHG emissions categorized by cell generation and grouped by deciles.", 
       x = NULL, y = bquote("Emissions (kg CO"["2"] ~ " eq./user)")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 10)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Technology')) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 1799))

######################################
##SSA per user Annualized Emissions ##
######################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_decile_emissions.csv'))

data <- data %>%
  mutate(annualized_per_user_kg_trimmed = ifelse(annualized_per_user_ghg > 
         quantile(annualized_per_user_ghg, 0.95), quantile(annualized_per_user_ghg, 0.95),
         ifelse(annualized_per_user_ghg < quantile(annualized_per_user_ghg, 0.05), 
         quantile(annualized_per_user_ghg, 0.05), annualized_per_user_ghg)))

df <- data %>%
  group_by(decile, cell_generation) %>%
  summarize(mean_annualized_per_user = mean(annualized_per_user_kg_trimmed),
            sd_annualized_per_user = sd(annualized_per_user_kg_trimmed))

df$decile = factor(df$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
    'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
    'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
   'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
   'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
   'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
   'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
   'Decile 10 \n(<9 km²)'))

annualized_per_user <- ggplot(df, aes(x = factor(decile), 
    y = mean_annualized_per_user, fill = factor(cell_generation))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_annualized_per_user - sd_annualized_per_user, 
       ymax = mean_annualized_per_user + sd_annualized_per_user),
       position = position_dodge(0.9), width = 0.2, size = 0.2) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
     digits = 2, format = "fg", flag = "#")), color = 'black', size = 3, position = 
     position_dodge(0.9), vjust = -0.5, hjust = 0.5) +
  scale_fill_viridis_d(direction = 1) + 
  labs(colour = NULL, title = " ", 
       subtitle = "(B) Annualized per user GHG emissions categorized by cell generation and grouped by deciles.", 
       x = NULL, y = bquote("Emissions (kg CO"["2"] ~ " eq./user)")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 10)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Technology')) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 164))

#####################################
##SSA per user Social Carbon Cost  ##
#####################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_decile_emissions.csv'))

data <- data %>%
  mutate(per_user_scc_trimmed = ifelse(per_user_scc_cost_usd > 
         quantile(per_user_scc_cost_usd, 0.95), quantile(per_user_scc_cost_usd, 0.95),
         ifelse(per_user_scc_cost_usd < quantile(per_user_scc_cost_usd, 0.05), 
         quantile(per_user_scc_cost_usd, 0.05), per_user_scc_cost_usd)))

df <- data %>%
  group_by(decile, cell_generation) %>%
  summarize(mean_per_user_scc = mean(per_user_scc_trimmed),
            sd_annualized_per_user_scc = sd(per_user_scc_trimmed))

df$decile = factor(df$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
  'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
  'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
  'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
  'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
  'Decile 10 \n(<9 km²)'))

per_user_scc <- ggplot(df, aes(x = factor(decile), 
  y = mean_per_user_scc, fill = factor(cell_generation))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_per_user_scc - sd_annualized_per_user_scc, 
      ymax = mean_per_user_scc + sd_annualized_per_user_scc),
      position = position_dodge(0.9), width = 0.2, size = 0.2) +
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
      digits = 2, format = "fg", flag = "#")), color = 'black', size = 3, position = 
      position_dodge(0.9), vjust = -0.5, hjust = 0.5) +
  scale_fill_viridis_d(direction = 1) + 
  labs(colour = NULL, title = "Social Cost of Carbon (SCC)", 
       subtitle = "(C) SCC per user categorized by cell generation and grouped by deciles.", 
       x = NULL, y = "Per user \nSocial Carbon Cost (USD)") + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 10)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Technology')) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 124))


################################################
##SSA Annualized per user Social Carbon Cost  ##
################################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_decile_emissions.csv'))

data <- data %>%
  mutate(annualized_per_user_scc_trimmed = ifelse(annualized_per_user_scc_cost_usd > 
         quantile(annualized_per_user_scc_cost_usd, 0.95), quantile(annualized_per_user_scc_cost_usd, 0.95),
         ifelse(annualized_per_user_scc_cost_usd < quantile(annualized_per_user_scc_cost_usd, 0.05), 
         quantile(annualized_per_user_scc_cost_usd, 0.05), annualized_per_user_scc_cost_usd)))

df <- data %>%
  group_by(decile, cell_generation) %>%
  summarize(mean_annualized_per_user_scc = mean(annualized_per_user_scc_trimmed),
            sd_annualized_per_user_scc = sd(annualized_per_user_scc_trimmed))

df$decile = factor(df$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
    'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
    'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
   'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
   'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
   'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
   'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
   'Decile 10 \n(<9 km²)'))


annualized_per_user_scc <- ggplot(df, aes(x = factor(decile), 
    y = mean_annualized_per_user_scc, fill = factor(cell_generation))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_annualized_per_user_scc - sd_annualized_per_user_scc, 
     ymax = mean_annualized_per_user_scc + sd_annualized_per_user_scc),
     position = position_dodge(0.9), width = 0.2, size = 0.2) +
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
      digits = 2, format = "fg", flag = "#")), color = 'black', size = 3, position = 
      position_dodge(0.9), vjust = -0.5, hjust = 0.5) +
  scale_fill_viridis_d(direction = 1) + 
  labs(colour = NULL, title = " ", 
       subtitle = "(D) Annualized SCC per user categorized by cell generation and grouped by deciles.", 
       x = NULL, y = bquote("Annualized \nSocial Carbon Cost (USD)")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 10)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Technology')) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 12.4))

########################
##PANEL USER EMISSIONS##
########################
aggregate_emissions <- ggarrange(per_user_ghgs, annualized_per_user, 
   per_user_scc, annualized_per_user_scc, ncol = 1, nrow = 4, align = c('hv'),
   common.legend = TRUE, legend='bottom') 

path = file.path(folder, 'figures', 'aggregate_emissions.png')
png(path, units="in", width=9, height=11, res=300)
print(aggregate_emissions)
dev.off()

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




