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

data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_decile_costs.csv'))
data$decile = factor(data$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 per km²)', 
   'Decile 2 \n(456 - 957 per km²)', 'Decile 3 \n(273 - 455 per km²)', 
   'Decile 4 \n(172 - 272 per km²)', 'Decile 5 \n(107 - 171 per km²)', 
   'Decile 6 \n(64 - 106 per km²)', 'Decile 7 \n(40 - 63 per km²)', 
   'Decile 8 \n(22 - 39 per km²)', 'Decile 9 \n(10 - 21 per km²)', 
   'Decile 10 \n(<9 per km²)'))

#############################
## Total Cost of Ownership ##
#############################

df = data %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(per_user_tco_usd),
            sd = sd(per_user_tco_usd))

tco_per_user <- ggplot(df, aes(x = decile, y = mean, fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
    position = position_dodge(.9), color = 'black',size = 0.2) + 
  scale_fill_brewer(palette = "Spectral", direction = -1) + 
  labs(colour = NULL, title = "Mobile broadband Total Cost of Ownership (TCO)", 
       subtitle = "(a) Per user TCO categorized by cell generation and grouped by deciles.", 
       x = NULL, y = "Costs (USD per user)") + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10, angle = 15),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 10)) +
    expand_limits(y = 0) +
    guides(fill = guide_legend(ncol = 5, title = 'Cell Generation')) +
    scale_x_discrete(expand = c(0, 0.15)) +
    scale_y_continuous(expand = c(0, 0),
    labels = function(y) format(y, scientific = FALSE),limits = c(0, 2199))

########################################
## Annualized Total Cost of Ownership ##
########################################

df = data %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(annualized_per_user_cost_usd),
            sd = sd(annualized_per_user_cost_usd))

anualized_tco_per_user <- ggplot(df, aes(x = decile, y = mean, 
                                  fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'black',size = 0.2) + 
  scale_fill_brewer(palette = "Spectral", direction = -1) + 
  labs(colour = NULL, title = " ", 
       subtitle = "(b) Annualized per user TCO categorized by cell generation and grouped by deciles.", 
       x = NULL, y = "Costs (USD per user)") + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10, angle = 15),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 10)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Cell Generation')) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 229))


#####################################
## Monthly Total Cost of Ownership ##
#####################################

df = data %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(monthly_per_user_cost_usd),
            sd = sd(monthly_per_user_cost_usd))

monthly_tco_per_user <- ggplot(df, aes(x = decile, y = mean, fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'black',size = 0.2) + 
  scale_fill_brewer(palette = "Spectral", direction = -1) + 
  labs(colour = NULL, title = " ", 
       subtitle = "(c) Monthly per user TCO categorized by cell generation and grouped by deciles.", 
       x = NULL, y = "Costs (USD per user)") + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10, angle = 15),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 10)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Cell Generation')) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 19))

#####################
##PANEL USER COSTS ##
#####################
aggregate_costs <- ggarrange(tco_per_user, anualized_tco_per_user, 
     monthly_tco_per_user, ncol = 1, nrow = 3, align = c('hv'),
     common.legend = TRUE, legend='bottom') 

path = file.path(folder, 'figures', 'aggregate_costs.png')
png(path, units = "in", width=10, height=12, res=300)
print(aggregate_costs)
dev.off()





