library(ggpubr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(readr)
library(RColorBrewer)
library(dplyr)
library("cowplot")

suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

data <- read.csv(file.path(folder, '..', 'results', 'cellular', 'mobile_capacity_results.csv'))
data$decile = factor(data$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
     'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
     'Decile 10'), labels = c('Decile 1 \n(>958 per km²)', 
     'Decile 2 \n(456 - 957 per km²)', 'Decile 3 \n(273 - 455 per km²)', 
     'Decile 4 \n(172 - 272 per km²)', 'Decile 5 \n(107 - 171 per km²)', 
     'Decile 6 \n(64 - 106 per km²)', 'Decile 7 \n(40 - 63 per km²)', 
     'Decile 8 \n(22 - 39 per km²)', 'Decile 9 \n(10 - 21 per km²)', 
     'Decile 10 \n(<9 per km²)'))

###############
## Path loss ##
###############
df = select(data, cell_generation, path_loss_db, trans_user_dist_km)

df = data %>%
  group_by(cell_generation, trans_user_dist_km) %>%
  summarise(mean = mean(path_loss_db),
            sd = sd(path_loss_db))

path_loss <- ggplot(df, aes(trans_user_dist_km, mean, color = cell_generation)) +
  geom_line(position = position_dodge(width = 0.5), size = 0.7) +
  labs(colour = 'Cell Generation', title = "Mobile Signal results.", 
       subtitle = "(a) Path loss.", 
       x = NULL, y = "Path loss (dB)") + 
  scale_color_brewer(palette = "Set1", direction = -1) +
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
  guides(fill = guide_legend(ncol = 5)) 

####################
## Received Power ##
####################
df = select(data, cell_generation, received_power_db, trans_user_dist_km)

df = data %>%
  group_by(cell_generation, trans_user_dist_km) %>%
  summarise(mean = mean(received_power_db),
    sd = sd(received_power_db))

received_power <- ggplot(df, aes(trans_user_dist_km, mean, color = cell_generation)) +
  geom_line(position = position_dodge(width = 0.5), size = 0.7) +
  labs(colour = 'Cell Generation', title = " ", 
       subtitle = "(b) Received power.", 
       x = NULL, y = "Received power (dB)") + 
  scale_color_brewer(palette = "Set1", direction = -1) +
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
    axis.title.x = element_text(size = 10)) 

############################
## Carrier to noise ratio ##
############################
df = select(data, cell_generation, cnr_db, trans_user_dist_km)

df = data %>%
  group_by(cell_generation, trans_user_dist_km) %>%
  summarise(mean = mean(cnr_db),
            sd = sd(cnr_db))

cnr <- ggplot(df, aes(trans_user_dist_km, mean, color = cell_generation)) +
  geom_line(position = position_dodge(width = 0.5), size = 0.7) +
  labs(colour = 'Cell Generation', title = " ", 
       subtitle = "(c) Carrier-to-noise ratio.", 
       x = NULL, y = "Carrier-to-noise ratio (dB)") + 
  scale_color_brewer(palette = "Set1", direction = -1) +
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
    axis.title.x = element_text(size = 10)) 

####################################
## Base Station per User Capacity ##
####################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_decile_capacity.csv'))
data$decile = factor(data$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
     'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
     'Decile 10'), labels = c('Decile 1 \n(>958 per km²)', 
     'Decile 2 \n(456 - 957 per km²)', 'Decile 3 \n(273 - 455 per km²)', 
     'Decile 4 \n(172 - 272 per km²)', 'Decile 5 \n(107 - 171 per km²)', 
     'Decile 6 \n(64 - 106 per km²)', 'Decile 7 \n(40 - 63 per km²)', 
     'Decile 8 \n(22 - 39 per km²)', 'Decile 9 \n(10 - 21 per km²)', 
     'Decile 10 \n(<9 per km²)'))

df = data %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(per_user_capacity_mbps),
            sd = sd(per_user_capacity_mbps))

capacity_per_user <- ggplot(df, aes(x = decile, y = mean, fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'black',size = 0.2) + 
  scale_fill_brewer(palette = "Spectral", direction = -1) + 
  labs(colour = NULL, title = "Mobile broadband capacity results", 
       subtitle = "(a) Per user capacity categorized by cell generation and grouped by deciles.", 
       x = NULL, y = "Capacity (Mbps/user)") +
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
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 0.6))

####################################
## Base Station per Area Capacity ##
####################################
df = data %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(per_area_capacity_mbps),
            sd = sd(per_area_capacity_mbps))

per_area_capacity <- ggplot(df, aes(x = decile, y = mean, fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'black',size = 0.2) + 
  scale_fill_brewer(palette = "Spectral", direction = -1) + 
  labs(colour = NULL, title = " ", 
       subtitle = "(b) Per area capacity categorized by cell generation and grouped by deciles.", 
       x = NULL, y = "Capacity (Mbps per km²)") +
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
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 199))

#########################
## PANEL USER CAPACITY ##
#########################
aggregate_signals <- ggarrange(path_loss, received_power, cnr,
     ncol = 3, #align = c('hv'),
     common.legend = TRUE, legend='bottom') 

aggregate_capacities <- ggarrange(aggregate_signals, capacity_per_user, 
     per_area_capacity, nrow = 3, #align = c('hv'),
     common.legend = TRUE, legend='bottom') 

path = file.path(folder, 'figures', 'aggregate_capacities.png')
png(path, units="in", width=11, height=14, res=300)
print(aggregate_capacities)
dev.off()
