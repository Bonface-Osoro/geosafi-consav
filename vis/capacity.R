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
     'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
    'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
    'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
    'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
    'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
    'Decile 10 \n(<9 km²)'))

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
  labs(colour = 'Mobile Technology', title = "Mobile Signal results.", 
       subtitle = "(A) Path loss.", 
       x = NULL, y = "Path loss (dB)") + 
  scale_color_viridis_d(direction = 1) +
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
  labs(colour = 'Mobile Technology', title = " ", 
       subtitle = "(B) Received power.", 
       x = NULL, y = "Received power (dB)") + 
  scale_color_viridis_d(direction = 1) +
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
  labs(colour = 'Mobile Technology', title = " ", 
       subtitle = "(C) Carrier-to-noise ratio.", 
       x = NULL, y = "Carrier-to-noise ratio (dB)") + 
  scale_color_viridis_d(direction = 1)  +
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
     'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
    'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
    'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
    'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
    'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
    'Decile 10 \n(<9 km²)'))

data$network_load_perc = factor(data$network_load_perc,
    levels = c(0.25, 0.5, 0.75),
    labels = c('Low (0.25%)', 'Baseline (50%)', 'High (75%)'))

df = data %>%
  group_by(cell_generation, decile, network_load_perc) %>%
  summarize(mean = mean(per_user_capacity_mbps),
            sd = sd(per_user_capacity_mbps))

capacity_per_user <- ggplot(df, aes(x = decile, y = mean, fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'red',size = 0.2) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
      digits = 2, format = "fg", flag = "#")), color = 'black', size = 3, position = 
      position_dodge(0.9), vjust = -0.4, hjust = 1) +
  scale_fill_viridis_d(direction = 1) +
  labs(colour = NULL, title = "Mobile broadband capacity results", 
       subtitle = "(D) Per user capacity categorized by cell generation, network load and grouped by deciles.", 
       x = NULL, y = "Capacity (Mbps/user)") +
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
    strip.text = element_text(size = 11),
    axis.title.x = element_text(size = 10)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Technology')) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 1250)) +
  facet_wrap( ~ network_load_perc, nrow = 3)

####################################
## Base Station per Area Capacity ##
####################################
df = data %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(per_area_capacity_mbps/1e3),
            sd = sd(per_area_capacity_mbps/1e3))

per_area_capacity <- ggplot(df, aes(x = decile, y = mean, fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'red',size = 0.2) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
      digits = 2, format = "fg", flag = "#")), color = 'black', size = 3, position = 
      position_dodge(0.9), vjust = -0.4, hjust = 1) +
  scale_fill_viridis_d(direction = 1) +
  labs(colour = NULL, title = " ", 
       subtitle = "(E) Per area capacity categorized by cell generation and grouped by deciles.", 
       x = NULL, y = "Capacity (Gbps per km²)") +
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
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 199))

#########################
## PANEL USER CAPACITY ##
#########################
aggregate_signals <- ggarrange(path_loss, received_power, cnr,
     ncol = 3, #align = c('hv'),
     common.legend = TRUE, legend='bottom') 

aggregate_capacities <- ggarrange(aggregate_signals, capacity_per_user, 
     nrow = 2, heights = c(1, 3), #align = c('hv'),
     common.legend = TRUE, legend='bottom') 

path = file.path(folder, 'figures', 'aggregate_capacities.png')
png(path, units="in", width=11, height=14, res=300)
print(aggregate_capacities)
dev.off()
