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
   'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
  'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
  'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
  'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
  'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
  'Decile 10 \n(<9 km²)'))


#############################
## Total Cost of Ownership ##
#############################

df = data %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(total_decile_tco_usd)/1e9,
            sd = sd(total_decile_tco_usd)/1e9)

total_tco <- ggplot(df, aes(x = decile, y = mean, fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
    position = position_dodge(.9), color = 'red',size = 0.3) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
     digits = 2, format = "fg", flag = "#")), color = 'black', size = 3.5, position = 
     position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = -1) + 
  labs(colour = NULL, title = "A", 
       x = NULL, y = "Costs (US$ billion)") + 
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
    guides(fill = guide_legend(ncol = 7, title = 'Mobile Technology')) +
    scale_x_discrete(expand = c(0, 0.15)) +
    scale_y_continuous(expand = c(0, 0),
    labels = function(y) format(y, scientific = FALSE),limits = c(0, 5.9))

########################################
## Annualized Total Cost of Ownership ##
########################################

df1 = data %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(annualized_per_user_cost_usd),
            sd = sd(annualized_per_user_cost_usd))

anualized_tco_per_user <- ggplot(df1, aes(x = decile, y = mean, 
                                  fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'red',size = 0.3) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
      digits = 2, format = "fg", flag = "#")), color = 'black', size = 3.5, position = 
      position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = -1) + 
  labs(colour = NULL, title = "B", x = NULL, y = "Costs (US$ per user)") + 
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
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 1249))


#####################
##PANEL USER COSTS ##
#####################
aggregate_costs <- ggarrange(total_tco, anualized_tco_per_user, 
     ncol = 1, nrow = 2, align = c('hv'),
     common.legend = TRUE, legend='bottom') 

path = file.path(folder, 'figures', 'aggregate_costs.png')
png(path, units = "in", width=8, height=9, res=300)
print(aggregate_costs)
dev.off()

