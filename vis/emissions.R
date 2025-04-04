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

data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_decile_emissions.csv'))
data$decile = factor(data$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
     'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
     'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 'Decile 2 \n(<957 km²)', 
     'Decile 3 \n(<455 km²)', 'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
     'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 'Decile 8 \n(<39 km²)', 
     'Decile 9 \n(<21 km²)', 'Decile 10 \n(<9 km²)'))

data$frequency_mhz = factor(
  data$frequency_mhz,
  levels = c(700, 800, 850, 900, 1800, 2100, 2300, 2500, 2600, 3500, 5800),
  labels = c('0.7 GHz (5G)', '0.8 GHz (4G)', '0.85 GHz (4G)', '0.9 GHz (4G)', 
             '1.8 GHz (4G)', '2.1 GHz (4G)', '2.3 GHz (4G)', '2.5 GHz (4G)',
             '2.6 GHz (4G)', '3.5 GHz (5G)', '5.8 GHz (5G)'))

############################
## Total Carbon Emissions ##
############################

df = data %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(total_emissions_ghg_kg)/1e9,
            sd = sd(total_emissions_ghg_kg)/1e9)

total_emissions <- ggplot(df, aes(x = decile, y = mean, fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'red',size = 0.3) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
         digits = 2, format = "g", flag = "#")), color = 'black', size = 3.5, position = 
              position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = -1) + 
  labs(colour = NULL, title = "A", x = NULL, 
       y = bquote("Emissions (Mt CO"["2"] ~ " e)")) + 
  theme(legend.position = 'bottom',
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
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 13))


##########################################
## Annualized Carbon Emissions per user ##
##########################################
df1 = data %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(annualized_per_user_ghg),
            sd = sd(annualized_per_user_ghg))

emissions_per_user <- ggplot(df1, aes(x = decile, y = mean, fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'red',size = 0.3) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
      digits = 2, format = "g", flag = "#")), color = 'black', size = 3.5, position = 
      position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = -1) + 
  labs(colour = NULL, title = "B", x = NULL, 
       y = bquote("Emissions (kg CO"["2"] ~ " e)")) + 
  theme(legend.position = 'bottom',
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
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 58))


###############################
## Total Social Carbon Costs ##
###############################
df2 = data %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(scc_cost_usd)/1e9,
            sd = sd(scc_cost_usd)/1e9)

total_scc <- ggplot(df2, aes(x = decile, y = mean, fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'red',size = 0.3) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
       digits = 2, format = "g", flag = "#")), color = 'black', size = 3.5, position = 
       position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = -1) + 
  labs(colour = NULL, title = "C", x = NULL, y = 'Total Social \nCarbon Cost (US$ billions)') + 
  theme(legend.position = 'bottom',
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
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 0.99))

#############################################
## Annualized Social Carbon Costs per user ##
#############################################
df3 = data %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(annualized_per_user_scc_cost_usd),
            sd = sd(annualized_per_user_scc_cost_usd))

per_user_scc <- ggplot(df3, aes(x = decile, y = mean, fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'red',size = 0.3) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
      digits = 2, format = "g", flag = "#")), color = 'black', size = 3.5, position = 
      position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = -1) + 
  labs(colour = NULL, title = "D", x = NULL, 
       y = 'Annualized Social \nCarbon Cost (US$ per user)') + 
  theme(legend.position = 'bottom',
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
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 4.2))

########################
##PANEL USER EMISSIONS##
########################
aggregate_emissions <- ggarrange(total_emissions, emissions_per_user, 
   total_scc, per_user_scc, ncol = 1, nrow = 4, align = c('hv'),
   common.legend = TRUE, legend='bottom') 

path = file.path(folder, 'figures', 'aggregate_emissions.png')
png(path, units="in", width=9, height=11, res=300)
print(aggregate_emissions)
dev.off()


