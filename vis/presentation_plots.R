library(ggpubr)
library(ggplot2)
library(tidyverse)
library(dplyr)


suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

##############################
## 1 MOBILE BROADBAND MODEL ##
##############################
##################
## 1.1 CAPACITY ##
##################
data <- read.csv(file.path(folder, '..', 'results', 'cellular', 
                           'mobile_capacity_results.csv'))
data <- data %>%
  filter(cell_generation == "4G" & frequency_mhz %in% c(700, 800, 1800,
                                                        2600, 3500, 5800))

data$frequency_mhz = factor(
  data$frequency_mhz,
  levels = c(700, 800, 1800, 2600, 3500, 5800),
  labels = c('0.7 GHz (5G)', '0.8 GHz (4G)', '1.8 GHz (4G)',
             '2.6 GHz (4G)', '3.5 GHz (5G)', '5.8 GHz (5G)'))

data$discrete <- cut(data$intersite_distance_km, seq(0,100,5))
data$continuous = ""
data$continuous[data$discrete == '(0,5]'] <- 2.5
data$continuous[data$discrete == '(5,10]'] <- 7.5
data$continuous[data$discrete == '(10,15]'] <- 12.5
data$continuous[data$discrete == '(15,20]'] <- 17.5
data$continuous[data$discrete == '(20,25]'] <- 22.5
data$continuous[data$discrete == '(25,30]'] <- 27.5
data$continuous[data$discrete == '(30,35]'] <- 32.5
data$continuous[data$discrete == '(35,40]'] <- 37.5
data$continuous[data$discrete == '(40,45]'] <- 42.5
data$continuous[data$discrete == '(45,50]'] <- 52.5
data$continuous[data$discrete == '(50,55]'] <- 57.5
data$continuous[data$discrete == '(55,60]'] <- 62.5
data$continuous[data$discrete == '(60,65]'] <- 67.5
data$continuous[data$discrete == '(65,70]'] <- 72.5
data$continuous[data$discrete == '(75,80]'] <- 77.5
data$continuous[data$discrete == '(80,85]'] <- 82.5
data$continuous[data$discrete == '(85,90]'] <- 87.5
data$continuous[data$discrete == '(90,95]'] <- 92.5
data$continuous[data$discrete == '(95,100]'] <- 97.5

df = select(data, path_loss_db, frequency_mhz, 
            continuous)

df$continuous = as.numeric(df$continuous)
df = df %>%
  group_by(frequency_mhz, continuous) %>%
  summarise(
    mean = mean(path_loss_db),
    sd = sd(path_loss_db))

######################################
##plot1 = Path Loss power line plot ##
######################################
path_loss <- ggplot(df, aes(continuous, mean, color = frequency_mhz)) + 
  geom_line(position = position_dodge(width = 0.5), size = 0.5) + 
  geom_point(size = 0.3, position=position_dodge(0.5)) +
  labs( colour = NULL,
        title = "Capacity",subtitle = 'A', x = "Inter-Site Distance (km)",
        y = "Path Loss (dB)",
        fill = "Frequency") + 
  scale_color_brewer(palette = "Dark2") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 10)) +
  guides(color = guide_legend(ncol = 6))

##########
## SINR ##
##########
df3 = select(data, sinr_db, frequency_mhz, 
             continuous)

df3$continuous = as.numeric(df3$continuous)
df3 = df3 %>%
  group_by(frequency_mhz, continuous) %>%
  summarise(
    mean = mean(sinr_db),
    sd = sd(sinr_db))

sinr_db <- ggplot(df3, aes(continuous, mean, color = frequency_mhz)) + 
  geom_line(position = position_dodge(width = 0.5), size = 0.5) + 
  geom_point(size = 0.3, position=position_dodge(0.5)) +
  labs( colour = NULL,
        subtitle = "B",x = "Inter-Site Distance (km)",
        y = "SINR (dB)",
        fill = "Frequency") + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 10),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 10)) +
  guides(color = guide_legend(nrow = 2))


data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_decile_capacity.csv'))
data <- data %>%
  filter(cell_generation == "4G" & frequency_mhz %in% c(700, 800, 1800,
                                                        2600, 3500, 5800))

data$frequency_mhz = factor(
  data$frequency_mhz,
  levels = c(700, 800, 1800, 2600, 3500, 5800),
  labels = c('0.7 GHz (5G)', '0.8 GHz (4G)', '1.8 GHz (4G)',
             '2.6 GHz (4G)', '3.5 GHz (5G)', '5.8 GHz (5G)'))

data$discrete <- cut(data$intersite_distance_km, seq(0,100,5))
data$continuous = ""
data$continuous[data$discrete == '(0,5]'] <- 2.5
data$continuous[data$discrete == '(5,10]'] <- 7.5
data$continuous[data$discrete == '(10,15]'] <- 12.5
data$continuous[data$discrete == '(15,20]'] <- 17.5
data$continuous[data$discrete == '(20,25]'] <- 22.5
data$continuous[data$discrete == '(25,30]'] <- 27.5
data$continuous[data$discrete == '(30,35]'] <- 32.5
data$continuous[data$discrete == '(35,40]'] <- 37.5
data$continuous[data$discrete == '(40,45]'] <- 42.5
data$continuous[data$discrete == '(45,50]'] <- 52.5
data$continuous[data$discrete == '(50,55]'] <- 57.5
data$continuous[data$discrete == '(55,60]'] <- 62.5
data$continuous[data$discrete == '(60,65]'] <- 67.5
data$continuous[data$discrete == '(65,70]'] <- 72.5
data$continuous[data$discrete == '(75,80]'] <- 77.5
data$continuous[data$discrete == '(80,85]'] <- 82.5
data$continuous[data$discrete == '(85,90]'] <- 87.5
data$continuous[data$discrete == '(90,95]'] <- 92.5
data$continuous[data$discrete == '(95,100]'] <- 97.5

df = select(data, per_user_capacity_mbps, frequency_mhz, 
            continuous)

df$continuous = as.numeric(df$continuous)
df = df %>%
  group_by(frequency_mhz, continuous) %>%
  summarise(
    mean = mean(per_user_capacity_mbps),
    sd = sd(per_user_capacity_mbps))
#############################
## Spectral Efficiency PDF ##
#############################
data <- read.csv(file.path(folder, '..', 'results', 'cellular', 
                           'mobile_capacity_results.csv'))
data <- data %>%
  filter(cell_generation == "4G" & frequency_mhz %in% c(700, 800, 1800,
                                                        2600, 3500, 5800))

data$frequency_mhz = factor(
  data$frequency_mhz,
  levels = c(700, 800, 1800, 2600, 3500, 5800),
  labels = c('0.7 GHz (5G)', '0.8 GHz (4G)', '1.8 GHz (4G)',
             '2.6 GHz (4G)', '3.5 GHz (5G)', '5.8 GHz (5G)'))

spectral_pdf <- ggplot(data, aes(x = spectral_efficiency_bpshz, 
  color = frequency_mhz)) + 
  geom_density(position = position_dodge(width = 0.5), size = 0.5) +
  labs(title = "C", 
       x = "Spectral Efficiency", 
       y = "Density", color = "Frequency") +
  scale_color_brewer(palette = "Dark2") +
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
  guides(color = guide_legend(nrow = 2)) 

#######################
## Capacity per user ##
#######################
capacity_per_user <- ggplot(df, aes(continuous, mean, color = frequency_mhz)) + 
  geom_line(position = position_dodge(width = 0.5), size = 0.5) + 
  geom_point(size = 0.3, position=position_dodge(0.5)) +
  labs( colour = NULL,
        subtitle = "C",x = "Inter-Site Distance (km)",
        y = "Capacity \nper user (Mbps)",
        fill = "Frequency") + 
  scale_color_brewer(palette = "Dark2") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 10)) +
  guides(color = guide_legend(ncol = 6))

mobile_capacity <- ggarrange(path_loss, sinr_db, 
   capacity_per_user, ncol = 3, nrow = 1, align = c('hv'),
   common.legend = TRUE, legend='bottom') 

##############
## 1.2 COST ##
##############
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_decile_costs.csv'))
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
     digits = 2, format = "g", flag = "#")), color = 'black', size = 3.5, position = 
    position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = -1) + 
  labs(colour = NULL, title = "Mobile broadband cost per user results for SSA", 
       x = NULL, 
       y = "Costs (US$ per user)") + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 10)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Technology')) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 130))

data <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                           'SSA_decile_emissions.csv'))

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
  labs(colour = NULL, title = "Mobile broadband carbon emissions per user results for SSA", 
       x = NULL, 
       y = bquote("Emissions (kg CO"["2"] ~ " e)")) + 
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 10),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 13, face = 'bold'),
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

mobile_model <- ggarrange(mobile_capacity, anualized_tco_per_user, 
   emissions_per_user, ncol = 1, nrow = 3, align = c('hv'),
   common.legend = TRUE, legend='bottom') 

path = file.path(folder, 'dissertation_figures', 'mobile_per_user_results.png')
png(path, units="in", width=9, height=10, res=300)
print(mobile_model)
dev.off()

##############################
## 2 FIXED BROADBAND MODEL ##
##############################
##############
## 2.1 COST ##
##############
data <- subset(read.csv(file.path(folder, '..', '..', 'glassfiber', 'results', 'SSA', 
                     'SSA_fiber_cost_results.csv')), strategy == "access")

data$decile = factor(data$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 'Decile 2 \n(<957 km²)', 
   'Decile 3 \n(<455 km²)', 'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
   'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 'Decile 8 \n(<39 km²)', 
   'Decile 9 \n(<21 km²)', 'Decile 10 \n(<9 km²)'))

data$strategy <- factor(data$strategy,
   levels = c('access'), labels = c('New Access Network'))

data$algorithm <- factor(data$algorithm,levels = c('prims', 'pcsf'),
   labels = c("Minimum Spanning Tree (MST)",
   'Prize Collecting Steiner Tree (PCST)'))
df6 <- data %>% select(total_ssa_tco_usd, decile, algorithm)

df6 = df6 %>%
  group_by(decile, algorithm) %>%
  summarize(mean = mean(total_ssa_tco_usd/1e9),
            sd = sd(total_ssa_tco_usd/1e9))

label_totals <- df6 %>%
  group_by(decile, algorithm) %>%
  summarize(mean_value = sum(mean))

total_ssa_tco <- ggplot(df6, aes(x = decile, y = mean, fill = algorithm)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1,
                position = position_dodge(.9), color = 'red',size = 0.5) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
    digits = 2, format = "fg", flag = "#")), color = 'black', size = 3.5, position = 
    position_dodge(0.9), vjust = -0.2, hjust = 1.2) +
  scale_fill_viridis_d(direction = -1) +
  labs(colour = NULL, title = "Fiber broadband total cost results for SSA",
       x = NULL,
       y = bquote("Total TCO (US$ billions)"))+
theme(legend.position = 'bottom',
    axis.text.x = element_text(size = 11),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  ) + expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 3, 
  title = 'Optimization Algorithm')) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 79)) 

###################
## 2.2 EMISSIONS ##
###################
data <- subset(read.csv(file.path(folder, '..', '..', 'glassfiber', 'results', 
   'SSA', 'SSA_fiber_emission_results.csv')), strategy == "access")

data$decile = factor(data$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 'Decile 2 \n(<957 km²)', 
   'Decile 3 \n(<455 km²)', 'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
   'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 'Decile 8 \n(<39 km²)', 
   'Decile 9 \n(<21 km²)', 'Decile 10 \n(<9 km²)'))

data$strategy <- factor(data$strategy,
  levels = c('access'), labels = c('New Access Network'))

data$algorithm <- factor(data$algorithm,levels = c('prims', 'pcsf'),
  labels = c("Minimum Spanning Tree (MST)",
  'Prize Collecting Steiner Tree (PCST)'))

df7 <- data %>% 
  select(total_emissions_ssa_kg, strategy, decile, algorithm)

df7 = df7 %>%
  group_by(strategy, decile, algorithm) %>%
  summarize(mean = mean(total_emissions_ssa_kg/1e9),
            sd = sd(total_emissions_ssa_kg/1e9))

label_totals <- df7 %>%
  group_by(decile, strategy) %>%
  summarize(mean_value = sum(mean))

total_ssa_emissions <- 
  ggplot(df7, aes(x = decile, y = mean, fill = algorithm)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1,
                position = position_dodge(.9), color = 'red',size = 0.5) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
      digits = 2, format = "fg", flag = "#")), color = 'black', size = 3, position = 
      position_dodge(0.9), vjust = -0.2, hjust = 1.2) +
  scale_fill_viridis_d(direction = -1) + 
  labs(colour = NULL, 
       title = "Fiber broadband total carbon emission results for SSA", 
       x = NULL,
       y = bquote("Total emissions (Mt CO"["2"] ~ " e)")) +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 11),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 11)
  ) + expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 3, 
  title = 'Optimization Algorithm')) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 30)) +
  theme(strip.text = element_text(size = 14))

#############################
## 2.3 Social Carbon Costs ##
#############################
df8 <- data %>% 
  select(total_ssa_scc_usd, strategy, decile, algorithm)

df8 = df8 %>%
  group_by(strategy, decile, algorithm) %>%
  summarize(mean = mean(total_ssa_scc_usd/1e9),
            sd = sd(total_ssa_scc_usd/1e9))

label_totals <- df8 %>%
  group_by(decile, strategy) %>%
  summarize(mean_value = sum(mean))

total_ssa_scc <- ggplot(df8, aes(x = decile, y = mean, fill = algorithm)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1,
                position = position_dodge(.9), color = 'red',size = 0.5) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
      digits = 2, format = "fg", flag = "#")), color = 'black', size = 3, position = 
       position_dodge(0.9), vjust = -0.2, hjust = 1.2) +
  scale_fill_viridis_d(direction = -1) +
  labs(colour = NULL, 
       title = "Fiber broadband total Social Carbon Cost (SCC) results for SSA",
       x = NULL,
       y = bquote("Total \nSCC (US$ billions)")) +
  theme(legend.position = 'bottom',
    axis.text.x = element_text(size = 11),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 11)
  ) + expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 3, title = 'Network level')) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 2)) +
  theme(strip.text = element_text(size = 14))

fiber_model <- ggarrange(total_ssa_tco, total_ssa_emissions, 
    total_ssa_scc, ncol = 1, nrow = 3, align = c('hv'),
    common.legend = TRUE, legend='bottom') 

path = file.path(folder, 'dissertation_figures', 'fiber_per_user_results.png')
png(path, units="in", width=10, height=9, res=300)
print(fiber_model)
dev.off()

#################################
## 3 SATELLITE BROADBAND MODEL ##
##############################
##################
## 3.1 CAPACITY ##
##################
data <- subset(read.csv(file.path(folder, '..', '..', 'saleos', 'results', 
        'SSA','SSA_decile_capacity.csv')), constellation != "GEO")

data$constellation = factor(
  data$constellation,
  levels = c('Starlink', 'OneWeb', 'Kuiper'),
  labels = c('Starlink', 'OneWeb', 'Kuiper'))

data$decile = factor(data$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 'Decile 2 \n(<957 km²)', 
   'Decile 3 \n(<455 km²)', 'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
   'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 'Decile 8 \n(<39 km²)', 
   'Decile 9 \n(<21 km²)', 'Decile 10 \n(<9 km²)'))

df <- data %>% select(per_user_capacity_mbps, constellation, decile)

df = df %>%
  group_by(constellation, decile) %>%
  summarize(mean = mean(per_user_capacity_mbps),
            sd = sd(per_user_capacity_mbps))

label_totals <- df %>%
  group_by(constellation, decile) %>%
  summarize(mean_value = sum(mean))

per_user_capacity <- 
  ggplot(df, aes(x = decile, y = mean, fill = constellation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1,
                position = position_dodge(.9), color = 'red',size = 0.5) + 
  geom_text(aes(label = formatC(round(after_stat(y), 3), 
     digits = 2, format = "fg", flag = "#")), color = 'black', size = 3.5, position = 
     position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = 1) + 
  labs(colour = NULL, 
      title = "Satellite broadband capacity per user results for SSA", x = NULL, 
      y = bquote("Capacity (Mbps)")) +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 11),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  ) + scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 63)) +
  guides(fill = guide_legend(ncol = 3, title = 'Constellation')) +
  theme(strip.text = element_text(size = 14)) 

##############
## 3.2 COST ##
##############
data1 <- subset(read.csv(file.path(folder, '..', '..', 'saleos', 'results', 
  'SSA', 'SSA_decile_cost.csv')), constellation != "GEO")

data1$constellation = factor(
  data1$constellation,
  levels = c('Starlink', 'OneWeb', 'Kuiper'),
  labels = c('Starlink', 'OneWeb', 'Kuiper'))

data1$decile = factor(data1$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 'Decile 2 \n(<957 km²)', 
   'Decile 3 \n(<455 km²)', 'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
   'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 'Decile 8 \n(<39 km²)', 
   'Decile 9 \n(<21 km²)', 'Decile 10 \n(<9 km²)'))

df2 <- data1 %>% select(annualized_per_user_tco_usd, constellation, decile)

df2 = df2 %>%
  group_by(constellation, decile) %>%
  summarize(mean = mean(annualized_per_user_tco_usd),
            sd = sd(annualized_per_user_tco_usd))

label_totals <- df2 %>%
  group_by(constellation, decile) %>%
  summarize(mean_value = sum(mean))

annualized_user_tco_ssa <- ggplot(df2, aes(x = decile, y = mean, fill = constellation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1,
      position = position_dodge(.9), color = 'red',size = 0.5) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
      digits = 2, format = "fg", flag = "#")), color = 'black', size = 3.5, position = 
      position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = 1) +
  labs(colour = NULL, title = "Satellite broadband cost per user results for SSA", 
       x = NULL, y = bquote("Annualized \nTCO per user (US$)")) +
  theme(legend.position = 'bottom',
    axis.text.x = element_text(size = 11),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 11)
  ) + scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 2439)) +
  theme(strip.text = element_text(size = 14))

###################
## 3.3 EMISSIONS ##
###################
data2 <- subset(read.csv(file.path(folder, '..', '..', 'saleos', 'results', 'SSA', 
  'SSA_decile_emissions.csv')), constellation != "geo_generic")

data2$constellation = factor(
  data2$constellation,
  levels = c('starlink', 'oneweb', 'kuiper'),
  labels = c('Starlink', 'OneWeb', 'Kuiper'))

data2$decile = factor(data2$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
     'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
     'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 'Decile 2 \n(<957 km²)', 
     'Decile 3 \n(<455 km²)', 'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
     'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 'Decile 8 \n(<39 km²)', 
     'Decile 9 \n(<21 km²)', 'Decile 10 \n(<9 km²)'))

df4 <- data2 %>% select(annualized_per_user_emissions_kg, constellation, decile)

annualized_user_emissions_ssa <- ggplot(df4, aes(x = decile, 
  y = annualized_per_user_emissions_kg, fill = constellation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
     digits = 3, format = "fg", flag = "#")), color = 'black', size = 3.5, position = 
     position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = 1) +
  labs(colour = NULL, title = "Satellite broadband emissions per user results for SSA", 
       x = NULL, y = bquote("Emissions (kg CO"["2"] ~ " e)")) +
  theme(legend.position = 'bottom',
    axis.text.x = element_text(size = 11),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 11)
  ) + scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 1049)) +
  theme(strip.text = element_text(size = 14))

satellite_model <- ggarrange(per_user_capacity, annualized_user_tco_ssa, 
   annualized_user_emissions_ssa, ncol = 1, nrow = 3, align = c('hv'),
   common.legend = TRUE, legend='bottom') 

path = file.path(folder, 'dissertation_figures', 'satellite_per_user_results.png')
png(path, units="in", width=9, height=9, res=300)
print(satellite_model)
dev.off()






