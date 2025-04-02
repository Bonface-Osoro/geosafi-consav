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

####################
## SIGNAL RESULTS ##
####################
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
        title = "A",x = "Inter-Site Distance (km)",
        y = "Path Loss (dB)",
        fill = "Frequency") + 
  scale_color_brewer(palette = "Dark2") +
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
  guides(color = guide_legend(ncol = 6))


####################
## Received power ##
####################
df2 = select(data, received_power_db, frequency_mhz, 
              continuous)

df2$continuous = as.numeric(df2$continuous)
df2 = df2 %>%
  group_by(frequency_mhz, continuous) %>%
  summarise(
    mean = mean(received_power_db),
    sd = sd(received_power_db))

Received_Power <- ggplot(df2, aes(continuous, mean, color = frequency_mhz)) + 
  geom_line(position = position_dodge(width = 0.5), size = 0.5) + 
  geom_point(size = 0.3, position=position_dodge(0.5)) +
  labs( colour = NULL,
        title = "B",x = "Inter-Site Distance (km)",
        y = "Received Power (dB)",
        fill = "Frequency") + 
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
        title = "C",x = "Inter-Site Distance (km)",
        y = "SINR (dB)",
        fill = "Frequency") + 
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

#########################
## Spectral Efficiency ##
#########################
df4 = select(data, spectral_efficiency_bpshz, frequency_mhz, 
              continuous)

df4$continuous = as.numeric(df4$continuous)
df4 = df4 %>%
  group_by(frequency_mhz, continuous) %>%
  summarise(
    mean = mean(spectral_efficiency_bpshz),
    sd = sd(spectral_efficiency_bpshz))

spectral_efficiency <- ggplot(df4, aes(continuous, mean, color = frequency_mhz)) + 
  geom_line(position = position_dodge(width = 0.5), size = 0.5) + 
  geom_point(size = 0.3, position=position_dodge(0.5)) +
  labs( colour = NULL,
        title = "D",x = "Inter-Site Distance (km)",
        y = "Spectral Efficiency \n(Bps/Hz)",
        fill = "Frequency") + 
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

spectral_pdf <- ggplot(data, aes(x = spectral_efficiency_bpshz, color = frequency_mhz)) + 
geom_density(position = position_dodge(width = 0.5), size = 0.5) +
labs(title = "E", 
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


######################
## CAPACITY RESULTS ##
######################
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

#######################
## Capacity per user ##
#######################
capacity_per_user <- ggplot(df, aes(continuous, mean, color = frequency_mhz)) + 
  geom_line(position = position_dodge(width = 0.5), size = 0.5) + 
  geom_point(size = 0.3, position=position_dodge(0.5)) +
  labs( colour = NULL,
        title = "F",x = "Inter-Site Distance (km)",
        y = "Capacity per user (Mbps)",
        fill = "Frequency") + 
  scale_color_brewer(palette = "Dark2") +
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
  guides(color = guide_legend(ncol = 6))

#####################
## Number of Sites ##
#####################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_number_of_sites.csv'))
data$decile = factor(data$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
     'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
     'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 'Decile 2 \n(<957 km²)', 
     'Decile 3 \n(<455 km²)', 'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
     'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 'Decile 8 \n(<39 km²)', 
     'Decile 9 \n(<21 km²)', 'Decile 10 \n(<9 km²)'))

data <- data %>%
  filter(frequency_mhz %in% c(700, 800, 1800,2600, 3500, 5800))

data$frequency_mhz = factor(
  data$frequency_mhz,
  levels = c(700, 800, 1800, 2600, 3500, 5800),
  labels = c('0.7 GHz (5G)', '0.8 GHz (4G)', '1.8 GHz (4G)',
             '2.6 GHz (4G)', '3.5 GHz (5G)', '5.8 GHz (5G)'))

df = data %>%
  group_by(frequency_mhz, decile) %>%
  summarize(mean = mean(number_of_sites),
            sd = sd(number_of_sites))

number_of_sites <- ggplot(df, aes(x = decile, y = mean, fill = frequency_mhz)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'red',size = 0.2) + 
  geom_text(aes(label = formatC(signif(after_stat(y), 4), 
     digits = 2, format = "d", flag = "#")), color = 'black', size = 3, position = 
     position_dodge(0.9), vjust = .5, hjust = -0.1, angle = 90) +
  scale_fill_viridis_d(direction = -1) +
  labs(colour = NULL, title = "D",x = NULL, y = "Number of New Sites") +
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
  guides(fill = guide_legend(ncol = 5, title = 'Frequency')) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Frequency')) +
  scale_x_discrete(expand = c(0, 0)) +   
  scale_y_continuous(expand = c(0, 0), 
  labels = function(y) format(y, scientific = FALSE), 
  limits = c(0, 750)) 

#################
## Panel Plots ##
#################
signal_plots <- ggarrange(path_loss, Received_Power, sinr_db, 
    spectral_efficiency, spectral_pdf, capacity_per_user, nrow = 3, ncol = 2, 
    common.legend = TRUE, legend='bottom') 

path = file.path(folder, 'figures', 'signal_plots.png')
png(path, units="in", width=7, height=8, res=300)
print(signal_plots)
dev.off()







