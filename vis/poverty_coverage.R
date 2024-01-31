library(ggpubr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(readr)

suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

############################################
##SSA Absolute Poor Population by Regions ##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poverty_results.csv'))

data = data %>%
  distinct(iso3, poverty_range, region, population, .keep_all = TRUE) %>%
  group_by(iso3, poverty_range, region) %>%
  summarize(poor_pops = sum(poor_population),
            total_pops = sum(population))

df = data %>%
  group_by(poverty_range, region) %>%
  summarize(poor_pop = sum(poor_pops),
            total_pop = sum(total_pops))

df$poverty_range = factor(
  df$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 
             'Below $US 5.5'))

poor_population_region <-
  ggplot(df,  aes(x = region, y = poor_pop/1e6, fill = poverty_range)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) + coord_flip() + 
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
                                digits = 3, format = "fg", flag = "#")),
            size = 1.8, position = position_dodge(0.9),
            vjust = 0.5, hjust = -0.3) +
  labs(colour = NULL,
       title = '(A) SSA Poor Population.',
       subtitle = 'By poverty rate and region.',
       x = NULL,
       y = 'Poor Population (in millions)',
       fill = NULL) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 5),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        axis.title.x = element_text(size = 8)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Poverty Rate')) +
  scale_fill_viridis_d(direction = 1) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 360))

############################################
##SSA Relative Poor Population by Regions ##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poverty_results.csv'))

data = data %>%
  distinct(iso3, poverty_range, region, population, .keep_all = TRUE) %>%
  group_by(iso3, poverty_range, region) %>%
  summarize(poor_pops = sum(poor_population),
            total_pops = sum(population))

df = data %>%
  group_by(poverty_range, region) %>%
  summarize(poor_pop = sum(poor_pops),
            total_pop = sum(total_pops))

df$poverty_range = factor(
  df$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 
             'Below $US 5.5'))

df$perc = (df$poor_pop / df$total_pop) * 100

relative_region_poor_population <-
  ggplot(df,  aes(x = region, y = perc, fill = poverty_range)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) +  coord_flip() +
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
                                digits = 3,format = 'fg', flag = '#')), size = 1.8,
            position = position_dodge(0.9), vjust = 0.5, hjust = -0.1) +
  labs(colour = NULL, title = '(B) Relative Poor Population in SSA.',
       subtitle = 'By poverty rate and SSA region.', x = NULL,
       y = 'Percentage of Population (%)', fill = NULL) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 5),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        axis.title.x = element_text(size = 8)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Poverty Rate')) +
  scale_fill_viridis_d(direction = 1) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0), labels = function(y)
  format(y, scientific = FALSE), limits = c(0, 100)) 

############################################
##SSA Absolute Poor Population by Regions ##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_results.csv'))

data = data %>%
  distinct(iso3, technology, region, population, .keep_all = TRUE) %>%
  group_by(iso3, technology, region) %>%
  summarize(pop_unconnects = sum(pop_unconnected),
            total_pops = sum(population))

df = data %>%
  group_by(technology, region) %>%
  summarize(pop_unconnect = sum(pop_unconnects),
            total_pop = sum(total_pops))

df$technology = factor(
  df$technology,
  levels = c('GSM', '3G', '4G'),
  labels = c('2G', '3G', '4G'))

df$unconnected_pop = (df$total_pop - df$pop_unconnect) 

unconnected_population_region <-
  ggplot(df,  aes(x = region, y = unconnected_pop/1e6, fill = technology)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) + coord_flip() + 
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
                                digits = 3, format = "fg", flag = "#")),
            size = 1.8, position = position_dodge(0.9),
            vjust = 0.5, hjust = -0.3) +
  labs(colour = NULL,
       title = '(C) SSA Unconnected Population.',
       subtitle = 'By cellular technology and region.',
       x = NULL,
       y = 'Poor Population (in millions)',
       fill = NULL) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 5),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        axis.title.x = element_text(size = 8)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Cellular Technology')) +
  scale_fill_viridis_d(direction = 1) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 110))

###################################################
##SSA Relative Unconnected Population by Regions ##
###################################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_results.csv'))

data = data %>%
  distinct(iso3, technology, region, population, .keep_all = TRUE) %>%
  group_by(iso3, technology, region) %>%
  summarize(pop_unconnects = sum(pop_unconnected),
            total_pops = sum(population))

df = data %>%
  group_by(technology, region) %>%
  summarize(pop_unconnect = sum(pop_unconnects),
            total_pop = sum(total_pops))

df$technology = factor(
  df$technology,
  levels = c('GSM', '3G', '4G'),
  labels = c('2G', '3G', '4G'))

df$unconnected_pop = (df$total_pop - df$pop_unconnect) 
df$perc = (df$unconnected_pop / df$total_pop) * 100

relative_region_unconnected_population <-
  ggplot(df,  aes(x = region, y = perc, fill = technology)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) +  coord_flip() +
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
                                digits = 3,format = 'fg', flag = '#')), size = 1.8,
            position = position_dodge(0.9), vjust = 0.5, hjust = -0.1) +
  labs(colour = NULL, title = '(D) Relative Unconnected Population in SSA.',
       subtitle = 'By cellular technology and SSA region.', x = NULL,
       y = 'Percentage of Population (%)', fill = NULL) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 5),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        axis.title.x = element_text(size = 8)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Cellular Technology')) +
  scale_fill_viridis_d(direction = 1) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0), labels = function(y)
    format(y, scientific = FALSE), limits = c(0, 100)) 

#################################################
##PANEL PLOTS FOR POOR & UNCONNECTED POPULATION##
#################################################
poor_panel <- ggarrange(poor_population_region,
   relative_region_poor_population,
   ncol = 2, nrow = 1, align = c('hv'),
   common.legend = TRUE, legend='bottom')

unconnected_panel <- ggarrange(unconnected_population_region,
   relative_region_unconnected_population,
   ncol = 2, nrow = 1, align = c('hv'),
   common.legend = TRUE, legend='bottom')

panel_poor_unconnected_population <- ggarrange(
  poor_panel,
  unconnected_panel,
  nrow = 2, align = c('hv'),
  common.legend = TRUE, legend='bottom',
  heights=c(.6,1))

path = file.path(folder, 'figures', 'poverty_unconnected_region.png')
png(path, units="in", width=8.3, height=7, res=300)
print(panel_poor_unconnected_population)
dev.off()




