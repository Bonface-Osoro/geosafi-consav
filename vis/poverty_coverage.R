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

#################################################
##SSA Absolute Uncovered Population by Regions ##
#################################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_tech_reg.csv'))

df = data %>%
  group_by(technology, region) %>%
  summarize(pop_unconnected = sum(pop_unconnected))

df$technology = factor(
  df$technology,
  levels = c('GSM', '3G', '4G'),
  labels = c('2G', '3G', '4G'))

unconnected_population_region <-
  ggplot(df,  aes(x = region, y = pop_unconnected/1e6, fill = technology)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) + coord_flip() + 
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
                                digits = 3, format = "fg", flag = "#")),
            size = 1.8, position = position_dodge(0.9),
            vjust = 0.5, hjust = -0.3) +
  labs(colour = NULL,
       title = ' ',
       subtitle = '(D) Absolute population by cellular technology and regions.',
       x = NULL,
       y = 'Uncovered Population (in millions)',
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
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Technology')) +
  scale_fill_viridis_d(direction = 1) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 400))

#################################################
##SSA Absolute Uncovered Population by Geotypes##
#################################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_tech_geo.csv'))

df = data %>%
  group_by(technology, geotype) %>%
  summarize(pop_unconnected = sum(pop_unconnected))

df$technology = factor(
  df$technology,
  levels = c('GSM', '3G', '4G'),
  labels = c('2G', '3G', '4G'))

df$geotype = factor(
  df$geotype,
  levels = c('remote', 'rural', 'suburban', 'urban'),
  labels = c('Remote', 'Rural', 'Suburban', 'Urban')
)

unconnected_population_geotype <-
  ggplot(df,  aes(x = geotype, y = pop_unconnected/1e6, fill = technology)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) + coord_flip() + 
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
     digits = 3, format = "fg", flag = "#")),
     size = 1.8, position = position_dodge(0.9),
     vjust = 0.5, hjust = -0.3) +
  labs(colour = NULL,
       title = ' ',
       subtitle = '(B) Absolute population by cellular technology and geotypes',
       x = NULL,
       y = 'Uncovered Population (in millions)',
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
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Technology')) +
  scale_fill_viridis_d(direction = 1) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 600))


###################################################
##SSA Relative Unconnected Population by Regions ##
###################################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_tech_reg.csv'))
country_info = read_csv(file.path(folder, '..', 'data','raw', 'countries.csv'))
data = merge(data, country_info, by = 'iso3')

df = data %>%
  distinct(technology, region, pop_unconnected, population, .keep_all = TRUE) %>%
  group_by(technology, region) %>%
  summarize(pop_unconnected = sum(pop_unconnected),
            total_pop = sum(population))

df$perc = (df$pop_unconnected / df$total_pop) * 100

df$technology = factor(
  df$technology,
  levels = c('GSM', '3G', '4G'),
  labels = c('2G', '3G', '4G')
)

relative_region_uncovered_population <-
  ggplot(df,  aes(x = region, y = perc, fill = technology)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) +  coord_flip() +
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
     digits = 3,format = 'fg', flag = '#')), size = 1.8,
     position = position_dodge(0.9), vjust = 0.5, hjust = -0.1) +
  labs(colour = NULL, title = ' ',
       subtitle = '(C) Relative population by cellular technology and regions', x = NULL,
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
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Phone Technology')) +
  scale_fill_viridis_d(direction = 1) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0), labels = function(y)
    format(y, scientific = FALSE), limits = c(0, 100))


#################################################
##SSA Relative Uncovered population by geotypes##
#################################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_tech_geo.csv'))
country_info = read_csv(file.path(folder, '..', 'data','raw', 'countries.csv'))
data = merge(data, country_info, by = 'iso3')

df = data %>%
  distinct(technology, pop_unconnected, geotype, population, .keep_all = TRUE) %>%
  group_by(technology, geotype) %>%
  summarize(pop_unconnected = sum(pop_unconnected),
            total_pop = sum(population))

df$perc = (df$pop_unconnected / df$total_pop) * 100

df$technology = factor(
  df$technology,
  levels = c('GSM', '3G', '4G'),
  labels = c('2G', '3G', '4G')
)

df$geotype = factor(
  df$geotype,
  levels = c('remote', 'rural', 'suburban', 'urban'),
  labels = c('Remote', 'Rural', 'Suburban', 'Urban')
)

relative_geo_tech_uncovered_population <-
  ggplot(df,  aes(x = geotype, y = perc, fill = technology)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) +  coord_flip() +
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
                                digits = 3,format = 'fg', flag = '#')), size = 1.8,
            position = position_dodge(0.9), vjust = 0.5, hjust = -0.1)+
  labs(colour = NULL, title = 'SSA Relative and Absolute Uncovered Population',
       subtitle = '(A) Relative population by cellular technology and geotypes.', x = NULL,
       y = 'Percentage of Population (%)', fill = NULL) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 5),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11, face = 'bold'),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        axis.title.x = element_text(size = 8)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Phone Technology')) +
  scale_fill_viridis_d(direction = 1) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0), labels = function(y)
    format(y, scientific = FALSE), limits = c(0, 55))


#################################################
##PANEL PLOTS FOR POOR & UNCONNECTED POPULATION##
#################################################
poor_panel <- ggarrange(poor_population_region,
   relative_region_poor_population,
   ncol = 2, nrow = 1, align = c('hv'),
   common.legend = TRUE, legend='bottom')

unconnected_panel <- ggarrange(relative_geo_tech_uncovered_population,
   unconnected_population_geotype,
   relative_region_uncovered_population,
   unconnected_population_region,
   ncol = 2, nrow = 2, align = c('hv'),
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

path = file.path(folder, 'figures', 'uncovered_population.png')
png(path, units="in", width=9, height=8, res=300)
print(unconnected_panel)
dev.off()



