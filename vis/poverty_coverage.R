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

############################################
##SSA Absolute Poor Population by Regions ##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poverty_results.csv'))
names(data)[names(data) == "GID_1"] <- "GID_2"
gid_pop <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                                 'SSA_subregional_population_deciles.csv'))
gid_pop <- gid_pop[, c("GID_2", "decile")]
data <- merge(data, gid_pop, by = "GID_2")

data = data %>%
  group_by(poverty_range, decile) %>%
  summarize(poor_pops = sum(poor_population))

df = data %>%
  group_by(poverty_range, decile) %>%
  summarize(poor_pop = sum(poor_pops))

df$decile = factor(df$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 per km²)', 
   'Decile 2 \n(456 - 957 per km²)', 'Decile 3 \n(273 - 455 per km²)', 
   'Decile 4 \n(172 - 272 per km²)', 'Decile 5 \n(107 - 171 per km²)', 
   'Decile 6 \n(64 - 106 per km²)', 'Decile 7 \n(40 - 63 per km²)', 
   'Decile 8 \n(22 - 39 per km²)', 'Decile 9 \n(10 - 21 per km²)', 
   'Decile 10 \n(<9 per km²)'))

df$poverty_range = factor(
  df$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 
             'Below $US 5.5'))

poor_population_region <-
  ggplot(df,  aes(x = decile, y = poor_pop/1e6, fill = poverty_range)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) + coord_flip() + 
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
                                digits = 3, format = "fg", flag = "#")),
            size = 2, position = position_dodge(0.9),
            vjust = 0.5, hjust = -0.3) +
  labs(colour = NULL,
       title = 'SSA Population below poverty line.',
       subtitle = '(a) Absolute Population',
       x = NULL,
       y = 'Poor Population (in millions)',
       fill = NULL) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 5),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11, face = 'bold'),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        axis.title.x = element_text(size = 8)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Poverty Rate')) +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 200))

############################################
##SSA Relative Poor Population by Regions ##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poverty_results.csv'))
names(data)[names(data) == "GID_1"] <- "GID_2"
gid_pop <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                              'SSA_subregional_population_deciles.csv'))
gid_pop <- gid_pop[, c("GID_2", "decile")]
data <- merge(data, gid_pop, by = "GID_2")

data = data %>%
  group_by(poverty_range, decile) %>%
  summarize(poor_pops = sum(poor_population),
            total_pops = sum(population))

df = data %>%
  group_by(poverty_range, decile) %>%
  summarize(poor_pop = sum(poor_pops),
            total_pop = sum(total_pops))

df$decile = factor(df$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 per km²)', 
   'Decile 2 \n(456 - 957 per km²)', 'Decile 3 \n(273 - 455 per km²)', 
   'Decile 4 \n(172 - 272 per km²)', 'Decile 5 \n(107 - 171 per km²)', 
   'Decile 6 \n(64 - 106 per km²)', 'Decile 7 \n(40 - 63 per km²)', 
   'Decile 8 \n(22 - 39 per km²)', 'Decile 9 \n(10 - 21 per km²)', 
   'Decile 10 \n(<9 per km²)'))

df$poverty_range = factor(
  df$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 
             'Below $US 5.5'))

df$perc = (df$poor_pop / df$total_pop) * 100

relative_region_poor_population <-
  ggplot(df,  aes(x = decile, y = perc, fill = poverty_range)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) +  coord_flip() +
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
                                digits = 3,format = 'fg', flag = '#')), size = 2,
            position = position_dodge(0.9), vjust = 0.5, hjust = -0.1) +
  labs(colour = NULL, title = ' ',
       subtitle = '(b) Relative Population.', x = NULL,
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
  guides(fill = guide_legend(ncol = 5, title = 'Poverty Rate')) +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0), labels = function(y)
    format(y, scientific = FALSE), limits = c(0, 100)) 

##########################################
##SSA Poor Population below poverty map ##
##########################################
africa_data <- st_read(file.path(folder, '..', 'data', 'raw', 
                                 'Africa_Boundaries', 'SSA_combined_shapefile.shp'))
africa_shp <- africa_data %>%
  select(GID_0, NAME_0, GID_1, GID_2, geometry)

new_names <- c('iso3', 'country', 'gid_1', 'GID_1', 'geometry')
colnames(africa_shp) <- new_names
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poverty_results.csv'))

data = data %>%
  group_by(GID_1, poverty_range) %>%
  summarize(poor_pops = sum(poor_population),
            total_pops = sum(population))

data$rel_pop = (data$poor_pops / data$total_pops) * 100

data$poverty_range = factor(
  data$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 'Below $US 5.5'))

merged_data <- merge(africa_shp, data, by = "GID_1")

pop_bins <- c(-Inf, 10, 20, 30, 40, 50, 60, 
              70, 80, 90, Inf)

merged_data <- na.omit(merged_data, cols = "rel_pop")

merged_data$population_bin <- cut(merged_data$rel_pop, breaks = pop_bins, 
    labels = c("Below 10%", "10 - 20%", "21 - 30%", "31 - 40%", 
    "41 - 50%", "51 - 60%", "61 - 70%", "71 - 80%", "81 - 90%", "Above 90%"))

poverty_maps <- ggplot() + 
  geom_sf(data = merged_data, aes(fill = population_bin), 
          linewidth = 0.001,) +
  scale_fill_brewer(palette = "Spectral", direction = -1) +
  labs(title = "(c) Relative population below poverty line in SSA.",
       subtitle = "Aggregated by normalized sub-regional population and grouped by poverty rate.",
       fill = "Range") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 5),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_markdown(size = 6),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7)) + 
  guides(fill = guide_legend(nrow = 2)) + 
  facet_wrap( ~ poverty_range, ncol = 4) +
  guides(fill = guide_legend(ncol = 10))

###################################
##PANEL PLOTS FOR POOR POPULATION##
###################################
poor_panel <- ggarrange(poor_population_region,
  relative_region_poor_population,
  ncol = 2,
  common.legend = TRUE, legend='bottom')

poor_panel_map <- ggarrange(poor_panel,
    poverty_maps,nrow = 2, align = c('hv'),
    common.legend = FALSE, legend='bottom')


path = file.path(folder, 'figures', 'poor_population.png')
png(path, units = "in", width = 10.5, height = 9.5, res = 300)
print(poor_panel_map)
dev.off()

########################
##UNCOVERED POPULATION##
########################

#################################################
##SSA Absolute Uncovered Population by deciles ##
#################################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                           'SSA_unconnected_mapping_results.csv'))
names(data)[names(data) == "GID_1"] <- "GID_2"
gid_pop <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                              'SSA_subregional_population_deciles.csv'))
data <- merge(data, gid_pop, by = "GID_2")

df = data %>%
  group_by(technology, decile) %>%
  summarize(pop_unconnected = sum(pop_unconnected))

df$technology = factor(
  df$technology,
  levels = c('GSM', '3G', '4G'),
  labels = c('2G', '3G', '4G'))

df$decile = factor(df$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 per km²)', 
   'Decile 2 \n(456 - 957 per km²)', 'Decile 3 \n(273 - 455 per km²)', 
   'Decile 4 \n(172 - 272 per km²)', 'Decile 5 \n(107 - 171 per km²)', 
   'Decile 6 \n(64 - 106 per km²)', 'Decile 7 \n(40 - 63 per km²)', 
   'Decile 8 \n(22 - 39 per km²)', 'Decile 9 \n(10 - 21 per km²)', 
   'Decile 10 \n(<9 per km²)'))

unconnected_population_geotype <-
  ggplot(df,  aes(x = decile, y = pop_unconnected/1e6, fill = technology)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) + coord_flip() + 
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
                                digits = 3, format = "fg", flag = "#")),
            size = 3.5, position = position_dodge(0.9),
            vjust = 0.5, hjust = -0.3) +
  labs(colour = NULL,
       title = 'Uncovered population in SSA.',
       subtitle = '(a) Absolute population by deciles',
       x = NULL,
       y = 'Uncovered Population (in millions)',
       fill = NULL) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 5),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 9, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        axis.title.x = element_text(size = 8)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Technology')) +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 149))

#################################################
##SSA Relative Uncovered population by geotypes##
#################################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                           'SSA_unconnected_mapping_results.csv'))
names(data)[names(data) == "GID_1"] <- "GID_2"
gid_pop <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                              'SSA_subregional_population_deciles.csv'))
data <- merge(data, gid_pop, by = "GID_2")

df = data %>%
  group_by(technology, decile) %>%
  summarize(pop_unconnected = sum(pop_unconnected),
            total_pop = sum(population))

df$perc = (df$pop_unconnected / df$total_pop) * 100

df$technology = factor(
  df$technology,
  levels = c('GSM', '3G', '4G'),
  labels = c('2G', '3G', '4G')
)

df$decile = factor(df$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 per km²)', 
   'Decile 2 \n(456 - 957 per km²)', 'Decile 3 \n(273 - 455 per km²)', 
   'Decile 4 \n(172 - 272 per km²)', 'Decile 5 \n(107 - 171 per km²)', 
   'Decile 6 \n(64 - 106 per km²)', 'Decile 7 \n(40 - 63 per km²)', 
   'Decile 8 \n(22 - 39 per km²)', 'Decile 9 \n(10 - 21 per km²)', 
   'Decile 10 \n(<9 per km²)'))

relative_geo_tech_uncovered_population <-
  ggplot(df,  aes(x = decile, y = perc, fill = technology)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) +  coord_flip() +
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
                                digits = 3,format = 'fg', flag = '#')), size = 3.5,
            position = position_dodge(0.9), vjust = 0.5, hjust = -0.1)+
  labs(colour = NULL, title = ' ',
       subtitle = '(b) Relative population by deciles', x = NULL,
       y = 'Percentage of Population (%)', fill = NULL) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 5),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11, face = 'bold'),
        plot.subtitle = element_text(size = 8),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        axis.title.x = element_text(size = 8)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Phone Technology')) +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0), labels = function(y)
    format(y, scientific = FALSE), limits = c(0, 109))

#################################################
##UNCONNECTED POPULATION BY CELLULAR TECHNOLOGY##
#################################################
africa_data <- st_read(file.path(folder, '..', 'data', 'raw', 'Africa_Boundaries', 'SSA_combined_shapefile.shp'))
africa_shp <- africa_data %>%
  select(GID_0, NAME_0, GID_1, GID_2, geometry)

new_names <- c('iso3', 'country', 'gid_1', 'GID_1', 'geometry')
colnames(africa_shp) <- new_names

######################################
##Uncovered Population by Technology##
######################################
data1 <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                           'SSA_unconnected_mapping_results.csv'))
pop <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                           'SSA_total_population.csv'))
pop <- pop %>% select('GID_1', 'population')
data <- merge(data1, pop, by = "GID_1")

data = data %>%
  group_by(GID_1, technology) %>%
  summarize(pop_unconnected = (sum(pop_unconnected)),
            total_pops = sum(population))

data$rel_pop = (data$pop_unconnected / data$total_pops) * 100

data$technology = factor(
  data$technology,
  levels = c('GSM', '3G', '4G'),
  labels = c('2G', '3G', '4G'))

merged_data <- merge(africa_shp, data, by = "GID_1")

pop_bins <- c(-Inf, 10, 20, 30, 40, 50, 60, 
              70, 80, 90, Inf)

#merged_data <- na.omit(merged_data, cols = "rel_pop")

merged_data$population_bin <- cut(merged_data$rel_pop, breaks = pop_bins, 
   labels = c("Below 10%", "10 - 20%", "21 - 30%", "31 - 40%", 
   "41 - 50%", "51 - 60%", "61 - 70%", "71 - 80%", "81 - 90%", "Above 90%"))

uncovered_technology <- ggplot() + 
  geom_sf(data = merged_data, aes(fill = population_bin), 
          linewidth = 0.001,) +
  scale_fill_brewer(palette = "Spectral", direction = -1) +
  labs(title = "(e) Uncovered population in SSA.",
       subtitle = "Aggregated by normalized sub-regional population and grouped by mobile technology.",
       fill = "Population") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 5),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_markdown(size = 6),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7)) + 
  guides(fill = guide_legend(nrow = 2)) + 
  facet_wrap( ~ technology, ncol = 4) +
  guides(fill = guide_legend(ncol = 5))

########################################
##PANEL PLOTS FOR UNCOVERED POPULATION##
########################################
uncovered_geotypes <- ggarrange(unconnected_population_geotype,
   relative_geo_tech_uncovered_population,
   ncol = 2,
   common.legend = TRUE, legend='bottom')

uncovered_panel_map <- ggarrange(uncovered_geotypes,
   uncovered_technology,nrow = 2, align = c('hv'),
   common.legend = FALSE, legend='bottom')

path = file.path(folder, 'figures', 'unconnect_2_3_4_G.png')
png(path, units = "in", width = 10.5, height = 9.5, res = 300)
print(uncovered_panel_map)
dev.off()

################################
##CONNECTION AND POVERTY PLOTS##
################################
africa_data <- st_read(file.path(folder, '..', 'data', 'raw', 'Africa_Boundaries', 'SSA_combined_shapefile.shp'))
africa_shp <- africa_data %>%
  select(GID_0, NAME_0, GID_1, GID_2, geometry)

new_names <- c('iso3', 'country', 'gid_1', 'GID_1', 'geometry')
colnames(africa_shp) <- new_names

############################################
##Poor (Below US$ 1.9) and uncovered by 2G##
############################################
data1 <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data1 <- na.omit(data1)
data1 <- data1[, c("GID_1", "technology", "region", "poverty_range", "poor_unconnected")]
pop <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                          'SSA_total_population.csv'))
pop <- pop %>% select('GID_1', 'population')
data <- merge(data1, pop, by = "GID_1")

data = data %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_uncovered_gid = mean(poor_unconnected),
            population)
data <- data[data$technology == "GSM", ]

data$rel_pop = (data$poor_uncovered_gid / data$population) * 100

data = data %>%
  group_by(GID_1, technology, poverty_range, rel_pop) %>%
  summarize(poor_unconnected = (sum(poor_uncovered_gid)))

data$poverty_range = factor(
  data$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 'Below $US 5.5'))

merged_data <- merge(africa_shp, data, by = "GID_1")
pop_bins <- c(-Inf, 10, 20, 30, 40, 50, 60, 
              70, 80, 90, Inf)

merged_data$population_bin <- cut(merged_data$rel_pop, breaks = pop_bins, 
   labels = c("Below 10%", "10 - 20%", "21 - 30%", "31 - 40%", 
   "41 - 50%", "51 - 60%", "61 - 70%", "71 - 80%", "81 - 90%", "Above 90%"))

uncovered_2g_poor <- ggplot() + 
  geom_sf(data = merged_data, aes(fill = population_bin), 
          linewidth = 0.001,) +
  scale_fill_brewer(palette = "Spectral", direction = -1) +
  labs(title = "Uncovered and below poverty line population.",
       subtitle = "(a) Uncovered by (2G).",
       fill = "Population") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 5),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_markdown(size = 6),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7)) + 
  guides(fill = guide_legend(nrow = 2)) + 
  facet_wrap( ~ poverty_range, ncol = 3) +
  guides(fill = guide_legend(ncol = 5))

############################
##Poor and Uncovered by 3G##
############################
data1 <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data1 <- na.omit(data1)
data1 <- data1[, c("GID_1", "technology", "region", "poverty_range", "poor_unconnected")]
pop <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                          'SSA_total_population.csv'))
pop <- pop %>% select('GID_1', 'population')
data <- merge(data1, pop, by = "GID_1")

data = data %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_uncovered_gid = mean(poor_unconnected),
            population)
data <- data[data$technology == "3G", ]

data$rel_pop = (data$poor_uncovered_gid / data$population) * 100

data = data %>%
  group_by(GID_1, technology, poverty_range, rel_pop) %>%
  summarize(poor_unconnected = (sum(poor_uncovered_gid)))

data$poverty_range = factor(
  data$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 'Below $US 5.5'))

merged_data <- merge(africa_shp, data, by = "GID_1")
pop_bins <- c(-Inf, 10, 20, 30, 40, 50, 60, 
              70, 80, 90, Inf)

merged_data$population_bin <- cut(merged_data$rel_pop, breaks = pop_bins, 
   labels = c("Below 10%", "10 - 20%", "21 - 30%", "31 - 40%", 
   "41 - 50%", "51 - 60%", "61 - 70%", "71 - 80%", "81 - 90%", "Above 90%"))

uncovered_3g_poor <- ggplot() + 
  geom_sf(data = merged_data, aes(fill = population_bin), 
          linewidth = 0.001,) +
  scale_fill_brewer(palette = "Spectral", direction = -1) +
  labs(title = ' ',
       subtitle = "(b) Uncovered by (3G).",
       fill = "Population") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 5),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_markdown(size = 6),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7)) + 
  guides(fill = guide_legend(nrow = 2)) + 
  facet_wrap( ~ poverty_range, ncol = 3) +
  guides(fill = guide_legend(ncol = 5))

############################
##Poor and Uncovered by 4G##
############################
data1 <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data1 <- na.omit(data1)
data1 <- data1[, c("GID_1", "technology", "region", "poverty_range", "poor_unconnected")]
pop <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                          'SSA_total_population.csv'))
pop <- pop %>% select('GID_1', 'population')
data <- merge(data1, pop, by = "GID_1")

data = data %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_uncovered_gid = mean(poor_unconnected),
            population)
data <- data[data$technology == "4G", ]
data$rel_pop = (data$poor_uncovered_gid / data$population) * 100

data = data %>%
  group_by(GID_1, technology, poverty_range, rel_pop) %>%
  summarize(poor_unconnected = (sum(poor_uncovered_gid)))

data$poverty_range = factor(
  data$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 'Below $US 5.5'))

merged_data <- merge(africa_shp, data, by = "GID_1")
pop_bins <- c(-Inf, 10, 20, 30, 40, 50, 60, 
              70, 80, 90, Inf)

merged_data$population_bin <- cut(merged_data$rel_pop, breaks = pop_bins, 
   labels = c("Below 10%", "10 - 20%", "21 - 30%", "31 - 40%", 
   "41 - 50%", "51 - 60%", "61 - 70%", "71 - 80%", "81 - 90%", "Above 90%"))

uncovered_4g_poor <- ggplot() + 
  geom_sf(data = merged_data, aes(fill = population_bin), 
          linewidth = 0.001,) +
  scale_fill_brewer(palette = "Spectral", direction = -1) +
  labs(title =' ',
       subtitle = "(c) Uncovered by (4G).",
       fill = "Population") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 5),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_markdown(size = 6),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7)) + 
  guides(fill = guide_legend(nrow = 2)) + 
  facet_wrap( ~ poverty_range, ncol = 3) +
  guides(fill = guide_legend(ncol = 5))


##########################################################
##SSA Absolute Uncovered and Poor Population by Regions ##
##########################################################
data1 <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data1 <- na.omit(data1)
data1 <- data1[, c("GID_1", "technology", "region", "poverty_range", "poor_unconnected")]
data1 = data1 %>%
  group_by(GID_1, technology, region, poverty_range) %>%
  summarize(poor_uncovered_gid = mean(poor_unconnected))

gid_pop <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                              'SSA_subregional_population_deciles.csv'))
data1 <- merge(data1, gid_pop, by = "GID_1")

df = data1 %>%
  group_by(technology, decile, poverty_range) %>%
  summarize(poor_uncovered = sum(poor_uncovered_gid)/1e3)

df$technology = factor(
  df$technology,
  levels = c('GSM', '3G', '4G'),
  labels = c('2G', '3G', '4G'))

df$poverty_range = factor(
  df$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 'Below $US 5.5'))

df$decile = factor(df$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 per km²)', 
   'Decile 2 \n(456 - 957 per km²)', 'Decile 3 \n(273 - 455 per km²)', 
   'Decile 4 \n(172 - 272 per km²)', 'Decile 5 \n(107 - 171 per km²)', 
   'Decile 6 \n(64 - 106 per km²)', 'Decile 7 \n(40 - 63 per km²)', 
   'Decile 8 \n(22 - 39 per km²)', 'Decile 9 \n(10 - 21 per km²)', 
   'Decile 10 \n(<9 per km²)'))

uncovered_poor_population <-
  ggplot(df,  aes(x = decile, y = poor_uncovered, fill = technology)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) + coord_flip() + 
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
                                digits = 3, format = "fg", flag = "#")),
            size = 3, position = position_dodge(0.9),
            vjust = 0.5, hjust = -0.3) +
  labs(colour = NULL,
       title = 'Uncovered and population below poverty line.',
       subtitle = '(a) Absolute population.',
       x = NULL,
       y = 'Uncovered and population below poverty line (`000`)',
       fill = NULL) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 5),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 9, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        axis.title.x = element_text(size = 8)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Technology')) +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 12000)) + 
  facet_wrap( ~ poverty_range, ncol = 5) 

##########################################################
##SSA Relative Uncovered and Poor Population by Regions ##
##########################################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data <- na.omit(data)
names(data)[names(data) == "GID_1"] <- "GID_2"
gid_pop <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                              'SSA_subregional_population_deciles.csv'))
data <- merge(data, gid_pop, by = "GID_2")

data$perc = (data$poor_unconnected / data$population) * 100

df = data %>%
  distinct(technology, decile, poverty_range, perc) %>%
  group_by(technology, decile, poverty_range) %>%
  summarize(mean_perc = mean(perc))

df$technology = factor(
  df$technology,
  levels = c('GSM', '3G', '4G'),
  labels = c('2G', '3G', '4G'))

df$poverty_range = factor(
  df$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 'Below $US 5.5'))

df$decile = factor(df$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 per km²)', 
   'Decile 2 \n(456 - 957 per km²)', 'Decile 3 \n(273 - 455 per km²)', 
   'Decile 4 \n(172 - 272 per km²)', 'Decile 5 \n(107 - 171 per km²)', 
   'Decile 6 \n(64 - 106 per km²)', 'Decile 7 \n(40 - 63 per km²)', 
   'Decile 8 \n(22 - 39 per km²)', 'Decile 9 \n(10 - 21 per km²)', 
   'Decile 10 \n(<9 per km²)'))

relative_uncovered_poor_population <-
  ggplot(df,  aes(x = decile, y = mean_perc, fill = technology)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) +  coord_flip() +
  geom_text(aes(label = formatC(signif(after_stat(y), 3), 
                                digits = 3,format = 'fg', flag = '#')), size = 3,
            position = position_dodge(0.9), vjust = 0.5, hjust = -0.1) +
  labs(colour = NULL, title = ' ', subtitle = '(b) Relative population.', 
       x = NULL,
       y = 'Percentage of uncovered and population below poverty line (%)', 
       fill = NULL) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 5),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 8),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        axis.title.x = element_text(size = 8)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Phone Technology')) +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0), labels = function(y)
    format(y, scientific = FALSE), limits = c(0, 109)) +
  facet_wrap( ~ poverty_range, ncol = 3) 


####################################################
##PANEL PLOTS FOR POOR AND 3G UNCOVERED POPULATION##
####################################################
relative_poor <- ggarrange(uncovered_poor_population,
  relative_uncovered_poor_population,
  nrow = 2,
  common.legend = TRUE, legend = 'bottom')

uncovered_poor <- ggarrange(uncovered_2g_poor,
   uncovered_3g_poor,
   uncovered_4g_poor,
   nrow = 3,
   common.legend = FALSE, legend = 'bottom')

path = file.path(folder, 'figures', 'poor_and_uncovered.png')
png(path, units = "in", width = 9.5, height = 8, res = 300)
print(relative_poor)
dev.off()

path = file.path(folder, 'figures', 'poor_and_uncovered_maps.png')
png(path, units = "in", width = 7, height = 9, res = 300)
print(uncovered_poor)
dev.off()

########################
## SATELLITE COVERAGE ##
########################
africa_data <- st_read(file.path(folder, '..', 'data', 'raw', 'Africa_Boundaries', 'SSA_combined_shapefile.shp'))
africa_shp <- africa_data %>%
  select(GID_0, NAME_0, GID_1, GID_2, geometry)

new_names <- c('iso3', 'country', 'gid_1', 'GID_1', 'geometry')
colnames(africa_shp) <- new_names

data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'satellite_coverage.csv'))
data <- na.omit(data)
data <- data[, c('GID_1', 'constellation', 'user_capacity_mbs_per_user')]
data = data %>%
  distinct(GID_1, constellation, user_capacity_mbs_per_user,.keep_all = TRUE) %>%
  group_by(GID_1, constellation) %>%
  summarize(sat_coverage = mean(user_capacity_mbs_per_user))

data$constellation = factor(
  data$constellation,
  levels = c('Starlink', 'OneWeb', 'Kuiper', 'GEO'))

merged_data <- merge(africa_shp, data, by = "GID_1")
cap_bins <- c(-Inf, 0.5, 1, 5, 10, 15, 25, 
              35, 45, 50, Inf)

merged_data$capacity_bin <- cut(merged_data$sat_coverage, breaks = cap_bins, 
  labels = c("Below 0.5 Mbps", "0.6 - 1 Mbps", "1.1 - 5 Mbps", "5.1 - 10 Mbps", 
             "10.1 - 15 Mbps", "15.1 - 25 Mps", "25.1 - 35 Mbps", 
             "35.1 - 45 Mbps", "45 - 50 Mbps", "Above 50 Mbps"))

satellite_coverage <- ggplot() + 
  geom_sf(data = africa_data, fill = "maroon", color = "black", linewidth = 0.01) +
  geom_sf(data = merged_data, aes(fill = capacity_bin), 
          linewidth = 0.001,) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title ='(a) Average per user capacities',
       subtitle = "Estimated satellite capacity per user for population uncovered by 2G mobile signal and living below US$ 1.9 per day.",
       fill = "Capacity per User") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 5),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_markdown(size = 6),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7)) + 
  guides(fill = guide_legend(nrow = 2)) + 
  facet_wrap( ~ constellation, ncol = 2) +
  guides(fill = guide_legend(ncol = 5)) 

path = file.path(folder, 'figures', 'satellite_coverage.png')
png(path, units = "in", width = 9, height = 8, res = 300)
print(satellite_coverage)
dev.off()

