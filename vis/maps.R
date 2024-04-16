library(ggpubr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(sf)
library(readr)
library(RColorBrewer)

suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

africa_data <- st_read(file.path(folder, '..', 'data', 'raw', 'Africa_Boundaries', 'SSA_combined_shapefile.shp'))
africa_shp <- africa_data %>%
  select(GID_0, NAME_0, GID_1, GID_2, geometry)

new_names <- c('iso3', 'country', 'gid_1', 'GID_1', 'geometry')
colnames(africa_shp) <- new_names

###############################################
##SSA Absolute Poor Population below poverty ##
###############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poverty_results.csv'))
#data <- data[data$poverty_range == "GSAP2_poor", ]

data = data %>%
  group_by(GID_1, poverty_range, region) %>%
  summarize(poor_pops = sum(poor_population))

data$poverty_range = factor(
  data$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 'Below $US 5.5'))

merged_data <- merge(africa_shp, data, by = "GID_1")

pop_bins <- c(-Inf, 200000, 500000, 850000, 1200000, 1600000, 2000000, 
              2500000, 3000000, 3500000, Inf)
merged_data$population_bin <- cut(merged_data$poor_pops, breaks = pop_bins, 
    labels = c("Below 200k", "201 - 500k", "501 - 850k", "0.851 - 1.2 Million", 
    "1.21 - 1.6 Million", "1.61 - 2 Million", "2.01 - 2.5 Million", 
    "2.51 - 3 Million", "3.01 - 3.5 Million", "Above 3.5 Million"))

poverty_maps <- ggplot() + 
  geom_sf(data = merged_data, aes(fill = population_bin), 
          linewidth = 0.001,) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Population below poverty level in SSA.",
       subtitle = "Aggregated by national sub-regional boundaries and poverty rate.",
       fill = "Range") +
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
  facet_wrap( ~ poverty_range, ncol = 4) +
  guides(fill = guide_legend(ncol = 5))

###################################
##PANEL PLOTS FOR POOR POPULATION##
###################################
path = file.path(folder, 'figures', 'poor_population.png')
png(path, units = "in", width = 10, height = 6, res = 300)
print(poverty_maps)
dev.off()

######################################
##UNCONNECTED POPULATION BY GEOTYPES##
######################################
africa_shp <- st_read(file.path(folder, '..', 'data', 'raw', 'Africa_Boundaries', 'Africa_Boundaries.shp'))
new_names <- c('objectid', 'iso3', 'country', 'continent', 'region', 'geometry')
colnames(africa_shp) <- new_names

########################
##Uncovered Population##
########################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_geo_reg.csv'))
data <- data[data$geotype != "suburban", ]

data = data %>%
  group_by(iso3, geotype) %>%
  summarize(unconnected_pop = (sum(pop_unconnected) / 1e6))

data$geotype = factor(
  data$geotype,
  levels = c('remote', 'rural', 'urban'),
  labels = c('Remote', 'Rural', 'Urban'))

merged_data <- merge(africa_shp, data, by = "iso3")
pop_bins <- c(-Inf, 2, 5, 10, 20, 30, 50, 
              80, 120, 150, Inf)
merged_data$population_bin <- cut(merged_data$unconnected_pop, breaks = pop_bins, 
    labels = c("Below 200k", "201 - 500k", "501 - 850k", "0.851 - 1.2 Million", 
    "1.21 - 1.6 Million", "1.61 - 2 Million", "2.01 - 2.5 Million", 
    "2.51 - 3 Million", "3.01 - 3.5 Million", "Above 3.5 Million"))

geotype_uncovered <- ggplot() + 
  geom_sf(data = merged_data, aes(fill = population_bin), 
          linewidth = 0.001,) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Uncovered Population SSA.",
       subtitle = "Presented per country and by geotypes.",
       fill = "Population (millions)") +
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
  facet_wrap( ~ geotype, ncol = 4) +
  guides(fill = guide_legend(ncol = 5))


######################################################
##PANEL PLOTS FOR UNCONNECTED BY POPULATION GEOTYPES##
######################################################
path = file.path(folder, 'figures', 'unconnected_geotype.png')
png(path, units = "in", width = 10, height = 10, res = 300)
print(geotype_uncovered)
dev.off()

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
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_mapping_results.csv'))

data = data %>%
  group_by(GID_1, technology, region) %>%
  summarize(pop_unconnected = (sum(pop_unconnected)))

data$technology = factor(
  data$technology,
  levels = c('GSM', '3G', '4G'),
  labels = c('2G', '3G', '4G'))

merged_data <- merge(africa_shp, data, by = "GID_1")

pop_bins <- c(-Inf, 5000, 10000, 20000, 50000, 80000, 100000, 
              150000, 200000, 500000, Inf)

merged_data$population_bin <- cut(merged_data$pop_unconnected, breaks = pop_bins, 
    labels = c("Below 5k", "51 - 10k", "11 - 20k", "21 - 50k", 
    "51 - 80k", "81 - 100k", "101 - 150k", 
    "151 - 200k", "201 - 500k", "Above 500k"))

uncovered_technology <- ggplot() + 
  geom_sf(data = merged_data, aes(fill = population_bin), 
          linewidth = 0.001,) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Uncovered Population in SSA.",
       subtitle = "Presented at sub-regional boundaries and mobile technology.",
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

###################################
##PANEL PLOTS FOR POOR POPULATION##
###################################
path = file.path(folder, 'figures', 'unconnect_2_3_4_G.png')
png(path, units = "in", width = 10, height = 6, res = 300)
print(uncovered_technology)
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
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data <- na.omit(data)
data <- data[data$technology == "GSM", ]

data = data %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_unconnected = (sum(poor_unconnected)))

data$poverty_range = factor(
  data$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 'Below $US 5.5'))

merged_data <- merge(africa_shp, data, by = "GID_1")
pop_bins <- c(-Inf, 5000, 10000, 20000, 50000, 80000, 100000, 
              500000, 1000000, 1500000, Inf)

merged_data$population_bin <- cut(merged_data$poor_unconnected, breaks = pop_bins, 
   labels = c("Below 5k", "51 - 10k", "11 - 20k", "21 - 50k", "51 - 80k", 
              "81 - 100k", "101 - 500k", "0.51 - 1 Million", "1.1 - 1.5 Million", 
              "Above 1.5 Million"))

uncovered_2g_poor <- ggplot() + 
  geom_sf(data = merged_data, aes(fill = population_bin), 
          linewidth = 0.001,) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Uncovered by 2G and Poor Population in SSA.",
       subtitle = "Presented at sub-regional boundaries and poverty range.",
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


####################################################
##PANEL PLOTS FOR POOR AND 2G UNCOVERED POPULATION##
####################################################
path = file.path(folder, 'figures', 'unconnect_poor_2G.png')
png(path, units = "in", width = 10, height = 6, res = 300)
print(uncovered_2g_poor)
dev.off()

############################
##Poor and Uncovered by 3G##
############################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data <- na.omit(data)
data <- data[data$technology == "3G", ]

data = data %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_unconnected = (sum(poor_unconnected)))

data$poverty_range = factor(
  data$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 'Below $US 5.5'))

merged_data <- merge(africa_shp, data, by = "GID_1")
pop_bins <- c(-Inf, 10000, 20000, 50000, 80000, 500000, 850000, 
              1200000, 1800000, 2200000, Inf)

merged_data$population_bin <- cut(merged_data$poor_unconnected, breaks = pop_bins, 
    labels = c("Below 10k", "11 - 20k", "21 - 50k", "51 - 80k", "81 - 500k", 
    "501 - 850k", "0.851 - 1.2 Million", "1.21 - 1.8 Million", "1.8 - 2.2 Million", 
    "Above 2.2 Million"))

uncovered_3g_poor <- ggplot() + 
  geom_sf(data = merged_data, aes(fill = population_bin), 
          linewidth = 0.001,) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Uncovered by 3G and Poor Population in SSA.",
       subtitle = "Presented at sub-regional boundaries and poverty range.",
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

####################################################
##PANEL PLOTS FOR POOR AND 3G UNCOVERED POPULATION##
####################################################
path = file.path(folder, 'figures', 'unconnect_poor_3G.png')
png(path, units = "in", width = 10, height = 6, res = 300)
print(uncovered_3g_poor)
dev.off()


############################
##Poor and Uncovered by 4G##
############################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data <- na.omit(data)
data <- data[data$technology == "4G", ]

data = data %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_unconnected = (sum(poor_unconnected)))

data$poverty_range = factor(
  data$poverty_range,
  levels = c('GSAP2_poor', 'GSAP2_po_1', 'GSAP2_po_2'),
  labels = c('Below $US 1.9', 'Below $US 3.2', 'Below $US 5.5'))

merged_data <- merge(africa_shp, data, by = "GID_1")
pop_bins <- c(-Inf, 50000, 100000, 250000, 500000, 1000000, 1500000, 
              2000000, 2500000, 3000000, Inf)

merged_data$population_bin <- cut(merged_data$poor_unconnected, breaks = pop_bins, 
   labels = c("Below 50k", "51 - 100k", "101 - 250k", "251 - 500k", "0.51 - 1 Million", 
   "1.1 - 1.5 Million", "1.51 - 2 Million", "2.1 - 2.5 Million", "2.51 - 3 Million", 
   "Above 3 Million"))

uncovered_4g_poor <- ggplot() + 
  geom_sf(data = merged_data, aes(fill = population_bin), 
          linewidth = 0.001,) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Uncovered by 4G and Poor Population in SSA.",
       subtitle = "Presented at sub-regional boundaries and poverty range.",
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


####################################################
##PANEL PLOTS FOR POOR AND 3G UNCOVERED POPULATION##
####################################################
path = file.path(folder, 'figures', 'unconnect_poor_4G.png')
png(path, units = "in", width = 10, height = 6, res = 300)
print(uncovered_4g_poor)
dev.off()

