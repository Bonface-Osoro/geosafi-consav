library(ggpubr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(sf)
library(readr)

suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

africa_data <- st_read(file.path(folder, '..', 'data', 'raw', 'Africa_Boundaries', 'SSA_combined_shapefile.shp'))
africa_shp <- africa_data %>%
  select(GID_0, NAME_0, GID_1, GID_2, geometry)

new_names <- c('iso3', 'country', 'gid_1', 'GID_1', 'geometry')
colnames(africa_shp) <- new_names

###############################################
##SSA Absolute Poor Population below US$ 1.9 ##
###############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poverty_results.csv'))
data <- data[data$poverty_range == "GSAP2_poor", ]

data = data %>%
  distinct(GID_1, poverty_range, region, population, .keep_all = TRUE) %>%
  group_by(GID_1, poverty_range, region) %>%
  summarize(poor_pops = (sum(poor_population) / 1e6),
            total_pops = sum(population))

merged_data <- merge(africa_shp, data, by = "GID_1")

brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$poor_pops))

create_sf_plot <-
  function(data, data_2, fill_variable, legend_title, plot_title,
           plot_subtitle) {
    # Get unique values
    unique_values <- unique(data[[fill_variable]])
    # Create a Brewer color palette
    num_colors <- length(unique_values) - 1
    colors <- brewer_color_ramp(num_colors)
    # Create a color gradient, including grey for zero
    gradient_colors <- c("grey", colors)
    gradient_breaks <- c(0, sort(unique_values[unique_values != 0]))
    plot_title <- paste0(plot_title, "\n")
    plot <-
      ggplot(data) + geom_sf(
        data = data_2,
        fill = NA,
        color = "dodgerblue",
        linewidth = 0.1,
        alpha = 1
      ) +
      geom_sf(
        aes(fill = .data[[fill_variable]]),
        linewidth = 0.01,
        alpha = 0.8,
        color = "white"
      ) +
      theme_transparent() + scale_fill_gradientn(
        colors = gradient_colors,
        values = scales::rescale(gradient_breaks),
        name = legend_title
      ) +
      labs(title = plot_title, subtitle = plot_subtitle) + theme(
        text = element_text(color = "#22211d", family = "Arial"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size = 12, face = 'bold', hjust = 0),
        plot.subtitle = element_text(size = 9),
        legend.position = 'bottom',
        legend.key.width = unit(0.05, "npc"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 8)
      ) +
      guides(fill = guide_colourbar(title.position = 'top', direction = "horizontal")) +
      coord_sf()
    return(plot)
  }

#################
##Below US$ 1.9##
#################
below_1_9 <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "poor_pops",
  legend_title = "Total population (millions)",
  plot_title = "Poor Population in SSA.",
  plot_subtitle = '(A) Living Below US$ 1.9'
)

###############################################
##SSA Absolute Poor Population Below $US 3.2 ##
###############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poverty_results.csv'))
data <- data[data$poverty_range == "GSAP2_po_1", ]

data = data %>%
  distinct(GID_1, poverty_range, region, population, .keep_all = TRUE) %>%
  group_by(GID_1, poverty_range, region) %>%
  summarize(poor_pops = (sum(poor_population) / 1e6),
            total_pops = sum(population))

merged_data <- merge(africa_shp, data, by = "GID_1")

brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$poor_pops))

below_3_2 <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "poor_pops",
  legend_title = "Total Population (millions)",
  plot_title = ' ',
  plot_subtitle = "(B) Living Below US$ 3.2"
)


###############################################
##SSA Absolute Poor Population Below $US 5.5 ##
###############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poverty_results.csv'))
data <- data[data$poverty_range == "GSAP2_po_2", ]

data = data %>%
  distinct(GID_1, poverty_range, region, population, .keep_all = TRUE) %>%
  group_by(GID_1, poverty_range, region) %>%
  summarize(poor_pops = (sum(poor_population) / 1e6),
            total_pops = sum(population))

merged_data <- merge(africa_shp, data, by = "GID_1")

brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$poor_pops))

below_5_5 <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "poor_pops",
  legend_title = "Total Population (millions)",
  plot_title = ' ',
  plot_subtitle = "(C) Poor Population Below US$ 5.5"
)

###################################
##PANEL PLOTS FOR POOR POPULATION##
###################################
poor_panel <- ggarrange(below_1_9, below_3_2, below_5_5,
    ncol = 3, nrow = 1, align = c('hv'),
    common.legend = FALSE, legend='bottom')

path = file.path(folder, 'figures', 'poor_1_9_3_2_5_5.png')
png(path, units = "in", width = 10, height = 6, res = 300)
print(poor_panel)
dev.off()

######################################
##UNCONNECTED POPULATION BY GEOTYPES##
######################################
africa_shp <- st_read(file.path(folder, '..', 'data', 'raw', 'Africa_Boundaries', 'Africa_Boundaries.shp'))
new_names <- c('objectid', 'iso3', 'country', 'continent', 'region', 'geometry')
colnames(africa_shp) <- new_names

###############################
##Uncovered Remote Population##
###############################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_geo_reg.csv'))
data <- data[data$geotype == "remote", ]

data = data %>%
  distinct(iso3, geotype, region,.keep_all = TRUE) %>%
  group_by(iso3, geotype, region) %>%
  summarize(unconnected_pop = (sum(pop_unconnected) / 1e6))

merged_data <- merge(africa_shp, data, by = "iso3")

brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$unconnected_pop))

remote_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "unconnected_pop",
  legend_title = "Total Population (millions)",
  plot_title = 'Uncovered Population by Geotypes.',
  plot_subtitle = "(A) Remote Population"
)

##############################
##Uncovered Rural Population##
##############################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_geo_reg.csv'))
data <- data[data$geotype == "rural", ]

data = data %>%
  distinct(iso3, geotype, region,.keep_all = TRUE) %>%
  group_by(iso3, geotype, region) %>%
  summarize(unconnected_pop = (sum(pop_unconnected) / 1e6))

merged_data <- merge(africa_shp, data, by = "iso3")

brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$unconnected_pop))

rural_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "unconnected_pop",
  legend_title = "Total Population (millions)",
  plot_title = ' ',
  plot_subtitle = "(B) Rural Population"
)

##############################
##Uncovered Urban Population##
##############################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_geo_reg.csv'))
data <- data[data$geotype == "urban", ]

data = data %>%
  distinct(iso3, geotype, region,.keep_all = TRUE) %>%
  group_by(iso3, geotype, region) %>%
  summarize(unconnected_pop = (sum(pop_unconnected) / 1e6))

merged_data <- merge(africa_shp, data, by = "iso3")

brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$unconnected_pop))

urban_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "unconnected_pop",
  legend_title = "Total Population (millions)",
  plot_title = ' ',
  plot_subtitle = "(C) Urban Population"
)

######################################################
##PANEL PLOTS FOR UNCONNECTED BY POPULATION GEOTYPES##
######################################################
geotype_panel <- ggarrange(remote_unconnected, 
   rural_unconnected,
   urban_unconnected, ncol = 3, nrow = 1, align = c('hv'),
   common.legend = FALSE, legend = 'bottom')

path = file.path(folder, 'figures', 'unconnected_geotype.png')
png(path, units = "in", width = 10, height = 10, res = 300)
print(geotype_panel)
dev.off()

#################################################
##UNCONNECTED POPULATION BY CELLULAR TECHNOLOGY##
#################################################
africa_data <- st_read(file.path(folder, '..', 'data', 'raw', 'Africa_Boundaries', 'SSA_combined_shapefile.shp'))
africa_shp <- africa_data %>%
  select(GID_0, NAME_0, GID_1, GID_2, geometry)

new_names <- c('iso3', 'country', 'gid_1', 'GID_1', 'geometry')
colnames(africa_shp) <- new_names

##############################
##Uncovered Population by 2G##
##############################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_mapping_results.csv'))
data <- data[data$technology == "GSM", ]

data = data %>%
  distinct(GID_1, technology, region, .keep_all = TRUE) %>%
  group_by(GID_1, technology, region) %>%
  summarize(pop_unconnected = (sum(pop_unconnected) / 1e6))

merged_data <- merge(africa_shp, data, by = "GID_1")

brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$pop_unconnected))

gsm_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "pop_unconnected",
  legend_title = "Total Population (millions)",
  plot_title = 'Uncovered Population in SSA.',
  plot_subtitle = "(A) By 2G"
)

##############################
##Uncovered Population by 3G##
##############################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_mapping_results.csv'))
data <- data[data$technology == "3G", ]

data = data %>%
  distinct(GID_1, technology, region, .keep_all = TRUE) %>%
  group_by(GID_1, technology, region) %>%
  summarize(pop_unconnected = (sum(pop_unconnected) / 1e6))

merged_data <- merge(africa_shp, data, by = "GID_1")

brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$pop_unconnected))

g_3_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "pop_unconnected",
  legend_title = "Total Population (millions)",
  plot_title = ' ',
  plot_subtitle = "(B) By 3G"
)

##############################
##Uncovered Population by 4G##
##############################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_unconnected_mapping_results.csv'))
data <- data[data$technology == "4G", ]

data = data %>%
  distinct(GID_1, technology, region, .keep_all = TRUE) %>%
  group_by(GID_1, technology, region) %>%
  summarize(pop_unconnected = (sum(pop_unconnected) / 1e6))

merged_data <- merge(africa_shp, data, by = "GID_1")

brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$pop_unconnected))

g_4_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "pop_unconnected",
  legend_title = "Total Population (millions)",
  plot_title = ' ',
  plot_subtitle = "(C) By 4G"
)

###################################
##PANEL PLOTS FOR POOR POPULATION##
###################################
unconnect_tech <- ggarrange(gsm_unconnected, g_3_unconnected, 
    g_4_unconnected, ncol = 3, nrow = 1, align = c('hv'),
    common.legend = FALSE, legend='bottom')

path = file.path(folder, 'figures', 'unconnect_2_3_4_G.png')
png(path, units = "in", width = 10, height = 6, res = 300)
print(unconnect_tech)
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
data <- data[data$technology == "GSM", ]
data <- data[data$poverty_range == "GSAP2_poor", ]

data = data %>%
  distinct(GID_1, technology, poverty_range, .keep_all = TRUE) %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_unconnected = (sum(poor_unconnected)) / 1e6)

merged_data <- merge(africa_shp, data, by = "GID_1")
brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$poor_unconnected))

poor_gsm_1_9_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "poor_unconnected",
  legend_title = "Total population (millions)",
  plot_title = 'Poor Population and Uncovered by 2G.',
  plot_subtitle = '(A) Uncovered and Living Below US$ 1.9'
)

############################################
##Poor (Below US$ 3.2) and uncovered by 2G##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data <- data[data$technology == "GSM", ]
data <- data[data$poverty_range == "GSAP2_po_1", ]

data = data %>%
  distinct(GID_1, technology, poverty_range, .keep_all = TRUE) %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_unconnected = (sum(poor_unconnected)) / 1e6)

merged_data <- merge(africa_shp, data, by = "GID_1")
brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$poor_unconnected))

poor_gsm_3_2_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "poor_unconnected",
  legend_title = "Total population (millions)",
  plot_title = ' ',
  plot_subtitle = '(B) Uncovered and Living Below US$ 3.2'
)

############################################
##Poor (Below US$ 5.5) and uncovered by 2G##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data <- data[data$technology == "GSM", ]
data <- data[data$poverty_range == "GSAP2_po_2", ]

data = data %>%
  distinct(GID_1, technology, poverty_range, .keep_all = TRUE) %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_unconnected = (sum(poor_unconnected)) / 1e6)

merged_data <- merge(africa_shp, data, by = "GID_1")
brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$poor_unconnected))

poor_gsm_5_5_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "poor_unconnected",
  legend_title = "Total population (millions)",
  plot_title = ' ',
  plot_subtitle = '(C) Uncovered and Living Below  US$ 5.5'
)

####################################################
##PANEL PLOTS FOR POOR AND 2G UNCOVERED POPULATION##
####################################################
unconnect_gsm_poor <- ggarrange(poor_gsm_1_9_unconnected, poor_gsm_3_2_unconnected, 
   poor_gsm_5_5_unconnected, ncol = 3, nrow = 1, align = c('hv'),
   common.legend = FALSE, legend='bottom')

path = file.path(folder, 'figures', 'unconnect_poor_2G.png')
png(path, units = "in", width = 10, height = 6, res = 300)
print(unconnect_gsm_poor)
dev.off()

############################################
##Poor (Below US$ 1.9) and Uncovered by 3G##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data <- data[data$technology == "3G", ]
data <- data[data$poverty_range == "GSAP2_poor", ]

data = data %>%
  distinct(GID_1, technology, poverty_range, .keep_all = TRUE) %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_unconnected = (sum(poor_unconnected)) / 1e6)

merged_data <- merge(africa_shp, data, by = "GID_1")
brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$poor_unconnected))

poor_3g_1_9_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "poor_unconnected",
  legend_title = "Total population (millions)",
  plot_title = 'Poor Population and Uncovered by 3G.',
  plot_subtitle = '(A) Uncovered and Living Below  US$ 1.9'
)

############################################
##Poor (Below US$ 3.2) and Uncovered by 3G##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data <- data[data$technology == "3G", ]
data <- data[data$poverty_range == "GSAP2_po_1", ]

data = data %>%
  distinct(GID_1, technology, poverty_range, .keep_all = TRUE) %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_unconnected = (sum(poor_unconnected)) / 1e6)

merged_data <- merge(africa_shp, data, by = "GID_1")
brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$poor_unconnected))

poor_3g_3_2_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "poor_unconnected",
  legend_title = "Total population (millions)",
  plot_title = ' ',
  plot_subtitle = '(B) Uncovered and Living Below  US$ 3.2'
)

############################################
##Poor (Below US$ 5.5) and Uncovered by 3G##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data <- data[data$technology == "3G", ]
data <- data[data$poverty_range == "GSAP2_po_2", ]

data = data %>%
  distinct(GID_1, technology, poverty_range, .keep_all = TRUE) %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_unconnected = (sum(poor_unconnected)) / 1e6)

merged_data <- merge(africa_shp, data, by = "GID_1")
brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$poor_unconnected))

poor_3g_5_5_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "poor_unconnected",
  legend_title = "Total population (millions)",
  plot_title = ' ',
  plot_subtitle = '(C) Uncovered and Living Below  US$ 5.5'
)


####################################################
##PANEL PLOTS FOR POOR AND 3G UNCOVERED POPULATION##
####################################################
unconnect_3g_poor <- ggarrange(poor_3g_1_9_unconnected, poor_3g_3_2_unconnected, 
     poor_3g_5_5_unconnected, ncol = 3, nrow = 1, align = c('hv'),
    common.legend = FALSE, legend='bottom')

path = file.path(folder, 'figures', 'unconnect_poor_3G.png')
png(path, units = "in", width = 10, height = 6, res = 300)
print(unconnect_3g_poor)
dev.off()


############################################
##Poor (Below US$ 1.9) and Uncovered by 4G##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data <- data[data$technology == "4G", ]
data <- data[data$poverty_range == "GSAP2_poor", ]

data = data %>%
  distinct(GID_1, technology, poverty_range, .keep_all = TRUE) %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_unconnected = (sum(poor_unconnected)) / 1e6)

merged_data <- merge(africa_shp, data, by = "GID_1")
brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$poor_unconnected))

poor_4g_1_9_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "poor_unconnected",
  legend_title = "Total population (millions)",
  plot_title = 'Poor Population and Uncovered by 4G.',
  plot_subtitle = '(A) Uncovered and Living Below  US$ 1.9'
)

############################################
##Poor (Below US$ 3.2) and Uncovered by 4G##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data <- data[data$technology == "4G", ]
data <- data[data$poverty_range == "GSAP2_po_1", ]

data = data %>%
  distinct(GID_1, technology, poverty_range, .keep_all = TRUE) %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_unconnected = (sum(poor_unconnected)) / 1e6)

merged_data <- merge(africa_shp, data, by = "GID_1")
brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$poor_unconnected))

poor_4g_3_2_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "poor_unconnected",
  legend_title = "Total population (millions)",
  plot_title = ' ',
  plot_subtitle = '(B) Uncovered and Living Below  US$ 3.2'
)

############################################
##Poor (Below US$ 5.5) and Uncovered by 4G##
############################################
data <- read.csv(file.path(folder, '..', 'results', 'SSA', 'SSA_poor_unconnected.csv'))
data <- data[data$technology == "4G", ]
data <- data[data$poverty_range == "GSAP2_po_2", ]

data = data %>%
  distinct(GID_1, technology, poverty_range, .keep_all = TRUE) %>%
  group_by(GID_1, technology, poverty_range) %>%
  summarize(poor_unconnected = (sum(poor_unconnected)) / 1e6)

merged_data <- merge(africa_shp, data, by = "GID_1")
brewer_color_ramp <- colorRampPalette(brewer.pal(11, "Spectral"))
num_colors <- length(unique(merged_data$poor_unconnected))

poor_4g_5_5_unconnected <- create_sf_plot(
  data = merged_data,
  merged_data,
  fill_variable = "poor_unconnected",
  legend_title = "Total population (millions)",
  plot_title = ' ',
  plot_subtitle = '(C) Uncovered and Living Below  US$ 5.5'
)

####################################################
##PANEL PLOTS FOR POOR AND 3G UNCOVERED POPULATION##
####################################################
unconnect_4g_poor <- ggarrange(poor_4g_1_9_unconnected, poor_4g_3_2_unconnected, 
    poor_4g_5_5_unconnected, ncol = 3, nrow = 1, align = c('hv'),
    common.legend = FALSE, legend='bottom')

path = file.path(folder, 'figures', 'unconnect_poor_4G.png')
png(path, units = "in", width = 10, height = 6, res = 300)
print(unconnect_4g_poor)
dev.off()

