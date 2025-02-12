library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(ggpubr)
library(ggspatial)

# Get relative path of fold containing this code
# Import data
suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

africa_data <- st_read(file.path(folder, '..', 'data', 'raw', 
              'Africa_Boundaries', 'Africa_Countries.shp'))

filename <- 'monthly_broadband_costs.csv'
data <- read.csv(file.path(folder, "..", "data", "raw", filename), 
                 stringsAsFactors = FALSE)

data <- africa_data %>% left_join(data, by = "ISO_3DIGIT")

##############################
## MONTHLY BROADBAND PRICE ###
##############################
cost_bins <- c(-Inf, 10, 20, 30, 40, 50, 60, 80, 100, Inf)
data$cost_bin <- cut(data$cost_per_month_usd, breaks = cost_bins, labels = 
   c("Below 10", "10 - 20", "20.1 - 30", "30.1 - 40", 
   "40.1 - 50", "50.1 - 60", "60.1 - 80", "80.1 - 100", "Above 100"))

monthly_price <- ggplot(data = data) +
  geom_sf(aes(fill = cost_bin), linewidth = 0.02) + 
  scale_fill_viridis_d(na.value = "grey50",direction = -1,
      name = "Monthly Cost (US$)",
      labels = function(x) ifelse(is.na(x), "No Data", x)) +
  labs(colour = NULL, 
       title = "Broadband price",
       subtitle = "Average cost of broadband service per month.") + 
  theme(axis.title.y = element_text(size = 6),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        plot.subtitle = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14)) + 
  guides(fill = guide_legend(nrow = 2)) + 
  guides(fill = guide_legend(ncol = 5)) 
  guides(fill = guide_legend(title = "Monthly Cost (US$)", 
         reverse = FALSE, ncol = 5)) +
    annotation_scale(location = "bl", width_hint = 0.5) + 
    coord_sf(crs = 4326) 

##################################
## BROADBAND PRICE PER CAPITA  ###
##################################
gni_bins <- c(-Inf, 3, 5, 10, 15, 20, 30, 40, 50, Inf)
data$gni_bin <- cut(data$broadband_gni, breaks = gni_bins, labels = 
    c("Below 2", "2.1 - 5", "5.1 - 10", "10.1 - 15", 
    "15.1 - 20", "20.1 - 30", "30.1 - 40", "40.1 - 50", "Above 50"))

gni_per_capita <- ggplot(data = data) +
  geom_sf(aes(fill = gni_bin), linewidth = 0.02) + 
  scale_fill_viridis_d(na.value = "grey50", 
     name = "Broadband GNI",
     labels = function(x) ifelse(is.na(x), "No Data", x)) +
  labs(colour = NULL, 
       title = "Monthly broadband price",
       subtitle = "Expressed as a percentage of monthly GNI per capita.") + 
  theme(    legend.position = 'bottom',
            axis.text.x = element_text(size = 14),
            panel.spacing = unit(0.6, "lines"),
            plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 16),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            axis.title.x = element_text(size = 14)) + 
  guides(fill = guide_legend(nrow = 2)) + 
  guides(fill = guide_legend(ncol = 5)) 
  guides(fill = guide_legend(title = "Percentage (%)", 
        reverse = FALSE, ncol = 5)) +
    annotation_scale(location = "bl", width_hint = 0.5) + 
    coord_sf(crs = 4326) 

######################
## MONTHLY INCOME  ###
######################
income_bins <- c(-Inf, 100, 150, 250, 350, 450, 550, 700, 900, 1000, Inf)
data$income_bin <- cut(data$monthly_GNI, breaks = income_bins, labels = 
    c("Below 100", "100 - 150", "151 - 250", "251 - 350", "351 - 450",
    "451 - 550", "551 - 700", "701 - 900", "901 - 1000", "Above 1000"))

monthly_income <- ggplot(data = data) +
  geom_sf(aes(fill = income_bin), linewidth = 0.02) + 
  scale_fill_viridis_d(na.value = "grey50",
                    name = "Income (US$)",
                    labels = function(x) ifelse(is.na(x), "No Data", x)) +
  labs(colour = NULL, 
       title = "Income",
       subtitle = "Average monthly income in SSA countries.") + 
  theme(axis.title.y = element_text(size = 6),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        plot.subtitle = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14)) + 
  guides(fill = guide_legend(nrow = 2)) + 
  guides(fill = guide_legend(ncol = 5)) 
  guides(fill = guide_legend(title = "Income (US$)", reverse = FALSE, ncol = 5)) +
  coord_sf(crs = 4326) 


##################
## COST PER GB ###
##################
gb_bins <- c(-Inf, 2, 3, 4, 5, 6, 7, 9, 10, Inf)
data$gb_bin <- cut(data$cost_per_1GB, breaks = gb_bins, labels = 
    c("Below 2", "2.1 - 3", "3.1 - 4", "4.1 - 5",
    "5.1 - 6", "6.1 - 7", "7.1 - 9", "9.1 - 10", "Above 10"))

cost_GB <- ggplot(data = data) +
  geom_sf(aes(fill = gb_bin), linewidth = 0.02) + 
  scale_fill_viridis_d(na.value = "grey50",
                       name = "Income (US$)",
                       labels = function(x) ifelse(is.na(x), "No Data", x)) +
  labs(colour = NULL, 
       title = "Cost per GB",
       subtitle = "Average cost of broadband per gigabyte (GB).") + 
  theme(axis.title.y = element_text(size = 6),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        plot.subtitle = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14)) + 
  guides(fill = guide_legend(nrow = 2)) + 
  guides(fill = guide_legend(ncol = 5)) 
  guides(fill = guide_legend(title = "Cost per GB (US$)", 
         reverse = FALSE, ncol = 5)) +
    annotation_scale(location = "bl", width_hint = 0.5) + 
    coord_sf(crs = 4326) 


path = file.path(folder, 'figures', 'monthly_income.png')
png(path, units = "in", width = 13, height = 13, res = 300)
print(monthly_income)
dev.off()

path = file.path(folder, 'figures', 'broadband_price.png')
png(path, units = "in", width = 13, height = 13, res = 300)
print(monthly_price)
dev.off()

path = file.path(folder, 'figures', 'broadband_price_gni.png')
png(path, units = "in", width = 13, height = 13, res = 300)
print(gni_per_capita)
dev.off()

path = file.path(folder, 'figures', 'broadband_cost_GB.png')
png(path, units = "in", width = 13, height = 13, res = 300)
print(cost_GB)
dev.off()




