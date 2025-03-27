library(ggpubr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(sf)
library(osmdata)
library(prettymapr)


suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

##############################
## MULTITECHNOLOGY CAPACITY ##
##############################
sat <- subset(
  read.csv(file.path(folder, '..', '..', 'saleos', 'results', 'SSA', 
                     'SSA_decile_capacity.csv')), constellation == "Starlink")
sat <- sat[, c("technology", "decile", "per_user_capacity_mbps")]

cell <- subset(
  read.csv(file.path(folder, '..', 'results', 'SSA', 
                     'SSA_decile_capacity.csv')), cell_generation == "4G")
cell <- cell[, c("technology", "decile", "per_user_capacity_mbps")]

df <- rbind(sat, cell)
df$decile = factor(df$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
    'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
    'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
    'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
    'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
    'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
    'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
    'Decile 10 \n(<9 km²)'))

df = df %>%
  group_by(technology, decile) %>%
  summarize(mean = mean(per_user_capacity_mbps),
            sd = sd(per_user_capacity_mbps))

df$technology <- factor(df$technology,
    levels = c('cellular', 'satellite'),
    labels = c('Mobile', 'Satellite'))

label_totals <- df %>%
  group_by(technology, decile) %>%
  summarize(mean_value = sum(mean))

per_user_capacity <- 
  ggplot(df, aes(x = decile, y = mean, fill = technology)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1,
                position = position_dodge(.9), color = 'red',size = 0.5) + 
  geom_text(aes(label = formatC(round(after_stat(y), 3), 
      digits = 2, format = "fg", flag = "#")), color = 'black', size = 3.5, position = 
      position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = 1) + 
  labs(colour = NULL, title = "A",
       x = "Population Density Decile (Population per km²)", 
       y = bquote("Average per user capacity (Mbps)")) +
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
  ) + expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 3, title = 'Technology')) +
  theme(strip.text = element_text(size = 14))

##########################
## MULTITECHNOLOGY COST ##
##########################
sat_cost <- subset(
  read.csv(file.path(folder, '..', '..', 'saleos', 'results', 'SSA', 
                     'SSA_decile_cost.csv')), constellation == "Starlink")
sat_cost <- sat_cost[, c("technology", "decile", "annualized_per_user_tco_usd")]

cell_cost <- subset(
  read.csv(file.path(folder, '..', 'results', 'SSA', 
                     'SSA_decile_costs.csv')), cell_generation == "4G")
cell_cost <- cell_cost[, c("technology", "decile", 
                           "annualized_per_user_cost_usd")]
cell_cost <- cell_cost %>% rename(annualized_per_user_tco_usd = 
                          annualized_per_user_cost_usd)

fib_cost <- subset(
  read.csv(file.path(folder, '..', '..', 'glassfiber', 'results', 'SSA', 
                     'SSA_fiber_cost_results.csv')), algorithm == "pcsf" & 
    strategy == "access")
fib_cost <- fib_cost[, c("technology", "decile", "per_user_annualized_usd")]
fib_cost <- fib_cost %>% rename(annualized_per_user_tco_usd = per_user_annualized_usd)

df1 <- rbind(sat_cost, cell_cost, fib_cost)

df1$decile = factor(df1$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
     'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
     'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
     'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
     'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
     'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
     'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
     'Decile 10 \n(<9 km²)'))

df1 = df1 %>%
  group_by(technology, decile) %>%
  summarize(mean = mean(annualized_per_user_tco_usd),
            sd = sd(annualized_per_user_tco_usd))

df1$technology <- factor(df1$technology,
    levels = c('fiber', 'cellular', 'satellite'),
    labels = c('Fixed Fiber', 'Mobile', 'Satellite'))

label_totals <- df1 %>%
  group_by(technology, decile) %>%
  summarize(mean_value = sum(mean))

per_user_cost <- 
  ggplot(df1, aes(x = decile, y = mean, fill = technology)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1,
                position = position_dodge(.9), color = 'red',size = 0.5) + 
  geom_text(aes(label = formatC(round(after_stat(y), 3), 
     digits = 2, format = "fg", flag = "#")), color = 'black', size = 3.5, position = 
     position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = -1) + 
  labs(colour = NULL, title = "B",  
       x = "Population Density Decile (Population per km²)", 
       y = bquote("Average per user cost (US$)")) +
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
  ) + expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 3, title = 'Technology')) +
  theme(strip.text = element_text(size = 14))

##############################
##PANEL USER COSTS CAPACITY ##
##############################
aggregate_cap_costs <- ggarrange(per_user_capacity, per_user_cost,
    ncol = 1, nrow = 2, align = c('hv'),
    common.legend = FALSE, legend='bottom')

path = file.path(folder, 'figures', 'cap_cost_multibroadband.png')
png(path, units="in", width=9, height=11, res=300)
print(aggregate_cap_costs)
dev.off()

###################################
## MULTITECHNOLOGY GHG EMISSIONS ##
###################################
sat_carbon <- subset(
  read.csv(file.path(folder, '..', '..', 'saleos', 'results', 'SSA', 
                     'SSA_decile_emissions.csv')), constellation == "starlink")
sat_carbon <- sat_carbon[, c("technology", "decile", 
            "annualized_per_user_emissions_kg", "annualized_per_user_SCC_usd")]
sat_carbon <- sat_carbon %>% rename(annualized_per_user_emissions = 
                                      annualized_per_user_emissions_kg)
sat_carbon <- sat_carbon %>% rename(per_user_annualized_scc_usd = 
                                      annualized_per_user_SCC_usd)

cell_carbon <- subset(
  read.csv(file.path(folder, '..', 'results', 'SSA', 
                     'SSA_decile_emissions.csv')), cell_generation == "4G")
cell_carbon <- cell_carbon[, c("technology", "decile", 
              "annualized_per_user_ghg", "annualized_per_user_scc_cost_usd")]
cell_carbon <- cell_carbon %>% rename(annualized_per_user_emissions = 
              annualized_per_user_ghg)
cell_carbon <- cell_carbon %>% rename(per_user_annualized_scc_usd = 
               annualized_per_user_scc_cost_usd)

fib_carbon <- subset(
  read.csv(file.path(folder, '..', '..', 'glassfiber', 'results', 'SSA', 
                     'SSA_fiber_emission_results.csv')), algorithm == "pcsf" & 
    strategy == "access")
fib_carbon <- fib_carbon[, c("technology", "decile", 
              "annualized_per_user_emissions", "per_user_annualized_scc_usd")]

df2 <- rbind(sat_carbon, cell_carbon, fib_carbon)

df2$decile = factor(df2$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
    'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
    'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
    'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
    'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
    'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
    'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
    'Decile 10 \n(<9 km²)'))

df2 = df2 %>%
  group_by(technology, decile) %>%
  summarize(mean = mean(annualized_per_user_emissions),
            sd = sd(annualized_per_user_emissions))

df2$technology <- factor(df2$technology,
     levels = c('fiber', 'cellular', 'satellite'),
     labels = c('Fixed Fiber', 'Mobile', 'Satellite'))

label_totals <- df2 %>%
  group_by(technology, decile) %>%
  summarize(mean_value = sum(mean))

per_user_emissions <- 
  ggplot(df2, aes(x = decile, y = mean, fill = technology)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1,
                position = position_dodge(.9), color = 'red',size = 0.5) + 
  geom_text(aes(label = formatC(round(after_stat(y), 3), 
      digits = 2, format = "fg", flag = "#")), color = 'black', size = 3, position = 
      position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = -1) + 
  labs(colour = NULL, title = "A",
       x = "Population Density Decile (Population per km²)", 
       y = bquote("Annualized emissions per user (kg CO"["2"] ~ " e)")) +
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
  ) +   scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 399)) +
  guides(fill = guide_legend(ncol = 3, title = 'Technology')) +
  theme(strip.text = element_text(size = 14))

###############################
## MULTITECHNOLOGY SCC COSTS ##
###############################
df3 <- rbind(sat_carbon, cell_carbon, fib_carbon)

df3$decile = factor(df3$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
    'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
    'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
    'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
    'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
    'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
    'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
    'Decile 10 \n(<9 km²)'))

df3 = df3 %>%
  group_by(technology, decile) %>%
  summarize(mean = mean(per_user_annualized_scc_usd),
            sd = sd(per_user_annualized_scc_usd))

df3$technology <- factor(df3$technology,
    levels = c('fiber', 'cellular', 'satellite'),
    labels = c('Fixed Fiber', 'Mobile', 'Satellite'))

label_totals <- df3 %>%
  group_by(technology, decile) %>%
  summarize(mean_value = sum(mean))

per_user_scc <- 
  ggplot(df3, aes(x = decile, y = mean, fill = technology)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1,
                position = position_dodge(.9), color = 'red',size = 0.5) + 
  geom_text(aes(label = formatC(round(after_stat(y), 3), 
       digits = 2, format = "fg", flag = "#")), color = 'black', size = 3.5, position = 
       position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = -1) + 
  labs(colour = NULL, title = "B", 
       x = "Population Density Decile (Population per km²)", 
       y = bquote("Annualized SCC per user (US$)")) +
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
  ) + scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 29)) +
  guides(fill = guide_legend(ncol = 3, title = 'Technology')) +
  theme(strip.text = element_text(size = 14))

#########################
## PANEL USER EMISSION ##
#########################
aggregate_emissions <- ggarrange(per_user_emissions, per_user_scc,
                                 ncol = 1, nrow = 2, align = c('hv'),
                                 common.legend = TRUE, legend='bottom')

path = file.path(folder, 'figures', 'emissions_multibroadband.png')
png(path, units="in", width=9, height=11, res=300)
print(aggregate_emissions)
dev.off()

###################################
## MULTITECHNOLOGY AFFORDABILITY ##
###################################
sat_cost <- subset(
  read.csv(file.path(folder, '..', '..', 'saleos', 'results', 'SSA', 
                     'SSA_decile_cost.csv')), constellation == "Starlink")
sat_cost <- sat_cost[, c("technology", "decile", "percent_gni")]

cell_cost <- subset(
  read.csv(file.path(folder, '..', 'results', 'SSA', 
                     'SSA_decile_costs.csv')), cell_generation == "4G")
cell_cost <- cell_cost[, c("technology", "decile", 
                           "percent_gni")]

fib_cost <- subset(
  read.csv(file.path(folder, '..', '..', 'glassfiber', 'results', 'SSA', 
                     'SSA_fiber_cost_results.csv')), algorithm == "pcsf" & 
    strategy == "access")
fib_cost <- fib_cost[, c("technology", "decile", "percent_gni")]
df1 <- rbind(sat_cost, cell_cost, fib_cost)

df1$decile = factor(df1$decile, levels = c('Decile 1', 'Decile 2', 'Decile 3', 
   'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 
   'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
  'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
  'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
  'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
  'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
  'Decile 10 \n(<9 km²)'))

df1 = df1 %>%
  group_by(technology, decile) %>%
  summarize(mean = mean(percent_gni),
            sd = sd(percent_gni))

df1$technology <- factor(df1$technology,
   levels = c('fiber', 'cellular', 'satellite'),
   labels = c('Fixed Fiber', 'Mobile', 'Satellite'))

label_totals <- df1 %>%
  group_by(technology, decile) %>%
  summarize(mean_value = sum(mean))

affordability_cost <- 
  ggplot(df1, aes(x = decile, y = mean, fill = technology)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1,
                position = position_dodge(.9), color = 'red',size = 0.5) + 
  geom_text(aes(label = formatC(round(after_stat(y), 2), 
      digits = 1, format = "fg", flag = "#")), color = 'black', size = 4, position = 
      position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = -1) + 
  labs(colour = NULL, x = "Population Density Decile (Population per km²)", 
       y = "Percentage of monthly GNI per capita (%)") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 12),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  ) + 
  guides(fill = guide_legend(ncol = 3, title = 'Technology')) +
  theme(strip.text = element_text(size = 14)) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 27)) +
  geom_vline(xintercept = 8.15, linetype = "dashed", color = "red", size = 0.5) +
  annotate("text", x = 8.15, y = 12, label = "Above 2% of monthly GNI per capita", 
           color = "red", size = 4, angle = 90, vjust = 1.4)

path = file.path(folder, 'figures', 'affordability_multibroadband.png')
png(path, units="in", width=9, height=7, res=300)
print(affordability_cost)
dev.off()

##########################
## MOBILE AFFORDABILITY ##
##########################
cell_cost <- subset(
  read.csv(file.path(folder, '..', 'results', 'SSA', 
                     'SSA_decile_costs.csv')))
cell_cost = cell_cost %>%
  group_by(cell_generation, decile) %>%
  summarize(mean = mean(percent_gni),
            sd = sd(percent_gni))

cell_cost$decile = factor(cell_cost$decile, levels = c('Decile 1', 'Decile 2', 
   'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 
   'Decile 9', 'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
  'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
  'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
  'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
  'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
  'Decile 10 \n(<9 km²)'))

cell_affordability <- ggplot(cell_cost, aes(x = decile, y = mean, fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'red',size = 0.2) + 
  geom_text(aes(label = formatC(round(after_stat(y), 3), 
      digits = 2, format = "fg", flag = "#")), color = 'black', size = 3.5, position = 
      position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = 1) + 
  labs(colour = NULL, title = "A", 
       x = NULL, y = "Percentage of \nmonthly GNI per capita (%)") + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 10)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Mobile Technology')) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 44)) +
  geom_vline(xintercept = 8, linetype = "dashed", color = "red", size = 0.5) +
  annotate("text", x = 8, y = 25, label = "Above 2% of monthly GNI per capita", 
           color = "red", size = 2.5, angle = 90, vjust = 1.4)

#########################
## FIBER AFFORDABILITY ##
#########################
fiber_cost <- subset(
  read.csv(file.path(folder, '..', '..', 'glassfiber', 'results', 'SSA', 
                     'SSA_fiber_cost_results.csv')), algorithm == "pcsf" & 
    strategy == "access")

cell_filtered <- cell_cost#[cell_cost$cell_generation == "4G", ]
colnames(cell_filtered)[colnames(cell_filtered) == "mean"] <- "values"
columns_to_drop <- c("sd")#, "cell_generation")
cell_filtered <- cell_filtered[, !(colnames(cell_filtered) %in% columns_to_drop)]

fiber_cost = fiber_cost %>%
  group_by(algorithm, decile) %>%
  summarize(mean = mean(percent_gni),
            sd = sd(percent_gni))

fiber_cost$decile = factor(fiber_cost$decile, levels = c('Decile 1', 'Decile 2', 
  'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 
  'Decile 9', 'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
  'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
  'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
  'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
  'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
  'Decile 10 \n(<9 km²)'))

fiber_cost <- merge(fiber_cost, cell_filtered, by = "decile", all = TRUE)
fiber_cost$percent_gni <- fiber_cost$values + fiber_cost$mean

fiber_cost$cell_generation <- factor(fiber_cost$cell_generation,
    levels = c('4G', '5G'),
    labels = c('Fiber + 4G', 'Fiber + 5G'))

fib_affordability <- ggplot(fiber_cost, aes(x = decile, y = percent_gni, 
                                            fill = cell_generation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = percent_gni - sd, ymax = percent_gni + sd), width = .2,
     position = position_dodge(.9), color = 'red',size = 0.2) + 
  geom_text(aes(label = formatC(round(after_stat(y), 3), 
     digits = 2, format = "fg", flag = "#")), color = 'black', size = 3.5, position = 
     position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = 1) + 
  labs(colour = NULL, title = "B", 
       x = NULL, y = "Percentage of \nmonthly GNI per capita (%)") + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 10)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Last Mile Access')) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 44)) +
  geom_vline(xintercept = 7, linetype = "dashed", color = "red", size = 0.5) +
  annotate("text", x = 7, y = 25, label = "Above 2% of monthly GNI per capita", 
           color = "red", size = 2.5, angle = 90, vjust = 1.4)

#############################
## SATELLITE AFFORDABILITY ##
#############################
sat_cost <- subset(
  read.csv(file.path(folder, '..', '..', 'saleos', 'results', 'SSA', 
                     'SSA_decile_cost.csv')), constellation != "GEO")
sat_cost = sat_cost %>%
  group_by(constellation, decile) %>%
  summarize(mean = mean(percent_gni),
            sd = sd(percent_gni))

sat_cost$constellation <- factor(sat_cost$constellation,
     levels = c('Starlink', 'OneWeb', 'Kuiper'),
     labels = c('Starlink', 'OneWeb', 'Kuiper'))

sat_cost$decile = factor(sat_cost$decile, levels = c('Decile 1', 'Decile 2', 
  'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 
  'Decile 9', 'Decile 10'), labels = c('Decile 1 \n(>958 km²)', 
  'Decile 2 \n(<957 km²)', 'Decile 3 \n(<455 km²)', 
  'Decile 4 \n(<272 km²)', 'Decile 5 \n(<171 km²)', 
  'Decile 6 \n(<106 km²)', 'Decile 7 \n(<63 km²)', 
  'Decile 8 \n(<39 km²)', 'Decile 9 \n(<21 km²)', 
  'Decile 10 \n(<9 km²)'))

sat_affordability <- ggplot(sat_cost, aes(x = decile, y = mean, fill = constellation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(.9), color = 'red',size = 0.2) + 
  geom_text(aes(label = formatC(round(after_stat(y), 3), 
      digits = 2, format = "fg", flag = "#")), color = 'black', size = 3.5, position = 
      position_dodge(0.9), vjust = 1.2, hjust = -0.2, angle = 90) +
  scale_fill_viridis_d(direction = 1) + 
  labs(colour = NULL, title = "C",  
       x = NULL, y = "Percentage of \nmonthly GNI per capita (%)") + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 10)) +
  expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 5, title = 'Constellation')) +
  scale_x_discrete(expand = c(0, 0.15)) +
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE),limits = c(0, 44))+
  geom_vline(xintercept = 6.84, linetype = "dashed", color = "red", size = 0.5) +
  annotate("text", x = 6.84, y = 25, label = "Above 2% of monthly GNI per capita", 
           color = "red", size = 2.5, angle = 90, vjust = 1.4)


aggregate_affordability <- ggarrange(cell_affordability, 
       fib_affordability, sat_affordability,
        ncol = 1, nrow = 3, align = c('hv'),
        common.legend = FALSE, legend='bottom')

path = file.path(folder, 'figures', 'affordability_technologies.png')
png(path, units="in", width=9, height=11, res=300)
print(aggregate_affordability)
dev.off()

#########################
## AFFORDABILITY  ##
#########################
ssa_borders <- st_read(file.path(folder, '..', 'data', 'raw', 
                                 'Africa_Boundaries', 'Africa_Countries.shp'))
continent <- st_read(file.path(folder, '..', 'data', 'raw', 
                               'continent', 'Africa_Boundaries.shp'))
africa_data <- st_read(file.path(folder, '..', '..', 'glassfiber', 'data', 'raw', 
   'Africa_Boundaries', 'SSA_combined_shapefile.shp'))
gid_pop <- read.csv(file.path(folder, '..', 'results', 'SSA', 
                              'SSA_subregional_population_deciles.csv'))
gid_pop <- gid_pop %>%
  mutate(choice = case_when(
    decile %in% c("Decile 10") ~ "Satellite",
    decile %in% c("Decile 6", "Decile 7", "Decile 8", "Decile 9") ~ "Mobile",
    decile %in% c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5") ~ "Fixed",
    TRUE ~ NA_character_
  ))
merged_data <- merge(africa_data, gid_pop, by = "GID_2")

affordability <- ggplot() + annotation_map_tile(type = "osm", zoom = 4) +
  geom_sf(data = africa_data, fill = "darkslateblue", 
          color = "black", linewidth = 0.01) +
  geom_sf(data = merged_data, aes(fill = choice), 
          linewidth = 0.001,) +
  geom_sf(data = ssa_borders, color = "grey40", fill = NA, size = 0.005) +
  scale_fill_viridis_d(direction = 1) +
  labs(fill = "Technology Choice") +
  theme(
    legend.position = 'bottom',
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
  guides(fill = guide_legend(ncol = 5)) + coord_sf(crs = 4326)

inset_map <- ggplot() +
  geom_sf(data = continent, fill = "white", color = "black", lwd = 0.05) + 
  coord_sf(crs = 4326) + 
  theme_void() 

inset_grob <- ggplotGrob(inset_map)
frame_grob <- rectGrob(gp = gpar(col = "black", fill = NA, lwd = 1))

affordability_with_inset <- affordability + 
  annotation_custom(grob = inset_grob, 
                    xmin = -15, xmax = 0, ymin = -35, ymax = -5) +  
  annotation_custom(grob = frame_grob, 
                    xmin = -15, xmax = 0, ymin = -28, ymax = -8)

path = file.path(folder, 'figures', 'preferred_technology_map.png')
png(path, units = "in", width = 10, height = 10, res = 300)
print(affordability_with_inset)
dev.off()




