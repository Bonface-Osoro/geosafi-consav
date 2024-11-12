library(ggpubr)
library(ggplot2)
library(tidyverse)
library(dplyr)


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
                                digits = 2, format = "fg", flag = "#")), color = 'black', size = 3, position = 
              position_dodge(0.9), vjust = -0.2, hjust = 1) +
  scale_fill_viridis_d(direction = 1) + 
  labs(colour = NULL, title = "(A) Multibroadband Technology Capacity Reported Per User", 
       subtitle = "Per user capacity categorized by deciles and grouped by technology.", 
       x = "Population Density Decile (Population per km²)", 
       y = bquote("Average per user capacity (Mbps/user)")) +
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
                                digits = 2, format = "fg", flag = "#")), color = 'black', size = 3, position = 
              position_dodge(0.9), vjust = -0.2, hjust = 1) +
  scale_fill_viridis_d(direction = 1) + 
  labs(colour = NULL, title = "(B) Multibroadband Technology Cost Reported Per User", 
       subtitle = "Annualized per user cost categorized by deciles and grouped by technology.", 
       x = "Population Density Decile (Population per km²)", 
       y = bquote("Average per user cost (US$/user)")) +
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
png(path, units="in", width=12, height=10, res=300)
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
              position_dodge(0.9), vjust = -0.2, hjust = 1) +
  scale_fill_viridis_d(direction = 1) + 
  labs(colour = NULL, title = "(A) Multibroadband Technology Greenhouse Gas (GHG) emissions Reported Per User", 
       subtitle = "Annualized per user GHG emissions categorized by deciles and grouped by technology.", 
       x = "Population Density Decile (Population per km²)", 
       y = bquote("Annualized emissions per user (kg CO"["2"] ~ " eq.)")) +
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
       digits = 2, format = "fg", flag = "#")), color = 'black', size = 3, position = 
       position_dodge(0.9), vjust = -0.2, hjust = 1) +
  scale_fill_viridis_d(direction = 1) + 
  labs(colour = NULL, title = "(B) Multibroadband Technology Social Carbon Cost (SCC) Reported Per User", 
       subtitle = "Annualized per user SCC emissions categorized by deciles and grouped by technology.", 
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
  ) + expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 3, title = 'Technology')) +
  theme(strip.text = element_text(size = 14))

#########################
## PANEL USER EMISSION ##
#########################
aggregate_emissions <- ggarrange(per_user_emissions, per_user_scc,
      ncol = 1, nrow = 2, align = c('hv'),
      common.legend = TRUE, legend='bottom')

path = file.path(folder, 'figures', 'emissions_multibroadband.png')
png(path, units="in", width=12, height=11, res=300)
print(aggregate_emissions)
dev.off()

