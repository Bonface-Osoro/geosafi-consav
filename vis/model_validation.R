library(ggpubr)
library(ggplot2)
library(readxl)

suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
##################
## Demand Model ##
##################
df <- read_excel(file.path(folder, '..', 'validation', 
                             'model_validation.xlsx'), sheet = "demand")

mobile_demand <- ggplot(data = df, aes(x = `Actual Values`/1e6, 
  y = `Model Values`/1e6)) + geom_point(size = 0.5, color = "black") +
  geom_smooth(method = "lm", color = "darkorange", size = 0.3, se = FALSE) +
  scale_fill_viridis_d(direction = 1) +
    labs(title = "A", x = "Actual (millions)",
       y = "Model (millions)") +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 8),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 9))

###################
##Capacity Model ##
###################
df1 <- read_excel(file.path(folder, '..', 'validation', 
                  'model_validation.xlsx'), sheet = "capacity")

mobile_capacity <- ggplot(data = df1, aes(x = `Actual Values`, 
  y = `Model Values`)) + geom_point(size = 0.5, color = "black") +
  geom_smooth(method = "lm", color = "darkorange", size = 0.3, se = FALSE) +
  scale_fill_viridis_d(direction = 1) +
  labs(title = "B", x = "Actual (Mbps)", y = "Model (Mbps)") +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 8),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 9))

################
## Cost Model ##
################
df2 <- read_excel(file.path(folder, '..', 'validation', 
       'model_validation.xlsx'), sheet = "cost")

mobile_cost <- ggplot(data = df1, aes(x = `Actual Values`, 
  y = `Model Values`)) + geom_point(size = 0.5, color = "black") +
  geom_smooth(method = "lm", color = "darkorange", size = 0.3, se = FALSE) +
  scale_fill_viridis_d(direction = 1) +
  labs(title = "C", x = "Actual (US$)", y = "Model (US$)") +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 8),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 9))

#####################
## Emissions Model ##
#####################
df3 <- read_excel(file.path(folder, '..', 'validation', 
       'model_validation.xlsx'), sheet = "emissions")

mobile_emissions <- ggplot(data = df3, aes(x = `Actual Values`, 
  y = `Model Values`)) + geom_point(size = 0.5, color = "black") +
  geom_smooth(method = "lm", color = "darkorange", size = 0.3, se = FALSE) +
  scale_fill_viridis_d(direction = 1) +
  labs(title = "D", x = bquote("Actual (kg CO"["2"] ~ " e)"), 
       y = bquote("Model (kg CO"["2"] ~ " e)")) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 8),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 9))

########################
##PANEL USER EMISSIONS##
########################
validations <- ggarrange(mobile_demand, mobile_capacity, 
   mobile_cost, mobile_emissions,
   ncol = 4, nrow = 1, align = c('hv'),
   common.legend = TRUE, legend='bottom') 

path = file.path(folder, 'figures', 'model_validation.png')
png(path, units="in", width=10, height=3.5, res=300)
print(validations)
dev.off()
