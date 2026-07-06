# Load packages
library(ggplot2)
library(cowplot)
library(dplyr)
library(car)
library(lme4)

# Import ggplot theme for plots
theme_tess <- function () { 
  theme_cowplot()+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))+
    theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
    theme(axis.text.x=element_text(size=23))+
    theme(axis.text.y=element_text(size=23))+
    theme(axis.title.x=element_text(size=23))+
    theme(axis.title.y=element_text(size=23))+
    theme(plot.title = element_text(hjust = 0.5,size=23))+
    theme(axis.title.y=element_text(size=23))
}
 
# Import data
data <-read.csv("./data/heatwavepopulation.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

# Make heatwave a factor
data$heatwave <- factor(data$heatwave, levels = c(0, 1))
  
# Make adapted_temp a factor
data$adapted_temp<- factor(data$adapted_temp,
                           levels = c("25", "30", "35"))

# Make weeks_since_heatwave numeric
data$weeks_since_heatwave <- as.numeric(as.character(data$weeks_since_heatwave))
  
#### PLOT ####

# Calculate summary statistics
  summary_data <- data %>%
    group_by(adapted_temp, heatwave, weeks_since_heatwave) %>%
    summarise(mean = mean(alive),
              n=n(),
              sd = sd(alive),
              se = sd / sqrt(n))
  summary_data$heatwave <- factor(summary_data$heatwave, levels = c(0, 1))
  summary_data$group_id <- interaction(summary_data$adapted_temp, summary_data$heatwave)
  data$group_id <- interaction(data$adapted_temp, data$heatwave)

# Space weeks_since_heatwave raw data points equally
  data <- data %>% 
    mutate(time_index = case_when(
      weeks_since_heatwave == 0  ~ 1,
      weeks_since_heatwave == 2  ~ 2,
      weeks_since_heatwave == 6  ~ 3,
      weeks_since_heatwave == 12 ~ 4,
      weeks_since_heatwave == 18 ~ 5))
  
# Space weeks_since_heatwave summary data points equally
  summary_data <- summary_data %>%
    mutate(time_index = case_when(
      weeks_since_heatwave == 0  ~ 1,
      weeks_since_heatwave == 2  ~ 2,
      weeks_since_heatwave == 6  ~ 3,
      weeks_since_heatwave == 12 ~ 4,
      weeks_since_heatwave == 18 ~ 5))
  
# Order and space heatwave and adapted_temp raw data points 
  data <- data %>%
    mutate(x_offset = case_when(
      heatwave == 0 & adapted_temp == "25" ~ -0.25,
      heatwave == 0 & adapted_temp == "30" ~ -0.15,
      heatwave == 0 & adapted_temp == "35" ~ -0.05,
      heatwave == 1 & adapted_temp == "25" ~  0.05,
      heatwave == 1 & adapted_temp == "30" ~  0.15,
      heatwave == 1 & adapted_temp == "35" ~  0.25))
  
# Order and space heatwave and adapted_temp summary data points 
  summary_data <- summary_data %>%
    mutate(x_offset = case_when(
      heatwave == 0 & adapted_temp == "25" ~ -0.25,
      heatwave == 0 & adapted_temp == "30" ~ -0.15,
      heatwave == 0 & adapted_temp == "35" ~ -0.05,
      heatwave == 1 & adapted_temp == "25" ~  0.05,
      heatwave == 1 & adapted_temp == "30" ~  0.15,
      heatwave == 1 & adapted_temp == "35" ~  0.25))
 
# Offset raw data points
  data <- data %>%
    mutate(x_pos = time_index + x_offset)
  
# Offset summary data points
  summary_data <- summary_data %>%
    mutate(x_pos = time_index + x_offset)
  

# Generate a plot of population size for different adapted temperatures with and without a heatwave
p <- ggplot(summary_data, aes(x = x_pos, y = mean, color = adapted_temp, 
                              shape = heatwave, group = group_id)) +
    geom_point(size = 4.5) +
    scale_x_continuous(breaks = 1:5, labels = c("0", "2", "6", "12", "18")) +
    geom_jitter(data = data, 
                aes(x = x_pos, y = alive, color = adapted_temp, shape = heatwave),
                width = 0.01, height = 0, size = 3, alpha = 0.4) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0, linewidth = 1.12) +
    scale_shape_manual(values = c("0" = 16, "1" = 17), 
                       name = "Heatwave", 
                       labels = c("0" = "Control", "1" = "Treatment")) +
    scale_color_manual(values = c("cornflowerblue", "darkorange", "brown3"),
                       name = "Historical temperature",
                       labels = c("25°C", "30°C", "35°C"),
                       breaks = c("25", "30", "35"),
                       guide = guide_legend(reverse = TRUE)) +
    xlab("Weeks since heatwave") +    
    ylab("Population size (live adult beetles)") +
    theme_tess() +
    theme(legend.title = element_text(size = 21),
    legend.text  = element_text(size = 17),
    legend.key.size = unit(0.6, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.spacing.y  = unit(0.6, "cm"))

#windows();p 

# Save the plot
ggsave(file="./figures/popsize.pdf", p, 
       width = 30, height = 18, units = "cm", dpi = 200) 

#### STATS ####

#------ Global model (through time) ------#

# Create a new column with specific population ids called "pop_id"
data <- data%>%
  mutate(pop_id = paste(adapted_temp,heatwave,population_id, sep = "_"))

# Check that n = 60 for pop_id
#n_distinct(data$pop_id)

# Construct a global linear model
lmmglobal <- lmer(alive ~ adapted_temp * heatwave * weeks_since_heatwave + 
                    (1|pop_id),data = data)

# Run a three-way ANOVA
Anova(lmmglobal, type = 3)
#significant three-way interaction

#------ Two-way ANOVAs for each time point ------#

# Filter data for Week 0
week0data <- data %>%
  filter(weeks_since_heatwave == 0)

# Construct a linear model
lm_week0<-lm(alive~adapted_temp*heatwave,data=week0data)

# Run an ANOVA
Anova(lm_week0,type="2")
#significant interaction

# Filter data for Week 2
week2data <- data %>%
    filter(weeks_since_heatwave == 2)

# Construct a linear model
lm_week2<-lm(alive~adapted_temp*heatwave,data=week2data)

# Run an ANOVA
Anova(lm_week2,type="2")
#significant interaction
  
# Filter data for Week 6
week6data <- data %>%
    filter(weeks_since_heatwave == 6)
  
# Construct a linear model
lm_week6<-lm(alive~adapted_temp*heatwave,data=week6data)

# Run an ANOVA
Anova(lm_week6,type="2")
#significant interaction
  
# Filter data for Week 12
week12data <- data %>%
    filter(weeks_since_heatwave == 12)

# Construct a linear model
lm_week12<-lm(alive~adapted_temp*heatwave,data=week12data)

# Run an ANOVA
Anova(lm_week12,type="2")
#significant interaction

# Filter data for Week 18
week18data <- data %>%
  filter(weeks_since_heatwave == 18)

# Construct a linear model
lm_week18<-lm(alive~adapted_temp*heatwave,data=week18data)

# Run an ANOVA
Anova(lm_week18,type="2")
#significant interaction
