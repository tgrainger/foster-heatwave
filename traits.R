# Load packages
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)
library(car) 
library(emmeans) 

# Import ggplot theme for plots
theme_tess <- function () { 
  theme_cowplot()+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))+
    theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
    theme(axis.text.x=element_text(size=20))+
    theme(axis.text.y=element_text(size=20))+
    theme(axis.title.x=element_text(size=20))+
    theme(axis.title.y=element_text(size=20))+
    theme(plot.title = element_text(hjust = 0.5,size=20))+
    theme(axis.title.y=element_text(size=20))}

#### BEFORE EXPERIMENT BODY SIZE ####

# Import data
data <-read.csv("./data/preheatwavebodysize.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

# Make adapted_temp a factor
data$adapted_temp <- factor(data$adapted_temp,
                            levels = c(25, 30, 35),
                            labels = c("25°C", "30°C", "35°C"))

# Convert grams to milligrams
data$weight <- data$weight * 1000

#------Stats------#

# Construct a linear model
lm_bodysize <- lm(weight ~ adapted_temp * sex, data = data)

# Check model assumptions
#windows();plot(lm_bodysize) 
#looks ok

# Run an ANOVA
Anova(lm_bodysize, type="2")
#significant interaction

# Run a Tukey test
emmeans(lm_bodysize, pairwise ~ adapted_temp | sex, adjust = "tukey")

#------Plot------#

# Order the x-axis raw data points
data <- data %>% 
  mutate(sex_temp = factor(paste(sex, adapted_temp),
      levels = c( "f 25°C", "f 30°C", "f 35°C", "m 25°C", "m 30°C", "m 35°C")))

# Calculate summary statistics
summary_data <- data %>%
  group_by(adapted_temp, sex) %>%
  summarise(mean = mean(weight),
            n=n(),
            sd = sd(weight),
            se = sd / sqrt(n), .groups = "drop") 

# Order the summary data
summary_data <- summary_data %>%
  arrange(sex, adapted_temp)

# Add letters denoting Tukey test results
summary_data$plotting_labels <- c("a", "a", "b", "a", "a", "a")

# Generate plot
p_pre <- ggplot(summary_data, aes(x = sex, y = mean, color = adapted_temp)) +
  geom_jitter(data = data, aes(x = sex, y = weight, color = adapted_temp), 
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.4), 
              size = 3, alpha = 0.3) +
  geom_point(size = 5, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(width = 0.4), width = 0, linewidth = 1.12) +
  geom_text(aes(label = plotting_labels, 
                group = adapted_temp,  
                y = mean + se + 0.22), 
            position = position_dodge(width = 0.4), 
            color = "black", 
            size = 6) +
  scale_color_manual(values = c("25°C" = "cornflowerblue", "30°C" = "darkorange", "35°C" = "brown3"), 
                     name = "Historical temperature") +
  scale_x_discrete(labels = c("f" = "Female", "m" = "Male")) +
  xlab("Sex") + 
  ylab("Body size (mg)") +
  labs(title = "Body size before experiment")+ 
  coord_cartesian(ylim = c(1.0, 1.8)) + 
  theme_tess() + 
  theme(aspect.ratio = 1)

windows();p_pre

####END OF EXPERIMENT BODY SIZE#### 

# Import data
data2 <-read.csv("./data/postheatwavebodysize.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

# Make adapted_temp a factor
data2$adapted_temp <- factor(data2$adapted_temp,
                            levels = c(25, 30, 35),
                            labels = c("25°C", "30°C", "35°C"))

# Make heatwave a factor
data2$heatwave <- as.factor(data2$heatwave)

# Convert grams to milligrams
data2$weight <- data2$weight * 1000

# Calculate mean body size from 5 females per population
means <- data2 %>%
  drop_na(weight) %>%
  group_by(heatwave, adapted_temp, sex, population_id) %>%
  summarise(pop_mean = mean(weight))

#------Stats------#

# Construct a linear model
lm_bodysize_post <- lm(pop_mean ~ adapted_temp * sex * heatwave, data = means)

#Check model assumptions
#windows();plot(lm_bodysize_post) 
#looks good

# Run an ANOVA
Anova(lm_bodysize_post, type="2")
# no significant interactions, only significant effects of adapted temp and sex

# Run a Tukey test
emmeans(lm_bodysize_post, pairwise ~ adapted_temp | sex * heatwave, 
        adjust = "tukey")

#------Plot------#

# Calculate summary statistics using the means for each population
summary_data2 <- means %>%
  group_by(heatwave, adapted_temp, sex) %>%
  summarise(mean = mean(pop_mean),
            n=n(),
            sd = sd(pop_mean),
            se = sd / sqrt(n), .groups = "drop")

# Make heatwave a factor in all data sets
summary_data2$heatwave <- factor(summary_data2$heatwave,
                                levels = c(0, 1),
                                labels = c("Control", "Treatment"))

data2$heatwave <- factor(data2$heatwave, levels = c(0, 1),
                        labels = c("Control", "Treatment"))

means$heatwave <- factor(means$heatwave, levels = c(0, 1),
                         labels = c("Control", "Treatment"))

# Group each unique combination of adapted_temp and sex
summary_data2$population_id <- interaction(summary_data2$adapted_temp, summary_data2$sex)
data2$population_id <- interaction(data2$adapted_temp, data2$sex)

# Order the x-axis in all data sets
data2 <- data2 %>%
  mutate(sex_heatwave = factor(paste(sex, heatwave), 
                                levels = c("f Control", 
                                           "f Treatment", "m Control", 
                                           "m Treatment")))
means <- means %>%
  mutate(sex_heatwave = factor(paste(sex, heatwave), 
                               levels = c("f Control", "f Treatment", 
                                          "m Control", "m Treatment")))
summary_data2 <- summary_data2 %>%
  mutate(sex_heatwave = factor(paste(sex, heatwave), 
                               levels = c("f Control", "f Treatment", 
                                          "m Control", "m Treatment")))

# Create custom spacing on the x-axis in all data sets
summary_data2 <- summary_data2 %>%
  mutate(x_pos = case_when(
    sex_heatwave == "f Control" ~ 1,
    sex_heatwave == "f Treatment" ~ 1.6,
    sex_heatwave == "m Control" ~ 2.4,
    sex_heatwave == "m Treatment" ~ 3.0))

means <- means %>%
  mutate(x_pos = case_when(
    sex_heatwave == "f Control" ~ 1,
    sex_heatwave == "f Treatment" ~ 1.6,
    sex_heatwave == "m Control" ~ 2.4,
    sex_heatwave == "m Treatment" ~ 3.0))

# Reorder summary data
summary_data2 <- summary_data2 %>%
  arrange(sex, heatwave, adapted_temp)

# Add letters denoting Tukey test results
summary_data2$plotting_labels <- c("a", "a", "a", "a", "a", "a", "a", "a", "b", "a", "ab", "b")

# Generate plot
p_post <- ggplot(summary_data2, aes(x = x_pos, y = mean, 
                color = adapted_temp, shape = heatwave)) +
  geom_point(size = 5, position = position_dodge(width = 0.4)) +
  geom_jitter(data = means, aes(x = x_pos, y = pop_mean, 
                                  color = adapted_temp,shape = heatwave),
              position = position_jitterdodge(jitter.width = 0, dodge.width = 0.4), 
              size = 3, alpha = 0.3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.4),
                width = 0, linewidth = 1.12) +
  geom_text(aes(label = plotting_labels,
                group = adapted_temp,
                y = mean + se + 0.2),
            position = position_dodge(width = 0.4),
            color = "black",
            size = 6) +
  scale_color_manual(
    values = c("25°C" = "cornflowerblue", "30°C" = "darkorange", "35°C" = "brown3"),
    name = "Historical temperature")+
  scale_shape_manual(values = c("Control" = 16, "Treatment" = 17), 
                     name = "Heatwave") + 
  scale_y_continuous(limits = c(1.0, 1.8)) +
  scale_x_continuous(
    breaks = c(1, 1.6, 2.4, 3.0),
    labels = c("Female", "Female", "Male", "Male")) +
  xlab("Sex") + 
  ylab("") +
  labs(title = "Body size at end of experiment")+ 
  theme_tess()+ 
  theme(aspect.ratio = 1)

#### FECUNDITY #### 

# Import data
f_data <-read.csv("./data/heatwavefecundity.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

# Make heatwave a factor
f_data$heatwave <- factor(f_data$heatwave, 
                          levels = c(0, 1), 
                          labels = c("Control", "Heatwave"))

# Make adapted_temp a factor
f_data$adapted_temp <- factor(f_data$adapted_temp,
                             levels = c(25, 30, 35),
                             labels = c("25°C", "30°C", "35°C"))

# Filter to remove all of the "nb" (no beetle) and "db" (dead beetle) tubes 
f_data_filtered <- f_data %>%
  filter((notes != "nb" & notes != "db") | is.na(notes))

#------Stats------#

# Filter data to include only no-heatwave populations (heatwave pops had near zero fecundity)
f_data_control <- f_data_filtered %>%
  filter(heatwave == "Control")

# Get population means
fec_means_control <- f_data_control %>%
  group_by(adapted_temp, population_id) %>%
  summarise(popmean = mean(egg_count))

# Construct a linear model
lm_fecundity_control <- lm(popmean ~ adapted_temp, data = fec_means_control)

#Check model assumptions
#windows();plot(lm_fecundity_control)
#looks ok
shapiro.test(residuals(lm_fecundity_control))
#normal
leveneTest(residuals(lm_fecundity_control) ~ fec_means_control$adapted_temp)
#equal variances

# Run an ANOVA
Anova(lm_fecundity_control, type = "2")

#------Plot------#

# Calculate the mean number of eggs for each population
egg_means <- f_data_filtered%>%
  group_by(adapted_temp, population_id, heatwave) %>%
  summarise(mean_egg_count = mean(egg_count, na.rm = TRUE), .groups = "drop")

# Calculate summary statistics using the egg_means for each population
f_summary_data <- egg_means %>%
  group_by(adapted_temp, heatwave) %>%
  summarise(mean = mean(mean_egg_count),
            n=n(),
            sd = sd(mean_egg_count),
            se = sd / sqrt(n))

# Reorder summary data
f_summary_data <- f_summary_data %>%
  arrange(heatwave, adapted_temp)

# Add letters denoting Tukey test results
f_summary_data$plotting_labels <- c("a", "a", "a", "a", "a", "a") 

# Create custom spacing for x-axis in all data sets
f_summary_data <- f_summary_data %>%
  mutate(x_pos_f = case_when(
    heatwave == "Control" & adapted_temp == "25°C" ~ 1.1,
    heatwave == "Control" & adapted_temp == "30°C" ~ 1.7,
    heatwave == "Control" & adapted_temp == "35°C" ~ 2.3,
    heatwave == "Heatwave" & adapted_temp == "25°C" ~ 3.3,
    heatwave == "Heatwave" & adapted_temp == "30°C" ~ 3.9,
    heatwave == "Heatwave" & adapted_temp == "35°C" ~ 4.5))

egg_means <- egg_means %>%
  mutate(x_pos_f = case_when(
    heatwave == "Control" & adapted_temp == "25°C" ~ 1.1,
    heatwave == "Control" & adapted_temp == "30°C" ~ 1.7,
    heatwave == "Control" & adapted_temp == "35°C" ~ 2.3,
    heatwave == "Heatwave" & adapted_temp == "25°C" ~ 3.3,
    heatwave == "Heatwave" & adapted_temp == "30°C" ~ 3.9,
    heatwave == "Heatwave" & adapted_temp == "35°C" ~ 4.5))

# Generate plot
f_p <- ggplot(f_summary_data, aes(x = x_pos_f, y = mean, color = adapted_temp, shape = heatwave)) +
  geom_point(size = 5, position = position_dodge(width = 0.5)) +
  geom_jitter(data = egg_means, 
              aes(x = x_pos_f, y = mean_egg_count, 
                  color = adapted_temp,
                  shape = heatwave,
                  group = population_id),
              position = position_jitterdodge(jitter.width = 0.06, 
                                              dodge.width = 0.2), 
              size = 3, alpha = 0.3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(width = 0.6),
                width = 0, linewidth = 1.12) +
  geom_text(aes(label = plotting_labels,
                group = adapted_temp,
                y = mean + se + 5.8),
            position = position_dodge(width = 0.5),
            color = "black",
            size = 7) +
  scale_color_manual(
    values = c("25°C" = "cornflowerblue", "30°C" = "darkorange", "35°C" = "brown3"),   
    name = "Historical temperature")+
  scale_y_continuous(limits = c(0, 24)) +
  scale_x_continuous(
    breaks = c(1.1, 1.7, 2.3, 3.3, 3.9, 4.5),
    labels = c("25°C", "30°C", "35°C", "25°C", "30°C", "35°C"),
    limits = c(0.8, 4.8)) +
  xlab("Historical temperature") +   
  ylab("Number of eggs per female") +
  labs(title = "Fecundity after heatwave")+ 
  theme_tess()+ 
  theme(aspect.ratio = 1,axis.text.x = element_text(size = 17))

#### COMBINED PLOT ####

# Adjust the axis text size for the combined plot
p_post <- p_post + theme(axis.text.x = element_text(size = 18))

# Remove the individual legends from all plots
p_pre_noleg  <- p_pre  + theme(legend.position = "none")
p_post_noleg <- p_post + theme(legend.position = "none")
f_p_noleg <- f_p + theme(legend.position = "none")

# Create a new legend 
legend <- get_legend(p_post +
    theme(legend.position = "right",
          legend.box = "vertical",
          legend.title = element_text(size = 22),
          legend.text  = element_text(size = 19),
          legend.key.size = unit(0.8, "cm"),
          legend.spacing.y = unit(0.2, "cm")))

# Center the new legend
legend_centered <- ggdraw() +
  draw_grob(legend, x = 0.7, y = 0.6, hjust = 0.5, vjust = 0.5)

# Create the top half of combined plot
top_row <- plot_grid(p_pre_noleg, p_post_noleg, labels = c("A", "B"),
                     label_size = 20, ncol = 2, align = "hv")

# Create the bottom half of combined plot
bottom_row <- plot_grid(f_p_noleg, legend_centered, labels = c("C", ""),
                        label_size = 20, ncol = 2, align = "hv")

# Create combined plot
combined <- plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(1, 1))

# Save plot
ggsave(filename = "./figures/traits.pdf",
         plot = combined, width = 30, height = 30, units = "cm", dpi = 300)

