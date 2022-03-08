########### Homework 5 #########
#### author: Karla Alujevic ####
################################

library(tidyverse)
library(ggforce) 
library(ggsci)
library(patchwork)
library(Hmisc)

setwd('~/Desktop/Data_Science_For_Biology_II/Part.4.DataVisualization/') #change to match your ggplot directory

bloom_df <- read.csv('bloom_df.csv')

bloom_df[1:5,]

str(bloom_df)


## Task 1: Continuous vs. continuous plot ##
#Create two scatterplots of logbodysize vs. trophic_position grouped by reg

# First plot (2 trend lines):

ggplot(data = bloom_df, aes(x = logbodysize, y = trophic_position)) +
  geom_point(aes(fill = reg, shape = reg), size=3, colour = "black") + 
  stat_smooth(aes(color= reg), method='lm', linetype='dashed', size=1) + #how to change color of stat smooth?
  scale_fill_manual(name = "reg:", values = c("red", "yellow")) + 
  scale_shape_manual(name = "reg:", values = c(21, 23)) +
  scale_color_manual(name = "reg:", values = c("red", "yellow")) + 
  xlab("Body size") +
  ylab("Trophic position") +
  ggtitle("Body size vs. Trophic position") +
  theme_bw()

# Second plot (single trend line):

ggplot(data = bloom_df, aes(x = logbodysize, y = trophic_position) ) +
  geom_point(aes(fill = reg, shape = reg), size=3, colour = "black") + 
  stat_smooth(colour = "black", method='lm', linetype='dashed', size=1) + 
  scale_fill_manual(name = "reg:", values = c("red", "yellow")) + 
  scale_shape_manual(name = "reg:", values = c(21, 23)) +
  xlab("Body size") +
  ylab("Trophic position") +
  ggtitle("Body size vs. Trophic position") +
  theme_bw()

#################################################################################

## Task 2: Continuous vs. categorical ##


#logbodysize and trophic_position need to be turned into long format

#library(reshape2)

head(bloom_df)


#bloom_df_long <- melt(bloom_df,
                  # ID variables - all the variables to keep but not split apart on
#                  id.vars=c("taxa", "genus", "species", "reg"),
                  # The source columns
#                  measure.vars=c("logbodysize", "trophic_position"),
                  # Name of the destination column that will identify the original
                  # column that the measurement came from
#                  variable.name="measurement",
#                  value.name="value"
#)

#head(bloom_df_long)


### logbody size to long (henious way):
logbodysize_sum_df <- bloom_df %>%
  group_by(reg) %>%
  summarise(mean = mean(logbodysize, na.rm = TRUE), # mean
            sd = sd(logbodysize, na.rm = TRUE), # standard deviation
            n = n()) %>% # count
  mutate(se = sd / sqrt(n), # standard error
         ci = 1.96 * se) # 95% confidence interval

logbodysize_sum_df$Trait <- 'logbodysize'

## trophic_position:
trophic_position_sum_df <- bloom_df %>%
  group_by(reg) %>%
  summarise(mean = mean(trophic_position, na.rm = TRUE), # mean
            sd = sd(trophic_position, na.rm = TRUE), # standard deviation
            n = n()) %>% # count
  mutate(se = sd / sqrt(n), # standard error
         ci = 1.96*se) # 95% confidence interval

trophic_position_sum_df$Trait <- 'trophic_position'

# combine data.frames:
bloom_sum_df <- rbind(logbodysize_sum_df,trophic_position_sum_df)
bloom_sum_df



### from wide to long (easy way):

bloom_long_df <- bloom_df %>%
  pivot_longer(cols = c(logbodysize, trophic_position), names_to = "Trait", values_to = "value")

bloom_long_df[1:10, ]


#### Plot 2.1 ####

ggplot(data = bloom_long_df, aes(x = reg, y = value, fill = reg)) +
                  facet_wrap(~Trait, nrow = 1, scales = "free") + 
                  stat_summary(fun = mean, geom = "bar") +
                  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", color = "black") +
                  theme_bw()


#### Plot 2.2 ####

ggplot(data = bloom_long_df, aes(x = reg, y = value, color = reg)) +
                  facet_wrap(~Trait, nrow = 1, scales = "free") + 
                  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", color = "black") + 
                  stat_summary(fun = mean, geom = "point", size = 5) +
                  theme_bw()
              

#### Plot 2.3 ####

ggplot(data = bloom_long_df, aes(x = reg, y = value, fill = reg)) +
                  facet_wrap(~Trait, nrow = 1, scales = "free") +
                  geom_boxplot() +
                  theme_bw()


#### Plot 2.4 ####

ggplot(data = bloom_long_df, aes(x = reg, y = value, fill = reg)) +
        facet_wrap(~Trait, nrow = 1, scales = "free") + 
        geom_sina(size = 3, pch=21, color = "black") +
        stat_summary(fun.data = mean_cl_boot, geom = "errorbar", color = "black") + 
        stat_summary(fun = mean, geom = "point", size = 5, colour = "black") +
        guides(fill=guide_legend(override.aes = list(size = 3, pch=21, color = "black"))) +   ## legends or axes overrides stuff and fixes it      
        theme_bw() 
   #     + theme(legend.position = "none")  # removes the legend (you need a separate theme())


#################################################################################

## Task 3: Layering your plots ##

# put them together using patchwork or ggarrange

bad <- ggplot(data = bloom_long_df, aes(x = reg, y = value, fill = reg)) + 
              facet_wrap(~Trait, nrow = 1) + 
              geom_bar(stat = "identity", position = "dodge") +
              ggtitle("Bad") + 
              theme(title = element_text(size = 20, colour = "black"))

bad


good <- ggplot(data = bloom_long_df, aes(x = reg, y = value, fill = reg)) +
              facet_wrap(~Trait, nrow = 1, scales = "free") + 
              geom_sina(size = 3, pch=21, color = "black") +
              stat_summary(fun.data = mean_cl_boot, geom = "errorbar", color = "black") + 
              stat_summary(fun = mean, geom = "point", size = 5, colour = "black") +
              guides(fill=guide_legend(override.aes = list(size = 3, pch=21, color = "black"))) +   ## legends or axes overrides stuff and fixes it      
              ggtitle("Good") +
              theme_bw() 

good


bad + good
