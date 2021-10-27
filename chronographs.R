### Meta ###
# Author: Andreas Höhn
# Version: 1.3
# Date: 27.10.2021
# About: Get data from TraMineR-Specific Objects for Chronographs and 
# estimate 95% CIs assuming it's a population proportion n > 30, and X ~ B(n, p)
# now in line with Rstudios coding / naming / assigning standards!

# -------------------------------------- # 

### Notes ###
# analysis part of the code uses the example provided here: (accessed: 23.08.2021)
# https://cran.r-project.org/web/packages/TraMineRextras/TraMineRextras.pdf (p.20/21)
# However, we run it on the entire data set 

# -------------------------------------- # 

### Load Libraries ###
library(TraMineR)
library(TraMineRextras)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)

### Load Data ###
data(biofam)

# -------------------------------------- # 

### Analysis Example ###

# define labels 
biofam_lab <- c("Parent", "Left", "Married",
                "Left+Marr","Child", "Left+Child",
                "Left+Marr+Child", "Divorced")
# analysis 
alph <- seqstatl(biofam[10:25])
biofam_seq <- seqdef(biofam, 10:25, alphabet = alph, labels = biofam_lab)

# create grouping variable with NA coded as "unknown" 
lang <- as.vector(biofam[["plingu02"]])
lang[is.na(lang)] <- "unknown"
lang <- factor(lang)

# chronograph by language group 
seqdplot(biofam_seq, group = lang)

# -------------------------------------- # 

### Extract Underlying Sequences from an object created with seqdef() ###
# here: by grouping variable language ("lang") 
lev <- levels(lang)
l <- length(lev)
seq_list <- list()
for (i in 1:l){
  seq_list[[i]] <- biofam_seq[lang == lev[i], ]
}

# -------------------------------------- # 

### Reshaping Function ###
# estimates fraction, group size, 95% CI
.ExtractData <- function(data_in, group_name){
  data_out <- data_in
  # create id and reshape
  data_out$id <- seq(1:dim(data_out)[1])
  data_out <- data_out %>%
    gather(key = "time", value = "value", -id) %>% 
    mutate(time = parse_number(time)) %>% 
    mutate(value = as.factor(value)) %>%
    arrange(id)
  # create fraction and group size
  group_size <- max(data_out$id)
  data_out <- data_out %>% 
    group_by(time) %>% 
    count(value) %>% 
    mutate(fraction = n / group_size) %>%
    mutate(group_size = group_size) %>%
    # 95% confidence interval for a population proportion n>30, and X ~ B(n, p) 
    mutate(fraction_95_low  = fraction - 
             1.96 * sqrt((fraction * (1 - fraction)) / group_size)) %>%
    mutate(fraction_95_high = fraction + 
             1.96 * sqrt((fraction * (1 - fraction)) / group_size)) %>%
    # no lower values smaller 1 / no higher values smaller 1
    mutate(fraction_95_low  = if_else(fraction_95_low  < 0, 0, fraction_95_low)) %>%    
    mutate(fraction_95_high = if_else(fraction_95_high > 1, 1, fraction_95_high)) %>% 
    # assign group name
    mutate(group = as.factor(group_name)) %>%
    # relocate columns 
    relocate(group, value, time, group_size, n,)
  # return #
  return(data_out)
}

# run function for all groups and bind in one data set
data_all <- rbind(
  .ExtractData(data_in = seq_list[[1]], group_name = lev[1]),
  .ExtractData(data_in = seq_list[[2]], group_name = lev[2]),
  .ExtractData(data_in = seq_list[[3]], group_name = lev[3]),
  .ExtractData(data_in = seq_list[[4]], group_name = lev[4]))

# correct label using pre-defined labels 
data_all$value <- factor(data_all$value,
                         levels = c(0:7),
                         labels = biofam_lab)

# -------------------------------------- # 

### Plot Results ###

# this figure should correspond to "seqdplot(biofam.seq, group=lang)"
PletteColBl <- c("#000000", # Divorced
                 "#D55E00", # Left+Marr+Child
                 "#FF0087", # Left + Child
                 "#0072B2", # Child
                 "#FFEEB9", # Left # Marr
                 "#FFB868", # Married
                 "#CAA3EC", # Left
                 "#90E671") # Parent

# using ggplot2
ggplot(data_all, aes(x = time, y = fraction,
           ymin = fraction_95_low, ymax = fraction_95_high, fill = fct_rev(value))) + 
  geom_bar(position = "fill", stat = "identity", colour = "black", size = 0.05) + 
  scale_fill_manual(values = PletteColBl) +
  facet_wrap(~ group) + 
  labs(fill = "Family Characteristics", 
       title = "Changes in Familiy Characteristics over Time",
       subtitle = "an example how to extract and process sequence analysis data from TraMineR-specific objects",
       caption = "Source: own calculation based on biofam dataset") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(name = "Time") + 
  scale_y_continuous(name = "Percentage", # transfer share into %
                     breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_minimal()

# -------------------------------------- # 

### Extract Data for Paper ###
data_all %>% filter(group == "french" & value=="Parent" & time == 15)
