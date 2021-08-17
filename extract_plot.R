### Meta ###
# Author: Andreas Höhn
# Version: 1.0
# Date: 17.08.2021
# About: Extract Data from TraMineR-Specific Class Objects for Chronographs
set.seed(10) # following example in package description

# ----------------- #

### Notes ###
# analysis part of the code follows the example provided here:
# https://cran.r-project.org/web/packages/TraMineRextras/TraMineRextras.pdf
# page 20/21
# accessed: 17.08.2021
# make sure, required libraries are installed

# -------------------------------------- # 

### Load Libraries ###
library(TraMineR)
library(TraMineRextras)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)


# -------------------------------------- # 

### Analysis Example ###

# load data #
data(biofam)
# define labels #
biofam.lab <- c("Parent", "Left", "Married",
                "Left+Marr","Child", "Left+Child",
                "Left+Marr+Child", "Divorced")
# analysis #
alph <- seqstatl(biofam[10:25])
biofam <- biofam[sample(nrow(biofam),150),]
biofam.seq <- seqdef(biofam, 10:25, alphabet=alph, labels=biofam.lab)

# grouping variable #
lang <- as.vector(biofam[["plingu02"]])
lang[is.na(lang)] <- "unknown"
lang <- factor(lang)

# chronogram by language group #
seqdplot(biofam.seq, group=lang)

# -------------------------------------- # 

### Extract and Reshape Data ###

# extract sequences by subset - here: language #
lev <- levels(lang)
l <- length(lev)
seq.list <- list()
for (i in 1:l){
  seq.list[[i]] <- biofam.seq[lang==lev[i],]
}

# reshaping in functional form #
extract.data <- function(data.in, group.name){
  data.out <- data.in
  # create id and reshape
  data.out$id <- seq(1:dim(data.out)[1])
  data.out <- data.out %>%
    gather(key="time", value="value", -id) %>% 
    mutate(time = parse_number(time)) %>% 
    mutate(value = as.factor(value)) %>%
    arrange(id)
  # create percentage
  data.out <- data.out %>% 
    group_by(time) %>% 
    count(value) %>% 
    mutate(percentage = (n/max(n)) * 100) %>%
    mutate(group = as.factor(group.name))
  # return #
  return(data.out)
}

# run function for all groups
group1.test <- extract.data(data.in = seq.list[[1]],
                            group.name = lev[1])
group2.test <- extract.data(data.in = seq.list[[2]],
                            group.name = lev[2])
group3.test <- extract.data(data.in = seq.list[[3]],
                            group.name = lev[3])
group4.test <- extract.data(data.in = seq.list[[4]],
                            group.name = lev[4])

# bind data # 
data.plot <- rbind(group1.test,group2.test,group3.test,group4.test)

# correct label using pre-defined labels #
data.plot$value <- factor(data.plot$value,
                      levels=c(0:7),
                      labels=biofam.lab)

# -------------------------------------- # 

### Plot Results ###

# a neutral colour-blind palette
PletteColBl <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                 "#F0E442", "#0072B2", "#D55E00")

# this figure should correspond to "seqdplot(biofam.seq, group=lang)"
ggplot(data.plot, aes(x=time, y=percentage, fill=fct_rev(value))) + 
  geom_bar(position="fill", stat="identity", colour="black") + 
  scale_fill_manual(values=PletteColBl) +
  facet_wrap(~group) + 
  labs(fill="Legend Title") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(name = "Time") + 
  scale_y_continuous(name = "Percentage",
                     breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_bw()


