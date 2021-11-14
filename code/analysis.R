library(tidyverse)

asp <- read.csv("./data_raw/ASP_data.csv", header = TRUE)
gtm <- read.csv("./data_raw/GTM_data.csv", header = TRUE)

### paired samples t-test of before-after call rates asp ----

#H0:m=0
#Ha:m>0

###create a df for visualizing data and running summary statistics

before <- asp$RateBefore
after <- asp$RateAfter

summarydf <- data.frame( 
    group = rep(c("before", "after"), each = 94),
    c_rate = c(before,  after)
)

###Calculate summary statistics of asp data
library(dplyr)

group_by(summarydf, group) %>%
    summarise(
        count = n(),
        mean = mean(c_rate, na.rm = TRUE),
        sd = sd(c_rate, na.rm = TRUE)
    )

###Visualize data using boxplots

library("ggpubr")

ggboxplot(summarydf, x = "group", y = "c_rate", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("before", "after"),
          ylab = "Call rate", xlab = "Groups")

###show the paired connections between before and after boxplots

# install.packages("PairedData")
library(PairedData)

# Subset call rate data before treatment
before <- subset(summarydf,  group == "before", c_rate,
                 drop = TRUE)
# subset call rate data after treatment
after <- subset(summarydf,  group == "after", c_rate,
                drop = TRUE)
# Plot paired data
pd <- paired(before, after)
plot(pd, type = "profile") + theme_bw()

#wow! I'm pretty sure if there was ANY relationship it would not look like a cats cradle but lets run the t-test anyways...

###paired t-test assumptions
#Assumption 1: Are the two samples paired?
    #yes, samples were collected before and after anthropogenic noise events
#Assumption 2: Is this a large sample?
    #yes, n>30 thus we can assume a normal distribution

###Compute t-test
ttest <- t.test(before, after, "greater", paired = TRUE)
ttest
#pval = 0.6754

### One-Way Repeated Measures ANOVA before-during-after call rates asp ----

#https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/
