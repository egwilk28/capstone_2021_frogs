# install.packages("tidyverse")
library(tidyverse)
# install.packages("dplyr")
library(dplyr)
# install.packages("ggpubr")
library(ggpubr)
# install.packages("PairedData")
library(PairedData)
# install.packages("rstatix")
library(rstatix)

asp <- read.csv("./data_raw/ASP_data.csv", header = TRUE)
gtm <- read.csv("./data_raw/GTM_data.csv", header = TRUE)

### Paired t-test of before-after call rates asp ----

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
group_by(summarydf, group) %>%
    summarise(
        count = n(),
        mean = mean(c_rate, na.rm = TRUE),
        sd = sd(c_rate, na.rm = TRUE)
    )

###Visualize data using boxplots
ggboxplot(summarydf, x = "group", y = "c_rate", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("before", "after"),
          ylab = "Call rate", xlab = "Groups")

###show the paired connections between before and after boxplots
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

#summarize and visualize data
summarydf <- asp %>%
    gather(key = "treatment", value = "call_rate", RateBefore, RateDuring, RateAfter) %>%
    convert_as_factor(treatment) #prepare a df with a column called "treatment" that shows before/during/after

#add an ID column to df bc the ANOVA test needs it
summarydf$ID <- seq(1: 282) #282 is the dim of summarydf

summarydf <- summarydf %>% convert_as_factor(ID) #convert ID into a factor type

summarydf %>%
    group_by(treatment) %>%
    get_summary_stats(call_rate, type = "mean_sd") #get mean call rate and sd of treatments

bxp <- ggboxplot(summarydf, x = "treatment", y = "call_rate", xlab = "Treatment", ylab = "Call rate", main = "Call rates before, during, and after noise events at ASP", order = c("RateBefore", "RateDuring", "RateAfter"), ggtheme = theme_gray()) # create boxplot of treatments
bxp


### check ANOVA assumptions
#identify extreme outliers
extremeo <- summarydf %>%
    group_by(treatment) %>%
    identify_outliers(call_rate) #no extreme outliers

#assess normality
ggqqplot(summarydf, "call_rate", facet.by = "treatment")
#it's not perfect but let's run the test anyways

aov.test <- anova_test(data = summarydf, dv = call_rate, wid = ID, within = treatment)
get_anova_table(aov.test)

### Two sample t-test of comparing call rates at ASP and GTM ----

#We predict that the avg call rate of species at asp will be less than the avg call rate of species at the gtm
#H0:m asp = m gtm
#Ha:m asp < m gtm

#summarize and visualize data

ttestdf <- read.csv("./data_raw/2_samp_ttest_df.csv", header = TRUE)

