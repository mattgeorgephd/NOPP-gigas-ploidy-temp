# Title: generate plots for diploid and triploid pacific oysters temperature experiment
# Author: Matthew George; mattgeorgephd@gmail.com
# Date: 05/2021

## clear
rm(list=ls())

## Grab the WD from the file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); getwd()

## Load R packages
library(readxl)
library(ggplot2)
library(stringr)
library(tidyverse)
library(Johnson)
library(agricolae)
library(nlme)
library(multcomp)

## Set ggplot theme
my_theme <- theme(line              = element_line(size=1.5),
                  rect              = element_rect(size=1.5),
                  text              = element_text(size=14,color="black"),
                  panel.background  = element_blank(),
                  panel.grid.major  = element_blank(), 
                  panel.grid.minor  = element_blank(),
                  axis.text.x       = element_text(size=16,color="black"),
                  axis.text.y       = element_text(size=16,color="black"),
                  axis.title.x      = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                  axis.title.y      = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                  axis.ticks.x      = element_line(color="black"),
                  axis.ticks.y      = element_line(color="black"),
                  # axis.line         = element_line(color = "black", size = 0.1),
                  panel.border      = element_rect(color = "black", fill=NA, size=1.5),
                  legend.key        = element_blank()) # removes background of legend bullets


###########################################################################################################################
### [1] Boxplot - feeding_rate vs. time by ploidy

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('feeding_rate'); getwd()

trt_list <- read_excel("feeding_rate.xlsx", sheet = "trt_list", col_names = TRUE)

FR_plot           <- read_excel("feeding_rate.xlsx", sheet = "heat", col_names = TRUE)
FR_plot$ploidy    <- factor(FR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
FR_plot$temp      <- factor(FR_plot$temp, levels=c("10","30"),ordered=TRUE)
FR_plot$timepoint <- factor(FR_plot$timepoint, levels=c("-10","1","5","10","15","20"),ordered=TRUE)
FR_plot$trt       <- factor(FR_plot$trt,levels=trt_list$trt_list,ordered=TRUE)

bp1 <- ggplot(FR_plot, aes(x=timepoint, y=feeding_rate, group=as.factor(trt), fill=ploidy)) +
  geom_boxplot(colour = "grey30", size = 0.8,outlier.colour="grey30", outlier.shape = 16,
               outlier.size=1, notch=FALSE) +
  scale_fill_manual(values=c("royalblue1", "orangered1")) +
  # geom_point() +
  # scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 42)) +
  my_theme

bp1

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("BOXPLOT_feeding_rate_timeseries.tiff",
       plot   = bp1,
       dpi    = 600,
       device = "jpeg",
       width  = 9,
       height = 6,
       units  = "in")


###########################################################################################################################
### [2] Line graph - SMR vs. time by ploidy - heated

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('feeding_rate'); getwd()

trt_list <- read_excel("feeding_rate.xlsx", sheet = "trt_list", col_names = TRUE)

FR_plot           <- read_excel("feeding_rate.xlsx", sheet = "heat", col_names = TRUE)
FR_plot$ploidy    <- factor(FR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
FR_plot$temp      <- factor(FR_plot$temp, levels=c("10","30"),ordered=TRUE)
FR_plot$timepoint <- factor(FR_plot$timepoint, levels=c("-10","1","5","10","15","20"),ordered=TRUE)
FR_plot$trt       <- factor(FR_plot$trt,levels=trt_list$trt_list,ordered=TRUE)

p1 <- ggplot(FR_plot, aes(x=timepoint, y=feeding_rate,group=as.factor(ID),color=ploidy)) +
  # geom_point() +
  geom_line(size=1) +
  scale_color_manual(values=c("royalblue1","orangered1")) +
  # scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 22)) +
  # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) +
  my_theme

p1

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("LINEGRAPH_feeding_rate_timeseries.tiff",
       plot   = p1,
       dpi    = 600,
       device = "jpeg",
       width  = 9,
       height = 6,
       units  = "in")

###########################################################################################################################
### [3] Boxplot - SMR vs. time by ploidy - heated

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('respirometry/pt_whitney_exp/output'); getwd()

trt_list <- read_excel("processed_summary.xlsx", sheet = "trt_list", col_names = TRUE)

MR_plot           <- read_excel("processed_summary.xlsx", sheet = "heat", col_names = TRUE)
MR_plot$ploidy    <- factor(MR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
MR_plot$temp      <- factor(MR_plot$temp, levels=c("10","30"),ordered=TRUE)
MR_plot$timepoint <- factor(MR_plot$timepoint, levels=c("-10","1","5","10","15","20"),ordered=TRUE)
MR_plot$trt       <- factor(MR_plot$trt,levels=trt_list$trt_list,ordered=TRUE)

bp1 <- ggplot(MR_plot, aes(x=timepoint, y=SMR, group=as.factor(trt), fill=ploidy)) +
  geom_boxplot(colour = "grey30", size = 0.8,outlier.colour="grey30", outlier.shape = 16,
               outlier.size=1, notch=FALSE) +
  scale_fill_manual(values=c("royalblue1", "orangered1")) +
  # geom_point() +
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 42)) +
  # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) + 
  theme(line              = element_line(size=0.8),
        rect              = element_rect(size=1),
        text              = element_text(size=14,color="black"),
        panel.background  = element_blank(),
        panel.grid.major  = element_blank(), 
        panel.grid.minor  = element_blank(),
        axis.text.x       = element_text(size=16,color="black"),
        axis.text.y       = element_text(size=16,color="black"),
        axis.title.x      = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y      = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.ticks.x      = element_line(color="black"),
        axis.ticks.y      = element_line(color="black"),
        # axis.line         = element_line(color = "black", size = 0.1),
        panel.border      = element_rect(color = "black", fill=NA, size=1.5),
        legend.key        = element_blank()) # removes background of legend bullets

bp1

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("BOXPLOT_SMR_timeseries.tiff",
       plot   = bp1,
       dpi    = 600,
       device = "jpeg",
       width  = 9,
       height = 6,
       units  = "in")

###########################################################################################################################
### [4] Boxplot - SMR vs. time by ploidy - heated

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('respirometry/pt_whitney_exp/output'); getwd()

trt_list <- read_excel("processed_summary.xlsx", sheet = "trt_list", col_names = TRUE)

MR_plot           <- read_excel("processed_summary.xlsx", sheet = "heat", col_names = TRUE)
MR_plot$ploidy    <- factor(MR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
MR_plot$temp      <- factor(MR_plot$temp, levels=c("10","30"),ordered=TRUE)
MR_plot$timepoint <- factor(MR_plot$timepoint, levels=c("-10","1","5","10","15","20"),ordered=TRUE)
MR_plot$trt       <- factor(MR_plot$trt,levels=trt_list$trt_list,ordered=TRUE)

p1 <- ggplot(MR_plot, aes(x=timepoint, y=SMR,group=as.factor(ID),color=ploidy)) +
  # geom_point() +
  geom_line(size=1) +
  scale_color_manual(values=c("royalblue1","orangered1")) +
  # scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 22)) +
  # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) +
  my_theme

p1

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("LINEGRAPH_SMR_timeseries.tiff",
       plot   = p1,
       dpi    = 600,
       device = "jpeg",
       width  = 9,
       height = 6,
       units  = "in")

###########################################################################################################################
### [5] Boxplot - size vs. time by ploidy - heated

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('morphometrics'); getwd()

trt_list <- read_excel("morphometrics.xlsx", sheet = "trt_list", col_names = TRUE)

MR_plot           <- read_excel("morphometrics.xlsx", sheet = "heat", col_names = TRUE)
MR_plot$ploidy    <- factor(MR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
# MR_plot$temp      <- factor(MR_plot$temp, levels=c("10","30"),ordered=TRUE)
MR_plot$timepoint <- factor(MR_plot$timepoint, levels=c("-10","1","5","10","15","20"),ordered=TRUE)
MR_plot$trt       <- factor(MR_plot$trt,levels=trt_list$trt_list,ordered=TRUE)

bp1 <- ggplot(MR_plot, aes(x=timepoint, y=shell_length, group=as.factor(trt), fill=ploidy)) +
  geom_boxplot(colour = "grey30", size = 0.8,outlier.colour="grey30", outlier.shape = 16,
               outlier.size=1, notch=FALSE) +
  scale_fill_manual(values=c("royalblue1", "orangered1")) +
  # geom_point() +
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 42)) +
  # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) + 
  theme(line              = element_line(size=0.8),
        rect              = element_rect(size=1),
        text              = element_text(size=14,color="black"),
        panel.background  = element_blank(),
        panel.grid.major  = element_blank(), 
        panel.grid.minor  = element_blank(),
        axis.text.x       = element_text(size=16,color="black"),
        axis.text.y       = element_text(size=16,color="black"),
        axis.title.x      = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y      = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.ticks.x      = element_line(color="black"),
        axis.ticks.y      = element_line(color="black"),
        # axis.line         = element_line(color = "black", size = 0.1),
        panel.border      = element_rect(color = "black", fill=NA, size=1.5),
        legend.key        = element_blank()) # removes background of legend bullets

bp1

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("BOXPLOT_shell_length_timeseries.tiff",
       plot   = bp1,
       dpi    = 600,
       device = "jpeg",
       width  = 9,
       height = 6,
       units  = "in")


###########################################################################################################################
### [6] Kaplan-Meier - mortality vs time - heated

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('mortality'); getwd()

# trt_list <- read_excel("morphometrics.xlsx", sheet = "trt_list", col_names = TRUE)

# mortality barplot
Mort_plot        <- read_excel("mortality.xlsx", sheet = "heat", col_names = TRUE)
Mort_plot$ploidy <- factor(Mort_plot$ploidy, ordered=TRUE)
# Mort_plot$temp   <- factor(Mort_plot$temp, levels=c("30","35","36","38","40"),ordered=TRUE)

library(survival)
library(ggplot2)
library(dplyr)
library(ggfortify)

km     <- with(Mort_plot, Surv(days,ploidy))
km_fit <- survfit(Surv(days) ~ 1, data=Mort_plot)
summary(km_fit)
autoplot(km_fit)

km_trt_fit <- survfit(Surv(days) ~ ploidy, data=Mort_plot)
plot(km_trt_fit, ylim=c(0.5,1))
summary(km_trt_fit)
autoplot(km_trt_fit) + ylim=c(50,100) +  my_theme

aa_fit <-aareg(Surv(days) ~ ploidy, 
               data = Mort_plot)
aa_fit

km_trt_fit

p1 <- ggplot(Mort_plot, aes(x=as.factor(temp),y=mortality,fill=p)) +
  geom_errorbar(aes(ymin=mortality-(mortality*0.2), ymax=mortality+se), width=0.2, position=position_dodge(0.9),colour="grey30", size=1.2) +
  geom_col(position="dodge2",fill=c("royalblue1","orangered1","royalblue1","orangered1","royalblue1","orangered1","royalblue1","orangered1","royalblue1","orangered1")) +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 120)) +
  scale_x_discrete(c(30,35,36,38,40)) +
  my_theme

p1


###########################################################################################################################
### STATS

library(tidyverse)
library(ggpubr)
library(rstatix)



x = MR_plot$SMR
qqnorm(x) # check for normality
qqline(x) # Draw the line
result <- shapiro.test(x) # p-value fail = good, don't need transformation
print(result$p.value)
if(result$p.value<0.05)     {
  x_johnson <- RE.Johnson(x) # transform
  x_transformed = x_johnson$transformed
  qqnorm(x_transformed) # check linearity of tranformed data
  qqline(x_transformed)
  print(shapiro.test(x_transformed))
  x <- x_transformed  
  print("transformed!",quote=FALSE)}
shapiro.test(x)

MR_plot$SMR <- x


summary(my_anova <- aov(SMR ~ ploidy*timepoint*|, data = MR_plot))
TukeyHSD(my_anova, ordered = TRUE)


res.aov <- anova_test(
  data = MR_plot, dv = SMR, wid = as.factor(MR_plot$ID),
  within = c(timepoint, ploidy)
)
get_anova_table(res.aov)



## Transform data
x = FR_plot$feeding_rate
qqnorm(x) # check for normality
qqline(x) # Draw the line
result <- shapiro.test(x) # p-value fail = good, don't need transformation
print(result$p.value)
if(result$p.value<0.05)     {
  x_johnson <- RE.Johnson(x) # transform
  x_transformed = x_johnson$transformed
  qqnorm(x_transformed) # check linearity of tranformed data
  qqline(x_transformed)
  print(shapiro.test(x_transformed))
  x <- x_transformed  
  print("transformed!",quote=FALSE)}
shapiro.test(x)

FR_plot$feeding_rate <- x


# Statistical testings
summary(aov(feeding_rate ~ ploidy*timepoint*temp, data = FR_plot))

tx <- with(data, interaction(MN,DMA))
amod <- aov(kPa ~ tx, data=data)
HSD.test(amod, "tx", group=TRUE, console=TRUE)

summary(my_anova <- aov(mortality ~ temp*p, data = MR_plot))
TukeyHSD(my_anova, ordered = TRUE)

ggsave("temp_ploidy_mortality.tiff",
       plot   = p1,
       dpi    = 600,
       device = "tiff",
       width  = 5.5,
       height = 5,
       units  = "in")

summary(my_anova <- aov(umol_L_hr ~ timepoint*temp*ploidy, data = MR_plot))
TukeyHSD(my_anova, ordered = TRUE)


