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
### [1] Boxplot - SMR vs. time by ploidy - heated, heated+desiccation

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('respirometry/output'); getwd()

trt_list <- read_excel("processed_summary_oyster.xlsx", sheet = "trt_list", col_names = TRUE)

HEAT_plot            <- read_excel("processed_summary_oyster.xlsx", sheet = "combo", col_names = TRUE)
HEAT_plot$ploidy     <- factor(HEAT_plot$ploidy, levels=c("D","T"),ordered=TRUE)
HEAT_plot$timepoint  <- factor(HEAT_plot$timepoint, levels=c("-10","1","2","6","10"),ordered=TRUE)
HEAT_plot$trt_list   <- factor(HEAT_plot$trt_list,levels=trt_list$trt_list,ordered=TRUE)

HEAT_plot_control <- HEAT_plot %>% filter(trt == "control")
HEAT_plot_heat    <- HEAT_plot %>% filter(trt == "heat_only" | trt == "desiccation")
HEAT_plot_desiccation <- HEAT_plot %>% filter(trt == "desiccation" & ploidy == "T")


HEAT_plot_desiccation %>%
  group_by(timepoint) %>%
  get_summary_stats(SMR, type = "mean_sd")


bp1 <- ggplot(HEAT_plot_control, aes(x=timepoint, y=SMR, group=as.factor(trt_list), fill=trt_list)) +
        geom_boxplot(colour = "grey30", size = 0.8,outlier.colour="grey30", outlier.shape = 16,
                     outlier.size=1, notch=FALSE) +
        scale_fill_manual(values=trt_list$trt_colors) +
        # geom_point() +
        scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.52)) +
        # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) + 
        my_theme

bp1

bp2 <- ggplot(HEAT_plot_heat, aes(x=timepoint, y=SMR, group=as.factor(trt_list), fill=trt_list)) +
        geom_boxplot(colour = "grey30", size = 0.8,outlier.colour="grey30", outlier.shape = 16,
                     outlier.size=1, notch=FALSE) +
        scale_fill_manual(values=trt_list$trt_colors[5:24]) +
        # geom_point() +
        scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.52)) +
        # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) + 
        my_theme

bp2

bp3 <- ggplot(HEAT_plot_desiccation, aes(x=timepoint, y=SMR, group=as.factor(trt_list), fill=trt_list)) +
  geom_boxplot(colour = "black", size = 0.8,outlier.colour="black", outlier.shape = 16,
               outlier.size=1, notch=FALSE) +
  scale_fill_manual(values=trt_list$trt_colors[15:19]) +
  # geom_point() +
  scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.52)) +
  # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) + 
  my_theme

bp3



current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('..'); setwd('202107_EXP2/plots'); getwd()

ggsave("BOXPLOT_SMR_timeseries_control.jpeg",
       plot   = bp1,
       dpi    = 600,
       device = "jpeg",
       width  = 4,
       height = 6,
       units  = "in")


ggsave("BOXPLOT_SMR_timeseries_heated.jpeg",
       plot   = bp2,
       dpi    = 600,
       device = "jpeg",
       width  = 12,
       height = 6,
       units  = "in")

ggsave("BOXPLOT_SMR_timeseries_dess_T.jpeg",
       plot   = bp3,
       dpi    = 600,
       device = "jpeg",
       width  = 6,
       height = 5,
       units  = "in")

###########################################################################################################################
### [2] Linegraph - SMR vs. time by ploidy - heated

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('respirometry/output'); getwd()

trt_list <- read_excel("processed_summary_oyster.xlsx", sheet = "trt_list", col_names = TRUE)

HEAT_plot            <- read_excel("processed_summary_oyster.xlsx", sheet = "combo", col_names = TRUE)
HEAT_plot$ploidy     <- factor(HEAT_plot$ploidy, levels=c("D","T"),ordered=TRUE)
HEAT_plot$timepoint  <- factor(HEAT_plot$timepoint, levels=c("-10","1","2","6","10"),ordered=TRUE)
# HEAT_plot$trt        <- factor(HEAT_plot$trt,levels=trt_list$trt_list,ordered=TRUE)

HEAT_plot_control <- HEAT_plot %>% filter(trt == "control")
HEAT_plot_heat    <- HEAT_plot %>% filter(trt == "heat_only")
HEAT_plot_desi    <- HEAT_plot %>% filter(trt == "desiccation")

p1 <- ggplot(HEAT_plot_heat, aes(x=timepoint, y=SMR, group=as.factor(ID), color=trt_list)) +
      # geom_point() +
      geom_line(size=1) +
      # facet_wrap(~ID) +
      scale_color_manual(values=c(trt_list$trt_colors[5:9],trt_list$trt_colors[15:19])) +
      scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.52)) +
      # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) +
      my_theme

p1

p2 <- ggplot(HEAT_plot_desi, aes(x=timepoint, y=SMR, group=as.factor(ID), color=trt_list)) +
      # geom_point() +
      geom_line(size=1) +
      # facet_wrap(~ID) +
      scale_color_manual(values=c(trt_list$trt_colors[10:14],trt_list$trt_colors[20:24])) +
      scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.52)) +
      # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) +
      my_theme

p2


current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("LINEGRAPH_SMR_timeseries_heated.tiff",
       plot   = p1,
       dpi    = 600,
       device = "jpeg",
       width  = 12,
       height = 6,
       units  = "in")

ggsave("LINEGRAPH_SMR_timeseries_desiccation.tiff",
       plot   = p2,
       dpi    = 600,
       device = "jpeg",
       width  = 12,
       height = 6,
       units  = "in")

###########################################################################################################################
### [3] Linegraph - SMR vs. time by ploidy - oysters that died in heated trt

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('respirometry/output'); getwd()

trt_list <- read_excel("processed_summary.xlsx", sheet = "trt_list", col_names = TRUE)

MR_plot           <- read_excel("processed_summary.xlsx", sheet = "heat", col_names = TRUE)
MR_plot           <- filter(MR_plot,MR_plot$death=="yes")
MR_plot$ploidy    <- factor(MR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
# MR_plot$temp      <- factor(MR_plot$temp, levels=c("10","30"),ordered=TRUE)
MR_plot$timepoint <- factor(MR_plot$timepoint, levels=c("-10","1","2","6","10"),ordered=TRUE)
MR_plot$trt       <- factor(MR_plot$trt,levels=trt_list$trt_list,ordered=TRUE)

p3 <- ggplot(MR_plot, aes(x=timepoint, y=SMR,group=as.factor(ID),color=ploidy)) +
      # geom_point() +
      geom_line(size=1) +
      # facet_wrap(~ID) +
      scale_color_manual(values=c("royalblue1","orangered1")) +
      scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.5)) +
      # scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 22)) +
      # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) +
      my_theme

p3

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("LINEGRAPH_SMR_death.tiff",
       plot   = p3,
       dpi    = 600,
       device = "jpeg",
       width  = 5,
       height = 5,
       units  = "in")


###########################################################################################################################
### [4] Boxplot - SMR vs. time by ploidy - control treatment

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('respirometry/output'); getwd()

MR_plot           <- read_excel("processed_summary.xlsx", sheet = "control", col_names = TRUE)
MR_plot$ploidy    <- factor(MR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
# MR_plot$temp      <- factor(MR_plot$temp, levels=c("10","30"),ordered=TRUE)
MR_plot$timepoint <- factor(MR_plot$timepoint, levels=c("-10","10"),ordered=TRUE)
MR_plot$trt       <- factor(MR_plot$trt,levels=c('D_-10','T_-10','D_10','T_10'),ordered=TRUE)

bp2 <- ggplot(MR_plot, aes(x=timepoint, y=SMR, group=as.factor(trt), fill=ploidy)) +
  geom_boxplot(colour = "grey30", size = 0.8,outlier.colour="grey30", outlier.shape = 16,
               outlier.size=1, notch=FALSE) +
  scale_fill_manual(values=c("royalblue1", "orangered1")) +
  # geom_point() +
  scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.5)) +
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

bp2

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('..'); setwd('202107_EXP2/plots'); getwd()

ggsave("BOXPLOT_SMR_timeseries_control.jpeg",
       plot   = bp2,
       dpi    = 600,
       device = "jpeg",
       width  = 4,
       height = 6,
       units  = "in")

###########################################################################################################################
### [5] Linegraph - SMR vs. time by ploidy - control

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('respirometry/output'); getwd()

MR_plot           <- read_excel("processed_summary.xlsx", sheet = "control", col_names = TRUE)
MR_plot$ploidy    <- factor(MR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
# MR_plot$temp      <- factor(MR_plot$temp, levels=c("10","30"),ordered=TRUE)
MR_plot$timepoint <- factor(MR_plot$timepoint, levels=c("-10","10"),ordered=TRUE)
MR_plot$trt       <- factor(MR_plot$trt,levels=c('D_-10','T_-10','D_10','T_10'),ordered=TRUE)

p4 <- ggplot(MR_plot, aes(x=timepoint, y=SMR,group=as.factor(ID),color=ploidy)) +
  geom_point() +
  geom_line(size=1) +
  # facet_wrap(~ID) +
  scale_color_manual(values=c("royalblue1","orangered1")) +
  scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.5)) +
  # scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 22)) +
  # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) +
  my_theme

p4

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("LINEGRAPH_SMR_control.jpeg",
       plot   = p4,
       dpi    = 600,
       device = "jpeg",
       width  = 4,
       height = 6,
       units  = "in")

###########################################################################################################################
### [6] Boxplot/Linegraph - FR vs. time by ploidy/treatment - heated

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('feeding_rate'); getwd()

trt_list <- read_excel("FR.xlsx", sheet = "trt_list", col_names = TRUE)

FR_plot           <- read_excel("FR.xlsx", sheet = "heat", col_names = TRUE)
FR_plot$ploidy    <- factor(FR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
# MR_plot$temp      <- factor(MR_plot$temp, levels=c("10","30"),ordered=TRUE)
FR_plot$timepoint <- factor(FR_plot$timepoint, levels=c("-10","1","2","6","10"),ordered=TRUE)
FR_plot$trt_list       <- factor(FR_plot$trt_list,levels=trt_list$trt_list,ordered=TRUE)

bp3 <- ggplot(FR_plot, aes(x=timepoint, y=size_corrected_FR, group=as.factor(trt_list), fill=trt_list)) +
       geom_boxplot(colour = "grey30", size = 0.8,outlier.colour="grey30", outlier.shape = 16,
                     outlier.size=1, notch=FALSE) +
       scale_fill_manual(values=trt_list$trt_colors) +
       # geom_point() +
       scale_y_continuous(breaks = seq(0, 6e5, 1e5), limits = c(0, 6e5)) +
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

bp3

p5 <- ggplot(FR_plot, aes(x=timepoint, y=size_corrected_FR,group=as.factor(ID),color=trt_list)) +
      # geom_point() +
      geom_line(size=1) +
      # facet_wrap(~ID) +
      scale_color_manual(values=trt_list$trt_colors) +
      scale_y_continuous(breaks = seq(0, 6e5, 1e5), limits = c(0, 6e5)) +
      # scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.5)) +
      # scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 22)) +
      # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) +
      my_theme

p5

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('..'); setwd('202107_EXP2/plots'); getwd()

ggsave("BOXPLOT_FR_timeseries_heated.jpeg",
       plot   = bp3,
       dpi    = 600,
       device = "jpeg",
       width  = 12,
       height = 6,
       units  = "in")

ggsave("LINEGRAPH_FR_timeseries_heated.jpeg",
       plot   = p5,
       dpi    = 600,
       device = "jpeg",
       width  = 12,
       height = 6,
       units  = "in")

#####################################################################################################
### [7] Linegraph - FR vs. time by ploidy - all oysters

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('feeding_rate'); getwd()

trt_list <- read_excel("FR.xlsx", sheet = "trt_list", col_names = TRUE)

FR_plot           <- read_excel("FR.xlsx", sheet = "heat", col_names = TRUE)
FR_plot$ploidy    <- factor(FR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
# MR_plot$temp      <- factor(MR_plot$temp, levels=c("10","30"),ordered=TRUE)
FR_plot$timepoint <- factor(FR_plot$timepoint, levels=c("-10","1","2","6","10"),ordered=TRUE)
FR_plot$trt       <- factor(FR_plot$trt,levels=trt_list$trt_list,ordered=TRUE)

p5 <- ggplot(FR_plot, aes(x=timepoint, y=size_corrected_FR,group=as.factor(ID),color=ploidy)) +
  # geom_point() +
  geom_line(size=1) +
  # facet_wrap(~ID) +
  scale_color_manual(values=c("royalblue1","orangered1")) +
  scale_y_continuous(breaks = seq(0, 6e5, 1e5), limits = c(0, 6e5)) +
  # scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.5)) +
  # scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 22)) +
  # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) +
  my_theme

p5

p6 <- ggplot(FR_plot, aes(x=timepoint, y=size_corrected_FR,group=as.factor(ID),color=ploidy)) +
  # geom_point() +
  geom_line(size=1) +
  facet_wrap(~ID) +
  scale_color_manual(values=c("royalblue1","orangered1")) +
  scale_y_continuous(breaks = seq(0, 6e5, 1e5), limits = c(0, 6e5)) +
  # scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.5)) +
  # scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 22)) +
  # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) +
  my_theme

p6

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("LINEGRAPH_FR_timeseries_heated.jpeg",
       plot   = p5,
       dpi    = 600,
       device = "jpeg",
       width  = 9,
       height = 6,
       units  = "in")

ggsave("LINEGRAPH_FR_timeseries_heated_facet.jpeg",
       plot   = p6,
       dpi    = 600,
       device = "jpeg",
       width  = 30,
       height = 20,
       units  = "in")


###########################################################################################################################
### [8] Linegraph - FR vs. time by ploidy - oysters that died in heated trt

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('feeding_rate'); getwd()

trt_list <- read_excel("FR.xlsx", sheet = "trt_list", col_names = TRUE)

FR_plot           <- read_excel("FR.xlsx", sheet = "heat", col_names = TRUE)
FR_plot           <- filter(FR_plot, FR_plot$death == "yes")
FR_plot$ploidy    <- factor(FR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
# MR_plot$temp      <- factor(MR_plot$temp, levels=c("10","30"),ordered=TRUE)
FR_plot$timepoint <- factor(FR_plot$timepoint, levels=c("-10","1","2","6","10"),ordered=TRUE)
FR_plot$trt       <- factor(FR_plot$trt,levels=trt_list$trt_list,ordered=TRUE)

p7 <- ggplot(FR_plot, aes(x=timepoint, y=size_corrected_FR,group=as.factor(ID),color=ploidy)) +
      # geom_point() +
      geom_line(size=1) +
      # facet_wrap(~ID) +
      scale_color_manual(values=c("royalblue1","orangered1")) +
      scale_y_continuous(breaks = seq(0, 6e5, 1e5), limits = c(0, 6e5)) +
      # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) +
      my_theme

p7

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("LINEGRAPH_FR_death.jpeg",
       plot   = p7,
       dpi    = 600,
       device = "jpeg",
       width  = 5,
       height = 5,
       units  = "in")

#####################################################################################################
### [9] Boxplot - FR vs. time by ploidy - control

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('feeding_rate'); getwd()

trt_list <- read_excel("FR.xlsx", sheet = "trt_list", col_names = TRUE)

FR_plot           <- read_excel("FR.xlsx", sheet = "control", col_names = TRUE)
# FR_plot           <- filter(FR_plot, FR_plot$death == "yes")
FR_plot$ploidy    <- factor(FR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
# MR_plot$temp      <- factor(MR_plot$temp, levels=c("10","30"),ordered=TRUE)
FR_plot$timepoint <- factor(FR_plot$timepoint, levels=c("-10","1","2","6","10"),ordered=TRUE)
FR_plot$trt       <- factor(FR_plot$trt,levels=trt_list$trt_list,ordered=TRUE)


bp4 <- ggplot(FR_plot, aes(x=timepoint, y=size_corrected_FR, group=as.factor(trt), fill=ploidy)) +
      geom_boxplot(colour = "grey30", size = 0.8,outlier.colour="grey30", outlier.shape = 16,
                   outlier.size=1, notch=FALSE) +
      scale_fill_manual(values=c("royalblue1", "orangered1")) +
      # geom_point() +
      scale_y_continuous(breaks = seq(0, 6e5, 1e5), limits = c(0, 6e5)) +
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

bp4

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('..'); setwd('202107_EXP2/plots'); getwd()

ggsave("BOXPLOT_FR_timeseries_control.jpeg",
       plot   = bp4,
       dpi    = 600,
       device = "jpeg",
       width  = 4,
       height = 6,
       units  = "in")

#####################################################################################################
### [10] LINEGRAPH - FR vs. time by ploidy - control

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('feeding_rate'); getwd()

trt_list <- read_excel("FR.xlsx", sheet = "trt_list", col_names = TRUE)

FR_plot           <- read_excel("FR.xlsx", sheet = "control", col_names = TRUE)
# FR_plot           <- filter(FR_plot, FR_plot$death == "yes")
FR_plot$ploidy    <- factor(FR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
# MR_plot$temp      <- factor(MR_plot$temp, levels=c("10","30"),ordered=TRUE)
FR_plot$timepoint <- factor(FR_plot$timepoint, levels=c("-10","1","2","6","10"),ordered=TRUE)
FR_plot$trt       <- factor(FR_plot$trt,levels=trt_list$trt_list,ordered=TRUE)

p8 <- ggplot(FR_plot, aes(x=timepoint, y=size_corrected_FR,group=as.factor(ID),color=ploidy)) +
      geom_point() +
      geom_line(size=1) +
      # facet_wrap(~ID) +
      scale_color_manual(values=c("royalblue1","orangered1")) +
      scale_y_continuous(breaks = seq(0, 6e5, 1e5), limits = c(0, 6e5)) +
      # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) +
      my_theme

p8

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("LINEGRAPH_FR_control.jpeg",
       plot   = p8,
       dpi    = 600,
       device = "jpeg",
       width  = 4,
       height = 6,
       units  = "in")



#####################################################################################################
### [6] Kaplan-Meier - mortality vs time - heated
# 
# current_path <- getActiveDocumentContext()$path
# setwd(dirname(current_path )); setwd('mortality'); getwd()
# 
# # trt_list <- read_excel("morphometrics.xlsx", sheet = "trt_list", col_names = TRUE)
# 
# # mortality barplot
# Mort_plot        <- read_excel("mortality.xlsx", sheet = "heat", col_names = TRUE)
# Mort_plot$ploidy <- factor(Mort_plot$ploidy, ordered=TRUE)
# # Mort_plot$temp   <- factor(Mort_plot$temp, levels=c("30","35","36","38","40"),ordered=TRUE)
# 
# library(survival)
# library(ggplot2)
# library(dplyr)
# library(ggfortify)
# 
# km     <- with(Mort_plot, Surv(days,ploidy))
# km_fit <- survfit(Surv(days) ~ 1, data=Mort_plot)
# summary(km_fit)
# autoplot(km_fit)
# 
# km_trt_fit <- survfit(Surv(days) ~ ploidy, data=Mort_plot)
# plot(km_trt_fit, ylim=c(0.5,1))
# summary(km_trt_fit)
# autoplot(km_trt_fit) + ylim=c(50,100) +  my_theme
# 
# aa_fit <-aareg(Surv(days) ~ ploidy, 
#                data = Mort_plot)
# aa_fit
# 
# km_trt_fit
# 
# p1 <- ggplot(Mort_plot, aes(x=as.factor(temp),y=mortality,fill=p)) +
#   geom_errorbar(aes(ymin=mortality-(mortality*0.2), ymax=mortality+se), width=0.2, position=position_dodge(0.9),colour="grey30", size=1.2) +
#   geom_col(position="dodge2",fill=c("royalblue1","orangered1","royalblue1","orangered1","royalblue1","orangered1","royalblue1","orangered1","royalblue1","orangered1")) +
#   scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 120)) +
#   scale_x_discrete(c(30,35,36,38,40)) +
#   my_theme
# 
# p1
# 
# 
# ###########################################################################################################################
# ### STATS
# 
# library(tidyverse)
# library(ggpubr)
# library(rstatix)
# 
# 
# 
# x = MR_plot$SMR
# qqnorm(x) # check for normality
# qqline(x) # Draw the line
# result <- shapiro.test(x) # p-value fail = good, don't need transformation
# print(result$p.value)
# if(result$p.value<0.05)     {
#   x_johnson <- RE.Johnson(x) # transform
#   x_transformed = x_johnson$transformed
#   qqnorm(x_transformed) # check linearity of tranformed data
#   qqline(x_transformed)
#   print(shapiro.test(x_transformed))
#   x <- x_transformed  
#   print("transformed!",quote=FALSE)}
# shapiro.test(x)
# 
# MR_plot$SMR <- x
# 
# 
# summary(my_anova <- aov(SMR ~ ploidy*timepoint*|, data = MR_plot))
# TukeyHSD(my_anova, ordered = TRUE)
# 
# 
# res.aov <- anova_test(
#   data = MR_plot, dv = SMR, wid = as.factor(MR_plot$ID),
#   within = c(timepoint, ploidy)
# )
# get_anova_table(res.aov)

######################################################################################################
## STATS - heated treatment

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('respirometry/output'); getwd()

trt_list <- read_excel("processed_summary_oyster.xlsx", sheet = "trt_list", col_names = TRUE)

MR_plot           <- read_excel("processed_summary_oyster.xlsx", sheet = "combo", col_names = TRUE)
MR_plot$ploidy    <- factor(MR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
# MR_plot$temp      <- factor(MR_plot$temp, levels=c("10","30"),ordered=TRUE)
MR_plot$timepoint <- factor(MR_plot$timepoint, levels=c("-10","1","2","6","10"),ordered=TRUE)
MR_plot$trt_list       <- factor(MR_plot$trt_list,levels=trt_list$trt_list,ordered=TRUE)


HEAT_plot_desiccation

## Transform data
x = HEAT_plot_desiccation$SMR
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

HEAT_plot_desiccation$SMR <- x

# Statistical testings
summary(aov(SMR ~ (ploidy*timepoint) + Error(factor(ID)), data = MR_plot))
summary(aov(SMR ~ (timepoint) + Error(factor(ID)), data = HEAT_plot_desiccation))


tx <- with(MR_plot, interaction(ploidy,timepoint))
amod <- aov(SMR ~ tx, data=MR_plot)
HSD.test(amod, "tx", group=TRUE, console=TRUE)

tx <- with(HEAT_plot_desiccation, interaction(ploidy,timepoint))
amod <- aov(SMR ~ tx, data=HEAT_plot_desiccation)
HSD.test(amod, "tx", group=TRUE, console=TRUE)


# FR
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); setwd('feeding_rate'); getwd()

trt_list <- read_excel("FR.xlsx", sheet = "trt_list", col_names = TRUE)

FR_plot           <- read_excel("FR.xlsx", sheet = "heat", col_names = TRUE)
# FR_plot           <- filter(FR_plot, FR_plot$death == "yes")
FR_plot$ploidy    <- factor(FR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
# MR_plot$temp      <- factor(MR_plot$temp, levels=c("10","30"),ordered=TRUE)
FR_plot$timepoint <- factor(FR_plot$timepoint, levels=c("-10","1","2","6","10"),ordered=TRUE)
FR_plot$trt       <- factor(FR_plot$trt,levels=trt_list$trt_list,ordered=TRUE)

## Transform data
x = FR_plot$size_corrected_FR
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

FR_plot$size_corrected_FR <- x

# Statistical testings
summary(aov(size_corrected_FR ~ ploidy*timepoint, data = FR_plot))

tx <- with(FR_plot, interaction(ploidy,timepoint))
amod <- aov(size_corrected_FR ~ tx, data= FR_plot)
HSD.test(amod, "tx", group=TRUE, console=TRUE)


# summary(my_anova <- aov(mortality ~ temp*p, data = MR_plot))
# TukeyHSD(my_anova, ordered = TRUE)
# 
# summary(my_anova <- aov(umol_L_hr ~ timepoint*temp*ploidy, data = MR_plot))
# TukeyHSD(my_anova, ordered = TRUE)
