# Title: generate plots for desiccation pilot
# Author: Matthew George; mattgeorgephd@gmail.com
# Date: 07/2021

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
library(scales)


## Set ggplot theme
my_theme <- theme(line              = element_line(size=1.5),
                  rect              = element_rect(size=1.5),
                  text              = element_text(size=14,color="black"),
                  panel.background  = element_blank(),
                  panel.grid.major  = element_blank(), 
                  panel.grid.minor  = element_blank(),
                  axis.text.x       = element_text(size=16,color="black",angle=90),
                  axis.text.y       = element_text(size=16,color="black"),
                  axis.title.x      = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                  axis.title.y      = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                  axis.ticks.x      = element_line(color="black"),
                  axis.ticks.y      = element_line(color="black"),
                  # axis.line         = element_line(color = "black", size = 0.1),
                  panel.border      = element_rect(color = "black", fill=NA, size=1.5),
                  legend.key        = element_blank()
                  ) # removes background of legend bullets


###########################################################################################################################
### [1] Line graph

current_path <- getActiveDocumentContext()$path
# setwd(dirname(current_path )); setwd('feeding_rate'); getwd()

FR_plot <- read_excel("pilot.xlsx", sheet = "average", col_names = TRUE)

lims <- as.POSIXct(strptime(c("2021-07-09 08:00","2021-07-09 18:00"), format = "%Y-%m-%d %H:%M"))    

p1 <- ggplot(FR_plot, aes(x=date, y=temp)) +
            geom_smooth(aes(ymin=temp-temp_se,ymax=temp+temp_se)) +
            # geom_line(size=1) +
            # scale_color_manual(values=c("royalblue1","orangered1")) +
            scale_y_continuous(breaks = seq(10, 60, 5), limits = c(10, 55)) +
            scale_x_datetime(breaks = date_breaks("1 hour"),labels=date_format("%H:%M"))+ 
            # scale_x_date(date_labels = "%b/%d") +
            my_theme

p1

current_path <- getActiveDocumentContext()$path
# setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("temp.jpeg",
       plot   = p1,
       dpi    = 300,
       device = "jpeg",
       width  = 5,
       height = 5,
       units  = "in")

###########################################################################################################################
### [2] Line graph

current_path <- getActiveDocumentContext()$path
# setwd(dirname(current_path )); setwd('feeding_rate'); getwd()

FR_plot <- read_excel("pilot.xlsx", sheet = "oysters", col_names = TRUE)

lims <- as.POSIXct(strptime(c("2021-07-09 08:00","2021-07-09 18:00"), format = "%Y-%m-%d %H:%M"))    

library(scales)

p1 <- ggplot(FR_plot, aes(x=time, y=temp, color=as.factor(oyster))) +
      geom_smooth() +
      # geom_line(size=1) +
      # scale_color_manual(values=c("royalblue1","orangered1")) +
      scale_y_continuous(breaks = seq(10, 60, 5), limits = c(10, 55)) +
      scale_x_datetime(breaks = date_breaks("1 hour"),labels=date_format("%H:%M"))+ 
      # scale_x_date(date_labels = "%b/%d") +
      my_theme

p1

current_path <- getActiveDocumentContext()$path
# setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("oysters.jpeg",
       plot   = p1,
       dpi    = 300,
       device = "jpeg",
       width  = 6,
       height = 5,
       units  = "in")
