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

FR_plot <- read_excel("tank_temps.xlsx", sheet = "combo", col_names = TRUE)

# lims <- as.POSIXct(strptime(c("2021-07-13 07:00","2021-07-13 14:00"), format = "%Y-%m-%d %H:%M"))    

p1 <- ggplot(data=FR_plot) +
            geom_smooth(aes(x=time,
                            y=humid_mean/2.5, 
                            ymin=humid_mean/2.5-humid_se,
                            ymax=humid_mean/2.5+humid_se,
                            group="humidity",
                            color="humidity")) +  
            geom_smooth(aes(x=time,
                            y=temp_mean, 
                            ymin=temp_mean-temp_se,
                            ymax=temp_mean+temp_se,
                            group=group,
                            color=group)) +
            scale_y_continuous(sec.axis = sec_axis( trans=~.*2.5, name="Second Axis")) +
            # geom_line(size=1) +
            # scale_color_manual(values=c("royalblue1","orangered1")) +
            # scale_y_continuous(breaks = seq(10, 60, 5), limits = c(10, 55)) +
            scale_x_datetime(breaks = date_breaks("1 hour"),labels=date_format("%H:%M"))+ 
            # scale_x_date(date_labels = "%b/%d") +
            my_theme

p1

current_path <- getActiveDocumentContext()$path
# setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("temp_combo.jpeg",
       plot   = p1,
       dpi    = 300,
       device = "jpeg",
       width  = 7,
       height = 5,
       units  = "in")

###########################################################################################################################
### [2] Line graph

current_path <- getActiveDocumentContext()$path
# setwd(dirname(current_path )); setwd('feeding_rate'); getwd()

FR_plot <- read_excel("oyster_internal_temps.xlsx", sheet = "max", col_names = TRUE)

# lims <- as.POSIXct(strptime(c("2021-07-13 07:00","2021-07-13 14:00"), format = "%Y-%m-%d %H:%M"))    

library(scales)

p1 <- ggplot(FR_plot, aes(x=shell_volume, y=max_temp)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x) +
      # geom_line(size=1) +
      # scale_color_manual(values=c("royalblue1","orangered1")) +
      scale_y_continuous(breaks = seq(10, 60, 10), limits = c(20, 60)) +
      scale_x_continuous(breaks = seq(60, 140, 20), limits = c(50, 150)) + 
      my_theme

p1

current_path <- getActiveDocumentContext()$path
# setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("max_temp.jpeg",
       plot   = p1,
       dpi    = 300,
       device = "jpeg",
       width  = 4,
       height = 5,
       units  = "in")
