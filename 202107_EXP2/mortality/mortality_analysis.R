# Title: generate line graphs for mortality
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
library(tidyverse)


## Set ggplot theme
my_theme <- theme(line              = element_line(size=1.5),
                  rect              = element_rect(size=1.5),
                  text              = element_text(size=14,color="black"),
                  panel.background  = element_blank(),
                  panel.grid.major  = element_blank(), 
                  panel.grid.minor  = element_blank(),
                  axis.text.x       = element_text(size=16,color="black"), #,angle=90),
                  axis.text.y       = element_text(size=16,color="black"),
                  axis.title.x      = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                  axis.title.y      = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                  axis.ticks.x      = element_line(color="black"),
                  axis.ticks.y      = element_line(color="black"),
                  # axis.line         = element_line(color = "black", size = 0.1),
                  panel.border      = element_rect(color = "black", fill=NA, size=1.5),
                  legend.key        = element_blank() # removes background of legend bullets
                  ) 


###########################################################################################################################
### [1] Line graph

current_path <- getActiveDocumentContext()$path
# setwd(dirname(current_path )); setwd('feeding_rate'); getwd()

FR_plot     <- read_excel("mortality.xlsx", sheet = "plot2", col_names = TRUE)
trt_list    <- read_excel("mortality.xlsx", sheet = "trt_list", col_names = TRUE)
FR_plot$trt <- factor(FR_plot$trt,levels=trt_list$trt_list,ordered=TRUE)

FR_plot_control   <- FR_plot %>% filter(trt == "D-control" | trt == "T-control")
FR_plot_heated    <- FR_plot %>% filter(trt == "D-heat_only" | trt == "T-heat_only" | trt == "D-heat_desiccation" | trt == "T-heat_desiccation")
FR_plot_des_T     <- FR_plot %>% filter(trt == "T-heat_desiccation" | trt == "D-control" | trt == "T-heat_only")

p1 <- ggplot(data=FR_plot_control,aes(x=day,y=survival,color=trt)) +
            geom_line(size=1.2,aes(linetype=trt)) +
            scale_linetype_manual(values=c("solid","solid"))+
            scale_color_manual(values=c("royalblue1","orangered1")) +
            scale_y_continuous(breaks = seq(0, 100, 10), limits = c(60,105)) +
            scale_x_continuous(breaks = seq(-30,30,10) , limits = c(-32,32))+ 
            # scale_x_date(date_labels = "%b/%d") +
            my_theme

p1

p2 <- ggplot(data=FR_plot_heated,aes(x=day,y=mortality,color=trt)) +
        geom_line(size=1.2,aes(linetype=trt)) +
        scale_linetype_manual(values=c("solid","dotted","solid","dotted"))+
        scale_color_manual(values=c("royalblue1","royalblue2","orangered1","orangered2")) +
        scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0,42)) +
        scale_x_continuous(breaks = seq(-30,30,10) , limits = c(-32,32))+ 
        # scale_x_date(date_labels = "%b/%d") +
        my_theme

p2

p3 <- ggplot(data=FR_plot,aes(x=day,y=survival,color=trt)) +
        geom_line(size=1.2,aes(linetype=trt)) +
        scale_linetype_manual(values=c("solid","dotted","twodash","solid","dotted","twodash"))+
        scale_color_manual(values=c("royalblue1","royalblue2","royalblue3","orangered1","orangered2","orangered3")) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(60,105)) +
        scale_x_continuous(breaks = seq(-30,30,10) , limits = c(-32,32))+ 
        # scale_x_date(date_labels = "%b/%d") +
        my_theme

p3

p4 <- ggplot(data=FR_plot_des_T,aes(x=day,y=survival,color=trt)) +
                geom_line(size=1.2,aes(linetype=trt)) +
                scale_linetype_manual(values=c("solid","solid","twodash"))+
                scale_color_manual(values=c("royalblue1","orangered1", "orangered3")) +
                scale_y_continuous(breaks = seq(0, 100, 10), limits = c(60,105)) +
                scale_x_continuous(breaks = seq(-30,30,10) , limits = c(-32,32))+ 
                # scale_x_date(date_labels = "%b/%d") +
                my_theme

p4

current_path <- getActiveDocumentContext()$path
# setwd(dirname(current_path )); setwd('plots'); getwd()

ggsave("mortality_control.jpeg",
       plot   = p1,
       dpi    = 300,
       device = "jpeg",
       width  = 8,
       height = 5,
       units  = "in")

ggsave("mortality_heat.jpeg",
       plot   = p2,
       dpi    = 300,
       device = "jpeg",
       width  = 6,
       height = 5,
       units  = "in")

ggsave("mortality_all.jpeg",
       plot   = p3,
       dpi    = 300,
       device = "jpeg",
       width  = 8,
       height = 5,
       units  = "in")

ggsave("mortality_desiccation.jpeg",
       plot   = p4,
       dpi    = 300,
       device = "jpeg",
       width  = 6,
       height = 5,
       units  = "in")

####################################################################################################
### Tank level effects analysis

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path )); # setwd('feeding_rate'); getwd()

# FR_plot <- read_excel("mortality.xlsx", sheet = "data", col_names = TRUE)
# trt_list <- read_excel("processed_summary.xlsx", sheet = "trt_list", col_names = TRUE)

MR_plot           <- read_excel("mortality.xlsx", sheet = "data", col_names = TRUE)
MR_plot$ploidy    <- factor(MR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
MR_plot$tank      <- factor(MR_plot$tank)
# MR_plot$timepoint <- factor(MR_plot$timepoint, levels=c("-10","1","2","6","10"),ordered=TRUE)
# MR_plot$mortality   <- factor(MR_plot$mortality,levels=trt_list$trt_list,ordered=TRUE)

## Transform data
x = MR_plot$day
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

# Statistical testings
summary(aov(day ~ as.factor(trt)*tank, data = MR_plot))

tx <- with(MR_plot, interaction(ploidy,timepoint))
amod <- aov(SMR ~ tx, data=MR_plot)
HSD.test(amod, "tx", group=TRUE, console=TRUE)

