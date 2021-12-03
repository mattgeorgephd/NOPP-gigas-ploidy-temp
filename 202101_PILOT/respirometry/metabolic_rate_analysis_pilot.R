# Title: metabolic rate in diploid and triploid pacific oysters with temperature 
# Author: Matthew George; mattgeorgephd@gmail.com
# Date: 02/17/2021

## clear
rm(list=ls())

## Grab the WD from the file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))
getwd()

## Load R packages
library(readxl)
library(ggplot2)
library(stringr)
library(tidyverse)
# library(nlme)
# library(multcomp)
# library(RColorBrewer)
# library(scales)


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



#######################################################################################################################################

setwd("garage-pilot/paired_measurements")
getwd()

# grab list of file names from folder
f <- Sys.glob("*.xlsx")
head(f)

# Loop through and grab data using lapply
dat = lapply(f, function(i)
  {
  x = read_excel(i, sheet = "SABD0002000012, Ch 1", col_names = TRUE)
  # Get the columns you want
  x = x[, c(3, 7, 9)] 
  x = x[c(-1:-10),] # remove the first 10 to get rid of noise
  colnames(x) <- c("time", "oxy", "temp")
  # Break up the name and add columns for oyster name and treatment setpoint
  output1 <- strsplit(i,'\\.') # split name by period
  output1 <- unlist(output1,use.names=FALSE) # get rid of the annoying list format
  output2 <- output1[1] # remove file extension
  output3 <- strsplit(output2[1],'\\_') # split file name by underscore
  output3 <- unlist(output3,use.names=FALSE) # get rid of the annoying list format
  date_collected <- output3[1] # get date collected
  ploidy  <- output3[2] # get ploidy (D or T)
  oyster <- output3[3] # get oyster name
  tempset <- output3[4] # get temp set point
  x$date = date_collected
  x$ploidy = ploidy
  x$oyster = oyster
  x$tempset = as.numeric(tempset)
  # Return your data
  x
})

# consolidate dataset into a data frame
dat = do.call("rbind.data.frame", dat)
dat <- unite(data=dat,"ID",c(ploidy,oyster),sep="",remove=FALSE)
dat <- unite(data=dat,"ID_sort",c(ID,tempset),sep="_",remove=FALSE)

# nest data by run
dat_nested <- dat %>% 
  group_by(ID_sort) %>%
  nest() %>% 
  ungroup()

# loop through the data and scale time by start (set first time point to zero)
steps <- seq(from=1,to=nrow(dat_nested),by=1)
for (i in steps) {
  
  pulled_data   <- dat_nested[[2]][[i]]
  steps_pulled  <- seq(from=1,to=nrow(pulled_data),by=1)
  start         <- pulled_data[1,1]
  adjusted_time <- data.frame(time = integer(max(steps_pulled)))
  
  for (j in steps_pulled) {
    a <- pulled_data[j,1]
    b <- a - start
    adjusted_time[j,1] = b
  }
  
  pulled_data[,1] = adjusted_time
  dat_nested[[2]][[i]] <- pulled_data
  
  if(i == min(steps)) print("Wait")
  if(i == max(steps)/2) print("Wait")
  if(i == max(steps)) print("Finished")

}

# Export list of file names before review
setwd('..')
getwd()
write.csv(x=dat_nested$ID_sort,file="sample_name_output.csv")

# Plot the lines one by one to find ones to trash. Add start and stop to for recalcs in constraints tab of MR_output.
steps <- seq(from=1,to=nrow(dat_nested),by=1)
for (i in steps){
  pulled_data   <- dat_nested[[2]][[i]]
  print(ggplot(data=pulled_data, aes(time,oxy)) +
          geom_smooth(aes(time,oxy),method="lm",color="red",se=FALSE) +
          geom_point() + 
          ggtitle(dat_nested$ID_sort[i]) + 
          my_theme)
  readline(prompt="Is this plot good? Hit Enter for Next.")
}


# dat %>%
#   group_by(oyster) %>%
#   filter(ploidy == "T" & temp_set == 35) %>%
#   ggplot(aes(time,oxy)) + geom_point() + my_theme

#####################################################################################################################
## Calculate metabolic rate
setwd('..')
setwd('garage-pilot')

## Input constraints
constraints <- read_excel("sample_name_output.xlsx", sheet = "sample_name_output", col_names = TRUE)

# calculate slope
steps <- seq(from=1,to=nrow(dat_nested),by=1)
oxy_hr <- data.frame(ID_sort = character(nrow(dat_nested)),umol_L = integer(nrow(dat_nested)))

for (i in steps) {
  
  a <- dat_nested[[2]][[i]]
  
  if(constraints[i,2] == 1){
    start_calc = round((constraints[i,4]/0.33),0)
    stop_calc  = round((constraints[i,5]/0.33),0) 
    
    x <- a$time[start_calc[1,1]:stop_calc[1,1]]
    y <- a$oxy[start_calc[1,1]:stop_calc[1,1]]
    
    } 
  else {
    start_calc = 0
    stop_calc = nrow(a)
    
    x <- a$time[start_calc:stop_calc]
    y <- a$oxy[start_calc:stop_calc]    
    
    }

  fit <- lm(y ~ x)
  fit
  
  a <- as.numeric(fit$coefficients[2])*100+as.numeric(fit$coefficients[1])
  b <- as.numeric(fit$coefficients[2])*160+as.numeric(fit$coefficients[1])
  
  oxy_hr[i,2] = a-b
  oxy_hr[i,1] = dat_nested[[1]][[i]]
  
} ## looks into the constraints tab of MR_output and calculates slope, umol/L oxy consumption per hour, outputs oxy_hr 

# output calculated umol/L/hr
write.csv(oxy_hr,"oxy_hr.csv", row.names = FALSE)

# oyster_weights = read_excel("oyster_weights.xlsx", sheet = "weights", col_names = TRUE)

#####################################################################################################################
## PLOT SMR ((umol O2)/L/hr/gram)

# dat %>%
#   group_by(oyster) %>%
#   filter(ploidy == "T" & temp_set == 35) %>%
#   ggplot(aes(time,oxy)) + geom_point() + my_theme


## Plot
# MR_plot        <- read_excel("MR_output.xlsx", sheet = "MR_summary", col_names = TRUE)
# # MR_plot$ploidy <- factor(MR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
# # MR_plot$temp   <- factor(MR_plot$temp, levels=c("10","20","30","35","40"),ordered=TRUE)
# 
# 
# p1 <- ggplot(MR_plot) +
#       geom_errorbar(aes(x=temp,
#                          ymin=D_mean-D_se,
#                          ymax=D_mean+D_se),
#                      col="grey30",width=1,size=1.2,position_dodge(0.8)) +
#       geom_point(aes(x=temp,y=D_mean,group=1), col="royalblue1", size=3.0, position=position_dodge(2)) +
#       geom_line(aes(x=temp,y=D_mean,group=1), col="royalblue1" , size=1.2, position=position_dodge(2)) +
#       geom_errorbar( aes(x=temp,
#                      ymin=T_mean-D_se,
#                      ymax=T_mean+D_se),
#                  col="grey30",width=1,size=1.2, position=position_dodge(0.8)) +
#       geom_point(aes(x=temp,y=T_mean,group=1), col="orangered1" , size=3.0, position=position_dodge(0.8)) +
#       geom_line(aes(x=temp,y=T_mean,group=1), col="orangered1" , size=1.2, position=position_dodge(0.8)) +
#       scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 22)) +
#       scale_x_continuous(breaks = seq(10, 40, 5), limits = c(8, 42)) +
#       my_theme
# 
# p1
# 
# ggsave("MR_timeseries_ploidy.tiff",
#        plot   = p1,
#        dpi    = 600,
#        device = "tiff",
#        width  = 4,
#        height = 5,
#        units  = "in")
# 
# MR_plot        <- read_excel("MR_output.xlsx", sheet = "MR", col_names = TRUE)
# MR_plot$ploidy <- factor(MR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
# MR_plot$temp   <- factor(MR_plot$temp, levels=c("10","20","30","35","40"),ordered=TRUE)
# 
# summary(my_anova <- aov(MR ~ temp*ploidy, data = MR_plot))
# TukeyHSD(my_anova, ordered = TRUE)
# 
# 
# 
# bp1 <- ggplot(MR_plot, aes(x=temp, y=MR, fill=ploidy)) +
#        geom_boxplot(colour = "grey30", size = 0.8,outlier.colour="grey30", outlier.shape = 16,
#                    outlier.size=1, notch=FALSE) +
#        scale_fill_manual(values=c("royalblue1", "orangered1")) +
#        # geom_point() +
#        scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 42)) +
#        # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) + 
#        theme(line              = element_line(size=0.8),
#                     rect              = element_rect(size=1),
#                     text              = element_text(size=14,color="black"),
#                     panel.background  = element_blank(),
#                     panel.grid.major  = element_blank(), 
#                     panel.grid.minor  = element_blank(),
#                     axis.text.x       = element_text(size=16,color="black"),
#                     axis.text.y       = element_text(size=16,color="black"),
#                     axis.title.x      = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
#                     axis.title.y      = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
#                     axis.ticks.x      = element_line(color="black"),
#                     axis.ticks.y      = element_line(color="black"),
#                     # axis.line         = element_line(color = "black", size = 0.1),
#                     panel.border      = element_rect(color = "black", fill=NA, size=1.5),
#                     legend.key        = element_blank()) # removes background of legend bullets
# 
# bp1
# 
# ggsave("temp_ploidy_MR.tiff",
#        plot   = bp1,
#        dpi    = 600,
#        device = "tiff",
#        width  = 7,
#        height = 5,
#        units  = "in")


# Paired plot
MR_plot          <- read_excel("oxy_hr_paired.xlsx", sheet = "oxy_hr", col_names = TRUE)
MR_plot$ploidy   <- factor(MR_plot$ploidy, levels=c("D","T"),ordered=TRUE)
MR_plot$temp   <- factor(MR_plot$temp, levels=c("10","20","30"),ordered=TRUE)


p1 <- ggplot(MR_plot, aes(x=temp, y=umol_L_hr,group=as.factor(ID),color=ploidy)) +
      geom_point() +
      geom_line() +
      # scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 22)) +
      # scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 32)) +
      my_theme

p1



## mortality barplot
MR_plot        <- read_excel("oyster_ploidy_mortality_pilot.xlsx", sheet = "plot", col_names = TRUE)
MR_plot$p      <- factor(MR_plot$p, levels=c("D","T"),ordered=TRUE)
MR_plot$temp   <- factor(MR_plot$temp, levels=c("30","35","36","38","40"),ordered=TRUE)


p1 <- ggplot(MR_plot, aes(x=as.factor(temp),y=mortality,fill=p)) +
  geom_errorbar(aes(ymin=mortality-(mortality*0.2), ymax=mortality+se), width=0.2, position=position_dodge(0.9),colour="grey30", size=1.2) +
  geom_col(position="dodge2",fill=c("royalblue1","orangered1","royalblue1","orangered1","royalblue1","orangered1","royalblue1","orangered1","royalblue1","orangered1")) +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 120)) +
  scale_x_discrete(c(30,35,36,38,40)) + 
  my_theme

p1

summary(my_anova <- aov(mortality ~ temp*p, data = MR_plot))
TukeyHSD(my_anova, ordered = TRUE)

ggsave("temp_ploidy_mortality.tiff",
       plot   = p1,
       dpi    = 600,
       device = "tiff",
       width  = 5.5,
       height = 5,
       units  = "in")






