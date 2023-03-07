## Modified by JM on 7/8/2020 for SR stage plotting

##-----------------------------------------------------------------------


setwd("D:/Dropbox/School Stuff/Grad School/Research/Hydrology/ConditionPaperPlots/")

#set up data by day and then the lines you want to visualize#
#JM Comment: Keep in to test with Jon's data
#BK <- read.csv('ExampleFromJonCody/BK_Salinity.csv')


#MO215
#MO215 <- read.csv("MO215_water_level_StageCorrectedCalc_02112023.csv")
MO215 <- read.csv("MO215_water_level_StageCorrectedCalc_02112023_CompleteTimeseries.csv")


library(tidyverse)
library(scales)
library(ggplot2)



#Add for either Bottle Creek or MO215, depending on what we do first#

MO215$day <- format(as.Date(MO215$Date, format='%m/%d/%Y'), format = '%m/%d')
MO215$year <- format(as.Date(MO215$Date, format='%m/%d/%Y'), format = '%Y')
str(MO215)


MO215 <- MO215 %>% filter(Stage_cm != 'NA')

MO215_mean <- MO215 %>% group_by(day) %>% summarize(mean_stage = mean(Stage_cm, na.rm = TRUE),
                                              sd_stage = sd(Stage_cm, na.rm = TRUE),
                                              n= n(),
                                              se_stage = sd_stage/sqrt(n),
                                              year = "1996-2020 Mean")
MO215_mean$Day <- as.Date(MO215_mean$day, format='%m/%d')
str(MO215_mean)

MO215_mean2 <- MO215_mean%>% 
  select(day, mean_stage, sd_stage, se_stage, n)

ForPlots <- merge(MO215, MO215_mean2)



##------------------------------------------------------------------------
##Graphing##

#col <- c("black", "steelblue1", "steelblue3", 'blue', 'navy')

str(ForPlots)
ForPlots$Date <- as.Date(ForPlots$Date, format='%m/%d/%Y')
ForPlots$day <- as.Date(ForPlots$day, format='%m/%d')
#ForPlots$year <- format(as.Date(ForPlots$Date, format='%m/%d/%Y'), format = '%Y')


library(RColorBrewer)
colo <- colorRampPalette(c("lightblue", "darkblue"))(28)

ForPlots1 <- ForPlots

ForPlots1$year <- as.numeric(ForPlots1$year)
str(ForPlots1)
ForPlots_MAPyears <- subset(ForPlots1, ForPlots1$year >= 2004 & ForPlots1$year <= 20)


#MO215
# This is for every year 1996 -2023
ggplot(ForPlots)+
  geom_ribbon(aes(day, ymin = mean_stage - (1.96*sd_stage/sqrt(n)),
                  ymax = mean_stage + (1.96*sd_stage/sqrt(n)), group = year), fill="lightgrey", alpha = 0.6)+
  geom_line(aes(day, Stage_cm, group = year, col = year), size = 1.0)+
  scale_color_manual(values = colo) +
  geom_line(aes(day, mean_stage, group = year), color="black", size = 1.5)+
  labs(title = "Shark River Stage at Headwaters", x = "Month", y = "Stage (cm)") +
  theme_classic()+
  theme(axis.text = element_text(size = 10, color = "black", face = "bold"),
      legend.text = element_text(size = 10, color = "black", face = "bold"),
      axis.title = element_text(size = 16, face = "bold"), 
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5))+
  scale_x_date(breaks = seq(as.Date("01/01", format = '%m/%d'),
                            as.Date("12/31", format = '%m/%d'), by = "1 month"),date_labels = "%m")
  

# This is just for MAP years
MAPYearPlots <- subset(ForPlots, ForPlots$year >= 2004 & ForPlots$year <= 2021)
colo1 <- colorRampPalette(c("lightblue", "darkblue"))(18)

PlottingHydroMAP <- ggplot(MAPYearPlots)+
  geom_ribbon(aes(day, ymin = mean_stage - (1.96*sd_stage/sqrt(n)),
                  ymax = mean_stage + (1.96*sd_stage/sqrt(n)), group = year), fill="gray", alpha = 0.6)+
  geom_line(aes(day, Stage_cm, group = year, col = year), size = 1.0)+
  scale_color_manual(values = colo1) +
  geom_line(aes(day, mean_stage, group = year), color="black", size = 1.5)+
  geom_hline(aes(yintercept = 30), linetype=4, size=1.25, color="black")+
#  labs(title = "Shark River Stage at Headwaters", x = "Month", y = "Stage (cm)") +
  labs(x = "Month", y = "Marsh Water Level at MO215 (cm)") +
  theme_classic()+
  theme(axis.text = element_text(size = 12, color = "black", face = "bold"),
        legend.text = element_text(size = 12, color = "black", face = "bold"),
        axis.title = element_text(size = 14, face = "bold"), 
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))+
  scale_x_date(breaks = seq(as.Date("01/01", format = '%m/%d'),
                            as.Date("12/31", format = '%m/%d'), by = "1 month"),date_labels = "%m")

ggsave(filename='MO215_Stage_ConditionPaper_02112023_Final2.pdf', plot=PlottingHydroMAP,
       scale = 1.2,
       width = 8,
       height = 4.5,
       units = c("in"),
       dpi = 300)



ggsave(filename = "MO215_Stage_ConditionPaper_02112023_Final.png", 
       units="in", width=8, height=4.5, 
       dpi=300)



# Below is pieces of old code for reference

labs(title = "Shark River Stage at Headwaters", x = "Month", y = "Stage (cm)")

  geom_ribbon(aes(ymin=mean_stage - se_stage , ymax=mean_stage + se_stage, x=day, fill = "slategray4"), alpha = 0.3)
  geom_ribbon(aes(day, ymin = mean_stage - (1.96*sd_stage/sqrt(n)),
                                  ymax = mean_stage + (1.96*sd_stage/sqrt(n))), fill="slategray4", alpha = 0.6)
  theme_classic()+
  geom_vline(aes(xintercept = as.integer(as.Date("2021-04-01"))), linetype=4, size=0.5, color="black")+
  geom_vline(aes(xintercept = as.integer(as.Date("2021-11-15"))), linetype=4, size=0.5, color="black")+
  geom_line(data = MO215_mean, aes(Day, mean_stage, colour = year), inherit.aes=F,
            size = 1.5)+
  geom_line(data = MO215_2012, aes(Day, Stage_cm, colour = year), inherit.aes=F,
            size = 1.0)+
  geom_line(data = MO215_2015, aes(Day, Stage_cm, colour = year), inherit.aes=F,
            size = 1.0)+  
  geom_line(data = MO215_2017, aes(Day, Stage_cm, colour = year), inherit.aes=F,
            size = 1.0)+
  geom_line(data = MO215_2019, aes(Day, Stage_cm, colour = year), inherit.aes=F,
            size = 1.0)+
  scale_y_continuous(breaks=seq(-10,100,10), limits = c(-10,100))+
  scale_x_date(breaks = seq(as.Date("2020-01-01"),
                            as.Date("2020-12-31"), by = "1 month"),date_labels = "%b")+
  labs(title = "Shark River Stage at Headwaters", x = "Month", y = "Stage (cm)")+
  scale_colour_manual(values= col,
                      name="Year", 
                      breaks=c("1996-2020 Mean", "2012", "2015", "2017", "2019"), 
                      labels = c("1996-2020 Mean","2012", "2015", "2017", "2019"))+
  theme(axis.text = element_text(size = 10, color = "black", face = "bold"),
        legend.text = element_text(size = 10, color = "black", face = "bold"),
        axis.title = element_text(size = 16, face = "bold"), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))



ggsave(filename = "MO215_Stage_AFSBaltimore_10302021_HighMigrationYears_Final.png", 
       units="in", width=10, height=4.5, 
       dpi=300)










