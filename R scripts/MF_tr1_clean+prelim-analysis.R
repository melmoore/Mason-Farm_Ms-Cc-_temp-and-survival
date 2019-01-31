#Cleaning and preliminary analysis for Mason Farm temperature data (trial round 1)

#Load libraries

library(readr)
library(ggplot2)
library(tidyr)
library(Rmisc)
library(dplyr)
library(viridis)
library(cowplot)
library(extrafont)
library(lubridate)


#-------------------
#load data

mf <- read_csv("trial round 1/CR3000 Bert_Table1_7-19-18.csv", 
               col_names = FALSE)
View(mf)


#-----------------
#Cleaning raw data


#removing first 4 rows--extraneous data from the datalogger. putting into own dataframe

xinfo<-mf[c(1:4),]
mf<-mf[-c(1:4),]


#renaming columns

mf<-mf %>% rename(date.time = X1, 
                  rec.num = X2,
                  se_volt = X3,
                  tc1 = X4,
                  tc2 = X5,
                  tc3 = X6,
                  tc4 = X7,
                  tc5 = X8,
                  tc6 = X9,
                  tc7 = X10,
                  tc8 = X11,
                  tc9 = X12,
                  tc10 = X13)

#-----------------------------------------
#converting date and time to julian 

#separate date and time by space
mf<-mf %>% separate(date.time, c("date", "time"), sep=" ")

#create column with julian date
mf$date.j<-strptime(mf$date, "%m/%d")$yday+1


#separates time into hours and minutes, changes class from character to numeric. Calculates decimal time
#as minutes per hour (/60), then calculates decimal time as hour per day (/24), then adds to julian date
#to create column with julian day and decimal time of each recorded temp
mf <- mf %>% separate(time, c("h", "m"), ":", remove=FALSE) %>%
  mutate(h = as.numeric(h)) %>% mutate(m = as.numeric(m)) %>%
  mutate(time.dec = h+m/60) %>%
  mutate(time.dec.24 = time.dec/24) %>%
  mutate(date.time.j = date.j+time.dec.24)


#------------------------
#remove data from before the datalogger was placed at Mason Farm

mf.cl<-subset(mf, date.time.j>184.3750)



#---------------------
#create long dataframe to be able to plot multiple thermocouples together

mf.lng <- mf.cl %>% gather(tc, temp, 7:16) 

#create column with thermocouple position data
mf.lng$pos<-ifelse(mf.lng$tc=="tc2", "mid_plant_front",
                ifelse(mf.lng$tc=="tc4", "soil",
                       ifelse(mf.lng$tc=="tc5", "em_cage",
                              ifelse(mf.lng$tc=="tc6", "low_plant",
                                     ifelse(mf.lng$tc=="tc8", "high.plant",
                                            ifelse(mf.lng$tc=="tc10", "mid_plant_back",0))))))

#-------------------------
#Plotting temperature over trial round 1

#converting temp to class numeric
mf.lng$temp<-as.numeric(mf.lng$temp)

#removing tc that were not recording
mf.lng$temp[is.nan(mf.lng$temp)]<-0
mf.lng<-subset(mf.lng, temp!=0)

#removing invalid, negative and very large values (misreading tc??)
mf.lng<-subset(mf.lng, temp>0 & temp<100)
mf.lng<-subset(mf.lng, tc!="tc1")


#plotting tc temp data against date.time.j--group and color by tc
tr1.plot<-ggplot(mf.lng, aes(x=date.time.j, y=temp, group=pos, color=pos))
tr1.plot+geom_point(
       )+geom_line()









