library(readxl)
library(dplyr)
library(ggplot2) 
library(sqldf)


library(tidyr)

raw <- read_excel("C:/Users/Gautam/Downloads/final3.xlsx") # after appending the 3 new columns and removing unnecessary columns
#creating individual data frames for each health indicator

raw_sepsis <- subset(raw, Sepsis!='NA')
raw_LBW <- subset(raw, LBW!='NA')
raw_Pneumonia <- subset(raw, Pneumonia!='NA')
raw_Diarrhea <- subset(raw, Diarrhea!='NA')
raw_Fever <- subset(raw, Fever!='NA')
raw_Measles <- subset(raw, Measles!='NA')

#removing individual nulls and modifying the same data frame
raw <- subset(raw, Sepsis!='NA')
raw <- subset(raw, LBW!='NA')
raw <- subset(raw, Pneumonia!='NA')
raw <- subset(raw, Diarrhea!='NA')
raw <- subset(raw, Fever!='NA')
raw <- subset(raw, Measles!='NA')


getmode <- function(v)
{
  unique_df <- unique(v)
  unique_df[which.max(tabulate(match(v, unique_df)))]
}
  
#reporting mean, median, mode, std deviation of the 6 DV's
unique_raw_sepsis = raw_sepsis[!duplicated(raw_sepsis$sdyid),]
mean(unique_raw_sepsis$Sepsis)
median(unique_raw_sepsis$Sepsis)
sd(unique_raw_sepsis$Sepsis)
getmode(unique_raw_sepsis$Sepsis)

unique_raw_LBW = raw_LBW[!duplicated(raw_LBW$sdyid),]
mean(unique_raw_LBW$LBW)
median(unique_raw_LBW$LBW)
sd(unique_raw_LBW$LBW)
getmode(unique_raw_LBW$LBW)

unique_raw_Pneumonia = raw_Pneumonia[!duplicated(raw_Pneumonia$sdyid),]
mean(unique_raw_LBW$Pneumonia)
median(unique_raw_LBW$Pneumonia)
sd(unique_raw_LBW$Pneumonia)
getmode(unique_raw_LBW$Pneumonia)

unique_raw_Diarrhea = raw_Diarrhea[!duplicated(raw_Diarrhea$sdyid),]
mean(unique_raw_Diarrhea$Diarrhea)
median(unique_raw_Diarrhea$Diarrhea)
sd(unique_raw_Diarrhea$Diarrhea)
getmode(unique_raw_Diarrhea$Diarrhea)

unique_raw_Fever = raw_Fever[!duplicated(raw_Fever$sdyid),]
mean(unique_raw_Fever$Fever)
median(unique_raw_Fever$Fever)
sd(unique_raw_Fever$Fever)
getmode(unique_raw_Fever$Fever)

unique_raw_Measles = raw_Measles[!duplicated(raw_Measles$sdyid),]
mean(unique_raw_Measles$Measles)
median(unique_raw_Measles$Measles)
sd(unique_raw_Measles$Measles)
getmode(unique_raw_Measles$Measles)

# histograms before finding outliers

ysk<- subset(raw_sepsis, season == "Kharif")
ysr <-subset(raw_sepsis, season == "Rabi")
yss<- subset(raw_sepsis, season == "Summer")
ysw <-subset(raw_sepsis, season == "Whole Year")


ysk$s <- 'kharif'
ysr$s <- 'rabi'
yss$s <- 'summer'
ysw$s <- 'whole year'

sepsis_season <- rbind(ysk,ysr,yss,ysw)

ggplot(sepsis_season, aes(x=Sepsis, fill = season))+geom_histogram(alpha=0.6)

ys2011<- subset(raw_sepsis, year == "2011")
ys2012 <-subset(raw_sepsis, year == "2012")
ys2013<- subset(raw_sepsis, year == "2013")
ys2014 <-subset(raw_sepsis, year == "2014")
ys2015<- subset(raw_sepsis, year == "2015")
ys2016 <-subset(raw_sepsis, year == "2016")


ys2011$y <- '2011'
ys2012$y <- '2012'
ys2013$y <- '2013'
ys2014$y <- '2014'
ys2015$y <- '2015'
ys2016$y <- '2016'


year_sepsis <- rbind(ys2011,ys2012,ys2013,ys2014,ys2015,ys2016)


ggplot(year_sepsis, aes(x=Sepsis, fill = y))+
  geom_histogram(alpha=0.6)

ggplot(year_sepsis, aes(x=Sepsis, fill = y))+geom_density(alpha = 0.2)

######################
ylk<- subset(raw_LBW, season == "Kharif")
ylr <-subset(raw_LBW, season == "Rabi")
yls<- subset(raw_LBW, season == "Summer")
ylw <-subset(raw_LBW, season == "Whole Year")


ylk$s <- 'kharif'
ylr$s <- 'rabi'
yls$s <- 'summer'
ylw$s <- 'whole year'

LBW_season <- rbind(ylk,ylr,yls,ylw)

ggplot(LBW_season, aes(x=LBW, fill = season))+geom_histogram(alpha=0.6)

yl2011<- subset(raw_LBW, year == "2011")
yl2012 <-subset(raw_LBW, year == "2012")
yl2013<- subset(raw_LBW, year == "2013")
yl2014 <-subset(raw_LBW, year == "2014")
yl2015<- subset(raw_LBW, year == "2015")
yl2016 <-subset(raw_LBW, year == "2016")


yl2011$y <- '2011'
yl2012$y <- '2012'
yl2013$y <- '2013'
yl2014$y <- '2014'
yl2015$y <- '2015'
yl2016$y <- '2016'


year_LBW <- rbind(yl2011,yl2012,yl2013,yl2014,yl2015,yl2016)


ggplot(year_LBW, aes(x=LBW, fill = y))+
  geom_histogram(alpha=0.6)

ggplot(year_LBW, aes(x=LBW, fill = y))+geom_density(alpha = 0.2)

###############
ypk<- subset(raw_Pneumonia, season == "Kharif")
ypr <-subset(raw_Pneumonia, season == "Rabi")
yps<- subset(raw_Pneumonia, season == "Summer")
ypw <-subset(raw_Pneumonia, season == "Whole Year")


ypk$s <- 'kharif'
ypr$s <- 'rabi'
yps$s <- 'summer'
ypw$s <- 'whole year'

Pneumonia_season <- rbind(ypk,ypr,yps,ypw)

ggplot(Pneumonia_season, aes(x=Pneumonia, fill = season))+geom_histogram(alpha=0.6)

yp2011<- subset(raw_Pneumonia, year == "2011")
yp2012 <-subset(raw_Pneumonia, year == "2012")
yp2013<- subset(raw_Pneumonia, year == "2013")
yp2014 <-subset(raw_Pneumonia, year == "2014")
yp2015<- subset(raw_Pneumonia, year == "2015")
yp2016 <-subset(raw_Pneumonia, year == "2016")


yp2011$y <- '2011'
yp2012$y <- '2012'
yp2013$y <- '2013'
yp2014$y <- '2014'
yp2015$y <- '2015'
yp2016$y <- '2016'


year_Pneumonia <- rbind(yp2011,yp2012,yp2013,yp2014,yp2015,yp2016)


ggplot(year_Pneumonia, aes(x=Pneumonia, fill = y))+
  geom_histogram(alpha=0.6)

ggplot(year_sepsis, aes(x=Sepsis, fill = y))+geom_density(alpha = 0.2)

#####################
ydk<- subset(raw_Diarrhea, season == "Kharif")
ydr <-subset(raw_Diarrhea, season == "Rabi")
yds<- subset(raw_Diarrhea, season == "Summer")
ydw <-subset(raw_Diarrhea, season == "Whole Year")


ydk$s <- 'kharif'
ydr$s <- 'rabi'
yds$s <- 'summer'
ydw$s <- 'whole year'

Diarrhea_season <- rbind(ydk,ydr,yds,ydw)

ggplot(Diarrhea_season, aes(x=Diarrhea, fill = season))+geom_histogram(alpha=0.6)

yd2011<- subset(raw_Diarrhea, year == "2011")
yd2012 <-subset(raw_Diarrhea, year == "2012")
yd2013<- subset(raw_Diarrhea, year == "2013")
yd2014 <-subset(raw_Diarrhea, year == "2014")
yd2015<- subset(raw_Diarrhea, year == "2015")
yd2016 <-subset(raw_Diarrhea, year == "2016")


yd2011$y <- '2011'
yd2012$y <- '2012'
yd2013$y <- '2013'
yd2014$y <- '2014'
yd2015$y <- '2015'
yd2016$y <- '2016'


year_Diarrhea <- rbind(yd2011,yd2012,yd2013,yd2014,yd2015,yd2016)


ggplot(year_Diarrhea, aes(x=Diarrhea, fill = y))+
  geom_histogram(alpha=0.6)

ggplot(year_sepsis, aes(x=Sepsis, fill = y))+geom_density(alpha = 0.2)
###############

yfk<- subset(raw_Fever, season == "Kharif")
yfr <-subset(raw_Fever, season == "Rabi")
yfs<- subset(raw_Fever, season == "Summer")
yfw <-subset(raw_Fever, season == "Whole Year")


yfk$s <- 'kharif'
yfr$s <- 'rabi'
yfs$s <- 'summer'
yfw$s <- 'whole year'

Fever_season <- rbind(yfk,yfr,yfs,yfw)

ggplot(Fever_season, aes(x=Fever, fill = season))+geom_histogram(alpha=0.6)

yf2011<- subset(raw_Fever, year == "2011")
yf2012 <-subset(raw_Fever, year == "2012")
yf2013<- subset(raw_Fever, year == "2013")
yf2014 <-subset(raw_Fever, year == "2014")
yf2015<- subset(raw_Fever, year == "2015")
yf2016 <-subset(raw_Fever, year == "2016")


yf2011$y <- '2011'
yf2012$y <- '2012'
yf2013$y <- '2013'
yf2014$y <- '2014'
yf2015$y <- '2015'
yf2016$y <- '2016'


year_Fever <- rbind(yf2011,yf2012,yf2013,yf2014,yf2015,yf2016)


ggplot(year_Fever, aes(x=Fever, fill = y))+
  geom_histogram(alpha=0.6)

ggplot(year_sepsis, aes(x=Sepsis, fill = y))+geom_density(alpha = 0.2)

#################################


ymk<- subset(raw_Measles, season == "Kharif")
ymr <-subset(raw_Measles, season == "Rabi")
yms<- subset(raw_Measles, season == "Summer")
ymw <-subset(raw_Measles, season == "Whole Year")


ymk$s <- 'kharif'
ymr$s <- 'rabi'
yms$s <- 'summer'
ymw$s <- 'whole year'

Measles_season <- rbind(ymk,ymr,yms,ymw)

ggplot(Measles_season, aes(x=Measles, fill = season))+geom_histogram(alpha=0.6)

ym2011<- subset(raw_Measles, year == "2011")
ym2012 <-subset(raw_Measles, year == "2012")
ym2013<- subset(raw_Measles, year == "2013")
ym2014 <-subset(raw_Measles, year == "2014")
ym2015<- subset(raw_Measles, year == "2015")
ym2016 <-subset(raw_Measles, year == "2016")


ym2011$y <- '2011'
ym2012$y <- '2012'
ym2013$y <- '2013'
ym2014$y <- '2014'
ym2015$y <- '2015'
ym2016$y <- '2016'


year_Measles <- rbind(ym2011,ym2012,ym2013,ym2014,ym2015,ym2016)


ggplot(year_Measles, aes(x=Measles, fill = y))+
  geom_histogram(alpha=0.6)

ggplot(year_sepsis, aes(x=Sepsis, fill = y))+geom_density(alpha = 0.2)

#########################

# Removing outliers 

raw_sepsis <- subset(raw_sepsis,Sepsis <= 30)
raw_LBW <- subset(raw_LBW,LBW <= 60)
raw_Pneumonia <- subset(raw_Pneumonia,Pneumonia <= 40)
raw_Diarrhea <- subset(raw_Diarrhea,Diarrhea <= 20.45)
raw_Fever <- subset(raw_Fever,Fever <= 32)
raw_Measles <- subset(raw_Measles,Measles <= 8.9)


new2 = raw_sepsis[which(raw_sepsis$area_cc_total != 0), ] 

new3 <- sqldf("select *, (select sum(t1.yield_area_cc_total)/sum(t1.area_cc_total) from new2 t1 
                 where T.state = t1.state and T.district = t1.district and T.year = t1.year and T.cropcategory = t1.cropcategory) 
      as yield_index from new2 T" )

raw_sepsis <- sqldf("select *, (select ((T2.yield_index - T1.yield_index)/T1.yield_index) from new3 as T1 where T2.year = T1.year+1 and T1.state = T2.state and T1.district = T2.district and T1.cropcategory = T2.cropcategory)as rate from new3 as T2")


new2 = raw_LBW[which(raw_LBW$area_cc_total != 0), ] 

new3 <- sqldf("select *, (select sum(t1.yield_area_cc_total)/sum(t1.area_cc_total) from new2 t1 
                 where T.state = t1.state and T.district = t1.district and T.year = t1.year and T.cropcategory = t1.cropcategory) 
      as yield_index from new2 T" )

raw_LBW <- sqldf("select *, (select ((T2.yield_index - T1.yield_index)/T1.yield_index) from new3 as T1 where T2.year = T1.year+1 and T1.state = T2.state and T1.district = T2.district and T1.cropcategory = T2.cropcategory)as rate from new3 as T2")

new2 = raw_Pneumonia[which(raw_Pneumonia$area_cc_total != 0), ] 

new3 <- sqldf("select *, (select sum(t1.yield_area_cc_total)/sum(t1.area_cc_total) from new2 t1 
                 where T.state = t1.state and T.district = t1.district and T.year = t1.year and T.cropcategory = t1.cropcategory) 
      as yield_index from new2 T" )

raw_Pneumonia <- sqldf("select *, (select ((T2.yield_index - T1.yield_index)/T1.yield_index) from new3 as T1 where T2.year = T1.year+1 and T1.state = T2.state and T1.district = T2.district and T1.cropcategory = T2.cropcategory)as rate from new3 as T2")


new2 = raw_Diarrhea[which(raw_Diarrhea$area_cc_total != 0), ] 

new3 <- sqldf("select *, (select sum(t1.yield_area_cc_total)/sum(t1.area_cc_total) from new2 t1 
                 where T.state = t1.state and T.district = t1.district and T.year = t1.year and T.cropcategory = t1.cropcategory) 
      as yield_index from new2 T" )

raw_Diarrhea <- sqldf("select *, (select ((T2.yield_index - T1.yield_index)/T1.yield_index) from new3 as T1 where T2.year = T1.year+1 and T1.state = T2.state and T1.district = T2.district and T1.cropcategory = T2.cropcategory)as rate from new3 as T2")



new2 = raw_Fever[which(raw_Fever$area_cc_total != 0), ] 

new3 <- sqldf("select *, (select sum(t1.yield_area_cc_total)/sum(t1.area_cc_total) from new2 t1 
                 where T.state = t1.state and T.district = t1.district and T.year = t1.year and T.cropcategory = t1.cropcategory) 
      as yield_index from new2 T" )

raw_Fever <- sqldf("select *, (select ((T2.yield_index - T1.yield_index)/T1.yield_index) from new3 as T1 where T2.year = T1.year+1 and T1.state = T2.state and T1.district = T2.district and T1.cropcategory = T2.cropcategory)as rate from new3 as T2")


new2 = raw_Measles[which(raw_Measles$area_cc_total != 0), ] 

new3 <- sqldf("select *, (select sum(t1.yield_area_cc_total)/sum(t1.area_cc_total) from new2 t1 
                 where T.state = t1.state and T.district = t1.district and T.year = t1.year and T.cropcategory = t1.cropcategory) 
      as yield_index from new2 T" )

raw_Measles <- sqldf("select *, (select ((T2.yield_index - T1.yield_index)/T1.yield_index) from new3 as T1 where T2.year = T1.year+1 and T1.state = T2.state and T1.district = T2.district and T1.cropcategory = T2.cropcategory)as rate from new3 as T2")



unique_raw_sepsis = raw_sepsis[!duplicated(raw_sepsis$sdyid),]
unique_raw_LBW = raw_LBW[!duplicated(raw_LBW$sdyid),]
unique_raw_Pneumonia = raw_Pneumonia[!duplicated(raw_Pneumonia$sdyid),]
unique_raw_Diarrhea = raw_Diarrhea[!duplicated(raw_Diarrhea$sdyid),]
unique_raw_Fever = raw_Fever[!duplicated(raw_Fever$sdyid),]
unique_raw_Measles = raw_Measles[!duplicated(raw_Measles$sdyid),]


# finding correlation coefficients
#sepsis 
cor(unique_raw_sepsis$Sepsis,unique_raw_sepsis$gdp)
cor(unique_raw_sepsis$Sepsis,unique_raw_sepsis$tap)
cor(unique_raw_sepsis$Sepsis,unique_raw_sepsis$beds)

cropcash <- subset(raw_sepsis,cropcategory=="Cash")
cropcash <- cropcash[!duplicated(cropcash$sdyid),]

croppulse <- subset(raw_sepsis,cropcategory=="Pulse")
croppulse <- croppulse[!duplicated(croppulse$sdyid),]

cropcereal <- subset(raw_sepsis,cropcategory=="Cereal")
cropcereal <- cropcereal[!duplicated(cropcereal$sdyid),]

crophorti <- subset(raw_sepsis,cropcategory=="Horticulture")
crophorti <- crophorti[!duplicated(crophorti$sdyid),]

cropoil <- subset(raw_sepsis,cropcategory=="Oilseed")
cropoil <- cropoil[!duplicated(cropoil$sdyid),]

cropcoarse <- subset(raw_sepsis,cropcategory=="Coarse Cereal")
cropcoarse <- cropcoarse[!duplicated(cropcoarse$sdyid),]

cor(cropcash$Sepsis,cropcash$yield_index)
cor(croppulse$Sepsis,croppulse$yield_index)
cor(cropcereal$Sepsis,cropcereal$yield_index)
cor(crophorti$Sepsis,crophorti$yield_index)
cor(cropoil$Sepsis,cropoil$yield_index)
cor(cropcoarse$Sepsis,cropcoarse$yield_index)
cor(cropcash$Sepsis,cropcash$rate,use="complete.obs")
cor(croppulse$Sepsis,croppulse$rate,use="complete.obs")
cor(cropcereal$Sepsis,cropcereal$rate,use="complete.obs")
cor(crophorti$Sepsis,crophorti$rate,use="complete.obs")
cor(cropoil$Sepsis,cropoil$rate,use="complete.obs")
cor(cropcoarse$Sepsis,cropcoarse$rate,use="complete.obs")


#LBW
cor(unique_raw_LBW$LBW,unique_raw_LBW$gdp)
cor(unique_raw_LBW$LBW,unique_raw_LBW$tap)
cor(unique_raw_LBW$LBW,unique_raw_LBW$beds)

cropcash <- subset(raw_LBW,cropcategory=="Cash")
cropcash <- cropcash[!duplicated(cropcash$sdyid),]

croppulse <- subset(raw_LBW,cropcategory=="Pulse")
croppulse <- croppulse[!duplicated(croppulse$sdyid),]

cropcereal <- subset(raw_LBW,cropcategory=="Cereal")
cropcereal <- cropcereal[!duplicated(cropcereal$sdyid),]

crophorti <- subset(raw_LBW,cropcategory=="Horticulture")
crophorti <- crophorti[!duplicated(crophorti$sdyid),]

cropoil <- subset(raw_LBW,cropcategory=="Oilseed")
cropoil <- cropoil[!duplicated(cropoil$sdyid),]

cropcoarse <- subset(raw_LBW,cropcategory=="Coarse Cereal")
cropcoarse <- cropcoarse[!duplicated(cropcoarse$sdyid),]

cor(cropcash$LBW,cropcash$yield_index)
cor(croppulse$LBW,croppulse$yield_index)
cor(cropcereal$LBW,cropcereal$yield_index)
cor(crophorti$LBW,crophorti$yield_index)
cor(cropoil$LBW,cropoil$yield_index)
cor(cropcoarse$LBW,cropcoarse$yield_index)
cor(cropcash$LBW,cropcash$rate,use="complete.obs")
cor(croppulse$LBW,croppulse$rate,use="complete.obs")
cor(cropcereal$LBW,cropcereal$rate,use="complete.obs")
cor(crophorti$LBW,crophorti$rate,use="complete.obs")
cor(cropoil$LBW,cropoil$rate,use="complete.obs")
cor(cropcoarse$LBW,cropcoarse$rate,use="complete.obs")

#Pneumonia

cor(unique_raw_Pneumonia$Pneumonia,unique_raw_Pneumonia$gdp)
cor(unique_raw_Pneumonia$Pneumonia,unique_raw_Pneumonia$tap)
cor(unique_raw_Pneumonia$Pneumonia,unique_raw_Pneumonia$beds)

cropcash <- subset(raw_Pneumonia,cropcategory=="Cash")
cropcash <- cropcash[!duplicated(cropcash$sdyid),]

croppulse <- subset(raw_Pneumonia,cropcategory=="Pulse")
croppulse <- croppulse[!duplicated(croppulse$sdyid),]

cropcereal <- subset(raw_Pneumonia,cropcategory=="Cereal")
cropcereal <- cropcereal[!duplicated(cropcereal$sdyid),]

crophorti <- subset(raw_Pneumonia,cropcategory=="Horticulture")
crophorti <- crophorti[!duplicated(crophorti$sdyid),]

cropoil <- subset(raw_Pneumonia,cropcategory=="Oilseed")
cropoil <- cropoil[!duplicated(cropoil$sdyid),]

cropcoarse <- subset(raw_Pneumonia,cropcategory=="Coarse Cereal")
cropcoarse <- cropcoarse[!duplicated(cropcoarse$sdyid),]

cor(cropcash$Pneumonia,cropcash$yield_index)
cor(croppulse$Pneumonia,croppulse$yield_index)
cor(cropcereal$Pneumonia,cropcereal$yield_index)
cor(crophorti$Pneumonia,crophorti$yield_index)
cor(cropoil$Pneumonia,cropoil$yield_index)
cor(cropcoarse$Pneumonia,cropcoarse$yield_index)
cor(cropcash$Pneumonia,cropcash$rate,use="complete.obs")
cor(croppulse$Pneumonia,croppulse$rate,use="complete.obs")
cor(cropcereal$Pneumonia,cropcereal$rate,use="complete.obs")
cor(crophorti$Pneumonia,crophorti$rate,use="complete.obs")
cor(cropoil$Pneumonia,cropoil$rate,use="complete.obs")
cor(cropcoarse$Pneumonia,cropcoarse$rate,use="complete.obs")

#Diarrhea
cor(unique_raw_Diarrhea$Diarrhea,unique_raw_Diarrhea$gdp)
cor(raw_Diarrhea$Diarrhea,raw_Diarrhea$tap)
cor(unique_raw_Diarrhea$Diarrhea,unique_raw_Diarrhea$beds)

cropcash <- subset(raw_Diarrhea,cropcategory=="Cash")
cropcash <- cropcash[!duplicated(cropcash$sdyid),]

croppulse <- subset(raw_Diarrhea,cropcategory=="Pulse")
croppulse <- croppulse[!duplicated(croppulse$sdyid),]

cropcereal <- subset(raw_Diarrhea,cropcategory=="Cereal")
cropcereal <- cropcereal[!duplicated(cropcereal$sdyid),]

crophorti <- subset(raw_Diarrhea,cropcategory=="Horticulture")
crophorti <- crophorti[!duplicated(crophorti$sdyid),]

cropoil <- subset(raw_Diarrhea,cropcategory=="Oilseed")
cropoil <- cropoil[!duplicated(cropoil$sdyid),]

cropcoarse <- subset(raw_Diarrhea,cropcategory=="Coarse Cereal")
cropcoarse <- cropcoarse[!duplicated(cropcoarse$sdyid),]

cor(cropcash$Diarrhea,cropcash$yield_index)
cor(croppulse$Diarrhea,croppulse$yield_index)
cor(cropcereal$Diarrhea,cropcereal$yield_index)
cor(crophorti$Diarrhea,crophorti$yield_index)
cor(cropoil$Diarrhea,cropoil$yield_index)
cor(cropcoarse$Diarrhea,cropcoarse$yield_index)
cor(cropcash$Diarrhea,cropcash$rate,use="complete.obs")
cor(croppulse$Diarrhea,croppulse$rate,use="complete.obs")
cor(cropcereal$Diarrhea,cropcereal$rate,use="complete.obs")
cor(crophorti$Diarrhea,crophorti$rate,use="complete.obs")
cor(cropoil$Diarrhea,cropoil$rate,use="complete.obs")
cor(cropcoarse$Diarrhea,cropcoarse$rate,use="complete.obs")

#fever

cor(unique_raw_Fever$Fever,unique_raw_Fever$gdp)
cor(raw_Fever$Fever,raw_Fever$tap)
cor(unique_raw_Fever$Fever,unique_raw_Fever$beds)

cropcash <- subset(raw_Fever,cropcategory=="Cash")
cropcash <- cropcash[!duplicated(cropcash$sdyid),]

croppulse <- subset(raw_Fever,cropcategory=="Pulse")
croppulse <- croppulse[!duplicated(croppulse$sdyid),]

cropcereal <- subset(raw_Fever,cropcategory=="Cereal")
cropcereal <- cropcereal[!duplicated(cropcereal$sdyid),]

crophorti <- subset(raw_Fever,cropcategory=="Horticulture")
crophorti <- crophorti[!duplicated(crophorti$sdyid),]

cropoil <- subset(raw_Fever,cropcategory=="Oilseed")
cropoil <- cropoil[!duplicated(cropoil$sdyid),]

cropcoarse <- subset(raw_Fever,cropcategory=="Coarse Cereal")
cropcoarse <- cropcoarse[!duplicated(cropcoarse$sdyid),]

cor(cropcash$Fever,cropcash$yield_index)
cor(croppulse$Fever,croppulse$yield_index)
cor(cropcereal$Fever,cropcereal$yield_index)
cor(crophorti$Fever,crophorti$yield_index)
cor(cropoil$Fever,cropoil$yield_index)
cor(cropcoarse$Fever,cropcoarse$yield_index)
cor(cropcash$Fever,cropcash$rate,use="complete.obs")
cor(croppulse$Fever,croppulse$rate,use="complete.obs")
cor(cropcereal$Fever,cropcereal$rate,use="complete.obs")
cor(crophorti$Fever,crophorti$rate,use="complete.obs")
cor(cropoil$Fever,cropoil$rate,use="complete.obs")
cor(cropcoarse$Fever,cropcoarse$rate,use="complete.obs")

# Measles
cor(unique_raw_Measles$Measles,unique_raw_Measles$gdp)
cor(raw_Measles$Measles,raw_Measles$tap)
cor(unique_raw_Measles$Measles,unique_raw_Measles$beds)

cropcash <- subset(raw_Measles,cropcategory=="Cash")
cropcash <- cropcash[!duplicated(cropcash$sdyid),]

croppulse <- subset(raw_Measles,cropcategory=="Pulse")
croppulse <- croppulse[!duplicated(croppulse$sdyid),]

cropcereal <- subset(raw_Measles,cropcategory=="Cereal")
cropcereal <- cropcereal[!duplicated(cropcereal$sdyid),]

crophorti <- subset(raw_Measles,cropcategory=="Horticulture")
crophorti <- crophorti[!duplicated(crophorti$sdyid),]

cropoil <- subset(raw_Measles,cropcategory=="Oilseed")
cropoil <- cropoil[!duplicated(cropoil$sdyid),]

cropcoarse <- subset(raw_Measles,cropcategory=="Coarse Cereal")
cropcoarse <- cropcoarse[!duplicated(cropcoarse$sdyid),]

cor(cropcash$Measles,cropcash$yield_index)
cor(croppulse$Measles,croppulse$yield_index)
cor(cropcereal$Measles,cropcereal$yield_index)
cor(crophorti$Measles,crophorti$yield_index)
cor(cropoil$Measles,cropoil$yield_index)
cor(cropcoarse$Measles,cropcoarse$yield_index)
cor(cropcash$Measles,cropcash$rate,use="complete.obs")
cor(croppulse$Measles,croppulse$rate,use="complete.obs")
cor(cropcereal$Measles,cropcereal$rate,use="complete.obs")
cor(crophorti$Measles,crophorti$rate,use="complete.obs")
cor(cropoil$Measles,cropoil$rate,use="complete.obs")
cor(cropcoarse$Measles,cropcoarse$rate,use="complete.obs")



# Models
# part A
model1 <- lm(LBW ~ gdp + tap + beds, unique_raw_LBW)
print(summary(model1))

# resetting crop data frames for the other regression models
cropcash <- subset(raw_LBW,cropcategory=="Cash")
cropcash <- cropcash[!duplicated(cropcash$sdyid),]

croppulse <- subset(raw_LBW,cropcategory=="Pulse")
croppulse <- croppulse[!duplicated(croppulse$sdyid),]

cropcereal <- subset(raw_LBW,cropcategory=="Cereal")
cropcereal <- cropcereal[!duplicated(cropcereal$sdyid),]

crophorti <- subset(raw_LBW,cropcategory=="Horticulture")
crophorti <- crophorti[!duplicated(crophorti$sdyid),]

cropoil <- subset(raw_LBW,cropcategory=="Oilseed")
cropoil <- cropoil[!duplicated(cropoil$sdyid),]

cropcoarse <- subset(raw_LBW,cropcategory=="Coarse Cereal")
cropcoarse <- cropcoarse[!duplicated(cropcoarse$sdyid),]


# part B
model2 <- lm(LBW ~ gdp + tap + beds + yield_index, cropcash)
print(summary(model2))

model3 <- lm(LBW ~ gdp + tap + beds + yield_index, croppulse)
print(summary(model3))

model4 <- lm(LBW ~ gdp + tap + beds + yield_index, cropcereal)
print(summary(model4))

model5 <- lm(LBW ~ gdp + tap + beds + yield_index, crophorti)
print(summary(model5))

model6 <- lm(LBW ~ gdp + tap + beds + yield_index, cropoil)
print(summary(model6))

model7 <- lm(LBW ~ gdp + tap + beds + yield_index, cropcoarse)
print(summary(model7))

#part C

cropcash <- subset(raw_LBW,cropcategory=="Cash")
cropcash <- cropcash[!duplicated(cropcash$sdyid),]

croppulse <- subset(raw_LBW,cropcategory=="Pulse")
croppulse <- croppulse[!duplicated(croppulse$sdyid),]

cropcereal <- subset(raw_LBW,cropcategory=="Cereal")
cropcereal <- cropcereal[!duplicated(cropcereal$sdyid),]

crophorti <- subset(raw_LBW,cropcategory=="Horticulture")
crophorti <- crophorti[!duplicated(crophorti$sdyid),]

cropoil <- subset(raw_LBW,cropcategory=="Oilseed")
cropoil <- cropoil[!duplicated(cropoil$sdyid),]

cropcoarse <- subset(raw_LBW,cropcategory=="Coarse Cereal")
cropcoarse <- cropcoarse[!duplicated(cropcoarse$sdyid),]

LBW_CC_Yield <- rbind(cropcash,cropcereal,cropcoarse,crophorti,cropoil,croppulse)

LBW_CC_Yield <- transform(LBW_CC_Yield, cash= ifelse(LBW_CC_Yield$cropcategory == "Cash",yield_index,0))
LBW_CC_Yield <- transform(LBW_CC_Yield, pulses= ifelse(LBW_CC_Yield$cropcategory == "Pulse",yield_index,0))
LBW_CC_Yield <- transform(LBW_CC_Yield, cereals= ifelse(LBW_CC_Yield$cropcategory == "Cereal",yield_index,0))
LBW_CC_Yield <- transform(LBW_CC_Yield, ccereals= ifelse(LBW_CC_Yield$cropcategory == "Coarse Cereal",yield_index,0))
LBW_CC_Yield <- transform(LBW_CC_Yield, horticulture= ifelse(LBW_CC_Yield$cropcategory == "Horticulture",yield_index,0))
LBW_CC_Yield <- transform(LBW_CC_Yield, oilseed= ifelse(LBW_CC_Yield$cropcategory == "Oilseed",yield_index,0))


model8 <- lm(LBW ~ gdp + tap + beds + cash + pulses + cereals + 
               ccereals + horticulture + oilseed, LBW_CC_Yield)
print(summary(model8))

#PART D
model9 <- lm(LBW ~ gdp + tap + beds + rate, cropcash)
print(summary(model9))

model10 <- lm(LBW ~ gdp + tap + beds + rate, croppulse)
print(summary(model10))

model11 <- lm(LBW ~ gdp + tap + beds + rate, cropcereal)
print(summary(model11))

model12 <- lm(LBW ~ gdp + tap + beds + rate, crophorti)
print(summary(model12))

model13 <- lm(LBW ~ gdp + tap + beds + rate, cropoil)
print(summary(model13))

model14 <- lm(LBW ~ gdp + tap + beds + rate, cropcoarse)
print(summary(model14))

# Part E
cropcash <- subset(raw_LBW,cropcategory=="Cash")
cropcash <- cropcash[!duplicated(cropcash$sdyid),]

croppulse <- subset(raw_LBW,cropcategory=="Pulse")
croppulse <- croppulse[!duplicated(croppulse$sdyid),]

cropcereal <- subset(raw_LBW,cropcategory=="Cereal")
cropcereal <- cropcereal[!duplicated(cropcereal$sdyid),]

crophorti <- subset(raw_LBW,cropcategory=="Horticulture")
crophorti <- crophorti[!duplicated(crophorti$sdyid),]

cropoil <- subset(raw_LBW,cropcategory=="Oilseed")
cropoil <- cropoil[!duplicated(cropoil$sdyid),]

cropcoarse <- subset(raw_LBW,cropcategory=="Coarse Cereal")
cropcoarse <- cropcoarse[!duplicated(cropcoarse$sdyid),]

LBW_CC_Rate <- rbind(cropcash,cropcereal,cropcoarse,crophorti,cropoil,croppulse)



LBW_CC_Rate <- transform(LBW_CC_Rate, Cash= ifelse(LBW_CC_Rate$cropcategory == "Cash",rate,0))
LBW_CC_Rate <- transform(LBW_CC_Rate, pulses= ifelse(LBW_CC_Rate$cropcategory == "Pulse",rate,0))
LBW_CC_Rate <- transform(LBW_CC_Rate, cereals= ifelse(LBW_CC_Rate$cropcategory == "Cereal",rate,0))
LBW_CC_Rate <- transform(LBW_CC_Rate, ccereals= ifelse(LBW_CC_Rate$cropcategory == "Coarse Cereal",rate,0))
LBW_CC_Rate <- transform(LBW_CC_Rate, horticulture= ifelse(LBW_CC_Rate$cropcategory == "Horticulture",rate,0))
LBW_CC_Rate <- transform(LBW_CC_Rate, oilseed= ifelse(LBW_CC_Rate$cropcategory == "Oilseed",rate,0))


model15 <- lm(LBW ~ gdp + tap + beds + Cash + pulses + cereals + 
                ccereals + horticulture + oilseed, LBW_CC_Rate)
print(summary(model15))
#part F

cropcash <- subset(raw_LBW,cropcategory=="Cash")
cropcash<- cropcash[which(cropcash$gdp != 0),]
cropcash<- cropcash[which(cropcash$tap != 0),]
cropcash<- cropcash[which(cropcash$beds != 0),]
cropcash<- cropcash[which(cropcash$yield_index != 0),]
cropcash <- cropcash[!duplicated(cropcash$sdyid),]


croppulse <- subset(raw_LBW,cropcategory=="Pulse")
croppulse<- croppulse[which(croppulse$gdp != 0),]
croppulse<- croppulse[which(croppulse$tap != 0),]
croppulse<- croppulse[which(croppulse$beds != 0),]
croppulse<- croppulse[which(croppulse$yield_index != 0),]
croppulse <- croppulse[!duplicated(croppulse$sdyid),]


cropcereal <- subset(raw_LBW,cropcategory=="Cereal")
cropcereal<- cropcereal[which(cropcereal$gdp != 0),]
cropcereal<- cropcereal[which(cropcereal$tap != 0),]
cropcereal<- cropcereal[which(cropcereal$beds != 0),]
cropcereal<- cropcereal[which(cropcereal$yield_index != 0),]
cropcereal <- cropcereal[!duplicated(cropcereal$sdyid),]


crophorti <- subset(raw_LBW,cropcategory=="Horticulture")
crophorti<- crophorti[which(crophorti$gdp != 0),]
crophorti<- crophorti[which(crophorti$tap != 0),]
crophorti<- crophorti[which(crophorti$beds != 0),]
crophorti<- crophorti[which(crophorti$yield_index != 0),]
crophorti <- crophorti[!duplicated(crophorti$sdyid),]


cropoil <- subset(raw_LBW,cropcategory=="Oilseed")
cropoil<- cropoil[which(cropoil$gdp != 0),]
cropoil<- cropoil[which(cropoil$tap != 0),]
cropoil<- cropoil[which(cropoil$beds != 0),]
cropoil<- cropoil[which(cropoil$yield_index != 0),]
cropoil <- cropoil[!duplicated(cropoil$sdyid),]


cropcoarse <- subset(raw_LBW,cropcategory=="Coarse Cereal")
cropcoarse<- cropcoarse[which(cropcoarse$gdp != 0),]
cropcoarse<- cropcoarse[which(cropcoarse$tap != 0),]
cropcoarse<- cropcoarse[which(cropcoarse$beds != 0),]
cropcoarse<- cropcoarse[which(cropcoarse$yield_index != 0),]
cropcoarse <- cropcoarse[!duplicated(cropcoarse$sdyid),]


model16 <- lm(LBW ~ log(gdp) + log(tap) + log(beds) + log(yield_index), cropcash)
print(summary(model16))

model17 <- lm(LBW ~ log(gdp) + log(tap) + log(beds) + log(yield_index), cropcereal)
print(summary(model17))

model18 <- lm(LBW ~ log(gdp) + log(tap) + log(beds) + log(yield_index), crophorti)
print(summary(model18))

model19 <- lm(LBW ~ log(gdp) + log(tap) + log(beds) + log(yield_index), croppulse)
print(summary(model19))

model20 <- lm(LBW ~ log(gdp) + log(tap) + log(beds) + log(yield_index), cropoil)
print(summary(model20))

model21 <- lm(LBW ~ log(gdp) + log(tap) + log(beds) + log(yield_index), cropcoarse)
print(summary(model21))

#part G
cropcash <- subset(raw_LBW,cropcategory=="Cash")
cropcash <- cropcash[!duplicated(cropcash$sdyid),]

croppulse <- subset(raw_LBW,cropcategory=="Pulse")
croppulse <- croppulse[!duplicated(croppulse$sdyid),]

cropcereal <- subset(raw_LBW,cropcategory=="Cereal")
cropcereal <- cropcereal[!duplicated(cropcereal$sdyid),]

crophorti <- subset(raw_LBW,cropcategory=="Horticulture")
crophorti <- crophorti[!duplicated(crophorti$sdyid),]

cropoil <- subset(raw_LBW,cropcategory=="Oilseed")
cropoil <- cropoil[!duplicated(cropoil$sdyid),]

cropcoarse <- subset(raw_LBW,cropcategory=="Coarse Cereal")
cropcoarse <- cropcoarse[!duplicated(cropcoarse$sdyid),]

LBW_log_rate <- rbind(cropcash,cropcereal,cropcoarse,crophorti,cropoil,croppulse)


LBW_log_rate <- LBW_log_rate[which(LBW_log_rate$gdp != 0),]
LBW_log_rate <- LBW_log_rate[which(LBW_log_rate$tap != 0),]
LBW_log_rate <- LBW_log_rate[which(LBW_log_rate$beds != 0),]
LBW_log_rate <- LBW_log_rate[which(LBW_log_rate$yield_index != 0),]


LBW_log_rate <- transform(LBW_log_rate, cash = ifelse(LBW_log_rate$cropcategory == "Cash",log(yield_index),0))
LBW_log_rate <- transform(LBW_log_rate, pulses = ifelse(LBW_log_rate$cropcategory == "Pulse",log(yield_index),0))
LBW_log_rate <- transform(LBW_log_rate, cereals = ifelse(LBW_log_rate$cropcategory == "Cereal",log(yield_index),0))
LBW_log_rate <- transform(LBW_log_rate, ccereals = ifelse(LBW_log_rate$cropcategory == "Coarse Cereal",log(yield_index),0))
LBW_log_rate <- transform(LBW_log_rate, horticulture = ifelse(LBW_log_rate$cropcategory == "Horticulture",log(yield_index),0))
LBW_log_rate <- transform(LBW_log_rate, oilseed = ifelse(LBW_log_rate$cropcategory == "Oilseed",log(yield_index),0))

model22 <- lm(LBW ~ log(gdp) + log(tap) + log(beds) + cash + pulses + cereals + 
                ccereals + horticulture + oilseed, LBW_log_rate)
print(summary(model22))



# other required stuff
cor(raw$gdp,raw$beds)
cor(raw$gdp,raw$tap)
