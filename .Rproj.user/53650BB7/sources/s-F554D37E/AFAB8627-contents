
#Quick view

install.packages("funModeling")
library(funModeling)

df_status(heart_disease)
plot_num(heart_disease)
profiling_num(heart_disease)

#Univariate Quantitative

mean(mtcars$hp)
sd(mtcars$hp) 

#Bivariate Quantitative
?cor
cor(airquality$Ozone,airquality$Temp) #?[1] NA

CarTable<-table(mtcars$cyl,mtcars$gear)
prop.table(CarTable,margin = 1)

#dplyr

library(dplyr)

install.packages("SportsAnalytics")

library(SportsAnalytics)
NBA1516<-fetch_NBAPlayerStatistics("15-16")

filter(NBA1516,Assists>100,Steals>20) %>% select(Name,Team,Position,GamesPlayed,TotalMinutesPlayed,Assists,Steals)

FiltNBAT<-NBA1516[(NBA1516$Assists>100)&(NBA1516$Steals>20),]

meanNBA<-summarise(FiltNBAT,MeanGamesPlayed=mean(GamesPlayed),MeanTotalMinutesPlayed=mean(TotalMinutesPlayed))

SumMeanNBA<-filter(NBA1516,Assists>100,Steals>20) %>% summarise(MeanGamesPlayed=mean(GamesPlayed),MeanTotalMinutesPlayed=mean(TotalMinutesPlayed))

AllNBA<-filter(NBA1516,Assists>100,Steals>20) %>% group_by(Position) %>% summarise(MeanGamesPlayed=mean(GamesPlayed),MeanTotalMinutesPlayed=mean(TotalMinutesPlayed)) %>% arrange(desc(MeanGamesPlayed))

#practice mtcars

mttable<-table(mtcars$am,mtcars$amcyl)
prop.table(mttable,margin = 1)

#practice NBA1718 FieldGoalsRate

NBA1718<-fetch_NBAPlayerStatistics("17-18")
NBA1718filter<-mutate(NBA1718,FieldGoalsRate=round((FieldGoalsMade/FieldGoalsAttempted*100), digits = 2)) %>% select(Name,Team,Position,GamesPlayed,FieldGoalsRate) %>% arrange(desc(FieldGoalsRate)) %>% head(10)

#practice NBA1617 SAN GamesPlayed

NBA1617<-fetch_NBAPlayerStatistics("16-17")

filter(NBA1516,Assists>100,Steals>20) %>% filter(Team=="SAN") %>% arrange(desc(GamesPlayed))

#practice airquality

airQ<-airquality %>% group_by(Month) %>% summarise(OzoneMean=mean(Ozone,na.rm=T),
                                             OzoneSd=sd(Ozone,na.rm=T),
                                             WindMean=mean(Wind,na.rm=T),
                                             WindSd=sd(Wind,na.rm=T),
                                             TempMean=mean(Temp,na.rm=T),
                                             TempSd=sd(Temp,na.rm=T))

#5月的臭氧濃度是否為常態分佈>t-test
airQMay<-filter(airquality,Month==5) 
shapiro.test(airQMay$Ozone)
#5月不是常態分佈

#7月的臭氧濃度是否為常態分佈>t-test
airQJuly<-filter(airquality,Month==7)
shapiro.test(airQJuly$Ozone)
#7月是常態分佈

#變異數檢定>f-test
var.test(airQMay$Ozone, airQJuly$Ozone)
#p-value = 0.08355>0.05 變異數相同的假設成立

# 獨立雙樣本 t 檢定（當變異數相同）
t.test(airQMay$Ozone, airQJuly$Ozone, var.equal = T)
#p-value = 2.207e-05<0.05 5月跟7月變異數是有差異的


#rmarkdown

install.packages("rmarkdown")

#data.table

library(data.table)
NBA1718<-fetch_NBAPlayerStatistics("17-18")
NBA1718DT <- data.table(NBA1718)
NBA1718DT[Assists>100,.(meanSteals=mean(Steals),meanTotalRe=mean(TotalRebounds)),by=Team]

#practice NBA1819 0516

library(SportsAnalytics)
library(data.table)
NBA1819<-fetch_NBAPlayerStatistics("18-19")
NBA1819DT<- data.table(NBA1819)

NBA1819DT[TotalPoints>2000,.N]
NBA1819DT[TotalPoints>2000,.(meanSteals=mean(Steals),meanTotalRe=mean(TotalRebounds))]
filter(NBA1819,TotalPoints>2000) %>% summarise(MeanSteals=mean(Steals),MeanTotalRebounds=mean(TotalRebounds))

NBA1819DT[GamesPlayed>50,.(meanSteals=mean(Steals),meanTotalRe=mean(TotalRebounds)),by=Position]
filter(NBA1819,GamesPlayed>50) %>%  group_by(Position) %>% summarise(MeanSteals=mean(Steals),MeanTotalRebounds=mean(TotalRebounds))

teamH<-NBA1819 %>% group_by(Team) %>%  arrange(Team,desc(TotalPoints)) %>% slice(1)

# mtcars statistics

install.packages("MASS")
library(MASS)

mtcarsAmCyl<- na.omit(subset(mtcars, select = c(am, cyl)))

mtcarsAmCyl.table <- table(mtcarsAmCyl$am, mtcarsAmCyl$cyl) 


chisq.test(mtcarsAmCyl.table)
#p-value=0.01265< 0.05, reject H0,Cyl and Am have relative by chi-squared test
#Chi-squared approximation may be incorrect: mtcarsAmCyl.table number<5 Chi-squared may not be accurate
fisher.test(mtcarsAmCyl.table)
#p-value=0.009105< 0.05, reject H0,Cyl and Am have relative by chi-squared test
















