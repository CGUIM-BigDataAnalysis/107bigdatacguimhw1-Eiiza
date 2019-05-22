
library(dplyr)
library(readr)


#1

#import data
eduSalary103<-read_csv("C:/Users/hsuen/Downloads/SalaryTable/103eduSalaryByCarrer.csv")
eduSalary106<-read_csv("C:/Users/hsuen/Downloads/SalaryTable/106eduSalatyByCarrer.csv")

#make sure the rownames are same, before joining by "大職業別"
eduSalary106$大職業別<-gsub("_","、",eduSalary106$大職業別)

#turn datatype to match each other
eduSalary106$`經常性薪資-女/男`<-gsub("…",NA,eduSalary106$`經常性薪資-女/男`)
eduSalary106$`經常性薪資-女/男`<-as.double(eduSalary106$`經常性薪資-女/男`)

#joining by "大職業別"
eduSalary0306<-merge(x = eduSalary103, y = eduSalary106, by = "大職業別", all = TRUE) %>% select(大職業別,`大學-薪資.x`,`大學-薪資.y`) %>% rename(Career=大職業別,Salary103=`大學-薪資.x`,Salary106=`大學-薪資.y`)

#取代字串
eduSalary0306$Salary103<-gsub("—",NA,eduSalary0306$Salary103)
eduSalary0306$Salary106<-gsub("—",NA,eduSalary0306$Salary106)

#turn datatype & new a proportion column
eduSalary0306$Salary103<-as.numeric(eduSalary0306$Salary103)
eduSalary0306$Salary106<-as.numeric(eduSalary0306$Salary106)

eduSalary0306$SalaryDiffer <-eduSalary0306$Salary106/eduSalary0306$Salary103

#choose SalaryDiffer bigger than 1 & arrange by SalaryDiffer
SalaryProve0306<-filter(eduSalary0306,SalaryDiffer>1) %>% arrange(desc(SalaryDiffer)) %>% head(10)

#choose SalaryDiffer bigger than 1.05
SalaryProveMore0306<-filter(eduSalary0306,SalaryDiffer>1.05)
nrow(SalaryProveMore0306)
arrange(SalaryProveMore0306,desc(SalaryDiffer))

#substring & take out the values
Career<-strsplit(SalaryProveMore0306$Career,"-")
for(n in 1:length(Career)){
  Career[n]<-Career[[n]][1]
}
table(unlist(Career))


#2

#import data
eduSalary104<-read_csv("C:/Users/hsuen/Downloads/SalaryTable/104eduSalaryByCarrer.csv")
eduSalary105<-read_csv("C:/Users/hsuen/Downloads/SalaryTable/105eduSalatyByCarrer.csv")

#new a table & joining by "大職業別"/Career

Salary34<-merge(x = eduSalary103, y = eduSalary104, by = "大職業別", all = TRUE) %>% select(大職業別,`大學-女/男.x`,`大學-女/男.y`) %>% rename(Career=大職業別,SalaryFM103=`大學-女/男.x`,SalaryFM104=`大學-女/男.y`)
Salary56<-merge(x = eduSalary105, y = eduSalary106, by = "大職業別", all = TRUE) %>% select(大職業別,`大學-女/男.x`,`大學-女/男.y`) %>% rename(Career=大職業別,SalaryFM105=`大學-女/男.x`,SalaryFM106=`大學-女/男.y`)
Salary3456<-merge(x = Salary34, y = Salary56, by = "Career", all = TRUE)

#取代字串
Salary3456$SalaryFM103<-gsub("—|…",NA,Salary3456$SalaryFM103)
Salary3456$SalaryFM104<-gsub("—|…",NA,Salary3456$SalaryFM104)
Salary3456$SalaryFM105<-gsub("—|…",NA,Salary3456$SalaryFM105)
Salary3456$SalaryFM106<-gsub("—|…",NA,Salary3456$SalaryFM106)

#turn datatype
Salary3456$SalaryFM103<-as.numeric(Salary3456$SalaryFM103)
Salary3456$SalaryFM104<-as.numeric(Salary3456$SalaryFM104)
Salary3456$SalaryFM105<-as.numeric(Salary3456$SalaryFM105)
Salary3456$SalaryFM106<-as.numeric(Salary3456$SalaryFM106)

#choose SalaryFM** smaller than 100 & arrange by SalaryFM**
Salary3<-filter(Salary3456,SalaryFM103<100) %>% arrange(SalaryFM103) %>% head(10) %>% select(Career,SalaryFM103)
Salary4<-filter(Salary3456,SalaryFM104<100) %>% arrange(SalaryFM104) %>% head(10) %>% select(Career,SalaryFM104)
Salary5<-filter(Salary3456,SalaryFM105<100) %>% arrange(SalaryFM105) %>% head(10) %>% select(Career,SalaryFM105)
Salary6<-filter(Salary3456,SalaryFM106<100) %>% arrange(SalaryFM106) %>% head(10) %>% select(Career,SalaryFM106)

#choose SalaryFM** bigger than 100 & arrange by SalaryFM**
SalaryF3<-filter(Salary3456,SalaryFM103>100) %>% arrange(desc(SalaryFM103)) %>% head(10) %>% select(Career,SalaryFM103)
SalaryF4<-filter(Salary3456,SalaryFM104>100) %>% arrange(desc(SalaryFM104)) %>% head(10) %>% select(Career,SalaryFM104)
SalaryF5<-filter(Salary3456,SalaryFM105>100) %>% arrange(desc(SalaryFM105)) %>% head(10) %>% select(Career,SalaryFM105)
SalaryF6<-filter(Salary3456,SalaryFM106>100) %>% arrange(desc(SalaryFM106)) %>% head(10) %>% select(Career,SalaryFM106)


#3

#get columns & rename columns
Salary106UI<-data.frame(Career=eduSalary106$大職業別,UniversityS=eduSalary106$`大學-薪資`,InstituteS=eduSalary106$`研究所及以上-薪資`)

#取代字串
Salary106UI$UniversityS<-gsub("—",NA,Salary106UI$UniversityS)
Salary106UI$InstituteS<-gsub("—",NA,Salary106UI$InstituteS)

#turn the datatype

Salary106UI$InstituteS<-as.numeric(Salary106UI$InstituteS)
Salary106UI$UniversityS<-as.numeric(Salary106UI$UniversityS)
str(Salary106UI)

#new a proportion column 
Salary106UI$proportion<-Salary106UI$InstituteS/Salary106UI$UniversityS
arrange(Salary106UI,desc(proportion)) %>% head(10)


#4

#choose "資訊及通訊傳播業" from Salary106UI table
ITSalary<-Salary106UI[grepl("資訊及通訊傳播業",Salary106UI$Career),] 

#new a Differ column
ITSalary$Differ<-ITSalary$InstituteS-ITSalary$UniversityS












