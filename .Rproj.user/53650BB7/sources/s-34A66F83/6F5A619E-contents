
#subset practice

View(mtcars)
subset(mtcars,cyl<=6)
mtcars[grepl("1|2|3|4|5|6",mtcars$cyl),]


mtcars[grepl("Toyota",row.names(mtcars)),]


#prectice in class (DCard)

library(rvest)
DcardCGU <-read_html("https://www.dcard.tw/f/cgu")
post_title <- DcardCGU %>% html_nodes(".PostEntry_unread_2U217-") %>% html_text()
poster<-DcardCGU %>% html_nodes(".PostAuthor_root_3vAJfe")%>% html_text()
post_like<- DcardCGU %>% html_nodes(".hlvyVg") %>% html_text()

class(post_like)
post_like<-as.integer(post_like)

DCardCGU_posts <- data.frame(title = post_title,poster=poster,likeN=post_like,stringsAsFactors = F)


#prectice in class (PTT)

PttHtml<-paste0("https://www.ptt.cc/bbs/Tech_Job/index",3390:3401,".html")

titleAll<-c()

for(url in PttHtml){
  Ptt <-read_html(url)
  PttTitle<-Ptt %>% html_nodes(".title a") %>% html_text()
  titleAll<-c(titleAll,PttTitle)
}

titleAll[grepl("面試",titleAll)]

Ptt <-read_html("https://www.ptt.cc/bbs/Tech_Job/index")
prePtt <- Ptt %>% html_nodes(".wide:nth-child(2)") %>% html_attr("href")

Pttnum<-as.numeric( gsub("/bbs/Tech_Job/index|.html","",prePtt))
(Pttnum-10):Pttnum

#practice in class json

library(jsonlite)
TYpark<-fromJSON("http://data.tycg.gov.tw/api/v1/rest/datastore/0daad6e6-0632-44f5-bd25-5e1de1e9146f?format=json")
TYPrecords<-data.frame(TYpark$result$records)

park<-TYPrecords[grepl("公園",TYPrecords$parkName),]

#bind

USPersonalExpenditure
USPersonalExpenditure<-cbind(USPersonalExpenditure,c(100,60,30,10,5))
USPersonalExpenditure$X1965 <-c(100,60,30,10,5)
USPersonalExpenditure[2]

#order

iris[order(iris$Sepal.Length,decreasing = T),]
DCardCGU_posts[order(DCardCGU_posts$likeN,decreasing = T),]


#merge

name<-data.frame(ID=c(1,2,3,4,5),
                 name=c("lillian","Ramanda","Sherry","Eliza","tiffiny"))
score<-data.frame(ID=c(2,3,5),
                  sco=c(87,78,90))
merge(name,score,by="ID")

#join

install.packages("dplyr")
library(dplyr)
inner_join(name,score,by="ID")
left_join(name,score,by="ID")
right_join()
full_join()
semi_join(name,score,by="ID")


library(dplyr)
library(readr)
education<-read_csv("C:/Users/hsuen/Downloads/opendata106Y020_new.csv")
population<-read_csv("C:/Users/hsuen/Downloads/opendata10612M010.csv")

innerJoin<-inner_join(education,population,by=c("village","site_id")) #how to join by two columns
innerJoin$edu_age_15up_total<-as.numeric(innerJoin$edu_age_15up_total)
innerJoin$birth_total<-as.numeric(innerJoin$birth_total)

is.numeric(innerJoin$edu_age_15up_total)
is.numeric(innerJoin$birth_total)

innerJoin$BirthRate<-(innerJoin$birth_total/innerJoin$edu_age_15up_total)
is.numeric(innerJoin$BirthRate)
innerJoin<-innerJoin[order(innerJoin$BirthRate,decreasing = T),]

View(innerJoin)


#Missing Value

install.packages("tidyr")
library(tidyr)

stateW<-read_csv("C:/Users/hsuen/Downloads/HEPATITIS_A_Cases.csv")
stateL <-gather(stateW,key=State,value=Value,colnames(stateW)[3:61])
str(stateL)

stateL$Value<-as.numeric(stateL$Value)
stateL<-stateL[complete.cases(stateL),]
str(stateL)



#exercise in class wedth turn long education list

library(tidyr)

education[5]
educationNo1<-education[-1,] 
educationL <-gather(education,key=Educate,value=Value,colnames(education)[6:length(education)])
educationLNew<-educationL[,-5]

#exercise in class


















