107-2 大數據分析方法 作業一
================
B0544225 Hsu-En Chan

搞不清楚各行各業的薪資差異嗎? 念研究所到底對第一份工作的薪資影響有多大? CP值高嗎? 透過分析**初任人員平均經常性薪資**- [開放資料連結](https://data.gov.tw/dataset/6647)，可初步了解台灣近幾年各行各業、各學歷的起薪。

比較103年度和106年度大學畢業者的薪資資料
----------------------------------------

### 資料匯入與處理

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 3.5.3

``` r
#import data
eduSalary103<-read_csv("C:/Users/hsuen/Downloads/SalaryTable/103eduSalaryByCarrer.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_double(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
eduSalary106<-read_csv("C:/Users/hsuen/Downloads/SalaryTable/106eduSalatyByCarrer.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
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
```

### 106年度薪資較103年度薪資高的職業有哪些?

``` r
#turn datatype & new a proportion column
eduSalary0306$Salary103<-as.numeric(eduSalary0306$Salary103)
eduSalary0306$Salary106<-as.numeric(eduSalary0306$Salary106)

eduSalary0306$SalaryDiffer <-eduSalary0306$Salary106/eduSalary0306$Salary103

#choose SalaryDiffer bigger than 1 & arrange by SalaryDiffer
SalaryProve0306<-filter(eduSalary0306,SalaryDiffer>1) %>% arrange(desc(SalaryDiffer)) %>% head(10)

knitr::kable(SalaryProve0306)
```

| Career                                    |  Salary103|  Salary106|  SalaryDiffer|
|:------------------------------------------|----------:|----------:|-------------:|
| 其他服務業-技術員及助理專業人員           |      24688|      27929|      1.131278|
| 住宿及餐飲業-服務及銷售工作人員           |      22564|      25486|      1.129498|
| 用水供應及污染整治業-技術員及助理專業人員 |      27944|      31560|      1.129402|
| 專業、科學及技術服務業-專業人員           |      29977|      33384|      1.113654|
| 其他服務業-技藝、機械設備操作及組裝人員   |      24222|      26880|      1.109735|
| 營造業-服務及銷售工作人員                 |      27164|      30125|      1.109005|
| 其他服務業-專業人員                       |      29000|      32000|      1.103448|
| 資訊及通訊傳播業-專業人員                 |      28839|      31817|      1.103263|
| 不動產業-專業人員                         |      30637|      33632|      1.097758|
| 教育服務業-事務支援人員                   |      22334|      24471|      1.095684|

由表格可以知道106薪資較103薪資高的前10名中第一名職業是其他服務業的技術人員，依序下來是住宿及餐飲業的服務銷售人員、用水供應及汙染整治業的技術人員及助理專業人員等。第一名職業：其他服務業的技術人員的103年薪資為24688、106年薪資為27929、薪資成長比率為113%。

### 提高超過5%的的職業有哪些?

``` r
#這是R Code Chunk
#choose SalaryDiffer bigger than 1.05
SalaryProveMore0306<-filter(eduSalary0306,SalaryDiffer>1.05)

knitr::kable(arrange(SalaryProveMore0306,desc(SalaryDiffer)))
```

| Career                                              |  Salary103|  Salary106|  SalaryDiffer|
|:----------------------------------------------------|----------:|----------:|-------------:|
| 其他服務業-技術員及助理專業人員                     |      24688|      27929|      1.131278|
| 住宿及餐飲業-服務及銷售工作人員                     |      22564|      25486|      1.129498|
| 用水供應及污染整治業-技術員及助理專業人員           |      27944|      31560|      1.129402|
| 專業、科學及技術服務業-專業人員                     |      29977|      33384|      1.113654|
| 其他服務業-技藝、機械設備操作及組裝人員             |      24222|      26880|      1.109735|
| 營造業-服務及銷售工作人員                           |      27164|      30125|      1.109005|
| 其他服務業-專業人員                                 |      29000|      32000|      1.103448|
| 資訊及通訊傳播業-專業人員                           |      28839|      31817|      1.103263|
| 不動產業-專業人員                                   |      30637|      33632|      1.097758|
| 教育服務業-事務支援人員                             |      22334|      24471|      1.095684|
| 住宿及餐飲業-技術員及助理專業人員                   |      25633|      28009|      1.092693|
| 專業、科學及技術服務業-技藝、機械設備操作及組裝人員 |      26211|      28595|      1.090954|
| 運輸及倉儲業-技藝、機械設備操作及組裝人員           |      28087|      30618|      1.090113|
| 其他服務業-事務支援人員                             |      23863|      26007|      1.089846|
| 教育服務業-服務及銷售工作人員                       |      24491|      26668|      1.088890|
| 用水供應及污染整治業                                |      27456|      29834|      1.086611|
| 用水供應及污染整治業-專業人員                       |      31444|      34107|      1.084690|
| 資訊及通訊傳播業                                    |      27055|      29198|      1.079209|
| 支援服務業-服務及銷售工作人員                       |      24166|      26001|      1.075933|
| 藝術、娛樂及休閒服務業-技藝、機械設備操作及組裝人員 |      24895|      26768|      1.075236|
| 資訊及通訊傳播業-事務支援人員                       |      25276|      27156|      1.074379|
| 教育服務業                                          |      24027|      25784|      1.073126|
| 營造業-專業人員                                     |      30580|      32785|      1.072106|
| 專業、科學及技術服務業                              |      27663|      29648|      1.071757|
| 支援服務業-技藝、機械設備操作及組裝人員             |      25365|      27169|      1.071122|
| 住宿及餐飲業                                        |      24646|      26398|      1.071087|
| 住宿及餐飲業-技藝、機械設備操作及組裝人員           |      24823|      26585|      1.070983|
| 電力及燃氣供應業-服務及銷售工作人員                 |      26013|      27837|      1.070119|
| 運輸及倉儲業-技術員及助理專業人員                   |      28974|      30993|      1.069683|
| 運輸及倉儲業-事務支援人員                           |      25886|      27685|      1.069497|
| 醫療保健服務業-技術員及助理專業人員                 |      28465|      30392|      1.067697|
| 專業、科學及技術服務業-技術員及助理專業人員         |      27195|      29016|      1.066961|
| 支援服務業-技術員及助理專業人員                     |      26933|      28696|      1.065459|
| 用水供應及污染整治業-服務及銷售工作人員             |      28736|      30593|      1.064623|
| 礦業及土石採取業-技藝、機械設備操作及組裝人員       |      26647|      28367|      1.064548|
| 用水供應及污染整治業-技藝、機械設備操作及組裝人員   |      26087|      27758|      1.064055|
| 服務業部門-技藝、機械設備操作及組裝人員             |      26722|      28376|      1.061897|
| 教育服務業-技術員及助理專業人員                     |      25675|      27250|      1.061344|
| 服務業部門-專業人員                                 |      30806|      32632|      1.059274|
| 運輸及倉儲業                                        |      28143|      29808|      1.059162|
| 資訊及通訊傳播業-技術員及助理專業人員               |      27288|      28902|      1.059147|
| 醫療保健服務業-技藝、機械設備操作及組裝人員         |      27409|      29028|      1.059068|
| 用水供應及污染整治業-事務支援人員                   |      25430|      26924|      1.058750|
| 金融及保險業-事務支援人員                           |      29070|      30771|      1.058514|
| 服務業部門-技術員及助理專業人員                     |      27882|      29504|      1.058174|
| 藝術、娛樂及休閒服務業-事務支援人員                 |      23602|      24970|      1.057961|
| 藝術、娛樂及休閒服務業                              |      25204|      26614|      1.055943|
| 營造業-事務支援人員                                 |      24892|      26256|      1.054797|
| 工業及服務業部門-專業人員                           |      30449|      32108|      1.054485|
| 服務業部門-事務支援人員                             |      25554|      26930|      1.053847|
| 電力及燃氣供應業-技藝、機械設備操作及組裝人員       |      27253|      28717|      1.053719|
| 教育服務業-專業人員                                 |      25722|      27101|      1.053612|
| 專業、科學及技術服務業-服務及銷售工作人員           |      26245|      27649|      1.053496|
| 服務業部門                                          |      27258|      28715|      1.053452|
| 其他服務業                                          |      24232|      25517|      1.053029|
| 製造業-專業人員                                     |      30035|      31612|      1.052505|
| 工業部門-專業人員                                   |      30215|      31775|      1.051630|
| 資訊及通訊傳播業-服務及銷售工作人員                 |      25995|      27296|      1.050048|

``` r
nrow(SalaryProveMore0306)
```

    ## [1] 58

由上面表格可以知道總共有58個職業薪資提高超過5%，全部職業總共有140個，不到全部職業的一半，好像是有點少。

### 主要的職業種別是哪些種類呢?

``` r
#substring & take out the values
Career<-strsplit(SalaryProveMore0306$Career,"-")
for(n in 1:length(Career)){
  Career[n]<-Career[[n]][1]
}
table(unlist(Career)) %>% knitr::kable()
```

| Var1                   |  Freq|
|:-----------------------|-----:|
| 工業及服務業部門       |     1|
| 工業部門               |     1|
| 不動產業               |     1|
| 支援服務業             |     3|
| 用水供應及污染整治業   |     6|
| 住宿及餐飲業           |     4|
| 其他服務業             |     5|
| 服務業部門             |     5|
| 金融及保險業           |     1|
| 專業、科學及技術服務業 |     5|
| 教育服務業             |     5|
| 資訊及通訊傳播業       |     5|
| 運輸及倉儲業           |     4|
| 電力及燃氣供應業       |     2|
| 製造業                 |     1|
| 營造業                 |     3|
| 醫療保健服務業         |     2|
| 藝術、娛樂及休閒服務業 |     3|
| 礦業及土石採取業       |     1|

從上表可以知道，用水供應及汙染整治業的職業種別最多，總共有六個。

男女同工不同酬現況分析
----------------------

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為103到106年度的大學畢業薪資。

### 103到106年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?

``` r
#import data
eduSalary104<-read_csv("C:/Users/hsuen/Downloads/SalaryTable/104eduSalaryByCarrer.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
eduSalary105<-read_csv("C:/Users/hsuen/Downloads/SalaryTable/105eduSalatyByCarrer.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
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
```

``` r
knitr::kable(Salary3)
```

| Career                                              |  SalaryFM103|
|:----------------------------------------------------|------------:|
| 礦業及土石採取業-技藝、機械設備操作及組裝人員       |        84.97|
| 教育服務業-技藝、機械設備操作及組裝人員             |        88.49|
| 其他服務業-技術員及助理專業人員                     |        89.36|
| 電力及燃氣供應業-技藝、機械設備操作及組裝人員       |        91.77|
| 礦業及土石採取業-服務及銷售工作人員                 |        92.57|
| 營造業                                              |        95.58|
| 教育服務業-事務支援人員                             |        95.83|
| 教育服務業                                          |        95.91|
| 藝術、娛樂及休閒服務業-技藝、機械設備操作及組裝人員 |        96.13|
| 其他服務業                                          |        96.21|

第一個表格是103年的薪資男女比例差異前10名，差異最大的是礦業及土石採取業的技藝、機械設備操作及組裝人員，薪資男女比例值為84.97。

``` r
knitr::kable(Salary4)
```

| Career                                            |  SalaryFM104|
|:--------------------------------------------------|------------:|
| 電力及燃氣供應業-技藝、機械設備操作及組裝人員     |        91.69|
| 教育服務業-服務及銷售工作人員                     |        91.90|
| 礦業及土石採取業-技術員及助理專業人員             |        92.42|
| 礦業及土石採取業-技藝、機械設備操作及組裝人員     |        93.10|
| 礦業及土石採取業                                  |        95.28|
| 其他服務業-事務支援人員                           |        95.47|
| 營造業-技藝、機械設備操作及組裝人員               |        95.64|
| 用水供應及污染整治業-技藝、機械設備操作及組裝人員 |        95.90|
| 營造業                                            |        96.35|
| 教育服務業                                        |        96.44|

第二個表格是104年的薪資男女比例差異前10名，差異最大的是電力及燃氣供應業的技藝、機械設備操作及組裝人員，薪資男女比例值為91.69。

在103薪資男女比例是第一名的是礦業及土石採取業的技藝、機械設備操作及組裝人員從第一名跑到第四名，薪資男女比例值為93.10，看起來進步很多。

``` r
knitr::kable(Salary5)
```

| Career                                        |  SalaryFM105|
|:----------------------------------------------|------------:|
| 不動產業-技藝、機械設備操作及組裝人員         |        91.38|
| 醫療保健服務業-專業人員                       |        94.98|
| 用水供應及污染整治業-事務支援人員             |        95.04|
| 營造業-事務支援人員                           |        95.65|
| 不動產業-事務支援人員                         |        95.66|
| 營造業                                        |        95.78|
| 營造業-專業人員                               |        96.52|
| 資訊及通訊傳播業-技藝、機械設備操作及組裝人員 |        96.64|
| 不動產業-服務及銷售工作人員                   |        96.68|
| 其他服務業                                    |        96.72|

第三個表格是105年的薪資男女比例差異前10名，差異最大的是不動產業的技藝、機械設備操作及組裝人員，薪資男女比例值為91.38。 礦業及土石採取業的技藝、機械設備操作及組裝人員已經沒有出現在前10名了。

``` r
knitr::kable(Salary6)
```

| Career                                        |  SalaryFM106|
|:----------------------------------------------|------------:|
| 電力及燃氣供應業-技藝、機械設備操作及組裝人員 |        95.51|
| 營造業-服務及銷售工作人員                     |        95.93|
| 其他服務業-事務支援人員                       |        96.23|
| 電力及燃氣供應業-技術員及助理專業人員         |        96.54|
| 其他服務業                                    |        96.57|
| 住宿及餐飲業-技藝、機械設備操作及組裝人員     |        96.58|
| 教育服務業-專業人員                           |        96.71|
| 營造業                                        |        96.71|
| 運輸及倉儲業-事務支援人員                     |        96.83|
| 其他服務業-技術員及助理專業人員               |        96.84|

第四個表格是106年的薪資男女比例差異前10名，差異最大的是電力及燃氣供應業的技藝、機械設備操作及組裝人員，薪資男女比例值為95.51。 從103到106年的薪資男女比例來看，第一名的薪資男女比例有逐漸在提高，也就是說，男女薪資差異有逐漸在縮小。

### 哪些行業女生薪資比男生薪資多?

``` r
#choose SalaryFM** bigger than 100 & arrange by SalaryFM**
SalaryF3<-filter(Salary3456,SalaryFM103>100) %>% arrange(desc(SalaryFM103)) %>% head(10) %>% select(Career,SalaryFM103)
SalaryF4<-filter(Salary3456,SalaryFM104>100) %>% arrange(desc(SalaryFM104)) %>% head(10) %>% select(Career,SalaryFM104)
SalaryF5<-filter(Salary3456,SalaryFM105>100) %>% arrange(desc(SalaryFM105)) %>% head(10) %>% select(Career,SalaryFM105)
SalaryF6<-filter(Salary3456,SalaryFM106>100) %>% arrange(desc(SalaryFM106)) %>% head(10) %>% select(Career,SalaryFM106)
```

``` r
knitr::kable(SalaryF3)
```

| Career |  SalaryFM103|
|:-------|------------:|

第一個表格甚麼都沒有，也就是說大多職業的薪資，男生都較女生高。

``` r
knitr::kable(SalaryF4)
```

| Career                                              |  SalaryFM104|
|:----------------------------------------------------|------------:|
| 專業、科學及技術服務業-技藝、機械設備操作及組裝人員 |       100.26|

第二個表格是104年的薪資男女比例差異女生比男生薪資高的前10名，雖然說要列出前10名，但是看起來只有一種職業女生的薪資比男生高，是專業、科學及技術服務業的技藝、機械設備操作及組裝人員。沒有其他職業了。

``` r
knitr::kable(SalaryF5)
```

| Career                |  SalaryFM105|
|:----------------------|------------:|
| 金融及保險業-專業人員 |       100.11|

第三個表格是105年的薪資男女比例差異女生比男生薪資高的前10名，還是一樣只有一種職業，是金融及保險業的專業人員。

``` r
knitr::kable(SalaryF6)
```

| Career                              |  SalaryFM106|
|:------------------------------------|------------:|
| 資訊及通訊傳播業-服務及銷售工作人員 |       100.33|

第四個表格是106年的薪資男女比例差異女生比男生薪資高的前10名，還是一樣只有一種職業，是資訊及通訊傳播業的服務及銷售工作人員。 由上面四個表格來看，女生比男生薪資高的很少，103年還幾乎沒有。

研究所薪資差異
--------------

以106年 度的資料來看，哪個職業別念研究所最划算呢 (研究所學歷薪資與大學學歷薪資增加比例最多)?

``` r
#get columns & rename columns
Salary106UI<-data.frame(Career=eduSalary106$大職業別,UniversityS=eduSalary106$`大學-薪資`,InstituteS=eduSalary106$`研究所及以上-薪資`)

#取代字串
Salary106UI$UniversityS<-gsub("—",NA,Salary106UI$UniversityS)
Salary106UI$InstituteS<-gsub("—",NA,Salary106UI$InstituteS)

#turn the datatype

Salary106UI$InstituteS<-as.numeric(Salary106UI$InstituteS)
Salary106UI$UniversityS<-as.numeric(Salary106UI$UniversityS)
str(Salary106UI)
```

    ## 'data.frame':    140 obs. of  3 variables:
    ##  $ Career     : Factor w/ 140 levels "工業及服務業部門",..: 1 7 2 4 5 3 6 8 14 9 ...
    ##  $ UniversityS: num  28446 32108 28647 26781 26644 ...
    ##  $ InstituteS : num  33633 36778 32424 30799 NA ...

``` r
#new a proportion column 
Salary106UI$proportion<-Salary106UI$InstituteS/Salary106UI$UniversityS
arrange(Salary106UI,desc(proportion)) %>% head(10) %>% knitr::kable()
```

| Career                              |  UniversityS|  InstituteS|  proportion|
|:------------------------------------|------------:|-----------:|-----------:|
| 礦業及土石採取業-事務支援人員       |        24815|       30000|    1.208946|
| 專業、科學及技術服務業              |        29648|       35666|    1.202982|
| 其他服務業-技術員及助理專業人員     |        27929|       33500|    1.199470|
| 專業、科學及技術服務業-事務支援人員 |        27035|       32234|    1.192306|
| 批發及零售業                        |        27611|       32910|    1.191916|
| 製造業                              |        28155|       33458|    1.188350|
| 藝術、娛樂及休閒服務業-事務支援人員 |        24970|       29657|    1.187705|
| 工業部門                            |        28263|       33448|    1.183455|
| 工業及服務業部門                    |        28446|       33633|    1.182346|
| 服務業部門                          |        28715|       33922|    1.181334|

上面的表格是念研究所最划算的前十名。礦業及土石採取業的事務支援人員是研究所學歷薪資與大學學歷薪資增加比例最多，共增加了0.21左右。

我有興趣的職業別薪資狀況分析
----------------------------

### 有興趣的職業別篩選，呈現薪資

``` r
#choose "資訊及通訊傳播業" from Salary106UI table
ITSalary<-Salary106UI[grepl("資訊及通訊傳播業",Salary106UI$Career),]
knitr::kable(ITSalary)
```

|     | Career                                        |  UniversityS|  InstituteS|  proportion|
|-----|:----------------------------------------------|------------:|-----------:|-----------:|
| 78  | 資訊及通訊傳播業                              |        29198|       33944|    1.162545|
| 79  | 資訊及通訊傳播業-專業人員                     |        31817|       36545|    1.148600|
| 80  | 資訊及通訊傳播業-技術員及助理專業人員         |        28902|       32354|    1.119438|
| 81  | 資訊及通訊傳播業-事務支援人員                 |        27156|       30856|    1.136250|
| 82  | 資訊及通訊傳播業-服務及銷售工作人員           |        27296|          NA|          NA|
| 83  | 資訊及通訊傳播業-技藝、機械設備操作及組裝人員 |        27200|          NA|          NA|
| 84  | 資訊及通訊傳播業-基層技術工及勞力工           |           NA|          NA|          NA|

上面的表格是我有興趣的職業別薪資狀況，我選擇了資訊及通訊傳播業。可以看的出來，我即使只有大學畢業，沒有讀研究所，還是領超過22K。但是想要賺比較多了話，選擇資訊及通訊傳播業的專業人員可以賺比較多。

### 這些職業別研究所薪資與大學薪資差多少呢？

``` r
#new a Differ column
ITSalary$Differ<-ITSalary$InstituteS-ITSalary$UniversityS
knitr::kable(ITSalary)
```

|     | Career                                        |  UniversityS|  InstituteS|  proportion|  Differ|
|-----|:----------------------------------------------|------------:|-----------:|-----------:|-------:|
| 78  | 資訊及通訊傳播業                              |        29198|       33944|    1.162545|    4746|
| 79  | 資訊及通訊傳播業-專業人員                     |        31817|       36545|    1.148600|    4728|
| 80  | 資訊及通訊傳播業-技術員及助理專業人員         |        28902|       32354|    1.119438|    3452|
| 81  | 資訊及通訊傳播業-事務支援人員                 |        27156|       30856|    1.136250|    3700|
| 82  | 資訊及通訊傳播業-服務及銷售工作人員           |        27296|          NA|          NA|      NA|
| 83  | 資訊及通訊傳播業-技藝、機械設備操作及組裝人員 |        27200|          NA|          NA|      NA|
| 84  | 資訊及通訊傳播業-基層技術工及勞力工           |           NA|          NA|          NA|      NA|

上面的表格是資訊及通訊傳播業研究所薪資與大學薪資差。 如果讀研究所了話，會比只有大學學歷薪資多出3400以上。如果是專業人員了話可以多出4728元。

但是還是不會改變我大學一畢業不想讀研究所的想法。說不定我比讀研究所的同學先工作的兩年，我的薪水就比原本多超過4718元。 但是我還是會讀研究所啦，只是不會一畢業就讀。
