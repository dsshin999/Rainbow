library(shiny)
library(ggplot2)
library(dplyr)
library(datasets)
library(RCurl)
library(RMySQL)
con<-dbConnect(dbDriver("MySQL"), dbname="nursingsvc", user="admin", password="akstp123", host="192.168.1.131", port=3306)
dbGetQuery(con, "SET NAMES 'utf8'")
dat<-dbReadTable(con, "chart")
dbDisconnect(con)

#실험군을 나이별로 분류해서 total.age에 저장하는 코드
experimental<-filter(dat, char=="experimental")
experimental.50<-filter(experimental, age<60)
experimental.60<-filter(experimental, age>=60, age<70)
experimental.70<-filter(experimental, age>=70, age<80)
experimental.over<-filter(experimental, age>=80)
comparison<-filter(dat, char=="comparison")
comparison.50<-filter(comparison, age<60)
comparison.60<-filter(comparison, age>=60, age<70)
comparison.70<-filter(comparison, age>=70, age<80)
comparison.over<-filter(comparison, age>=80)


aver.ex.50<-summarise(experimental.50, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.60<-summarise(experimental.60, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.70<-summarise(experimental.70, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.over<-summarise(experimental.over, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))


aver.cp.50<-summarise(comparison.50, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.60<-summarise(comparison.60, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.70<-summarise(comparison.70, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.over<-summarise(comparison.over, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))

age<-c(50, 60, 70, 80)
aver.cp<-rbind(aver.cp.50,aver.cp.60,aver.cp.70,aver.cp.over)
aver.cp$age<-age

aver.ex<-rbind(aver.ex.50,aver.ex.60,aver.ex.70,aver.ex.over)
aver.ex$age<-age

char<-c("comparison", "comparison", "comparison", "comparison")
aver.cp$char<-char
char<-c("experimental", "experimental","experimental","experimental")
aver.ex$char<-char
total.age<-rbind(aver.cp, aver.ex)
diff.st<-total.age$aver_af.st-total.age$aver_bf.st
diff.eq5d<-total.age$aver_af.eq-total.age$aver_bf.eq
diff.mr<-total.age$aver_af.mr-total.age$aver_bf.mr
total.age$diff.st<-diff.st
total.age$diff.eq<-diff.eq5d
total.age$diff.mr<-diff.mr

#실험군을 병명으로 구분해서 total.ill에 저장하는 코드
experimental<-filter(dat, char=="experimental")
experimental.a<-filter(experimental, illness=="A") #질병1
experimental.b<-filter(experimental, illness=="B") #질병2
experimental.c<-filter(experimental, illness=="C") #질병3
comparison<-filter(dat, char=="comparison")
comparison.a<-filter(comparison, illness=="A") #질병1
comparison.b<-filter(comparison, illness=="B") #질병2
comparison.c<-filter(comparison, illness=="C") #질병3

aver.ex.a<-summarise(experimental.a, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), 		aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.b<-summarise(experimental.b, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.c<-summarise(experimental.c, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))

aver.cp.a<-summarise(comparison.a, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.b<-summarise(comparison.b, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.c<-summarise(comparison.c, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))


aver.cp<-rbind(aver.cp.a,aver.cp.b,aver.cp.c)
aver.cp$illness<-c("A","B","C")
aver.cp
aver.ex<-rbind(aver.ex.a,aver.ex.b,aver.ex.c)
aver.ex$illness<-c("A","B","C")

char<-c("comparison", "comparison", "comparison")
aver.cp$char<-char
char<-c("experimental", "experimental","experimental")
aver.ex$char<-char
total.ill<-rbind(aver.cp, aver.ex)

diff.st<-total.ill$aver_af.st-total.ill$aver_bf.st
diff.eq5d<-total.ill$aver_af.eq-total.ill$aver_bf.eq
diff.mr<-total.ill$aver_af.mr-total.ill$aver_bf.mr
total.ill$diff.st<-diff.st
total.ill$diff.eq<-diff.eq5d
total.ill$diff.mr<-diff.mr

#실험군 사람들 추출, 월별 평균 내기
experimental<-filter(dat, char=="experimental")
monthly<-summarise(experimental, month1=mean(m1),month2=mean(m2),month3=mean(m3),month4=mean(m4),month5=mean(m5),month6=mean(m6),month7=mean(m7),month8=mean(m8),month9=mean(m9),month10=mean(m10),month11=mean(m11),month12=mean(m12))

#지표 추출
index <- colnames(total.age)

#사람 이름 추출하여 벡터로 저장
personname <- as.vector(dat[,1])
