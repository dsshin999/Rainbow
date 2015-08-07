library(shiny)
library(ggplot2)
library(dplyr)
library(datasets)
library(RCurl)

x<-getURL("https://raw.githubusercontent.com/dsshin999/INputData/master/yejink/R/chart.csv")
dat<-read.csv(text=x, fileEncoding = "UTF-8")
print(dat)
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
total<-rbind(aver.cp, aver.ex)
diff.st<-total$aver_af.st-total$aver_bf.st
diff.eq5d<-total$aver_af.eq-total$aver_bf.eq
diff.mr<-total$aver_af.mr-total$aver_bf.mr
total$diff.st<-diff.st
total$diff.eq<-diff.eq5d
total$diff.mr<-diff.mr

experimental<-filter(dat, char=="experimental")
monthly<-summarise(experimental, month1=mean(m1),month2=mean(m2),month3=mean(m3),month4=mean(m4),month5=mean(m5),month6=mean(m6),month7=mean(m7),month8=mean(m8),month9=mean(m9),month10=mean(m10),month11=mean(m11),month12=mean(m12))

index <- colnames(total)
personname <- as.vector(dat[,1])
