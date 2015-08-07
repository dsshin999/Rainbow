#데이터를 받아서 50대, 60대 70대, 80대 이상으로 데이터를 정렬하는 코드
library(ggplot2)
library(dplyr)

  dat<-read.csv(file.choose(),header=T,stringsAsFactors = F)
experimental<-filter(dat, char=="experimental")
experimental.50<-filter(experimental, age<60) #실험군 ~59
experimental.60<-filter(experimental, age>=60, age<70) #실험군 60대
experimental.70<-filter(experimental, age>=70, age<80) #실험군 70대
experimental.over<-filter(experimental, age>=80) #실험군 80~
comparison<-filter(dat, char=="comparison")
comparison.50<-filter(comparison, age<60) #대조군 ~59
comparison.60<-filter(comparison, age>=60, age<70) #대조군 60대
comparison.70<-filter(comparison, age>=70, age<80) #대조군 70대
comparison.over<-filter(comparison, age>=80) #대조군 80~

#실험군 데이터들을 나이대별로 평균내어 테이블로 저장
aver.ex.50<-summarise(experimental.50, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.60<-summarise(experimental.60, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.70<-summarise(experimental.70, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.over<-summarise(experimental.over, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))

#대조군 데이터를 나이대별로 평균내어 테이블로 저장
aver.cp.50<-summarise(comparison.50, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.60<-summarise(comparison.60, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.70<-summarise(comparison.70, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.over<-summarise(comparison.over, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))

age<-c(50, 60, 70, 80)
aver.cp<-rbind(aver.cp.50,aver.cp.60,aver.cp.70,aver.cp.over) #aver.cp에 모든 나이별 데이터가 한 테이블에 묶임
aver.cp$age<-age #나이대를 알려주는 열 추가

aver.ex<-rbind(aver.ex.50,aver.ex.60,aver.ex.70,aver.ex.over)
aver.ex$age<-age

char<-c("comparison", "comparison", "comparison", "comparison")
aver.cp$char<-char#대조군에 대조군임을 알려주는 열 추가
char<-c("experimental", "experimental","experimental","experimental")
aver.ex$char<-char
total<-rbind(aver.cp, aver.ex)#대조군 데이터와 실험군 데이터가 한 테이블에 묶임

ggplot(data=total, aes(x=age)) + #가로축을 나이대로
  geom_bar(width = 1.5, stat="identity", aes(x=age+2, y=aver_af.st, fill="after")) + #바의 넓이는 1.5, 이후 데이터는 이전 데이터보다 오른쪽에 위치하도록 2만큼 움직임
  geom_bar(width=1.5, stat="identity", aes(y=aver_bf.st, fill="before"))+ #이전 데이터 막대그래프로 그림
  scale_fill_manual(name="EQ5D Value", values=c("after"="light blue", "before"="#FF6969")) + #범례 추가
  ylab("strides") + #y축 이름 정함
  theme_bw() + #뒷배경을 하얗게
  facet_grid(.~char)#대조군, 실험군을 따로 구분해서 그림