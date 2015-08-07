experimental<-filter(dat, char=="experimental")
experimental.50<-filter(experimental, bf.strides<100) #실험군100이하
experimental.100<-filter(experimental, bf.strides>=100, bf.strides<200) #실험군 100대
experimental.200<-filter(experimental, bf.strides>=200, bf.strides<300) #실험군 200대
experimental.300<-filter(experimental, bf.strides>=300, bf.strides<400) #실험군 300대
experimental.over<-filter(experimental, bf.strides>=400) #실험군 400대

comparison<-filter(dat, char=="comparison")
comparison.50<-filter(comparison, bf.strides<100) #실험군 100이하
comparison.100<-filter(comparison, bf.strides>=100, bf.strides<200) #실험군 100대
comparison.200<-filter(comparison, bf.strides>=200, bf.strides<300) #실험군 200대
comparison.300<-filter(comparison, bf.strides>=300, bf.strides<400) #실험군 300대
comparison.over<-filter(comparison, bf.strides>=400) #실험군 400대

#평균값 구하기
aver.ex.50<-summarise(experimental.50, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.100<-summarise(experimental.100, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.200<-summarise(experimental.200, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.300<-summarise(experimental.300, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.over<-summarise(experimental.over, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))

aver.cp.50<-summarise(comparison.50, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.100<-summarise(comparison.100, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.200<-summarise(comparison.200, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.300<-summarise(comparison.300, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.over<-summarise(comparison.over, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))

#표 합치기
experimental.st<-rbind(aver.ex.50,aver.ex.100,aver.ex.200,aver.ex.300, aver.ex.over)
comparison.st<-rbind(aver.cp.50, aver.cp.100,aver.cp.200,aver.cp.300, aver.cp.over)

#그리고 보니 전 후 보행수는 필요가 없어서 지운다
experimental.st=experimental.st[c(T,T,T,T,F,T,T,T,T,F)]
comparison.st=comparison.st[c(T,T,T,T,F,T,T,T,T,F)]

#이전 보행수 단위 지정
experimental.st$stride_range<-c(50, 100, 200, 300, 400)
comparison.st$stride_range<-c(50, 100, 200, 300, 400)

#실험군, 대조군 분리
experimental.st$char<-c("experimental","experimental","experimental","experimental","experimental")
comparison.st$char<-c("comparison","comparison","comparison","comparison","comparison")

#실험군, 대조군 표 합치기
total.st<-rbind(experimental.st, comparison.st)

#그래프 그려준다.
ggplot(data=total.st, aes(x=stride_range)) + #가로축을 나이대로
  geom_bar(width = 20, stat="identity", aes(x=stride_range+25, y=aver_af.eq, fill="after")) + #바의 넓이는 1.5, 이후 데이터는 이전 데이터보다 오른쪽에 위치하도록 2만큼 움직임
  geom_bar(width=20, stat="identity", aes(y=aver_bf.eq, fill="before"))+ #이전 데이터 막대그래프로 그림
  scale_fill_manual(name="EQ5D Value", values=c("after"="light blue", "before"="#FF6969")) + #범례 추가
  ylab("EQ5D Value") + #y축 이름 정함
  theme_bw() + #뒷배경을 하얗게
  facet_grid(.~char) #대조군, 실험군을 따로 구분해서 그림
