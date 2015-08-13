library(shiny)
library(dplyr)
library(ggplot2)
library(datasets)
library(RCurl)
library(RMySQL)

shinyServer(function(input, output){
  output$plot <- renderPlot({
    
    
    if(input$x == "age")
    {#입력받은 사항에 따라 db에서 데이터를 읽어와서 그래프를 뿌린다.
      
      if(input$y == "stride")
      {dat<-dbGetQuery(con, "SELECT spec, age, stride FROM chart")}
      else if(input$y == "eq5d")
      {dat<-dbGetQuery(con, "SELECT spec, age, eq5d FROM chart")}
      else if(input$y == "pr")
      {dat<-dbGetQuery(con, "SELECT spec, age, pr FROM chart")}
      else if(input$y == "mr")
      {dat<-dbGetQuery(con, "SELECT spec, age, mr FROM chart")}
      else if(input$y == "gh")
      {dat<-dbGetQuery(con, "SELECT spec, age, gh FROM chart")}
      else if(input$y == "vt")
      {dat<-dbGetQuery(con, "SELECT spec, age, vt FROM chart")}
      
      
      experimental<-filter(dat, spec=="experimental")
      experimental.50<-filter(experimental, age<60)
      print(experimental.50)
      experimental.60<-filter(experimental, age>=60, age<70)
      print(experimental.60)
      experimental.70<-filter(experimental, age>=70, age<80)
      print(experimental.70)
      experimental.over<-filter(experimental, age>=80)
      
      
      comparison<-filter(dat, spec=="comparison")
      comparison.50<-filter(comparison, age<60)
      comparison.60<-filter(comparison, age>=60, age<70)
      comparison.70<-filter(comparison, age>=70, age<80)
      comparison.over<-filter(comparison, age>=80)
      
      
      if(input$y=="eq5d")
      {
        aver.ex.50<-summarise(experimental.50, aver_bf.eq=mean(eq5d))
        aver.ex.60<-summarise(experimental.60, aver_bf.eq=mean(eq5d))
        aver.ex.70<-summarise(experimental.70, aver_bf.eq=mean(eq5d))
        aver.ex.over<-summarise(experimental.over, aver_bf.eq=mean(eq5d))
        
        aver.cp.50<-summarise(comparison.50, aver_bf.eq=mean(eq5d))
        aver.cp.60<-summarise(comparison.60, aver_bf.eq=mean(eq5d))
        aver.cp.70<-summarise(comparison.70, aver_bf.eq=mean(eq5d))
        aver.cp.over<-summarise(comparison.over, aver_bf.eq=mean(eq5d))
      }
      else if(input$y==stride)
      {
        aver.ex.50<-summarise(experimental.50, aver_bf.eq=mean(stride))
        aver.ex.60<-summarise(experimental.60, aver_bf.eq=mean(stride))
        aver.ex.70<-summarise(experimental.70, aver_bf.eq=mean(stride))
        aver.ex.over<-summarise(experimental.over, aver_bf.eq=mean(stride))
        
        aver.cp.50<-summarise(comparison.50, aver_bf.eq=mean(stride))
        aver.cp.60<-summarise(comparison.60, aver_bf.eq=mean(stride))
        aver.cp.70<-summarise(comparison.70, aver_bf.eq=mean(stride))
        aver.cp.over<-summarise(comparison.over, aver_bf.eq=mean(stride))
      }
      else if(input$y==pr)
      {
        aver.ex.50<-summarise(experimental.50, aver_bf.eq=mean(pr))
        aver.ex.60<-summarise(experimental.60, aver_bf.eq=mean(pr))
        aver.ex.70<-summarise(experimental.70, aver_bf.eq=mean(pr))
        aver.ex.over<-summarise(experimental.over, aver_bf.eq=mean(pr))
        
        aver.cp.50<-summarise(comparison.50, aver_bf.eq=mean(pr))
        aver.cp.60<-summarise(comparison.60, aver_bf.eq=mean(pr))
        aver.cp.70<-summarise(comparison.70, aver_bf.eq=mean(pr))
        aver.cp.over<-summarise(comparison.over, aver_bf.eq=mean(pr))
      }
      else if(input$y==mr)
      {
        aver.ex.50<-summarise(experimental.50, aver_bf.eq=mean(mr))
        aver.ex.60<-summarise(experimental.60, aver_bf.eq=mean(mr))
        aver.ex.70<-summarise(experimental.70, aver_bf.eq=mean(mr))
        aver.ex.over<-summarise(experimental.over, aver_bf.eq=mean(mr))
        
        aver.cp.50<-summarise(comparison.50, aver_bf.eq=mean(mr))
        aver.cp.60<-summarise(comparison.60, aver_bf.eq=mean(mr))
        aver.cp.70<-summarise(comparison.70, aver_bf.eq=mean(mr))
        aver.cp.over<-summarise(comparison.over, aver_bf.eq=mean(mr))
      }
      else if(input$y==vt)
      {
        aver.ex.50<-summarise(experimental.50, aver_bf.eq=mean(vt))
        aver.ex.60<-summarise(experimental.60, aver_bf.eq=mean(vt))
        aver.ex.70<-summarise(experimental.70, aver_bf.eq=mean(vt))
        aver.ex.over<-summarise(experimental.over, aver_bf.eq=mean(vt))
        
        aver.cp.50<-summarise(comparison.50, aver_bf.eq=mean(vt))
        aver.cp.60<-summarise(comparison.60, aver_bf.eq=mean(vt))
        aver.cp.70<-summarise(comparison.70, aver_bf.eq=mean(vt))
        aver.cp.over<-summarise(comparison.over, aver_bf.eq=mean(vt))
      }
      else if(input$y==gh)
      {
        aver.ex.50<-summarise(experimental.50, aver_bf.eq=mean(gh))
        aver.ex.60<-summarise(experimental.60, aver_bf.eq=mean(gh))
        aver.ex.70<-summarise(experimental.70, aver_bf.eq=mean(gh))
        aver.ex.over<-summarise(experimental.over, aver_bf.eq=mean(gh))
        
        aver.cp.50<-summarise(comparison.50, aver_bf.eq=mean(gh))
        aver.cp.60<-summarise(comparison.60, aver_bf.eq=mean(gh))
        aver.cp.70<-summarise(comparison.70, aver_bf.eq=mean(gh))
        aver.cp.over<-summarise(comparison.over, aver_bf.eq=mean(gh))
      }
      
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
      names(total.age)[1]<-input$y
      print("total.age")
      print(total.age)
      
      p<- ggplot(total.age, aes_string(input$x, input$y)) + geom_bar(width=3, stat="identity", position="dodge")+theme_bw()
      
      if(input$color != "None")
      {
        
        p<-p+aes_string(fill=input$color)}
      
      print(p)
    }
    else{
      if(input$y==stride)
      {dat<-dbGetQuery(con, "SELECT spec, illness, stride FROM chart")}
      else if(input$y==eq5d)
      {dat<-dbGetQuery(con, "SELECT spec, illness, eq5d FROM chart")}
      else if(input$y==pr)
      {dat<-dbGetQuery(con, "SELECT spec, illness, pr FROM chart")}
      else if(input$y==mr)
      {dat<-dbGetQuery(con, "SELECT spec, illness, mr FROM chart")}
      else if(input$y==vt)
      {dat<-dbGetQuery(con, "SELECT spec, illness, vt FROM chart")}
      else if(input$y==gh)
      {dat<-dbGetQuery(con, "SELECT spec, illness, gh FROM chart")}
      
      experimental<-filter(dat, char=="experimental")
      experimental.a<-filter(experimental, illness=="A") #질병1
      experimental.b<-filter(experimental, illness=="B") #질병2
      experimental.c<-filter(experimental, illness=="C") #질병3
      comparison<-filter(dat, char=="comparison")
      comparison.a<-filter(comparison, illness=="A") #질병1
      comparison.b<-filter(comparison, illness=="B") #질병2
      comparison.c<-filter(comparison, illness=="C") #질병3
      
      aver.ex.a<-summarise(experimental.a, aver_bf.eq=mean(input$y))
      aver.ex.b<-summarise(experimental.b, aver_bf.eq=mean(input$y))
      aver.ex.c<-summarise(experimental.c, aver_bf.eq=mean(input$y))
      
      aver.cp.a<-summarise(comparison.a, aver_bf.eq=mean(input$y))
      aver.cp.b<-summarise(comparison.b, aver_bf.eq=mean(input$y))
      aver.cp.c<-summarise(comparison.c, aver_bf.eq=mean(input$y))
      
      
      aver.cp<-rbind(aver.cp.a,aver.cp.b,aver.cp.c)
      aver.cp$illness<-c("A","B","C")
      aver.ex<-rbind(aver.ex.a,aver.ex.b,aver.ex.c)
      aver.ex$illness<-c("A","B","C")
      
      char<-c("comparison", "comparison", "comparison")
      aver.cp$char<-char
      char<-c("experimental", "experimental","experimental")
      aver.ex$char<-char
      total.ill<-rbind(aver.cp, aver.ex)
      names(total.ill)[1]<-input$y
      p<- ggplot(total.ill, aes_string(x=input$x, y=input$y)) + geom_bar(width=.3, stat="identity", position="dodge") + theme_bw()
      if(input$color != "None")
      {p<-p+aes_string(fill=input$color) }
      
      print(p)
    }})
  
  #기간별 평균 걸음수 비교
  output$plot2 <- renderPlot({
    dat<-dbGetQuery(con, "SELECT stride, measure.time FROM chart WHERE measure.time=input$month1 OR measure.time=input$month2 AND spec='experimental'")
    type1<-filter(dat1, measure.time==input$month1)
    type2<-filter(dat1, measure.time==input$month2)
    sum1<-summarise(type1, strides=mean(stride))
    sum2<-summarise(type2, strides=mean(stride))
    sum<-rbind(sum1, sum2)
    time<-c(1,2)
    monthly<-data.frame(sum, time)
    
    h<-ggplot(monthly, aes(x=time, y=strides)) + geom_bar(stat="identity", postiion="dodge", fill="light steel blue", width=0.1)+theme_bw()
    print(h)
  })
  
  output$plot3 <- renderPlot({
    if(input$names != "None")
    {
      dat<-dbGetQuery(con, "SELECT input$value, measure.time FROM chart WHERE measure.time=input$month3 OR measure.time=input$month4 AND name=input$names")
      
      k<-ggplot(dat, aes(x=measure.time, y=input$value)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()+xlab("times")
      print(k)
    }
  })})