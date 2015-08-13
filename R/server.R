library(shiny)
library(dplyr)
library(ggplot2)
library(datasets)
library(RCurl)
library(RMySQL)

shinyServer(function(input, output){
  output$plot <- renderPlot({
    
    x<-input$x
    y<-input$y
    f<-input$from
    t<-input$to
    sqlStatement <- paste("select spec, ",x,", ",y," FROM chart WHERE measuretime>=",f," AND measuretime<=",t,"")
    dat<-dbGetQuery(con, sqlStatement)
    
      if(input$x=="age")
      {
      experimental<-filter(dat, spec=="experimental")
      experimental.50<-filter(experimental, age<60)
      experimental.60<-filter(experimental, age>=60, age<70)
      experimental.70<-filter(experimental, age>=70, age<80)
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
      else if(input$y=="stride")
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
      else if(input$y=="pr")
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
      else if(input$y=="mr")
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
      else if(input$y=="vt")
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
      else if(input$y=="gh")
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
      
      p<- ggplot(total.age, aes_string(input$x, input$y)) + geom_bar(width=3, stat="identity", position="dodge")+theme_bw()
      print(p)
      }
      
      else{

        experimental<-filter(dat, spec=="experimental")
        experimental.a<-filter(experimental, illness=="A") #질병1
        experimental.b<-filter(experimental, illness=="B") #질병2
        experimental.c<-filter(experimental, illness=="C") #질병3
        comparison<-filter(dat, spec=="comparison")
        comparison.a<-filter(comparison, illness=="A") #질병1
        comparison.b<-filter(comparison, illness=="B") #질병2
        comparison.c<-filter(comparison, illness=="C") #질병3
        
        if(input$y=="eq5d")
        {
        aver.ex.a<-summarise(experimental.a, aver_bf.eq=mean(eq5d))
        aver.ex.b<-summarise(experimental.b, aver_bf.eq=mean(eq5d))
        aver.ex.c<-summarise(experimental.c, aver_bf.eq=mean(eq5d))
        
        aver.cp.a<-summarise(comparison.a, aver_bf.eq=mean(eq5d))
        aver.cp.b<-summarise(comparison.b, aver_bf.eq=mean(eq5d))
        aver.cp.c<-summarise(comparison.c, aver_bf.eq=mean(eq5d))
        }
        else if(input$y=="stride")
        {aver.ex.a<-summarise(experimental.a, aver_bf.eq=mean(stride))
        aver.ex.b<-summarise(experimental.b, aver_bf.eq=mean(stride))
        aver.ex.c<-summarise(experimental.c, aver_bf.eq=mean(stride))
        aver.cp.a<-summarise(comparison.a, aver_bf.eq=mean(stride))
        aver.cp.b<-summarise(comparison.b, aver_bf.eq=mean(stride))
        aver.cp.c<-summarise(comparison.c, aver_bf.eq=mean(stride))
        }
        else if(input$y=="pr")
        {aver.ex.a<-summarise(experimental.a, aver_bf.eq=mean(pr))
        aver.ex.b<-summarise(experimental.b, aver_bf.eq=mean(pr))
        aver.ex.c<-summarise(experimental.c, aver_bf.eq=mean(pr))
        
        aver.cp.a<-summarise(comparison.a, aver_bf.eq=mean(pr))
        aver.cp.b<-summarise(comparison.b, aver_bf.eq=mean(pr))
        aver.cp.c<-summarise(comparison.c, aver_bf.eq=mean(pr))}
        else if(input$y=="mr")
        {aver.ex.a<-summarise(experimental.a, aver_bf.eq=mean(mr))
        aver.ex.b<-summarise(experimental.b, aver_bf.eq=mean(mr))
        aver.ex.c<-summarise(experimental.c, aver_bf.eq=mean(mr))
        
        aver.cp.a<-summarise(comparison.a, aver_bf.eq=mean(mr))
        aver.cp.b<-summarise(comparison.b, aver_bf.eq=mean(mr))
        aver.cp.c<-summarise(comparison.c, aver_bf.eq=mean(mr))
        }
        else if(input$y=="vt")
        {aver.ex.a<-summarise(experimental.a, aver_bf.eq=mean(vt))
        aver.ex.b<-summarise(experimental.b, aver_bf.eq=mean(vt))
        aver.ex.c<-summarise(experimental.c, aver_bf.eq=mean(vt))
        
        aver.cp.a<-summarise(comparison.a, aver_bf.eq=mean(vt))
        aver.cp.b<-summarise(comparison.b, aver_bf.eq=mean(vt))
        aver.cp.c<-summarise(comparison.c, aver_bf.eq=mean(vt))}
        else if(input$y=="gh")
        {aver.ex.a<-summarise(experimental.a, aver_bf.eq=mean(gh))
        aver.ex.b<-summarise(experimental.b, aver_bf.eq=mean(gh))
        aver.ex.c<-summarise(experimental.c, aver_bf.eq=mean(gh))
        
        aver.cp.a<-summarise(comparison.a, aver_bf.eq=mean(gh))
        aver.cp.b<-summarise(comparison.b, aver_bf.eq=mean(gh))
        aver.cp.c<-summarise(comparison.c, aver_bf.eq=mean(gh))}
        
        
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
        p<- ggplot(total.ill, aes_string(input$x, input$y)) + geom_bar(width=.3, stat="identity", position="dodge")+theme_bw()
        print(p)
        }
        
      
      
      if(input$color != "None")
      {
        p<-p+aes_string(fill=input$color)
        print(p)}
  })
  
  #기간별 평균 걸음수 비교
  output$plot2 <- renderPlot({
    m1<-input$month1
    m2<-input$month2
    val<-input$value
    
    sqlStatement <- paste("select measuretime,",val," FROM chart WHERE measuretime IN(",m1,", ",m2,") AND spec='experimental'")
    dat<-dbGetQuery(con, sqlStatement)
    type1<-filter(dat, measuretime==input$month1)
    type2<-filter(dat, measuretime==input$month2)
    
    if(val=="stride")
    {sum1<-summarise(type1, stride=mean(stride))
    sum2<-summarise(type2, stride=mean(stride))}
    else if(val=="eq5d")
    {sum1<-summarise(type1, eq5d=mean(eq5d))
    sum2<-summarise(type2, eq5d=mean(eq5d))}
    else if(val=="pr")
    {sum1<-summarise(type1, pr=mean(pr))
    sum2<-summarise(type2, pr=mean(pr))}
    else if(val=="mr")
    {sum1<-summarise(type1, mr=mean(mr))
    sum2<-summarise(type2, mr=mean(mr))}
    else if(val=="vt")
    {sum1<-summarise(type1, vt=mean(vt))
    sum2<-summarise(type2, vt=mean(vt))}
    else if(val=="gh")
    {sum1<-summarise(type1, gh=mean(gh))
    sum2<-summarise(type2, gh=mean(gh))}
    
    sum<-rbind(sum1, sum2)
    time<-c(m1,m2)
    monthly<-data.frame(sum, time)
    
    if(val=="eq5d")
    {h<-ggplot(monthly, aes(x=time, y=eq5d)) + geom_bar(stat="identity", position="dodge", fill="light steel blue", width=0.1)+theme_bw()}
    else if(val=="stride")
    {h<-ggplot(monthly, aes(x=time, y=stride)) + geom_bar(stat="identity", position="dodge", fill="light steel blue", width=0.1)+theme_bw()}
    else if(val=="pr")
    {h<-ggplot(monthly, aes(x=time, y=pr)) + geom_bar(stat="identity", position="dodge", fill="light steel blue", width=0.1)+theme_bw()}
    else if(val=="mr")
    {h<-ggplot(monthly, aes(x=time, y=mr)) + geom_bar(stat="identity", position="dodge", fill="light steel blue", width=0.1)+theme_bw()}
    else if(val=="vt")
    {h<-ggplot(monthly, aes(x=time, y=vt)) + geom_bar(stat="identity", position="dodge", fill="light steel blue", width=0.1)+theme_bw()}
    else if(val=='gh')
    {h<-ggplot(monthly, aes(x=time, y=gh)) + geom_bar(stat="identity", position="dodge", fill="light steel blue", width=0.1)+theme_bw()}
    
    print(h)
  })
  
  output$plot3 <- renderPlot({
    if(input$names != "None")
    {
      val2<-input$value2
      m3<-input$month3
      m4<-input$month4
      nam<-input$names
      sqlStatement <- paste("select measuretime, ",val2," FROM chart WHERE measuretime IN(",m3,", ",m4,") and name=\'",nam,"\'", sep="")
      dat<-dbGetQuery(con, sqlStatement)
      if(val2=="eq5d")
      {k<-ggplot(dat, aes(x=measuretime, y=eq5d)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()+xlab("times")}
      else if(val2=="stride")
      {k<-ggplot(dat, aes(x=measuretime, y=stride)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()+xlab("times")}
      else if(val2=="pr")
      {k<-ggplot(dat, aes(x=measuretime, y=pr)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()+xlab("times")}
      else if(val2=="mr")
      {k<-ggplot(dat, aes(x=measuretime, y=mr)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()+xlab("times")}
      else if(val2=="gh")
      {k<-ggplot(dat, aes(x=measuretime, y=gh)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()+xlab("times")}
      else if(val2=="vt")
      {k<-ggplot(dat, aes(x=measuretime, y=vt)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()+xlab("times")}
      
      print(k)
    }
  })})