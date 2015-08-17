library(shiny)
library(dplyr)
library(ggplot2)
library(datasets)
library(RCurl)
library(RMySQL)
library(gridExtra)

shinyServer(function(input, output){
  output$plot <- renderPlot({
    
    x<-input$x
    y<-input$y
    f<-input$from
    t<-input$to
    #sqlStatement <- paste("select spec, ",x,", ",y," FROM chart WHERE measuretime>=",f," AND measuretime<=",t,"")
    sqlStatement <- paste("select * from chart where measuretime>=",f," and measuretime<=",t,"")
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
      
      aver.ex.50<-summarise(experimental.50, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
      aver.ex.60<-summarise(experimental.60, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
      aver.ex.70<-summarise(experimental.70, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
      aver.ex.over<-summarise(experimental.over, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
      
      
      aver.cp.50<-summarise(comparison.50, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
      aver.cp.60<-summarise(comparison.60, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
      aver.cp.70<-summarise(comparison.70, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
      aver.cp.over<-summarise(comparison.over, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
      
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
      total.age1<-total.age
      #names(total.age)[1]<-input$y
      total.age$eq5d<-total.age$eq5d*500
      total.age$mr<-total.age$mr*10
      total.age$vt<-total.age$vt*10
      total.age$pr<-total.age$pr*10
      total.age$gh<-total.age$gh*10
      
      g.bottom <- ggplot(total.age, aes_string(x = input$x, y =input$y)) +
        geom_bar(stat="identity", position="dodge", width=3) +  #plot flow
        geom_line(aes_string(y = input$z))+geom_point(aes_string(y=input$z)) +  # plot rain2
        ## specify our yaxis limits and remove any axis expansion
        labs(x = "age", y = input$y) +
        theme_classic() +
        theme(plot.background = element_rect(fill = "transparent"),
              plot.margin = unit(c(2,0,1,1),units="lines"))
      
      g.y <- ggplot(total.age1, aes_string(x = input$x, y = input$z)) +
        theme_classic() + 
        geom_line(colour = "transparent") +
        labs(y = input$z) +
        ## Adjust the placement of the y axis title
        theme(axis.title.y = element_text(vjust = -5, hjust =0.4),  
              ## Adjust the justification of the y axis labels
              axis.text.y = element_text(vjust=-1),  
              ## Reverse the ticks to go on the other side
              axis.ticks.length = unit(-0.4,"cm"),
              ## Reverse spacing between ticks and text to move text to the right
              axis.ticks.margin = unit(-0.5, "cm"), 
              axis.title.x = element_blank(), ## Remove all x-axis attributes
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line.x = element_blank(),
              plot.background = element_rect(fill = "transparent"),
              plot.margin = unit(c(2,0,3.85,-1.5),units="lines"))
      ## Adjust the plot margins (top, left, bottom, right), to make the
      ## y-axis line up with the graph. Keep the top margin the same as
      ## with the graph to make the tops line up. Use negatives for the
      ## right margin to make the axis scoot up next to the graph. Use
      ## positives for the bottom margin to push the bottom up to match
      ## the x axis on the graph.
      
      ## Create viewports where 90% is for the graph and 10% for the axis
      vp1 <- viewport(width = 0.9, height = 1, x = 0, y = 0.5, just = c(0,0.5))
      vp2 <- viewport(width = 0.1, height = 1, x = 0.9, y = 0.5,just = c(0,0.5))
      
      ## Print the two graphs to the viewports.
      ## This means that the relative positions should change even if you
      ## strecth or compress the graphs.
      
      #gtop<- ggplot(total.age) + geom_bar(width=3, stat="identity", position="dodge", aes_string(input$x, input$y))+theme_bw()
      #gbot<- ggplot(total.age) + geom_line(aes_string(input$x, input$z))+theme_bw()+geom_point()
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
        
        aver.ex.a<-summarise(experimental.a, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
        aver.ex.b<-summarise(experimental.b, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
        aver.ex.c<-summarise(experimental.c, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
        
        aver.cp.a<-summarise(comparison.a, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
        aver.cp.b<-summarise(comparison.b, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
        aver.cp.c<-summarise(comparison.c, eq5d=mean(eq5d), stride=mean(stride), mr=mean(mr), vt=mean(vt), pr=mean(pr), gh=mean(gh))
        
        
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
        total.ill1<-total.ill
        total.ill$eq5d<-total.ill$eq5d*500
        total.ill$mr<-total.ill$mr*10
        total.ill$vt<-total.ill$vt*10
        total.ill$pr<-total.ill$pr*10
        total.ill$gh<-total.ill$gh*10
       
        g.bottom <- ggplot(total.ill, aes_string(x = input$x, y =input$y)) +
          geom_bar(stat="identity", position="dodge", width=.1) +  #plot flow
          geom_line(aes_string(y = input$z))+aes(group=char)+geom_point(aes_string(y=input$z)) +  # plot rain2
          ## specify our yaxis limits and remove any axis expansion
          labs(x = "Illness", y = input$y) +
          theme_classic() +
          theme(plot.background = element_rect(fill = "transparent"),
                plot.margin = unit(c(2,0,1,1),units="lines"))

        g.y <- ggplot(total.ill1, aes_string(x = input$x, y = input$z)) +
          theme_classic() + 
          geom_line(colour = "transparent") +
          labs(y = input$z) +
          ## Adjust the placement of the y axis title
          theme(axis.title.y = element_text(vjust = -5, hjust =0.4),  
                ## Adjust the justification of the y axis labels
                axis.text.y = element_text(vjust=-1),  
                ## Reverse the ticks to go on the other side
                axis.ticks.length = unit(-0.4,"cm"),
                ## Reverse spacing between ticks and text to move text to the right
                axis.ticks.margin = unit(-0.5, "cm"), 
                axis.title.x = element_blank(), ## Remove all x-axis attributes
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line.x = element_blank(),
                plot.background = element_rect(fill = "transparent"),
                plot.margin = unit(c(2,0,3.85,-1.5),units="lines"))
        ## Adjust the plot margins (top, left, bottom, right), to make the
        ## y-axis line up with the graph. Keep the top margin the same as
        ## with the graph to make the tops line up. Use negatives for the
        ## right margin to make the axis scoot up next to the graph. Use
        ## positives for the bottom margin to push the bottom up to match
        ## the x axis on the graph.
        
        ## Create viewports where 90% is for the graph and 10% for the axis
        vp1 <- viewport(width = 0.9, height = 1, x = 0, y = 0.5, just = c(0,0.5))
        vp2 <- viewport(width = 0.1, height = 1, x = 0.9, y = 0.5,just = c(0,0.5))
        
        
        #gtop<- ggplot(total.ill) + geom_bar(width=.1, stat="identity", position="dodge", aes_string(input$x, input$y))+theme_bw()
        #gbot<- ggplot(total.ill, aes_string(input$x, input$z)) + geom_line(aes(group=char)) +scale_x_discrete(breaks=1:3, labels=c("A", "B", "C"))+geom_point()+theme_bw()
        }
        
      if(input$color != "None")
      {
        g.bottom<-g.bottom+aes(fill=char, color=char) + theme(legend.position="top")
        print(g.bottom, vp=vp1)
        print(g.y, vp=vp2)
      }
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
      sqlStatement <- paste("select patient_id FROM patient WHERE name=\'",nam,"\'", sep="")
      id<-dbGetQuery(con, sqlStatement)
      sqlStatement <- paste("select measuretime, ",val2," FROM chart WHERE patient_id=",id,"")
      dat<-dbGetQuery(con, sqlStatement)

      if(val2=="eq5d")
      {k<-ggplot(dat, aes(x=measuretime, y=eq5d)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()+xlab("time")}
      else if(val2=="stride")
      {k<-ggplot(dat, aes(x=measuretime, y=stride)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()+xlab("time")}
      else if(val2=="pr")
      {k<-ggplot(dat, aes(x=measuretime, y=pr)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()+xlab("time")}
      else if(val2=="mr")
      {k<-ggplot(dat, aes(x=measuretime, y=mr)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()+xlab("time")}
      else if(val2=="gh")
      {k<-ggplot(dat, aes(x=measuretime, y=gh)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()+xlab("time")}
      else if(val2=="vt")
      {k<-ggplot(dat, aes(x=measuretime, y=vt)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()+xlab("time")}
      
      print(k)
    }
  })})