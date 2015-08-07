#'@return
#'@export
analyze<-function(){library(shiny)
  library(dplyr)
  library(ggplot2)
  library(datasets)
  
  dat <- read.csv(file.choose(), header=T,stringsAsFactors = F)
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
  
  server<-function(input, output){
    output$plot <- renderPlot({
      #op<-par(no.readonly=TRUE)
      
      #par(mfrow=c(2,2))
      
      if(input$x == "age")
      {
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
        
        p<- ggplot(total, aes_string(input$x, input$y)) + geom_bar(width=3, stat="identity", position="dodge")+theme_bw()
        
        if(input$color != "None")
        {p<-p+aes_string(fill=input$color)}
        
        print(p)
      }
      else{
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
        total<-rbind(aver.cp, aver.ex)
        
        diff.st<-total$aver_af.st-total$aver_bf.st
        diff.eq5d<-total$aver_af.eq-total$aver_bf.eq
        diff.mr<-total$aver_af.mr-total$aver_bf.mr
        total$diff.st<-diff.st
        total$diff.eq<-diff.eq5d
        total$diff.mr<-diff.mr
        
        p<- ggplot(total, aes_string(x=input$x, y=input$y)) + geom_bar(width=.3, stat="identity", position="dodge") + theme_bw()
        if(input$color != "None")
        {p<-p+aes_string(fill=input$color) }
        
        print(p)
      }})
    
    #기간별 평균 걸음수 비교
    output$plot2 <- renderPlot({
      experimental<-filter(dat, char=="experimental")
      monthly<-summarise(experimental, month1=mean(m1),month2=mean(m2),month3=mean(m3),month4=mean(m4),month5=mean(m5),month6=mean(m6),month7=mean(m7),month8=mean(m8),month9=mean(m9),month10=mean(m10),month11=mean(m11),month12=mean(m12))
      month<-c(input$month1, input$month2)
      stride<-c(monthly[,input$month1], monthly[,input$month2])
      mont<-data.frame(month,stride)
      h<-ggplot(mont, aes(x=month, y=stride)) + geom_bar(stat="identity", postiion="dodge", fill="light steel blue", width=0.1)+theme_bw()
      print(h)
    })
    
    output$plot3 <- renderPlot({
      if(input$names!="None")
      {
      experimental<-filter(dat, char=="experimental")
      people<-filter(experimental,name==input$names)
      persons<-summarise(people, month1=mean(m1),month2=mean(m2),month3=mean(m3),month4=mean(m4),month5=mean(m5),month6=mean(m6),month7=mean(m7),month8=mean(m8),month9=mean(m9),month10=mean(m10),month11=mean(m11),month12=mean(m12))
      month<-c(input$month3, input$month4)
      stride<-c(persons[,input$month3], persons[,input$month4])
      mont<-data.frame(month,stride)
      k<-ggplot(mont, aes(x=month, y=stride)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()
      print(k)
      }
    })
    
    
    
  }
  
  ui<-navbarPage("Tools for Analyzing Patients Status",
                 tabPanel("All",
                          pageWithSidebar
                          (
                          headerPanel("Differences between experimental and comparison group"),
                          
                          sidebarPanel(
                            selectInput("x", "Please Select x-axix", choices=c("age", "illness")),
                            selectInput("y", "indexes", choices=colnames(total)),
                            selectInput("color", "Seperation", c("None", "char"))
                          ),
                          
                          mainPanel(
                            plotOutput('plot'))
                          )),
                 tabPanel("Strides along to month",
                          pageWithSidebar
                          (
                          headerPanel("Comparing Strides along to month"),
                          
                          sidebarPanel(
                            selectInput("month1", "select month", choices=c("month1",  "month2",  "month3",  "month4",  "month5",  "month6",  "month7",  "month8",  "month9",  "month10", "month11", "month12")),
                            selectInput("month2", "select month2", choices=c("month1",  "month2",  "month3",  "month4",  "month5",  "month6",  "month7",  "month8",  "month9",  "month10", "month11", "month12"))
                          ),
                          
                          mainPanel(
                            plotOutput('plot2'))
                          )),
                 tabPanel("Individual Strides",
                          pageWithSidebar
                          (
                          headerPanel("Comparing Individual strides along to month"),
                          
                          sidebarPanel(
                            selectInput("month3", "select month", choices=c("month1",  "month2",  "month3",  "month4",  "month5",  "month6",  "month7",  "month8",  "month9",  "month10", "month11", "month12")),
                            selectInput("month4", "select month", choices=c("month1",  "month2",  "month3",  "month4",  "month5",  "month6",  "month7",  "month8",  "month9",  "month10", "month11", "month12")),
                            selectInput("names", "Name : ", choices=c("None", dat$name))
                          ),
                          
                          
                          mainPanel(
                            plotOutput('plot3'))
                          ))
  )
  shinyApp(ui=ui, server=server)
}
