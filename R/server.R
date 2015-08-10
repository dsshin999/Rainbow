library(shiny)
library(dplyr)
library(ggplot2)
library(datasets)
library(RCurl)

shinyServer(function(input, output){
  output$plot <- renderPlot({

    
    if(input$x == "age")
    {
      p<- ggplot(total.age, aes_string(input$x, input$y)) + geom_bar(width=3, stat="identity", position="dodge")+theme_bw()
      
      if(input$color != "None")
      {p<-p+aes_string(fill=input$color)}
      
      print(p)
    }
    else{
      p<- ggplot(total.ill, aes_string(x=input$x, y=input$y)) + geom_bar(width=.3, stat="identity", position="dodge") + theme_bw()
      if(input$color != "None")
      {p<-p+aes_string(fill=input$color) }
      
      print(p)
    }})
  
  #기간별 평균 걸음수 비교
  output$plot2 <- renderPlot({
    month<-c(input$month1, input$month2)
    print(experimental)
    stride<-c(monthly[,input$month1], monthly[,input$month2])
    mont<-data.frame(month,stride)
    h<-ggplot(mont, aes(x=month, y=stride)) + geom_bar(stat="identity", postiion="dodge", fill="light steel blue", width=0.1)+theme_bw()
    print(h)
  })
  
  output$plot3 <- renderPlot({
    if(input$names != "None")
    {
      people<-filter(experimental,name==input$names)
      persons<-summarise(people, month1=mean(m1),month2=mean(m2),month3=mean(m3),month4=mean(m4),month5=mean(m5),month6=mean(m6),month7=mean(m7),month8=mean(m8),month9=mean(m9),month10=mean(m10),month11=mean(m11),month12=mean(m12))
      month<-c(input$month3, input$month4)
      stride<-c(persons[,input$month3], persons[,input$month4])
      mont<-data.frame(month,stride)
      k<-ggplot(mont, aes(x=month, y=stride)) + geom_bar(stat="identity", positiion="dodge", fill="light steel blue", width=0.1)+theme_bw()
      print(k)
    }
  })})