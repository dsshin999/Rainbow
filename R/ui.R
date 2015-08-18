library(shiny)
library(ggplot2)
library(dplyr)
library(datasets)
library(RCurl)
shinyUI(

navbarPage("Tools for Analyzing Patients Status",
           tabPanel("All",
                    pageWithSidebar
                    (
                    headerPanel("Differences between experimental and comparison group"),
                    
                    sidebarPanel(
                      selectInput("x", "Please Select x-axis", choices=c("age", "illness")),
                      selectInput("y", "bar indexes", index, selected =index[2]),
                      selectInput("z", "line indexes", index),
                      selectInput("from", "Period, from :", c(1:maximum)), #추릴 기간 선정
                      selectInput("to", "Period, to :", c(1:maximum), selected = 2)
                    ),
                    
                    mainPanel(
                      plotOutput('plot'))
                    )),
           tabPanel("Indexes along to month",
                    pageWithSidebar
                    (
                    headerPanel("Comparing values along to month"),
                    
                    sidebarPanel(
                      selectInput("month1", "select month", choices=c(1:maximum)),
                      selectInput("month2", "select month2", choices=c(1:maximum), selected =2),
                      selectInput("value", "Index :", index)
                    ),
                    
                    mainPanel(
                      plotOutput('plot2'))
                    )),
           tabPanel("Individual Index",
                    pageWithSidebar
                    (
                    headerPanel("Comparing Individual index along to month"),
                    
                    sidebarPanel(
                      selectInput("month3", "Period, from :", c(1:maximum)), #추릴 기간 선정
                      selectInput("month4", "Period, to :", c(1:maximum), selected = 2),
                      selectInput("names", "Name : ", personname),
                      selectInput("value2", "Index :", index)
                    ),
                    
                    
                    mainPanel(
                      plotOutput('plot3'))
                    ))
))