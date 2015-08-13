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
                      selectInput("x", "Please Select x-axix", choices=c("age", "illness")),
                      selectInput("y", "indexes", index),
                      selectInput("color", "Seperation", c("None", "char")),
                      selectInput("from", "Period, from :", c(1:maximum)), #추릴 기간 선정
                      selectInput("to", "Period, to :", c(1:maximum))
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
                      selectInput("month2", "select month2", choices=c(1:maximum)),
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
                      selectInput("month3", "select month", choices=c(1:maximum)),
                      selectInput("month4", "select month", choices=c(1:maximum)),
                      selectInput("names", "Name : ", c("None", personname)),
                      selectInput("value2", "Index :", index)
                    ),
                    
                    
                    mainPanel(
                      plotOutput('plot3'))
                    ))
))