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
                      selectInput("names", "Name : ", c("None", personname))
                    ),
                    
                    
                    mainPanel(
                      plotOutput('plot3'))
                    ))
))