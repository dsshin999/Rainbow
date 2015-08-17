library(shiny)
library(ggplot2)
library(dplyr)
library(datasets)
library(RCurl)
library(RMySQL)
library(grid)

#서버에서 db를 utf8형식으로 읽기
con<-dbConnect(MySQL(), dbname="nursingsvc", username="admin", password="akstp123", host="puppet.rainbow-wireless.com", port=3336)
#con<-dbConnect(MySQL(), dbname="nursingsvc", username="admin", password="akstp123", host="192.168.1.131", port=3306)
dbGetQuery(con, "SET NAMES 'utf8'")

#지표
index <- c("eq5d", "stride", "pr","mr","vt","gh")

#사람 이름 추출하여 벡터로 저장
personname <- as.vector(dbGetQuery(con, "SELECT name FROM patient WHERE measuretime=1"))

#측정횟수 계산
maximum<-max(dbGetQuery(con, "SELECT measuretime FROM patient"))
