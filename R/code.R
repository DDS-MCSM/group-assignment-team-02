#' Title
#'
#' @details This are the details
#' @return
#' @export
#'
#' @examples
sample_function <- function() {
  print("Hello world")
}

if (!require(readr)) (install.packages("readr"))
library("readr")
if (!require(reshape2)) (install.packages("reshape2"))
library("reshape2")
if (!require(grid)) (install.packages("grid"))
library(grid)
if (!require(ggplot2)) (install.packages("ggplot2"))
library(ggplot2)
if (!require(tidyr)) (install.packages("tidyr"))
library(tidyr)
if (!require(dplyr)) (install.packages("dplyr"))
library(dplyr)
if (!require(plotly)) (install.packages("plotly"))
library(plotly)
if (!require(plyr)) (install.packages("plyr"))
library(plyr)
if (!require(lubridate)) (install.packages("lubridate"))
library(lubridate)
if (!require(grid)) (install.packages("grid"))
library(grid)
if (!require(gridExtra)) (install.packages("gridExtra"))
library(gridExtra)


#' Read CSV
#'
#' This function reads a CSV file
#'
#' @param
#' @return Data frame containing a representation of the data file without the last row
#' @export

downloadCSV <- function()
{
  ransomwareCSV <- read.csv2("https://ransomwaretracker.abuse.ch/feeds/csv", header = TRUE,skip = 8, check.names = TRUE, stringsAsFactors = TRUE,sep = ",", quote = '"')
  ransomwareCSV <- head(ransomwareCSV,-1)

  return (ransomwareCSV)
}


#' Change column name
#'
#' This function replaces in the dataframe (df) the name of the column (previous) with a new column name (new)
#'
#' @param df previous new
#' @return Dataframe with the new column name
#' @export

changeColumnName <- function(df,previous,new)
{
  colnames(df)[which(names(df) == previous)] <- new
  return (df)
}


#' Change date format
#'
#' This function changes the date format for the column "DateHour" of the "df" dataframe. The previous date format is "previous"; the new one is "new".
#' For exemple:
#' '%Y-%m-%d %H:%M:%S' to '%Y-%m-%d'
#'
#' @param df previous new
#' @return Data frame with the new date format in column DateHour
#' @export

changeDateFormat <- function(df,previous,new)
{
  df$date  <- format(as.POSIXct(df$DateHour,format=previous),format=new)
  return(df)
}


DeleteColumns <- function(df)
{
  df[4:5] <- list(NULL)
  df[5:7] <- list(NULL)
  return(df)
}


separateDate<-function(df)
{
  newColNames <- c("Date", "Hour")
  newCols <- colsplit(df$DateHour, " ", newColNames)
  rw <- cbind(df, newCols)
  return(rw)
}


SumColumnsMalwareTotal <- function(x)
{
  df<-data.frame(table(x$Malware),stringsAsFactors=FALSE)
  colnames(df) <- c("Malware","Freq")
  df <- subset(df, Freq!=0)
  df <- df[order(-df$Freq),]
  return(df)
}

SumColumnsMalware <- function(x)
{
  dfMax <- x[1:3,]

  #pct <- round(df$Freq/sum(df$Freq)*100,1)
  #pct <- sort.int(pct, decreasing = TRUE)
  #pctMax <- pct[1:3]

  #dfMax <- cbind(dfMax,pctMax=as.numeric(pctMax))

  column1 <- "Other"
  column2_total <- sum(x$Freq)
  column2_parcial <- sum(dfMax$Freq)
  column2_Other <- column2_total-column2_parcial

  #column3_parcial <- sum(dfMax$pctMax)
  #column3_Other <- 100-column3_parcial

  v <- list("Other",column2_Other)
  dfMax$Malware <- as.character(dfMax$Malware)
  dfMax <- rbind(dfMax,v)

  pct <- round(dfMax$Freq/sum(dfMax$Freq)*100,1)
  dfMax <- cbind(dfMax,pctMax=as.numeric(pct))

  return (dfMax)
}



# Plot Pie Chart
# Dibuja los tres Malware con más ocurrencias; los otros están integrados en "Other"
Plot_pie_Max<-function(x)
{
  dfMax <- x
  Malware <- dfMax$Malware


  plot_ly(dfMax, labels = ~Malware, values = ~pctMax, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~Freq,
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE) %>%
    layout(title = 'Ocurrencias Malware 2015-2018',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}

SemesterColumnsMalwareTotal <- function(x)
{
  newColNames <- c("Year", "Month", "Day")
  newCols <- colsplit(x$Date, "-", newColNames)
  a <- semester(x$Date, with_year = FALSE)
  xaux <- cbind(x, newCols)
  xaux <- cbind(xaux, Semester=a)


  temp1 <- filter(xaux, xaux$Malware %in% c("Locky","Cerber","TeslaCrypt"))

  temp_Locky <- filter(temp1, temp1$Malware =="Locky")
  temp_Cerber <- filter(temp1, temp1$Malware =="Cerber")
  temp_TeslaCrypt <- filter(temp1, temp1$Malware =="TeslaCrypt")


  temp_TeslaCrypt_1 <-temp_TeslaCrypt[order(-temp_TeslaCrypt$Year, -temp_TeslaCrypt$Semester),]
  df_Tesla<-data.frame(table(Malware=as.character(temp_TeslaCrypt_1$Malware),Semester=temp_TeslaCrypt_1$Semester,Year=temp_TeslaCrypt_1$Year),stringsAsFactors=FALSE)

  temp_Cerber_1 <-temp_Cerber[order(-temp_Cerber$Year, -temp_Cerber$Semester),]
  df_Cerber<-data.frame(table(Malware=as.character(temp_Cerber_1$Malware),Semester=temp_Cerber_1$Semester,Year=temp_Cerber_1$Year),stringsAsFactors=FALSE)

  temp_Locky_1 <-temp_Locky[order(-temp_Locky$Year, -temp_Locky$Semester),]
  df_Locky<-data.frame(table(Malware=as.character(temp_Locky_1$Malware),Semester=temp_Locky_1$Semester,Year=temp_Locky_1$Year),stringsAsFactors=FALSE)

  #Montamos un único dataframe con los tres parciales
  temp <- rbind(df_Tesla,df_Cerber)
  temp <- rbind(temp,df_Locky)

  temp <-temp[order(-temp$Year, -temp$Semester),]

  #Unimos las columnas Semester y Year para dibujar

  temp_plot<-unite(temp, Date,c(2:3),  sep = "-", remove = FALSE)

  return(temp_plot)
}




Bar_plot_semester_Locky <- function(x)
{
  temp_plot <- x
  temp_plot_Locky <- temp_plot
  temp_plot_Locky$Freq[temp_plot_Locky$Malware != "Locky"] <- 0
  Malware <- temp_plot_Locky$Malware
  Semestre <- as.character(temp_plot_Locky$Date)
  Semestre <- factor(Semestre, levels=unique(Semestre))
  Ocurrencias <-temp_plot_Locky$Freq

  plot_ly(temp_plot_Locky, x = ~Semestre, y = ~Ocurrencias, type = 'bar', name = 'Locky', marker = list(color = 'rgb(49,130,189)')) %>%
    layout(xaxis = list(title = "Semestre", tickangle = -45),
           yaxis = list(title = ""),
           margin = list(b = 100),
           barmode = 'group',
           title="LOCKY")

}

Bar_plot_semester_Cerber <- function(x)
{

  temp_plot_Cerber <- x
  temp_plot_Cerber$Freq[temp_plot_Cerber$Malware != "Cerber"] <- 0
  Malware <- temp_plot_Cerber$Malware
  Semestre <- as.character(temp_plot_Cerber$Date)
  Semestre <- factor(Semestre, levels=unique(Semestre))
  Ocurrencias <-temp_plot_Cerber$Freq

  plot_ly(temp_plot_Cerber, x = ~Semestre, y = ~Ocurrencias, type = 'bar', name = 'Cerber', marker = list(color = 'rgb(49,130,0)')) %>%
    layout(xaxis = list(title = "Semestre", tickangle = -45),
           yaxis = list(title = ""),
           margin = list(b = 100),
           barmode = 'group',
           title="CERBER")
}

Bar_plot_semester_Tesla <- function(x)
{

  temp_plot_Tesla <- x
  temp_plot_Tesla$Freq[temp_plot_Tesla$Malware != "TeslaCrypt"] <- 0
  Malware <- temp_plot_Tesla$Malware
  Semestre <- as.character(temp_plot_Tesla$Date)
  Semestre <- factor(Semestre, levels=unique(Semestre))
  Ocurrencias <-temp_plot_Tesla$Freq

  plot_ly(temp_plot_Tesla, x = ~Semestre, y = ~Ocurrencias, type = 'bar', name = 'Tesla', marker = list(color = 'rgb(130,30,0)')) %>%
    layout(xaxis = list(title = "Semestre", tickangle = -45),
           yaxis = list(title = ""),
           margin = list(b = 100),
           barmode = 'group',
           title="TESLACRYPT")
}


Bar_plot_year <- function(x,year)
{

  newColNames <- c("Year", "Month", "Day")
  newCols <- colsplit(x$Date, "-", newColNames)
  a <- semester(x$Date, with_year = FALSE)
  xaux <- cbind(x, newCols)
  xaux <- cbind(xaux, Semester=a)

  xaux <-xaux[order(-xaux$Year, -xaux$Semester),]
  xaux<-data.frame(table(Malware=as.character(xaux$Malware),Semester=xaux$Semester,Year=xaux$Year),stringsAsFactors=FALSE)

  #Montamos un único dataframe con los tres parciales
  xaux <-xaux[order(-xaux$Year, -xaux$Semester),]

  #Nos quedamos solo con las del último año (primer y segundo semestre 2018)
  xaux <- filter(xaux, xaux$Year==year)


  #Unimos las columnas Semester y Year para dibujar

  temp_plot<-unite(xaux, Date,c(2:3),  sep = "-", remove = FALSE)
  temp_plot <- subset(temp_plot, temp_plot$Freq>0)

  Malware <- temp_plot$Malware
  Semestre <- as.character(temp_plot$Date)
  Semestre <- factor(Semestre, levels=unique(Semestre))
  Ocurrencias <-temp_plot$Freq
  titulo <- paste("Year", year," ")


  plot_ly(temp_plot, x = ~Semestre, y = ~Ocurrencias, type = 'bar',split=~Malware) %>%
    layout(title = titulo)
}

Bar_plot_year_scale <- function(x,year,yMax)
{

  newColNames <- c("Year", "Month", "Day")
  newCols <- colsplit(x$Date, "-", newColNames)
  a <- semester(x$Date, with_year = FALSE)
  xaux <- cbind(x, newCols)
  xaux <- cbind(xaux, Semester=a)

  xaux <-xaux[order(-xaux$Year, -xaux$Semester),]
  xaux<-data.frame(table(Malware=as.character(xaux$Malware),Semester=xaux$Semester,Year=xaux$Year),stringsAsFactors=FALSE)

  #Montamos un único dataframe con los tres parciales
  xaux <-xaux[order(-xaux$Year, -xaux$Semester),]

  #Nos quedamos solo con las del último año (primer y segundo semestre 2018)
  xaux <- filter(xaux, xaux$Year==year)


  #Unimos las columnas Semester y Year para dibujar

  temp_plot<-unite(xaux, Date,c(2:3),  sep = "-", remove = FALSE)
  temp_plot <- subset(temp_plot, temp_plot$Freq>0)
  temp_plot <- subset(temp_plot, temp_plot$Freq<yMax)

  Malware <- temp_plot$Malware
  Semestre <- as.character(temp_plot$Date)
  Semestre <- factor(Semestre, levels=unique(Semestre))
  Ocurrencias <-temp_plot$Freq
  titulo <- paste("Year", year," ")


  plot_ly(temp_plot, x = ~Semestre, y = ~Ocurrencias, type = 'bar',split=~Malware) %>%
    layout(title = titulo) %>%
    layout(yaxis = list(range = c(0,yMax)))

}


dibjuar <- function()
{



  x <- rw
  newColNames <- c("Year", "Month", "Day")
  newCols <- colsplit(rw$Date, "-", newColNames)
  a <- semester(xaux$Date, with_year = FALSE)
  xaux <- cbind(x, newCols)
  xaux <- cbind(xaux, Semester=a)

  library(dplyr)
  temp1 <- filter(xaux, xaux$Malware %in% c("Locky","Cerber","TeslaCrypt"))

  temp_Locky <- filter(temp1, temp1$Malware =="Locky")
  temp_Cerber <- filter(temp1, temp1$Malware =="Cerber")
  temp_TeslaCrypt <- filter(temp1, temp1$Malware =="TeslaCrypt")

  library(plyr)
  temp_TeslaCrypt_1 <-temp_TeslaCrypt[order(-temp_TeslaCrypt$Year, -temp_TeslaCrypt$Semester),]
  df_Tesla<-data.frame(table(Malware=as.character(temp_TeslaCrypt_1$Malware),Semester=temp_TeslaCrypt_1$Semester,Year=temp_TeslaCrypt_1$Year),stringsAsFactors=FALSE)

  temp_Cerber_1 <-temp_Cerber[order(-temp_Cerber$Year, -temp_Cerber$Semester),]
  df_Cerber<-data.frame(table(Malware=as.character(temp_Cerber_1$Malware),Semester=temp_Cerber_1$Semester,Year=temp_Cerber_1$Year),stringsAsFactors=FALSE)

  temp_Locky_1 <-temp_Locky[order(-temp_Locky$Year, -temp_Locky$Semester),]
  df_Locky<-data.frame(table(Malware=as.character(temp_Locky_1$Malware),Semester=temp_Locky_1$Semester,Year=temp_Locky_1$Year),stringsAsFactors=FALSE)

   #Montamos un único dataframe con los tres parciales
  temp <- rbind(df_Tesla,df_Cerber)
  temp <- rbind(temp,df_Locky)

  temp <-temp[order(-temp$Year, -temp$Semester),]

  #Unimos las columnas Semester y Year para dibujar
  library(tidyr)
  temp_plot<-unite(temp, Date,c(2:3),  sep = "-", remove = FALSE)

  Malware <- temp_plot$Malware
  Semestre <- as.character(temp_plot$Date)
  Semestre <- factor(Semestre, levels=unique(Semestre))
  Ocurrencias <-temp_plot$Freq

  g = ggplot(temp_plot, aes(x=Semestre, y=temp_plot$Freq, fill=Malware) ) +
    labs(title = "Sin escala")+ylab("") +
    theme(plot.title = element_text(size = rel(1), colour = "darkblue"))
  g=g+geom_bar(position="dodge",stat="identity") + scale_fill_manual(values = alpha(c("orange", "blue","red"), 1)) +
    theme(axis.title.x = element_text(face="bold", size=10))

  g_escalado = ggplot(temp_plot, aes(x=Semestre, y=temp_plot$Freq, fill=Malware) ) +
    labs(title = "Escalado")+ylab("") +
    theme(plot.title = element_text(size = rel(1), colour = "darkblue"))
  g_escalado=g_escalado+geom_bar(position="dodge",stat="identity") + scale_fill_manual(values = alpha(c("orange", "blue","red"), 1)) +
    theme(axis.title.x = element_text(face="bold", size=10)) + coord_cartesian(ylim = c(0, 1500))

  library(grid)
 grid.arrange(g, g_escalado, nrow=1, ncol=2, top=textGrob("Nº Malware semestres 2015-2018", gp=gpar(col="darkblue",fontface="bold")))

}

#df <- downloadCSV()
#df <- DeleteColumns(df)
#df <- changeColumnName(df,"X..Firstseen..UTC.","DateHour")
#df <- separateDate(df)
#dfsum <- SumColumnsMalwareTotal(df)
#dfMax <- SumColumnsMalware(dfsum)
#dfSem <- SemesterColumnsMalwareTotal(df)


