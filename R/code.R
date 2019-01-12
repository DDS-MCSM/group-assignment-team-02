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


#' Delete columns
#'
#' This function deletes some columns of df dataframe:
#' (Host, URL,Registrar, IPaddress.es, ASN.s)
#'
#' @param df
#' @return Data frame without the deleted columns
#' @export
#'
DeleteColumns <- function(df)
{
  df[4:5] <- list(NULL)
  df[5:7] <- list(NULL)
  return(df)
}


#' Separate date columns
#'
#' This function separates the "DateHour" column of the df dataframe into new
#' columns "Date" and "Hour. The separator must be " "
#'
#' @param df
#' @return Dataframe with the two new columns: "Date" and "Hour"
#' @export
#'
separateDate<-function(df)
{
  newColNames <- c("Date", "Hour")
  newCols <- colsplit(df$DateHour, " ", newColNames)
  rw <- cbind(df, newCols)
  return(rw)
}


#' Sum the ocurrences of each Ransomware in 2015-2018
#'
#' This function sums the total occurrences during the period between 2015 and 2018, ordering
#' the result in descending order (by Frequency)
#'
#' @param df
#' @return Dataframe that relates the Malware with their total occurrences
#' @export
#'
SumColumnsMalwareTotal <- function(x)
{
  df<-data.frame(table(x$Malware),stringsAsFactors=FALSE)
  colnames(df) <- c("Malware","Freq")
  df <- subset(df, Freq!=0)
  df <- df[order(-df$Freq),]
  return(df)
}


#' Percentage of occurrences
#'
#' This function selects the first three malware (according to the number of occurrences)
#' and sums all the occurrences of the other nine malwares in a new row called "Other".
#' It also calculates the percentage of occurrences.
#'
#'
#' @param df
#' @return Dataframe showing the highest percentage of occurrences
#' @export
#'
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




#' Percentage of occurrences (Pie Chart)
#'
#' This function shows in a graphical way (Pie Chart) the dataframe calculated
#' by the function SumColumnsMalware (percentage of malware occurrences)
#'
#' @param df
#' @return Pie Chart showing the highest percentage of occurrences
#' @export
#'

Plot_pie_Max<-function(x)
{
  # Dibuja los tres Malware con más ocurrencias; los otros están integrados en "Other"
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


#' Malware occurrences per semester (2015-2018)
#'
#' This function lists the occurrences of each one of the principal malware (TeslaCrypt,
#' Cerber and Locky) for each semester during the years between 2015 and 2018
#'
#' @param df
#' @return Dataframe showing the occurrences of each malware (per semester)
#' @export
#'
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



#' Plot Locky occurrences (per semester)
#'
#' This function plots, from the dataframe calculated by the SemesterColumnsMalwareTotal function, the occurrences of Locky per semester (years [2015..2108])
#'
#' @param df
#' @return Bar plot showing the Locky occurrences (per semester)
#' @export
#'
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

#' Plot Ceber occurrences (per semester)
#'
#' This function plots, from the dataframe calculated by the SemesterColumnsMalwareTotal function, the occurrences of Ceber per semester (years [2015..2108])
#'
#' @param df
#' @return Bar plot showing the Ceber occurrences (per semester)
#' @export
#'
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

#' Plot TeslaCrypt occurrences (per semester)
#'
#' This function plots, from the dataframe calculated by the SemesterColumnsMalwareTotal function, the occurrences of TeslaCrypt per semester (years [2015..2108])
#'
#' @param df
#' @return Bar plot showing the TeslaCrypt occurrences (per semester)
#' @export
#'
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


#' Plot all the malware occurrences per year
#'
#' This function plots the occurrences of all the malware listed in the csv file during
#'  one year (shown by semester)
#'
#' @param df year
#' @return Bar plot showing the TeslaCrypt occurrences (per semester)
#' @export
#'
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

#' Plot all the malware occurrences per year (scaled)
#'
#' This function plots the occurrences of the malware listed in the csv file during one year
#' (shown by semester) whose number of occurrences are under the yMax value
#'
#' @param df year yMax
#' @return Bar plot showing the TeslaCrypt occurrences (per semester)
#' @export
#'
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

#' Delete columns country
#'
#' This function deletes some columns of df dataframe:
#' (Host, URL,Registrar, IPaddress.es)
#'
#' @param df
#' @return Data frame without the deleted columns
#' @export
#'
DeleteColumnsCountry <- function(df)
{
  df[4:9] <- list(NULL)
  return(df)
}

#' Dataframe by country
#'
#' This function arranges a dataframe (malware by country)
#'
#' @param df year
#' @return Dataframe
#' @export
#'
CountryMalwareTotal <- function(x,yearg)
{
  newColNames <- c("Year", "Month", "Day")
  newCols <- colsplit(x$Date, "-", newColNames)
  xaux <- cbind(x, newCols)

  xaux <- filter(xaux, xaux$Year==yearg)

  a <- xaux$Country
  a <- strsplit(as.character(a),split='|', fixed=TRUE)

  max.length <- max(sapply(a, length))
  ## Add NA values to list elements
  l <- lapply(a, function(v) { c(v, rep(NA, max.length-length(v)))})
  ## Rbind
  Country2 <- do.call(rbind, l)
  Country2 <- Country2[,1]
  xaux <-cbind(xaux, Country2)

  xaux[1:2] <- list(NULL)
  xaux[3:4] <- list(NULL)
  xaux[2] <- list(NULL)
  xaux[3:4] <- list(NULL)

  xaux <- subset(xaux, Country2!="")


  xaux2<-data.frame(table(Malware=as.character(xaux$Malware), Country=xaux$Country2),stringsAsFactors=FALSE)
  x <- aggregate(xaux2$Freq, by=list(xaux2$Country), sum)
  colnames(x) <- c("Country","SumMalware")

  pct <- round(x$SumMalware/sum(x$SumMalware)*100,1)
  dfMax <- cbind(x,pctMax=as.numeric(pct))

  dfMax <- dfMax[order(-dfMax$SumMalware),]
  dfMax <- dfMax[1:20,]
  row.has.na <- apply(dfMax, 1, function(x){any(is.na(x))})
  dfMax.filtered <- dfMax[!row.has.na,]
  dfMax.filtered <- dfMax.filtered[order(dfMax.filtered$pctMax),]

  return(dfMax.filtered)


  #plot_ly(x =dfMax$pctMax, y = as.character(dfMax$Country), type = 'bar', orientation = 'h')

}

#' Plot all the malware occurrences per country
#'
#' This function plots the occurrences of all the malware listed in the csv file per
#' country
#'
#' @param df year
#' @return Bar plot showing the ranswomware by country
#' @export
#'

Plot_bar_horizontal <- function(x,yg)
{
  f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "orange"
  )
  b <- list(
    title = "PAIS",
    titlefont = f1
  )
  a <- list(
    title = "PORCENTAJE",
    titlefont = f1
  )

  xform <- list(categoryorder = "array",
                categoryarray = as.character(x$Country))

  temp.Grade <- as.vector(x$Country) #get rid of factors
  temp.Grade = factor(temp.Grade,temp.Grade) #add ordered factors back

  plot_ly(x =x$pctMax, y = temp.Grade, type = 'bar', orientation = 'h') %>%
    layout(xaxis = a, yaxis = b, showlegend = FALSE, xaxis = xform)%>%
    layout(title = paste('MALWARE ', yg ,sep = " "))

}

Prueba <- function(x,country)
{
  x <- df
  country <- "US"
  newColNames <- c("Year", "Month", "Day")
  newCols <- colsplit(x$Date, "-", newColNames)
  xaux <- cbind(x, newCols)



  a <- xaux$Country
  a <- strsplit(as.character(a),split='|', fixed=TRUE)

  max.length <- max(sapply(a, length))
  ## Add NA values to list elements
  l <- lapply(a, function(v) { c(v, rep(NA, max.length-length(v)))})
  ## Rbind
  Country2 <- do.call(rbind, l)
  Country2 <- Country2[,1]
  xaux <-cbind(xaux, Country2)


  xaux[1:2] <- list(NULL)
  xaux[3:4] <- list(NULL)
  xaux[2] <- list(NULL)
  xaux[3:4] <- list(NULL)


  x<-data.frame(table(Malware=as.character(xaux$Malware), Country=xaux$Country2,Year=xaux$Year), stringsAsFactors=FALSE)

  colnames(x) <- c("Malware","Country","Year")



  row.has.na <- apply(x, 1, function(x){any(is.na(x))})
  xaux <- x[!row.has.na,]

 # plot_ly(x =x$Year, y = x$MAlware, type = 'bar')

  x <- aggregate(xaux$Freq, by=list(xaux$Malware), sum)
  x <- filter(x, x$Year=="2016")

  install.packages("rworldmap")
  library(rworldmap)

  mapped_data <- joinCountryData2Map(dfMax, joinCode = "ISO2",
                                     nameJoinColumn = "Country")
  par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
  mapCountryData(mapped_data, nameColumnToPlot = "Malware")

  return(dfMax.filtered)


  #plot_ly(x =dfMax$pctMax, y = as.character(dfMax$Country), type = 'bar', orientation = 'h')

}




#df <- downloadCSV()
#df <- DeleteColumnsCountry(df)
#df <- changeColumnName(df,"X..Firstseen..UTC.","DateHour")
#df <- separateDate(df)
#df1 <- CountryMalwareTotal(df,"2015")
#Plot_bar_horizontal(df1,"2015")
#df2 <- CountryMalwareTotal(df,"2016")
#Plot_bar_horizontal(df2,"2015")
#CountryMalwareTotal(df,2017)
#CountryMalwareTotal(df,2018)
#dfsum <- SumColumnsMalwareTotal(df)
#dfMax <- SumColumnsMalware(dfsum)
#dfSem <- SemesterColumnsMalwareTotal(df)


